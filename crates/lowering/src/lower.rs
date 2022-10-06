use defs::ids::{FreeFunctionId, LanguageElementId};
use diagnostics::Diagnostics;
use id_arena::Arena;
use itertools::{chain, zip_eq};
use semantic::corelib::unit_ty;
use semantic::db::SemanticGroup;
use utils::extract_matches;
use utils::ordered_hash_map::OrderedHashMap;
use utils::ordered_hash_set::OrderedHashSet;
use utils::unordered_hash_map::UnorderedHashMap;

use crate::diagnostic::LoweringDiagnosticKind::*;
use crate::diagnostic::{LoweringDiagnostic, LoweringDiagnostics};
use crate::objects::{
    Block, BlockEnd, BlockEndCallsite, BlockEndReturn, BlockId, Statement, StatementCall,
    StatementCallBlock, StatementLiteral, StatementTupleConstruct, StatementTupleDestruct,
    Variable, VariableId,
};

// TODO(spapini): Remove.
#[allow(dead_code)]
/// Context for lowering a function.
pub struct Lowerer<'db> {
    db: &'db dyn SemanticGroup,
    /// Semantic model for current function definition.
    function_def: &'db semantic::FreeFunctionDefinition,
    /// Current emitted diagnostics.
    diagnostics: LoweringDiagnostics,
    /// Arena of allocated lowered variables.
    variables: Arena<Variable>,
    /// Arena of allocated lowered blocks.
    blocks: Arena<Block>,
    semantic_defs: UnorderedHashMap<semantic::VarId, semantic::Variable>,
}

/// A lowered function code.
pub struct Lowered {
    /// Diagnostics produced while lowering.
    pub diagnostics: Diagnostics<LoweringDiagnostic>,
    /// Block id for the start of the lwoered function.
    pub root: Option<BlockId>,
    /// Arena of allocated lowered variables.
    pub variables: Arena<Variable>,
    /// Arena of allocated lowered blocks.
    pub blocks: Arena<Block>,
}

/// During lowering, a block has 3 states:
/// 1. BlockDuring: During lowering of its statements.
/// 2. BlockSealed: After lowering of its statements, before determining its final pulls, inputs
/// and outputs.
/// 3. Block: Lowering is done.
/// Scope of a block, describing its current state.
#[derive(Default)]
pub struct BlockScope {
    /// Living variables owned by this scope.
    living_variables: OrderedHashSet<VariableId>,
    /// Mapping from a semantic variable to its lowered variable in the current scope.
    /// Note: there might be lowered variables that did not originate from a semantic variable.
    /// Note: a semantic variable might have different variables in different scopes, and even
    /// in the same scope the mapped variable might change (e.g., assignment statement).
    semantic_variables: OrderedHashMap<semantic::VarId, VariableId>,
    /// Current sequence of lowered statements emitted.
    statements: Vec<Statement>,
    /// Semantic variables used inside the current block, and need to be moved from higher scopes.
    /// Note: Can't use semantic_variables, because it's changing.
    pulled_semantic_variables: OrderedHashMap<semantic::VarId, VariableId>,
}
impl BlockScope {
    /// Adds a lowered statement to the block.
    pub fn add_statement(&mut self, stmt: Statement) {
        self.statements.push(stmt);
    }
    /// Allocates a new variable by type.
    pub fn new_variable(&mut self, lowerer: &mut Lowerer, ty: semantic::TypeId) -> VariableId {
        let var_id = lowerer.variables.alloc(Variable { duplicatable: true, droppable: true, ty });
        self.living_variables.insert(var_id);
        var_id
    }
    /// Takes a variable (i.e. moving/consuming it).
    fn take(&mut self, lowerer: &mut Lowerer, var_id: VariableId) -> Option<VariableId> {
        let var = lowerer.variables.get(var_id).unwrap();
        if var.duplicatable {
            return if self.living_variables.contains(&var_id) { Some(var_id) } else { None };
        }
        if !self.living_variables.swap_remove(&var_id) {
            return None;
        }
        Some(var_id)
    }
    /// Fetches the lowered variable for a semantic variable in this context. If not found, pulls
    /// it from parent block.
    pub fn fetch_or_pull(
        &mut self,
        lowerer: &mut Lowerer,
        semantic_var_id: semantic::VarId,
    ) -> VariableId {
        if let Some(var_id) = self.semantic_variables.get(&semantic_var_id) {
            return *var_id;
        }
        self.pull_semantic_variable(lowerer, semantic_var_id)
    }
    /// Pulls a semantic variable from parent block.
    pub fn pull_semantic_variable(
        &mut self,
        lowerer: &mut Lowerer,
        semantic_var_id: semantic::VarId,
    ) -> id_arena::Id<Variable> {
        let ty = lowerer.semantic_defs[semantic_var_id].ty();
        let var_id = self.new_variable(lowerer, ty);
        let var_id = self.set_semantic_variable(semantic_var_id, var_id);
        self.pulled_semantic_variables.insert(semantic_var_id, var_id);
        var_id
    }
    pub fn set_semantic_variable(
        &mut self,
        semantic_var_id: semantic::VarId,
        var_id: VariableId,
    ) -> VariableId {
        self.semantic_variables.insert(semantic_var_id, var_id);
        var_id
    }
    fn take_semantic(
        &mut self,
        lowerer: &mut Lowerer,
        semantic_var: semantic::VarId,
    ) -> Option<VariableId> {
        let var_id = self.semantic_variables.get(&semantic_var)?;
        self.take(lowerer, *var_id)
    }
    pub fn seal(self, end: BlockSealedEnd) -> BlockSealed {
        BlockSealed { block: self, end }
    }

    fn destruct_all(&mut self, lowerer: &mut Lowerer) -> Option<Vec<VariableId>> {
        let mut drop_variables = Vec::new();
        for var_id in self.living_variables.clone() {
            if !lowerer.variables[var_id].droppable {
                // TODO(spapini): Find destructor.
                lowerer.diagnostics.report(todo!(), DestructNotFound);
                return None;
            }
            self.take(lowerer, var_id);
        }
        Some(drop_variables)
    }
}

pub enum BlockSealedEnd {
    Callsite(Option<VariableId>),
    Return(Vec<VariableId>),
    Unreachable,
}
pub struct BlockSealed {
    block: BlockScope,
    end: BlockSealedEnd,
}
impl BlockSealed {
    pub fn push_candidates(&self) -> OrderedHashSet<semantic::VarId> {
        // The candidates to push are all the living pulled variables.
        self.block
            .pulled_semantic_variables
            .iter()
            .filter(|(_, var_id)| self.block.living_variables.contains(*var_id))
            .map(|(semantic_var_id, _)| *semantic_var_id)
            .collect()
    }
    pub fn pulled_semantic_variables(&self) -> OrderedHashSet<semantic::VarId> {
        self.block.pulled_semantic_variables.keys().copied().collect()
    }
    fn fetch_or_pull(
        &mut self,
        lowerer: &mut Lowerer,
        semantic_var_id: semantic::VarId,
    ) -> VariableId {
        self.block.fetch_or_pull(lowerer, semantic_var_id)
    }
    /// Note: inputs must contain pulled_semantic_variables.
    /// The inputs will be composed of original inputs given at new(), and pulls.
    /// The caller should have the VariableId for all the inputs.
    /// Their order is determined here.
    /// The consumer needs to pass outputs on callsites.
    pub fn finalize(
        self,
        lowerer: &mut Lowerer,
        inputs_semantic_vars: &[semantic::VarId],
        vars_to_push: &OrderedHashSet<semantic::VarId>,
    ) -> Option<(Block, BlockEndInfo)> {
        let Self { mut block, end } = self;

        // Pull all required inputs.
        let input_semantic_vars: OrderedHashSet<_> =
            inputs_semantic_vars.iter().map(|semantic_var| semantic_var).collect();
        let inputs = inputs_semantic_vars
            .iter()
            .map(|semantic_var| block.fetch_or_pull(lowerer, *semantic_var))
            .collect();

        // Make sure inputs contains pulled_semantic_variables.
        for pulled_semantic_var in block.pulled_semantic_variables.keys() {
            if !input_semantic_vars.contains(pulled_semantic_var) {
                lowerer.diagnostics.report(todo!(), VariableNotFound { semantic_var: todo!() });
                return None;
            }
        }

        // Add pushed variables to the callsite output.
        let (end, end_info) = match end {
            BlockSealedEnd::Callsite(maybe_output) => {
                // Take pushed semantic variables.
                let pushes = vars_to_push
                    .iter()
                    .map(|semantic_var| {
                        block
                            .take_semantic(lowerer, *semantic_var)
                            .expect("Asked to push a dead variable")
                    })
                    .collect();

                let outputs = chain!(&maybe_output, &pushes).copied().collect();
                let drops = block.destruct_all(lowerer)?;
                (
                    BlockEnd::Callsite(BlockEndCallsite { outputs, drops }),
                    BlockEndInfo::Outputs { maybe_output, pushes },
                )
            }
            BlockSealedEnd::Return(returns) => {
                let drops = block.destruct_all(lowerer)?;
                (BlockEnd::Return(BlockEndReturn { returns, drops }), BlockEndInfo::Unreachable)
            }
            BlockSealedEnd::Unreachable => (BlockEnd::Unreachable, BlockEndInfo::Unreachable),
        };
        Some((Block { inputs, statements: block.statements, end }, end_info))
    }
}
pub enum BlockEndInfo {
    Outputs { maybe_output: Option<VariableId>, pushes: Vec<VariableId> },
    Unreachable,
}

impl<'db> Lowerer<'db> {
    /// Lowers a semantic free function.
    pub fn lower(db: &dyn SemanticGroup, free_function_id: FreeFunctionId) -> Option<Lowered> {
        let function_def = db.free_function_definition(free_function_id)?;
        let mut lowerer = Lowerer {
            db,
            function_def: &*function_def,
            diagnostics: LoweringDiagnostics::new(free_function_id.module(db.upcast())),
            variables: Arena::default(),
            blocks: Arena::default(),
            semantic_defs: UnorderedHashMap::default(),
        };

        let signature = db.free_function_declaration_signature(free_function_id)?;

        // Params.
        let inputs_semantic_vars: Vec<_> =
            signature.params.iter().map(|param| semantic::Variable::Param(param.clone())).collect();
        let inputs_semantic_var_ids: Vec<_> =
            inputs_semantic_vars.iter().map(|semantic_var| semantic_var.id()).collect();
        for semantic_var in inputs_semantic_vars {
            lowerer.semantic_defs.insert(semantic_var.id(), semantic_var);
        }

        // Fetch body block expr.
        let semantic_block =
            extract_matches!(&function_def.exprs[function_def.body], semantic::Expr::Block);
        // Lower block to a BlockSealed.
        let sealed_block = if let Some(sealed_block) = lowerer.lower_block(&semantic_block) {
            sealed_block
        } else {
            let Lowerer { diagnostics, variables, blocks, .. } = lowerer;
            return Some(Lowered {
                diagnostics: diagnostics.build(),
                root: None,
                variables,
                blocks,
            });
        };
        // Finalize using the real input => function parameters.
        let (final_block, _) = sealed_block.finalize(
            &mut lowerer,
            &inputs_semantic_var_ids,
            &OrderedHashSet::default(),
        )?;
        let root = Some(lowerer.blocks.alloc(final_block));

        let Lowerer { diagnostics, variables, blocks, .. } = lowerer;
        Some(Lowered { diagnostics: diagnostics.build(), root, variables, blocks })
    }

    // Lowers a semantic block.
    fn lower_block<'a: 'b, 'b>(&mut self, expr_block: &semantic::ExprBlock) -> Option<BlockSealed> {
        let mut scope = BlockScope::default();
        for (i, stmt_id) in expr_block.statements.iter().enumerate() {
            let stmt = &self.function_def.statements[*stmt_id];
            let lowered_stmt = self.lower_statement(&mut scope, stmt);

            // If flow is not reachable anymore, no need to continue emitting statements.
            match lowered_stmt {
                Ok(()) => {}
                Err(StatementLoweringFlowError::Failed) => {
                    return None;
                }
                Err(StatementLoweringFlowError::End(end)) => {
                    if i + 1 < expr_block.statements.len() {
                        let start_stmt =
                            &self.function_def.statements[expr_block.statements[i + 1]];
                        let end_stmt =
                            &self.function_def.statements[*expr_block.statements.last().unwrap()];
                        // Emit diagnostic fo the rest of the statements with unreachable.
                        self.diagnostics.report(
                            start_stmt.stable_ptr().untyped(),
                            Unreachable { last_statement_ptr: end_stmt.stable_ptr().untyped() },
                        );
                        return Some(scope.seal(end));
                    }
                }
            };
        }

        // Determine correct block end.
        let end = match expr_block.tail {
            Some(tail_expr) => {
                let lowered_expr = self.lower_expr(&mut scope, tail_expr);
                match lowered_expr {
                    Ok(LoweredExpr::AtVariable(var_id)) => BlockSealedEnd::Callsite(Some(var_id)),
                    Ok(LoweredExpr::Unit) => BlockSealedEnd::Callsite(None),
                    Err(LoweringFlowError::Unreachable) => BlockSealedEnd::Unreachable,
                    Err(LoweringFlowError::Failed) => {
                        return None;
                    }
                }
            }
            None => BlockSealedEnd::Callsite(None),
        };
        Some(scope.seal(end))
    }

    /// Lowers a semantic statement.
    pub fn lower_statement(
        &mut self,
        scope: &mut BlockScope,
        stmt: &semantic::Statement,
    ) -> Result<(), StatementLoweringFlowError> {
        match stmt {
            semantic::Statement::Expr(semantic::StatementExpr { expr, stable_ptr: _ }) => {
                self.lower_expr(scope, *expr)?;
            }
            semantic::Statement::Let(semantic::StatementLet { pattern, expr, stable_ptr: _ }) => {
                let lowered_expr = self.lower_expr(scope, *expr)?;
                match lowered_expr {
                    LoweredExpr::AtVariable(var_id) => {
                        self.lower_single_pattern(scope, pattern, var_id)
                    }
                    LoweredExpr::Unit => {
                        // TODO(spapini): Introduce a unit value using TupleConstruct.
                    }
                }
                let var_id = self.lower_expr(scope, *expr);
            }
            semantic::Statement::Return(semantic::StatementReturn { expr, stable_ptr }) => {
                let lowered_expr = self.lower_expr(scope, *expr)?;
                match lowered_expr {
                    LoweredExpr::AtVariable(var_id) => {
                        // TODO(spapini): Implicits and mutables.
                        return Err(StatementLoweringFlowError::End(BlockSealedEnd::Return(vec![
                            var_id,
                        ])));
                    }
                    LoweredExpr::Unit => {
                        // TODO(spapini): Implicits and mutables.
                        return Err(StatementLoweringFlowError::End(BlockSealedEnd::Return(
                            vec![],
                        )));
                    }
                }
            }
        }
        Ok(())
    }

    // TODO:(spapini): Separate match pattern from non-match (single) patterns in the semantic
    // model.
    /// Lowers a single-pattern (pattern that does not appear in a match. This includes structs,
    /// tuples, variables, etc...
    /// Adds the bound variables to the scope.
    /// Note that single patterns are the only way to bind new local variables in the semantic
    /// model.
    fn lower_single_pattern(
        &mut self,
        scope: &mut BlockScope,
        pattern: &semantic::Pattern,
        var_id: id_arena::Id<Variable>,
    ) {
        match pattern {
            semantic::Pattern::Literal(_) => unreachable!(),
            semantic::Pattern::Variable(semantic::PatternVariable { name: _, var: sem_var }) => {
                assert_eq!(self.variables[var_id].ty, sem_var.ty, "Wrong type.");
                let sem_var = semantic::Variable::Local(sem_var.clone());
                scope.set_semantic_variable(sem_var.id(), var_id);
                self.semantic_defs.insert(sem_var.id(), sem_var);
            }
            semantic::Pattern::Struct(_) => todo!(),
            semantic::Pattern::Tuple(semantic::PatternTuple { field_patterns, ty }) => {
                let tys = if let semantic::TypeLongId::Tuple(tys) = self.db.lookup_intern_type(*ty)
                {
                    tys
                } else {
                    panic!("Expected a tuple type.")
                };
                assert_eq!(
                    tys.len(),
                    field_patterns.len(),
                    "Expected the same number of tuple args."
                );
                let outputs: Vec<_> = tys.iter().map(|ty| scope.new_variable(self, *ty)).collect();
                let stmt = Statement::TupleDestruct(StatementTupleDestruct {
                    tys,
                    input: scope.take(self, var_id).unwrap(),
                    outputs: outputs.clone(),
                });
                scope.add_statement(stmt);
                for (var_id, pattern) in zip_eq(outputs, field_patterns) {
                    self.lower_single_pattern(scope, pattern, var_id);
                }
            }
            semantic::Pattern::Enum(_) => unreachable!(),
            semantic::Pattern::Otherwise(_) => {}
        }
    }

    /// Lowers a semantic expression.
    fn lower_expr<'a: 'b, 'b>(
        &mut self,
        scope: &'b mut BlockScope,
        expr_id: semantic::ExprId,
    ) -> Result<LoweredExpr, LoweringFlowError> {
        let expr = &self.function_def.exprs[expr_id];
        Ok(match expr {
            semantic::Expr::Tuple(_) => {
                LoweredExpr::AtVariable(scope.new_variable(self, expr.ty()))
            }
            semantic::Expr::Assignment(_) => todo!(),
            semantic::Expr::Block(expr) => {
                // Need to detect which variables are moved to the block's scope, and should be
                // passed as inputs.
                let sealed_block = self.lower_block(expr).ok_or(LoweringFlowError::Failed)?;
                // Get the required semantic variable ids for the block.
                // Pass the to the function in the same (arbitrary) order.
                // Note: Match lowering might choose to pass more inputs in their blocks.
                let inputs_semantic_var_ids: Vec<_> =
                    sealed_block.pulled_semantic_variables().into_iter().collect();
                // Get the pushed variables. Since some branches may consume the pulled variables,
                // the other branches need to conform and not push them either.
                // Intersect all push candidates.
                let vars_to_push = sealed_block.push_candidates();
                let (block, end_info) = sealed_block
                    .finalize(self, &inputs_semantic_var_ids, &vars_to_push)
                    .ok_or(LoweringFlowError::Failed)?;

                // Fetch the variable id or pull from parent block for each one.
                let inputs: Vec<_> = inputs_semantic_var_ids
                    .iter()
                    .map(|semantic_var_id| scope.fetch_or_pull(self, *semantic_var_id))
                    .collect();

                // TODO: In unreachable cases, we need to propagate unreachability somehow.
                let (binds, res) = match end_info {
                    BlockEndInfo::Outputs { maybe_output, pushes } => {
                        // Bind the outputs of the block locally.
                        // If there is a tail expression, get it.
                        let maybe_output = maybe_output.map(|_| scope.new_variable(self, expr.ty));
                        // Rebind the other outputs locally.
                        assert_eq!(pushes.len(), vars_to_push.len(), "");
                        let pushes = vars_to_push.into_iter().map(|semantic_var_id| {
                            let ty = self.semantic_defs[semantic_var_id].ty();
                            let new_var_id = scope.new_variable(self, ty);
                            let current_var_id =
                                scope.semantic_variables.get_mut(&semantic_var_id).unwrap();
                            *current_var_id = new_var_id;
                            *current_var_id
                        });
                        let binds = chain!(maybe_output, pushes).collect();
                        let res = if let Some(output) = maybe_output {
                            LoweredExpr::AtVariable(output)
                        } else {
                            LoweredExpr::Unit
                        };
                        (binds, Ok(res))
                    }
                    BlockEndInfo::Unreachable => (vec![], Err(LoweringFlowError::Unreachable)),
                };
                scope.add_statement(Statement::CallBlock(StatementCallBlock {
                    block: self.blocks.alloc(block),
                    inputs,
                    outputs: binds,
                }));
                res?
            }
            semantic::Expr::FunctionCall(expr) => {
                let inputs = self.lower_exprs_as_vars(expr, scope)?;

                // Allocate a new variable for the result of the function.
                let result_var = scope.new_variable(self, expr.ty);
                let outputs = vec![result_var];
                scope.add_statement(Statement::Call(StatementCall {
                    function: expr.function,
                    inputs,
                    outputs,
                }));
                LoweredExpr::AtVariable(result_var)
            }
            semantic::Expr::Match(_) => todo!(),
            semantic::Expr::Var(expr) => {
                // TODO(spapini): Differentiate between a TakenVar and a Var.
                let var_id = scope.fetch_or_pull(self, expr.var);
                LoweredExpr::AtVariable(scope.take(self, var_id).unwrap())
            }
            semantic::Expr::Literal(expr) => {
                let output = scope.new_variable(self, expr.ty);
                scope
                    .statements
                    .push(Statement::Literal(StatementLiteral { value: expr.value, output }));
                LoweredExpr::AtVariable(output)
            }
            semantic::Expr::MemberAccess(_) => todo!(),
            semantic::Expr::StructCtor(_) => todo!(),
            semantic::Expr::EnumVariantCtor(_) => todo!(),
            semantic::Expr::Missing(_) => return Err(LoweringFlowError::Failed),
        })
    }

    fn lower_exprs_as_vars(
        &mut self,
        expr: &semantic::ExprFunctionCall,
        scope: &mut BlockScope,
    ) -> Result<Vec<id_arena::Id<Variable>>, LoweringFlowError> {
        let inputs = expr
            .args
            .iter()
            .map(|arg_expr_id| Ok(self.lower_expr(scope, *arg_expr_id)?.to_var(self, scope)))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(inputs)
    }

    fn unit_var(&mut self, scope: &mut BlockScope) -> VariableId {
        let var_id = scope.new_variable(self, unit_ty(self.db));
        scope.add_statement(Statement::TupleConstruct(StatementTupleConstruct {
            inputs: vec![],
            output: var_id,
        }));
        var_id
    }
}

enum LoweredExpr {
    AtVariable(VariableId),
    Unit,
}
impl LoweredExpr {
    fn to_var(&self, lowerer: &mut Lowerer, scope: &mut BlockScope) -> VariableId {
        match self {
            LoweredExpr::AtVariable(var_id) => *var_id,
            LoweredExpr::Unit => lowerer.unit_var(scope),
        }
    }
}
enum LoweringFlowError {
    Failed,
    Unreachable,
}
pub enum StatementLoweringFlowError {
    Failed,
    End(BlockSealedEnd),
}
impl From<LoweringFlowError> for StatementLoweringFlowError {
    fn from(err: LoweringFlowError) -> Self {
        match err {
            LoweringFlowError::Failed => StatementLoweringFlowError::Failed,
            LoweringFlowError::Unreachable => {
                StatementLoweringFlowError::End(BlockSealedEnd::Unreachable)
            }
        }
    }
}
