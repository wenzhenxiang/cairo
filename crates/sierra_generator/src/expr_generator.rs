use std::collections::HashMap;

use defs::ids::{LocalVarId, VarId};
use semantic::db::SemanticGroup;
use semantic::ids::ExprId;
use semantic::semantic::{Expr, ExprMatch, MatchBranch, Pattern, Statement};

use crate::id_allocator::IdAllocator;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SierraVariable(String);
impl From<&str> for SierraVariable {
    fn from(name: &str) -> Self {
        SierraVariable(name.into())
    }
}

pub struct ExprGenerator<'a> {
    pub db: &'a dyn SemanticGroup,
    // TODO(lior): Should id_allocator be a reference or a value?
    pub id_allocator: IdAllocator,
    pub variables: &'a mut HashMap<LocalVarId, SierraVariable>,
}

impl<'a> ExprGenerator<'a> {
    // pub fn new(db: &dyn SemanticGroup) -> Self {
    //     return ExprGenerator { db, id_allocator: IdAllocator::default(), variables: };
    // }

    // Generates Sierra code that computes a given expression.
    pub fn generate_expression_code(&mut self, x: ExprId) -> (Vec<String>, SierraVariable) {
        self.generate_expression_code_by_val(&self.db.lookup_expr(x))
    }

    fn generate_expression_code_by_val(&mut self, x: &Expr) -> (Vec<String>, SierraVariable) {
        match x {
            Expr::ExprBlock(expr_block) => {
                let mut instructions: Vec<String> = vec![];
                for statement in &expr_block.statements {
                    match statement {
                        Statement::Expr(expr) => {
                            let (statement_instructions, _res) =
                                self.generate_expression_code(*expr);
                            instructions.extend(statement_instructions);
                        }
                        Statement::Let(statement_let) => {
                            let (statement_instructions, res) =
                                self.generate_expression_code(statement_let.expr);
                            instructions.extend(statement_instructions);
                            // TODO(lior): Make sure the variable was not set before.
                            self.variables.insert(statement_let.var, res);
                        }
                    }
                }
                // TODO: find a way to reduce code duplication.
                match expr_block.tail {
                    Some(expr_id) => {
                        let (tail_instructions, output_var) =
                            self.generate_expression_code(expr_id);
                        instructions.extend(tail_instructions);
                        return (instructions, output_var);
                    }
                    None => {
                        todo!();
                        // return (instructions, SierraVariable("UNIT".into()));
                    }
                }
            }
            Expr::ExprFunctionCall(expr_function_call) => {
                let mut instructions: Vec<String> = vec![];

                // Prepare the arguments.
                let mut args: Vec<SierraVariable> = vec![];
                for arg in &expr_function_call.args {
                    let (arg_instructions, res) = self.generate_expression_code(*arg);
                    instructions.extend(arg_instructions);
                    args.push(res);
                }

                let mut args_on_stack: Vec<String> = vec![];
                for arg_res in args {
                    let arg_var = self.id_allocator.allocate("arg");
                    instructions.push(format!("store_temp({}) -> ({});", arg_res.0, arg_var));
                    args_on_stack.push(arg_var);
                }

                // TODO: allocate_variable.
                let tmp_var = self.id_allocator.allocate("tmp");
                instructions.push(format!("func({}) -> ({});", args_on_stack.join(", "), tmp_var));
                return (instructions, SierraVariable(tmp_var));
            }
            Expr::ExprMatch(expr_match) => {
                return self.handle_felt_match(expr_match);
            }
            Expr::ExprVar(expr_var) => match expr_var.var {
                VarId::Param(_) => todo!(),
                VarId::Local(local_var) => {
                    return (vec![], self.variables[&local_var].clone());
                }
            },
            Expr::ExprLiteral(expr_literal) => {
                let tmp_var = self.id_allocator.allocate("literal");
                return (
                    vec![format!("literal<{}>() -> ({});", expr_literal.value, tmp_var)],
                    SierraVariable(tmp_var),
                );
            }
        }
    }

    fn handle_felt_match(&mut self, expr_match: &ExprMatch) -> (Vec<String>, SierraVariable) {
        let branches = &expr_match.branches;
        match &branches[..] {
            [
                MatchBranch { pattern: Pattern::Expr(expr), block: block0 },
                MatchBranch { pattern: Pattern::Otherwise, block: block_otherwise },
            ] => {
                let expr_literal =
                    if let Expr::ExprLiteral(expr_literal) = self.db.lookup_expr(*expr) {
                        expr_literal
                    } else {
                        unimplemented!();
                    };

                // TOOD(lior): Replace with diagnostics.
                assert_eq!(expr_literal.value, 0);

                let mut instructions: Vec<String> = vec![];

                // Generate instructions for the matched expression.
                let (match_expr_instructions, match_expr_res) =
                    self.generate_expression_code(expr_match.expr);
                instructions.extend(match_expr_instructions);

                // TODO(lior): Replace "???" with statementId.
                instructions
                    .push(format!("match_zero({}) -> {{ ???, fallthrough }};", match_expr_res.0));

                // TODO(lior): Don't use the same variable manager.
                // TODO(lior): Fix block0.clone().
                let (block0_instructions, block0_res) =
                    self.generate_expression_code_by_val(&Expr::ExprBlock(block0.clone()));
                instructions.extend(block0_instructions);
                // TODO(lior): Replace "???" with statementId.
                // TODO(lior): Fix block_otherwise.clone().
                let output_var = self.id_allocator.allocate("match_res");
                instructions.push(format!("store_temp({}) -> ({});", block0_res.0, output_var));
                instructions.push("jump ???;".into());

                let (block_otherwise_instructions, block_otherwise_res) =
                    self.generate_expression_code_by_val(&Expr::ExprBlock(block_otherwise.clone()));
                instructions.extend(block_otherwise_instructions);
                instructions
                    .push(format!("store_temp({}) -> ({});", block_otherwise_res.0, output_var));
                return (instructions, SierraVariable(output_var));
            }
            _ => {
                // TOOD(lior): Replace with diagnostics.
                unimplemented!();
            }
        }
    }
}
