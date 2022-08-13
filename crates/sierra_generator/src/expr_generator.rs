use std::collections::HashMap;

use crate::id_allocator::IdAllocator;
use defs::ids::{LocalVarId, VarId};
use semantic::db::SemanticGroup;
use semantic::ids::ExprId;
use semantic::semantic::{Expr, ExprMatch, MatchBranch, Pattern, Statement};

pub struct ExprGenerator<'a> {
    pub db: &'a dyn SemanticGroup,
    // TODO(lior): Should id_allocator be a reference or a value?
    pub id_allocator: &'a mut IdAllocator,
    pub variables: &'a mut HashMap<LocalVarId, String>,
}

impl<'a> ExprGenerator<'a> {
    // pub fn create(db: &dyn SemanticGroup) -> Self {
    //     return ExprGenerator { db, id_allocator: IdAllocator::default() };
    // }

    // Generates Sierra code that computes a given expression.
    pub fn generate_expression_code(&mut self, x: ExprId) -> (Vec<String>, String) {
        self.generate_expression_code_by_val(&self.db.lookup_expr(x))
    }

    fn generate_expression_code_by_val(&mut self, x: &Expr) -> (Vec<String>, String) {
        match x {
            Expr::ExprBlock(expr_block) => {
                let mut instructions: Vec<String> = vec![];
                for statement in &expr_block.statements {
                    match statement {
                        Statement::Expr(expr) => {
                            let (statement_instructions, _) = self.generate_expression_code(*expr);
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
                    None => todo!(),
                }
            }
            Expr::ExprFunctionCall(expr_function_call) => {
                let mut instructions: Vec<String> = vec![];

                // Prepare the arguments.
                let mut args: Vec<String> = vec![];
                for arg in &expr_function_call.args {
                    let (arg_instructions, res) = self.generate_expression_code(*arg);
                    instructions.extend(arg_instructions);
                    args.push(res);
                }

                let mut args_on_stack: Vec<String> = vec![];
                for arg_res in args {
                    let arg_var = self.id_allocator.allocate("arg");
                    instructions.push(format!("store_temp({}) -> ({});", arg_res, arg_var));
                    args_on_stack.push(arg_var);
                }

                let tmp_var = self.id_allocator.allocate("tmp");
                instructions.push(format!("func({}) -> ({});", args_on_stack.join(", "), tmp_var));
                return (instructions, tmp_var);
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
                    tmp_var,
                );
            }
        }
    }

    fn handle_felt_match(&mut self, expr_match: &ExprMatch) -> (Vec<String>, String) {
        let branches = &expr_match.branches;
        if !(branches.len() == 2) {
            // TOOD(lior): Replace with diagnostics.
            panic!("Unsupported match.");
        }
        match (&branches[0], &branches[1]) {
            (
                MatchBranch { pattern: Pattern::Expr(expr), block: block0 },
                MatchBranch { pattern: Pattern::Otherwise, block: block_otherwise },
            ) => {
                if let Expr::ExprLiteral(expr_literal) = self.db.lookup_expr(*expr) {
                    // TOOD(lior): Replace with diagnostics.
                    assert_eq!(expr_literal.value, 0);

                    let mut instructions: Vec<String> = vec![];
                    let output_var = self.id_allocator.allocate("match_res");
                    let (match_expr_instructions, match_expr_res) =
                        self.generate_expression_code(expr_match.expr);
                    instructions.extend(match_expr_instructions);
                    // TODO(lior): Replace "???" with statementId.
                    instructions.push(format!("match_zero({}) -> {{ ??? }};", match_expr_res));
                    // TODO(lior): Don't use the same variable manager.
                    // TODO(lior): Fix block0.clone().
                    let (block0_instructions, block0_res) =
                        self.generate_expression_code_by_val(&Expr::ExprBlock(block0.clone()));
                    instructions.extend(block0_instructions);
                    // TODO(lior): Replace "???" with statementId.
                    // TODO(lior): Fix block_otherwise.clone().
                    instructions.push(format!("store_temp({}) -> ({});", block0_res, output_var));
                    instructions.push("jump ???;".into());
                    let (block_otherwise_instructions, block_otherwise_res) = self
                        .generate_expression_code_by_val(&Expr::ExprBlock(block_otherwise.clone()));
                    instructions.extend(block_otherwise_instructions);
                    instructions
                        .push(format!("store_temp({}) -> ({});", block_otherwise_res, output_var));
                    return (instructions, output_var);
                } else {
                    // TOOD(lior): Replace with diagnostics.
                    panic!("Unsupported match.");
                }
            }
            _ => {
                // TOOD(lior): Replace with diagnostics.
                panic!("Unsupported match.");
            }
        }
    }
}
