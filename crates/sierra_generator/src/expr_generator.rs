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
    pub fn generate_expression_code(&mut self, x: ExprId, output_var: &str) -> Vec<String> {
        self.generate_expression_code_by_val(&self.db.lookup_expr(x), output_var)
    }

    fn generate_expression_code_by_val(&mut self, x: &Expr, output_var: &str) -> Vec<String> {
        let mut instructions: Vec<String> = vec![];
        match x {
            Expr::ExprBlock(expr_block) => {
                for statement in &expr_block.statements {
                    match statement {
                        Statement::Expr(expr) => {
                            let tmp_var = self.id_allocator.allocate("tmp");
                            instructions.extend(self.generate_expression_code(*expr, &tmp_var));
                        }
                        Statement::Let(statement_let) => {
                            let var_name = self.id_allocator.allocate("var");
                            // TODO(lior): Make sure the variable was not set before.
                            self.variables.insert(statement_let.var, var_name.clone());
                            instructions.extend(
                                self.generate_expression_code(statement_let.expr, &var_name),
                            );
                        }
                    }
                }
                // TODO: find a way to reduce code duplication.
                match expr_block.tail {
                    Some(expr_id) => {
                        instructions.extend(self.generate_expression_code(expr_id, output_var));
                    }
                    None => todo!(),
                }
            }
            Expr::ExprFunctionCall(expr_function_call) => {
                // Prepare the arguments.
                let mut args: Vec<String> = vec![];
                for arg in &expr_function_call.args {
                    let arg_name = self.id_allocator.allocate("arg");
                    instructions.extend(self.generate_expression_code(*arg, &arg_name));
                    args.push(arg_name);
                }
                instructions.push(format!("func({}) -> ({});", args.join(", "), output_var));
            }
            Expr::ExprMatch(expr_match) => {
                instructions.extend(self.handle_felt_match(expr_match, output_var));
            }
            Expr::ExprVar(expr_var) => match expr_var.var {
                VarId::Param(_) => todo!(),
                VarId::Local(local_var) => {
                    instructions
                        .push(format!("dup({}) -> ({});", self.variables[&local_var], output_var));
                }
            },
            Expr::ExprLiteral(expr_literal) => {
                instructions
                    .push(format!("literal<{}>() -> ({});", expr_literal.value, output_var));
            }
        }
        instructions
    }

    fn handle_felt_match(&mut self, expr_match: &ExprMatch, output_var: &str) -> Vec<String> {
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
                    let mut instructions: Vec<String> = vec![];
                    let var_name = self.id_allocator.allocate("match_val");
                    instructions.extend(self.generate_expression_code(expr_match.expr, &var_name));
                    // TODO(lior): Replace "???" with statementId.
                    instructions.push(format!("match_zero({}) -> {{ ??? }}", var_name));
                    // TODO(lior): Don't use the same variable manager.
                    // TODO(lior): Fix block0.clone().
                    instructions.extend(self.generate_expression_code_by_val(
                        &Expr::ExprBlock(block0.clone()),
                        &output_var,
                    ));
                    // TODO(lior): Replace "???" with statementId.
                    // TODO(lior): Fix block_otherwise.clone().
                    instructions.push("jump ???".into());
                    instructions.extend(self.generate_expression_code_by_val(
                        &Expr::ExprBlock(block_otherwise.clone()),
                        &output_var,
                    ));
                    return instructions;
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
