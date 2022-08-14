use ::defs::ids::VarId;
use ::semantic::ids::ExprId;
use ::semantic::semantic;

use crate::expr_generator_context::{ExprGeneratorContext, SierraVariable};

// Generates Sierra code that computes a given expression.
// Returns a list of Sierra instructions and the Sierra variable in which the result
// is stored.
pub fn generate_expression_code(
    context: &mut ExprGeneratorContext,
    x: ExprId,
) -> (Vec<String>, SierraVariable) {
    generate_expression_code_by_val(context, &context.get_db().lookup_expr(x))
}

fn generate_expression_code_by_val(
    context: &mut ExprGeneratorContext,
    x: &semantic::Expr,
) -> (Vec<String>, SierraVariable) {
    match x {
        semantic::Expr::ExprBlock(expr_block) => handle_block(context, expr_block),
        semantic::Expr::ExprFunctionCall(expr_function_call) => {
            handle_function_call(context, expr_function_call)
        }
        semantic::Expr::ExprMatch(expr_match) => handle_felt_match(context, expr_match),
        semantic::Expr::ExprVar(expr_var) => match expr_var.var {
            VarId::Param(_) => todo!(),
            VarId::Local(local_var) => {
                return (vec![], context.get_variable(local_var));
            }
        },
        semantic::Expr::ExprLiteral(expr_literal) => {
            let tmp_var = context.allocate_variable();
            return (vec![format!("literal<{}>() -> ({});", expr_literal.value, tmp_var)], tmp_var);
        }
    }
}

fn handle_block(
    context: &mut ExprGeneratorContext,
    expr_block: &semantic::ExprBlock,
) -> (Vec<String>, SierraVariable) {
    let mut instructions: Vec<String> = vec![];
    for statement in &expr_block.statements {
        match statement {
            semantic::Statement::Expr(expr) => {
                let (statement_instructions, _res) = generate_expression_code(context, *expr);
                instructions.extend(statement_instructions);
            }
            semantic::Statement::Let(statement_let) => {
                let (statement_instructions, res) =
                    generate_expression_code(context, statement_let.expr);
                instructions.extend(statement_instructions);
                context.register_variable(statement_let.var, res);
            }
        }
    }
    // TODO: find a way to reduce code duplication.
    match expr_block.tail {
        Some(expr_id) => {
            let (tail_instructions, output_var) = generate_expression_code(context, expr_id);
            instructions.extend(tail_instructions);
            return (instructions, output_var);
        }
        None => {
            todo!();
            // return (instructions, SierraVariable("UNIT".into()));
        }
    }
}

fn handle_function_call(
    context: &mut ExprGeneratorContext,
    expr_function_call: &semantic::ExprFunctionCall,
) -> (Vec<String>, SierraVariable) {
    let mut instructions: Vec<String> = vec![];
    let mut args: Vec<SierraVariable> = vec![];
    for arg in &expr_function_call.args {
        let (arg_instructions, res) = generate_expression_code(context, *arg);
        instructions.extend(arg_instructions);
        args.push(res);
    }
    let mut args_on_stack: Vec<String> = vec![];
    for arg_res in args {
        let arg_var = context.allocate_variable();
        instructions.push(format!("store_temp({}) -> ({});", arg_res, arg_var));
        args_on_stack.push(format!("{}", arg_var));
    }
    let tmp_var = context.allocate_variable();
    instructions.push(format!("func({}) -> ({});", args_on_stack.join(", "), tmp_var));
    return (instructions, tmp_var);
}

fn handle_felt_match(
    context: &mut ExprGeneratorContext,
    expr_match: &semantic::ExprMatch,
) -> (Vec<String>, SierraVariable) {
    let branches = &expr_match.branches;
    match &branches[..] {
        [
            semantic::MatchBranch { pattern: semantic::Pattern::Expr(expr), block: block0 },
            semantic::MatchBranch { pattern: semantic::Pattern::Otherwise, block: block_otherwise },
        ] => {
            let expr_literal = if let semantic::Expr::ExprLiteral(expr_literal) =
                context.get_db().lookup_expr(*expr)
            {
                expr_literal
            } else {
                unimplemented!();
            };

            // TOOD(lior): Replace with diagnostics.
            assert_eq!(expr_literal.value, 0);

            let mut instructions: Vec<String> = vec![];

            // Generate instructions for the matched expression.
            let (match_expr_instructions, match_expr_res) =
                generate_expression_code(context, expr_match.expr);
            instructions.extend(match_expr_instructions);

            // TODO(lior): Replace "???" with statementId.
            instructions.push(format!("match_zero({}) -> {{ ???, fallthrough }};", match_expr_res));

            // TODO(lior): Don't use the same variable manager.
            // TODO(lior): Fix block0.clone().
            let (block0_instructions, block0_res) = generate_expression_code_by_val(
                context,
                &semantic::Expr::ExprBlock(block0.clone()),
            );
            instructions.extend(block0_instructions);
            // TODO(lior): Replace "???" with statementId.
            // TODO(lior): Fix block_otherwise.clone().
            let output_var = context.allocate_variable();
            instructions.push(format!("store_temp({}) -> ({});", block0_res, output_var));
            instructions.push("jump ???;".into());

            let (block_otherwise_instructions, block_otherwise_res) =
                generate_expression_code_by_val(
                    context,
                    &semantic::Expr::ExprBlock(block_otherwise.clone()),
                );
            instructions.extend(block_otherwise_instructions);
            instructions.push(format!("store_temp({}) -> ({});", block_otherwise_res, output_var));
            return (instructions, output_var);
        }
        _ => {
            // TOOD(lior): Replace with diagnostics.
            unimplemented!();
        }
    }
}
