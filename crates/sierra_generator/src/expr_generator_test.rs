use std::collections::HashMap;

use defs::db::DefsDatabase;
use defs::ids::{LocalVarId, VarId};
use pretty_assertions::assert_eq;
use salsa::{InternId, InternKey};
use semantic::db::{HirDatabase, SemanticGroup};
use semantic::ids::{FunctionInstanceId, TypeId};
use semantic::semantic::{
    Expr, ExprBlock, ExprFunctionCall, ExprLiteral, ExprMatch, ExprVar, MatchBranch, Pattern,
    Statement, StatementLet,
};

use crate::expr_generator::{ExprGenerator, SierraVariable};
use crate::id_allocator::IdAllocator;

#[salsa::database(DefsDatabase, HirDatabase)]
#[derive(Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<DatabaseImpl>,
}
impl salsa::Database for DatabaseImpl {}

#[test]
fn test_expr_generator() {
    let db = DatabaseImpl::default();

    let ty = TypeId::from_intern_id(InternId::from(0u32));
    let literal7 = db.expr(Expr::ExprLiteral(ExprLiteral { value: 7, ty }));
    let var_x = LocalVarId::from_intern_id(InternId::from(3u32));
    let var_x_expr = db.expr(Expr::ExprVar(ExprVar { var: VarId::Local(var_x), ty }));

    let statement_let = StatementLet { var: var_x, expr: literal7 };
    let expr = db.expr(Expr::ExprFunctionCall(ExprFunctionCall {
        function: FunctionInstanceId::from_intern_id(InternId::from(1u32)),
        args: vec![var_x_expr, literal7],
        ty,
    }));
    let expr2 = db.expr(Expr::ExprFunctionCall(ExprFunctionCall {
        function: FunctionInstanceId::from_intern_id(InternId::from(2u32)),
        args: vec![expr, expr],
        ty,
    }));

    let block = db.expr(Expr::ExprBlock(ExprBlock {
        statements: vec![Statement::Let(statement_let), Statement::Expr(expr)],
        tail: Some(expr2),
        ty,
    }));

    let id_allocator = IdAllocator::default();
    let mut variables: HashMap<LocalVarId, SierraVariable> = HashMap::new();
    let mut expr_generator = ExprGenerator { db: &db, id_allocator, variables: &mut variables };
    let (instructions, res) = expr_generator.generate_expression_code(block);
    assert_eq!(
        instructions,
        vec![
            // let x = 7;
            "literal<7>() -> (literal0);",
            // foo(x, 7);
            "literal<7>() -> (literal1);",
            "store_temp(literal0) -> (arg2);",
            "store_temp(literal1) -> (arg3);",
            "func(arg2, arg3) -> (tmp4);",
            // foo(foo(x, 7), foo(x, 7))
            "literal<7>() -> (literal5);",
            "store_temp(literal0) -> (arg6);",
            "store_temp(literal5) -> (arg7);",
            "func(arg6, arg7) -> (tmp8);",
            "literal<7>() -> (literal9);",
            "store_temp(literal0) -> (arg10);",
            "store_temp(literal9) -> (arg11);",
            "func(arg10, arg11) -> (tmp12);",
            "store_temp(tmp8) -> (arg13);",
            "store_temp(tmp12) -> (arg14);",
            "func(arg13, arg14) -> (tmp15);",
        ]
    );

    assert_eq!(res, SierraVariable::from("tmp15"));
}

#[test]
fn test_match() {
    let db = DatabaseImpl::default();

    let ty = TypeId::from_intern_id(InternId::from(0u32));
    let literal7 = db.expr(Expr::ExprLiteral(ExprLiteral { value: 7, ty }));
    let var_x = LocalVarId::from_intern_id(InternId::from(3u32));
    let var_x_expr = db.expr(Expr::ExprVar(ExprVar { var: VarId::Local(var_x), ty }));

    let statement_let = StatementLet { var: var_x, expr: literal7 };

    let branch0 = MatchBranch {
        pattern: Pattern::Expr(db.expr(Expr::ExprLiteral(ExprLiteral { value: 0, ty }))),
        block: ExprBlock { statements: vec![], tail: Some(var_x_expr), ty },
    };
    let branch_otherwise = MatchBranch {
        pattern: Pattern::Otherwise,
        block: ExprBlock { statements: vec![], tail: Some(literal7), ty },
    };
    let match_statement = db.expr(Expr::ExprMatch(ExprMatch {
        expr: var_x_expr,
        branches: vec![branch0, branch_otherwise],
        ty,
    }));

    let block = db.expr(Expr::ExprBlock(ExprBlock {
        statements: vec![Statement::Let(statement_let)],
        tail: Some(match_statement),
        ty,
    }));

    let id_allocator = IdAllocator::default();
    let mut variables: HashMap<LocalVarId, SierraVariable> = HashMap::new();
    let mut expr_generator = ExprGenerator { db: &db, id_allocator, variables: &mut variables };
    let (instructions, res) = expr_generator.generate_expression_code(block);
    assert_eq!(
        instructions,
        vec![
            // let x = 7;
            "literal<7>() -> (literal0);",
            // match;
            "match_zero(literal0) -> { ???, fallthrough };",
            // Branch 0.
            "store_temp(literal0) -> (match_res1);",
            "jump ???;",
            // Branch otherwise.
            "literal<7>() -> (literal2);",
            "store_temp(literal2) -> (match_res1);",
        ]
    );

    assert_eq!(res, SierraVariable::from("match_res1"));
}
