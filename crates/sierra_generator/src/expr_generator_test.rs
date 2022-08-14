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

use crate::expr_generator::generate_expression_code;
use crate::expr_generator_context::{ExprGeneratorContext, SierraVariable};

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

    let mut expr_generator_context = ExprGeneratorContext::new(&db);
    let (instructions, res) = generate_expression_code(&mut expr_generator_context, block);
    assert_eq!(
        instructions,
        vec![
            // let x = 7;
            "literal<7>() -> (var0);",
            // foo(x, 7);
            "literal<7>() -> (var1);",
            "store_temp(var0) -> (var2);",
            "store_temp(var1) -> (var3);",
            "func(var2, var3) -> (var4);",
            // foo(foo(x, 7), foo(x, 7))
            "literal<7>() -> (var5);",
            "store_temp(var0) -> (var6);",
            "store_temp(var5) -> (var7);",
            "func(var6, var7) -> (var8);",
            "literal<7>() -> (var9);",
            "store_temp(var0) -> (var10);",
            "store_temp(var9) -> (var11);",
            "func(var10, var11) -> (var12);",
            "store_temp(var8) -> (var13);",
            "store_temp(var12) -> (var14);",
            "func(var13, var14) -> (var15);",
        ]
    );

    assert_eq!(res, SierraVariable::from(15));
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

    let mut expr_generator_context = ExprGeneratorContext::new(&db);
    let (instructions, res) = generate_expression_code(&mut expr_generator_context, block);
    assert_eq!(
        instructions,
        vec![
            // let x = 7;
            "literal<7>() -> (var0);",
            // match;
            "match_zero(var0) -> { ???, fallthrough };",
            // Branch 0.
            "store_temp(var0) -> (var1);",
            "jump ???;",
            // Branch otherwise.
            "literal<7>() -> (var2);",
            "store_temp(var2) -> (var1);",
        ]
    );

    assert_eq!(res, SierraVariable::from(1));
}
