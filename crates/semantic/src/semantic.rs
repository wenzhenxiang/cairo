use defs::ids::{LocalVarId, MemberId, ModuleItemId, ParamId, VarId};

use crate::ids::{ExprId, FunctionInstanceId, TypeId};

// Statements.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Statement {
    Expr(ExprId),
    Let(StatementLet),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct StatementLet {
    pub var: LocalVarId,
    pub expr: ExprId,
}

// Expressions.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Expr {
    ExprBlock(ExprBlock),
    ExprFunctionCall(ExprFunctionCall),
    ExprMatch(ExprMatch),
    ExprVar(ExprVar),
    ExprLiteral(ExprLiteral),
}
impl Expr {
    pub fn ty(&self) -> TypeId {
        match self {
            Expr::ExprBlock(expr) => expr.ty,
            Expr::ExprFunctionCall(expr) => expr.ty,
            Expr::ExprMatch(expr) => expr.ty,
            Expr::ExprVar(expr) => expr.ty,
            Expr::ExprLiteral(expr) => expr.ty,
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExprBlock {
    pub statements: Vec<Statement>,
    // Blocks may end with an expression, without a trailing `;`.
    // In this case, `tail` will be Some(expr) with that expression.
    // The block expression will evaluate to this tail expression.
    // Otherwise, this will be None.
    pub tail: Option<ExprId>,
    pub ty: TypeId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExprFunctionCall {
    pub function: FunctionInstanceId,
    pub args: Vec<ExprId>,
    pub ty: TypeId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExprMatch {
    pub expr: ExprId,
    pub branches: Vec<MatchBranch>,
    pub ty: TypeId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct MatchBranch {
    pub pattern: Pattern,
    pub block: ExprBlock,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Pattern {
    Otherwise,
    // TODO(lior): Should this be constant expression?
    Expr(ExprId),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExprVar {
    pub var: VarId,
    pub ty: TypeId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExprLiteral {
    // TODO(spapini): Fix the type of `value`.
    pub value: usize,
    pub ty: TypeId,
}

// Items.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FreeFunction {
    signature: Signature,
    body: ExprBlock,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Signature {
    // TODO(spapini): Generics parameters.
    params: Vec<ParamId>,
    ret: TypeId,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Struct {
    members: Vec<MemberId>,
}
// TODO(spapini): Add semantic representation for Params and Members.
// This will include their types.

// Module.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Module {
    items: Vec<ModuleItemId>,
}
