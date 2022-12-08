// Autogenerated file.
// TODO(Gil): push the generating code and point to it from here.

use syntax::node::db::SyntaxGroup;
use syntax::node::kind::SyntaxKind;
use syntax::node::SyntaxNode;

use crate::formatter::{
    BreakLinePointProperties, BreakLinePointType, BreakingPosition, SyntaxNodeFormat,
};

fn parent_kind(db: &dyn SyntaxGroup, syntax_node: &SyntaxNode) -> Option<SyntaxKind> {
    Some(syntax_node.parent()?.kind(db))
}
fn parent_parent_kind(db: &dyn SyntaxGroup, syntax_node: &SyntaxNode) -> Option<SyntaxKind> {
    Some(syntax_node.parent()?.parent()?.kind(db))
}

impl SyntaxNodeFormat for SyntaxNode {
    fn force_no_space_before(&self, db: &dyn SyntaxGroup) -> bool {
        match self.kind(db) {
            SyntaxKind::TokenDot
            | SyntaxKind::TokenColon
            | SyntaxKind::TokenColonColon
            | SyntaxKind::TokenComma
            | SyntaxKind::TokenSemicolon
            | SyntaxKind::TokenQuestionMark
            | SyntaxKind::TokenRParen
            | SyntaxKind::TokenRBrack => true,
            SyntaxKind::TokenLParen
                if matches!(
                    parent_parent_kind(db, self),
                    Some(SyntaxKind::FunctionSignature | SyntaxKind::AttributeArgs)
                ) =>
            {
                true
            }
            SyntaxKind::TokenLBrack
                if matches!(parent_parent_kind(db, self), Some(SyntaxKind::Attribute)) =>
            {
                true
            }
            SyntaxKind::TokenLT | SyntaxKind::TokenGT
                if matches!(
                    parent_parent_kind(db, self),
                    Some(
                        SyntaxKind::PathSegmentWithGenericArgs
                            | SyntaxKind::GenericArgs
                            | SyntaxKind::WrappedGenericParamList
                    )
                ) =>
            {
                true
            }
            _ => false,
        }
    }

    fn force_no_space_after(&self, db: &dyn SyntaxGroup) -> bool {
        match self.kind(db) {
            SyntaxKind::TokenDot
            | SyntaxKind::TokenNot
            | SyntaxKind::TokenColonColon
            | SyntaxKind::TokenLParen
            | SyntaxKind::TokenLBrack
            | SyntaxKind::TokenImplicits => true,
            SyntaxKind::ExprPath | SyntaxKind::TerminalIdentifier
                if matches!(
                    parent_kind(db, self),
                    Some(
                        SyntaxKind::ItemFreeFunction
                            | SyntaxKind::ItemExternFunction
                            | SyntaxKind::ExprFunctionCall
                            | SyntaxKind::PatternEnum
                            | SyntaxKind::PatternStruct
                    )
                ) =>
            {
                true
            }
            SyntaxKind::TokenMinus => {
                matches!(parent_parent_kind(db, self), Some(SyntaxKind::ExprUnary))
            }
            SyntaxKind::TokenLT
                if matches!(
                    parent_parent_kind(db, self),
                    Some(
                        SyntaxKind::PathSegmentWithGenericArgs
                            | SyntaxKind::GenericArgs
                            | SyntaxKind::WrappedGenericParamList
                    )
                ) =>
            {
                true
            }
            _ => false,
        }
    }

    fn should_change_indent(&self, db: &dyn SyntaxGroup) -> bool {
        matches!(
            self.kind(db),
            SyntaxKind::StatementList
                | SyntaxKind::MatchArms
                | SyntaxKind::ExprList
                | SyntaxKind::StructArgList
                | SyntaxKind::ParamList
                | SyntaxKind::GenericParamList
                | SyntaxKind::GenericArgList
        )
    }

    fn force_line_break(&self, db: &dyn SyntaxGroup) -> bool {
        match self.kind(db) {
            SyntaxKind::StatementLet
            | SyntaxKind::StatementExpr
            | SyntaxKind::StatementReturn
            | SyntaxKind::ItemFreeFunction
            | SyntaxKind::ItemExternFunction
            | SyntaxKind::ItemExternType
            | SyntaxKind::ItemTrait
            | SyntaxKind::ItemImpl
            | SyntaxKind::ItemStruct
            | SyntaxKind::Attribute
            | SyntaxKind::ItemEnum
            | SyntaxKind::ItemModule
            | SyntaxKind::ItemUse => true,
            SyntaxKind::TerminalComma
                if matches!(parent_kind(db, self), Some(SyntaxKind::MatchArms)) =>
            {
                true
            }
            SyntaxKind::TerminalLBrace => {
                matches!(parent_kind(db, self), Some(SyntaxKind::ExprBlock | SyntaxKind::ExprMatch))
            }
            _ => false,
        }
    }

    // TODO(gil): consider removing this function as it is no longer used.
    fn allow_newline_after(&self, _db: &dyn SyntaxGroup) -> bool {
        false
    }

    fn allowed_empty_between(&self, db: &dyn SyntaxGroup) -> usize {
        match self.kind(db) {
            SyntaxKind::ItemList => 2,
            SyntaxKind::StatementList => 1,
            _ => 0,
        }
    }

    fn is_protected_breaking_node(&self, db: &dyn SyntaxGroup) -> bool {
        matches!(
            self.kind(db),
            SyntaxKind::ExprParenthesized
                | SyntaxKind::StructArgList
                | SyntaxKind::ParamList
                | SyntaxKind::ExprList
        )
    }
    fn get_break_line_point_properties(
        &self,
        db: &dyn SyntaxGroup,
        position: BreakingPosition,
    ) -> Option<BreakLinePointProperties> {
        match self.kind(db) {
            SyntaxKind::ExprList => Some(BreakLinePointProperties {
                precedence: 6,
                break_type: BreakLinePointType::SeparatedListBreak,
            }),
            SyntaxKind::StructArgList => Some(BreakLinePointProperties {
                precedence: 7,
                break_type: BreakLinePointType::SeparatedListBreak,
            }),
            SyntaxKind::ParamList => Some(BreakLinePointProperties {
                precedence: 9,
                break_type: BreakLinePointType::SeparatedListBreak,
            }),
            SyntaxKind::TokenPlus | SyntaxKind::TokenMinus => {
                if let BreakingPosition::Leading = position {
                    Some(BreakLinePointProperties {
                        precedence: 10,
                        break_type: BreakLinePointType::Dangling,
                    })
                } else {
                    None
                }
            }
            SyntaxKind::TokenMul | SyntaxKind::TokenDiv => {
                if let BreakingPosition::Leading = position {
                    Some(BreakLinePointProperties {
                        precedence: 11,
                        break_type: BreakLinePointType::Dangling,
                    })
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}
