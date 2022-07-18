use diagnostics::WithDiag;
use filesystem::{db::FilesGroup, ids::FileId, Upcast};
use syntax::{
    node::{
        ast::{
            Empty, ExprBinary, ExprBlock, ExprLiteral, ExprPath, Identifier, ItemFunction,
            ItemList, ItemModule, ItemStruct, MemberList, PathSegment, StatementExpr,
            StatementList, SyntaxFile, Terminal, Trivia, TriviumWhitespace,
        },
        db::GreenInterner,
        SyntaxNode, SyntaxToken, TypedSyntaxNode,
    },
    token::{Token, TokenKind},
};

// Salsa database interface.
#[salsa::query_group(ParseDatabase)]
pub trait ParseGroup: Upcast<dyn GreenInterner> + FilesGroup {
    fn parse(&self, file: FileId) -> WithDiag<Option<SyntaxFile>>;
}

fn parse(db: &dyn ParseGroup, file: FileId) -> WithDiag<Option<SyntaxFile>> {
    let file_long = db.lookup_intern_file(file);
    let db: &dyn GreenInterner = db.upcast();
    let empty = Empty::new_green(db);
    let syntax_file = if file_long.0.ends_with("a.cairo") {
        SyntaxFile::new_green(
            db,
            ItemList::new_green(
                db,
                vec![ItemModule::new_green(
                    db,
                    empty,
                    Identifier::new_green(
                        db,
                        Terminal::new_green(
                            db,
                            empty,
                            SyntaxToken::new_green(db, TokenKind::Identifier, "b".into()),
                            empty,
                        ),
                    ),
                    empty,
                )],
            ),
            SyntaxToken::new_green(db, TokenKind::EndOfFile, "".into()),
        )
    } else {
        if file_long.0.ends_with("b.cairo") {
            SyntaxFile::new_green(
                db,
                ItemList::new_green(
                    db,
                    vec![
                        ItemStruct::new_green(
                            db,
                            empty,
                            Identifier::new_green(
                                db,
                                Terminal::new_green(
                                    db,
                                    empty,
                                    SyntaxToken::new_green(db, TokenKind::Identifier, "S".into()),
                                    empty,
                                ),
                            ),
                            empty,
                            MemberList::new_green(db, vec![]),
                            empty,
                        ),
                        ItemFunction::new_green(
                            db,
                            empty,
                            SyntaxToken::new_green(db, TokenKind::Identifier, "foo".into()),
                            empty,
                            empty,
                            empty,
                            empty,
                            empty,
                            ExprBlock::new_green(
                                db,
                                empty,
                                StatementList::new_green(
                                    db,
                                    vec![StatementExpr::new_green(
                                        db,
                                        ExprPath::new_green(
                                            db,
                                            vec![PathSegment::new_green(
                                                db,
                                                Identifier::new_green(
                                                    db,
                                                    Terminal::new_green(
                                                        db,
                                                        empty,
                                                        SyntaxToken::new_green(
                                                            db,
                                                            TokenKind::Identifier,
                                                            "S".into(),
                                                        ),
                                                        empty,
                                                    ),
                                                ),
                                                empty,
                                            )],
                                        ),
                                    )],
                                ),
                                empty,
                            ),
                        ),
                    ],
                ),
                SyntaxToken::new_green(db, TokenKind::EndOfFile, "".into()),
            )
        } else {
            return None.into();
        }
    };
    let root = SyntaxNode::new_root(syntax_file);
    Some(SyntaxFile::from_syntax_node(db, root)).into()
}
