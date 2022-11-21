use filesystem::ids::{FileLongId, VirtualFile};
use parser::parser::Parser;
use semantic::db::SemanticGroup;
use semantic::resolve_path::Resolver;

pub fn contract_by_name(db: &dyn SemanticGroup, path: String) {
    let mut resolver = Resolver::new(db, None, &[]);
    let file_id = db.intern_file(FileLongId::Virtual(VirtualFile{
         parent: None, name: "".into(), content: path.into() }));
    Parser::f
    resolver.resolve_generic_path(diagnostics, path)
}
