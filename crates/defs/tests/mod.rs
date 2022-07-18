use std::path::{Path, PathBuf};

use defs::{
    db::{DefsDatabase, DefsGroup},
    ids::ModLongId,
};
use filesystem::{
    db::{FilesDatabase, FilesGroup},
    ids::{CrateLongId, FileLongId},
    Upcast,
};
use parser::db::ParseDatabase;
use syntax::node::db::{GreenDatabase, GreenInterner};

#[salsa::database(GreenDatabase, FilesDatabase, ParseDatabase, DefsDatabase)]
#[derive(Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<DatabaseImpl>,
}
impl Upcast<dyn GreenInterner> for DatabaseImpl {
    fn upcast(&self) -> &(dyn GreenInterner + 'static) {
        &*self
    }
}
impl Upcast<dyn FilesGroup> for DatabaseImpl {
    fn upcast(&self) -> &(dyn FilesGroup + 'static) {
        &*self
    }
}
impl salsa::Database for DatabaseImpl {}

#[test]
fn test_defs() {
    let mut db_val = DatabaseImpl::default();
    let db = &mut db_val;
    let crt = db.intern_crate(CrateLongId("Crate".into()));
    db.set_crates(vec![crt]);
    let file_a = db.intern_file(FileLongId(Path::new("a.cairo").into()));
    db.set_crate_root(crt, Some(file_a));
    let file_b = db.intern_file(FileLongId(Path::new("a/b.cairo").into()));
    println!("{:?}", db.file_mapping());

    let mod_a = db.intern_module(ModLongId::CrateRoot(crt));
    let mod_b = db.intern_module(ModLongId::Submodule { parent: mod_a, name: "b".into() });
    let mod_c = db.intern_module(ModLongId::Submodule { parent: mod_a, name: "c".into() });
    println!("{:?}", db.mod_defs(mod_a));
    println!("{:?}", db.mod_defs(mod_b));
    println!("{:?}", db.mod_defs(mod_c));
}
