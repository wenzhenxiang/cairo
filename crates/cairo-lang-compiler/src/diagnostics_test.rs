use std::ops::Deref;
use std::sync::Arc;

use cairo_lang_filesystem::db::{FilesGroup, FilesGroupEx};
use cairo_lang_filesystem::ids::{CrateLongId, Directory};

use super::check_diagnostics;
use crate::db::RootDatabase;

#[derive(Default)]
struct CollectedDiagnostics {
    diagnostics: String,
}
impl CollectedDiagnostics {
    fn append(&mut self, s: &str) {
        self.diagnostics += s;
    }
}

fn test_diagnostics() {
    let mut db = RootDatabase::default();

    let crate_id = db.intern_crate(CrateLongId("bad_create".into()));
    db.set_crate_root(crate_id, Some(Directory("no/such/path".into())));

    let mut diagnostics = Box::new(CollectedDiagnostics::default());
    check_diagnostics(&mut db, Some(Box::new(|s| diagnostics.deref().append(&s))));
}
