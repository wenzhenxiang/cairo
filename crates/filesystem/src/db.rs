use std::sync::Arc;

use smol_str::SmolStr;

use crate::ids::{CrateId, CrateLongId, FileId, FileLongId};

// Salsa database interface.
#[salsa::query_group(FilesDatabase)]
pub trait FilesGroup {
    #[salsa::interned]
    fn intern_crate(&self, crt: CrateLongId) -> CrateId;
    #[salsa::interned]
    fn intern_file(&self, filename: FileLongId) -> FileId;
    // #[salsa::input]
    // fn file_content(&self, file_id: FileId) -> Arc<String>;
    #[salsa::input]
    fn crate_root(&self, crate: CrateId) -> Option<FileId>;
    #[salsa::input]
    fn crates(&self) -> Vec<CrateId>;
}

impl FileId {
    pub fn subfile(self, db: &dyn FilesGroup, name: &SmolStr) -> FileId {
        let path = db.lookup_intern_file(self).0;
        let stem = path.file_stem().unwrap();
        let mut subfile = path.clone();
        subfile.pop();
        subfile.push(stem);
        subfile.push(name.to_string() + ".cairo");
        db.intern_file(FileLongId(subfile))
    }
}
