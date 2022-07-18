use std::path::PathBuf;

use smol_str::SmolStr;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CrateLongId(pub SmolStr);

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct CrateId(salsa::InternId);
impl salsa::InternKey for CrateId {
    fn from_intern_id(id: salsa::InternId) -> Self {
        Self(id)
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FileLongId(pub PathBuf);

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct FileId(salsa::InternId);
impl salsa::InternKey for FileId {
    fn from_intern_id(id: salsa::InternId) -> Self {
        Self(id)
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}
