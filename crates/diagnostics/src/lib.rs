use std::path::{Path, PathBuf};

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct Diagonstics {
    entries: Vec<Diagnostic>,
}
impl Diagonstics {
    pub fn wrap<T>(self, value: T) -> WithDiag<T> {
        WithDiag(value, self)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Diagnostic {
    FileNotFound(FileNotFoundDiagnostic),
}
impl Diagnostic {
    fn message(&self) -> String {
        match self {
            Diagnostic::FileNotFound(diag) => {
                format!("File not found: {}", diag.path.to_str().unwrap())
            }
        }
    }
    fn bunch(self) -> Diagonstics {
        Diagonstics { entries: vec![self] }
    }
    fn wrap<T>(self, value: T) -> WithDiag<T> {
        self.bunch().wrap(value)
    }
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FileNotFoundDiagnostic {
    path: PathBuf,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct WithDiag<T>(T, Diagonstics);
impl<T> WithDiag<T> {
    pub fn unwrap(self, diagnostics: &mut Diagonstics) -> T {
        diagnostics.entries.extend(self.1.entries);
        self.0
    }
    pub fn ignore_err(self) -> T {
        self.0
    }
}
impl<T> From<T> for WithDiag<T> {
    fn from(value: T) -> Self {
        Self(value, Diagonstics::default())
    }
}
