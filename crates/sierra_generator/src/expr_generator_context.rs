use std::collections::HashMap;
use std::fmt::Display;

use defs::ids::LocalVarId;
use semantic::db::SemanticGroup;

use crate::id_allocator::IdAllocator;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SierraVariable(u64);
impl From<u64> for SierraVariable {
    fn from(id: u64) -> Self {
        SierraVariable(id)
    }
}
impl Display for SierraVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "var{}", self.0)
    }
}

pub struct ExprGeneratorContext<'a> {
    db: &'a dyn SemanticGroup,
    id_allocator: IdAllocator,
    variables: HashMap<LocalVarId, SierraVariable>,
}

impl<'a> ExprGeneratorContext<'a> {
    pub fn new(db: &'a dyn SemanticGroup) -> Self {
        return ExprGeneratorContext {
            db,
            id_allocator: IdAllocator::default(),
            variables: HashMap::new(),
        };
    }

    pub fn allocate_variable(&mut self) -> SierraVariable {
        SierraVariable::from(self.id_allocator.allocate())
    }

    pub fn get_db(&self) -> &'a dyn SemanticGroup {
        self.db
    }

    pub fn register_variable(&mut self, local_var_id: LocalVarId, sierra_var: SierraVariable) {
        // TODO(lior): Make sure the variable was not set before.
        self.variables.insert(local_var_id, sierra_var);
    }

    pub(crate) fn get_variable(&self, local_var: LocalVarId) -> SierraVariable {
        self.variables[&local_var].clone()
    }
}
