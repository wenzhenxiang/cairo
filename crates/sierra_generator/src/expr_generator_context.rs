use std::collections::hash_map;

use defs::diagnostic_utils::StableLocation;
use defs::ids::{FreeFunctionId, LanguageElementId, ModuleId};
use diagnostics::Diagnostics;
use smol_str::SmolStr;
use syntax::node::ids::SyntaxStablePtrId;
use utils::unordered_hash_map::UnorderedHashMap;

use crate::db::SierraGenGroup;
use crate::diagnostic::SierraGeneratorDiagnosticKind;
use crate::id_allocator::IdAllocator;
use crate::{pre_sierra, Diagnostic, SierraGeneratorDiagnostic};

/// Context for the methods that generate Sierra instructions for an expression.
pub struct ExprGeneratorContext<'a> {
    db: &'a dyn SierraGenGroup,
    function_id: FreeFunctionId,
    module_id: ModuleId,
    diagnostics: &'a mut Diagnostics<Diagnostic>,
    var_id_allocator: IdAllocator,
    label_id_allocator: IdAllocator,
    variables: UnorderedHashMap<defs::ids::VarId, sierra::ids::VarId>,
}
impl<'a> ExprGeneratorContext<'a> {
    /// Constructs an empty [ExprGeneratorContext].
    pub fn new(
        db: &'a dyn SierraGenGroup,
        function_id: FreeFunctionId,
        diagnostics: &'a mut Diagnostics<Diagnostic>,
    ) -> Self {
        ExprGeneratorContext {
            db,
            function_id,
            module_id: function_id.module(db.as_defs_group()),
            diagnostics,
            var_id_allocator: IdAllocator::default(),
            label_id_allocator: IdAllocator::default(),
            variables: UnorderedHashMap::default(),
        }
    }

    /// Allocates a new Sierra variable.
    pub fn allocate_sierra_variable(&mut self) -> sierra::ids::VarId {
        sierra::ids::VarId::from_usize(self.var_id_allocator.allocate())
    }

    /// Returns the SierraGenGroup salsa database.
    pub fn get_db(&self) -> &'a dyn SierraGenGroup {
        self.db
    }

    /// Attaches a local variable with its Sierra variable.
    /// See [Self::get_variable].
    pub fn register_variable(
        &mut self,
        local_var_id: defs::ids::VarId,
        sierra_var: sierra::ids::VarId,
    ) {
        match self.variables.entry(local_var_id) {
            hash_map::Entry::Occupied(_) => {
                // TODO(lior): Either panic!() if this is necessarily an internal compiler error
                //   or use diagnostics or Result.
                unimplemented!()
            }
            hash_map::Entry::Vacant(entry) => entry.insert(sierra_var),
        };
    }

    /// Returns the Sierra variable associated with the given local variable.
    /// See [Self::register_variable].
    pub fn get_variable(
        &mut self,
        local_var: defs::ids::VarId,
        stable_ptr: SyntaxStablePtrId,
    ) -> Option<sierra::ids::VarId> {
        let var = self.variables.get(&local_var);
        if var.is_none() {
            self.add_diagnostic(
                SierraGeneratorDiagnosticKind::InternalErrorUnknownVariable,
                stable_ptr,
            );
            return None;
        }
        var.cloned()
    }

    /// Generates a label id and a label statement.
    // TODO(lior): Consider using stable ids, instead of allocating sequential ids.
    pub fn new_label(&mut self) -> (pre_sierra::Statement, pre_sierra::LabelId) {
        let id = self.db.intern_label_id(pre_sierra::LabelLongId {
            parent: self.function_id,
            id: self.label_id_allocator.allocate(),
        });
        (pre_sierra::Statement::Label(pre_sierra::Label { id }), id)
    }

    fn get_extension_id_without_generics(
        &self,
        name: impl Into<SmolStr>,
    ) -> sierra::ids::ConcreteLibFuncId {
        self.db.intern_concrete_lib_func(sierra::program::ConcreteLibFuncLongId {
            generic_id: sierra::ids::GenericLibFuncId::from_string(name),
            generic_args: vec![],
        })
    }

    pub fn alloc_locals_libfunc_id(&self) -> sierra::ids::ConcreteLibFuncId {
        self.get_extension_id_without_generics("alloc_locals")
    }

    pub fn felt_const_libfunc_id(&self, value: usize) -> sierra::ids::ConcreteLibFuncId {
        self.db.intern_concrete_lib_func(sierra::program::ConcreteLibFuncLongId {
            generic_id: sierra::ids::GenericLibFuncId::from_string("felt_const"),
            generic_args: vec![sierra::program::GenericArg::Value(value as i64)],
        })
    }

    pub fn store_temp_libfunc_id(&self, ty: semantic::TypeId) -> sierra::ids::ConcreteLibFuncId {
        // TODO(orizi): Propagate the diagnostics or extract `get_concrete_type_id` usage out of
        // this function.
        self.db.intern_concrete_lib_func(sierra::program::ConcreteLibFuncLongId {
            generic_id: sierra::ids::GenericLibFuncId::from_string("store_temp"),
            generic_args: vec![sierra::program::GenericArg::Type(
                self.db.get_concrete_type_id(ty).expect("got unexpected diagnostics").unwrap(),
            )],
        })
    }

    pub fn function_call_libfunc_id(
        &self,
        func: semantic::FunctionId,
    ) -> sierra::ids::ConcreteLibFuncId {
        self.db.intern_concrete_lib_func(sierra::program::ConcreteLibFuncLongId {
            generic_id: sierra::ids::GenericLibFuncId::from_string("function_call"),
            generic_args: vec![sierra::program::GenericArg::UserFunc(
                self.db.intern_sierra_function(func),
            )],
        })
    }

    pub fn felt_ignore_libfunc_id(&self) -> sierra::ids::ConcreteLibFuncId {
        self.get_extension_id_without_generics("felt_ignore")
    }

    pub fn felt_dup_libfunc_id(&self) -> sierra::ids::ConcreteLibFuncId {
        self.get_extension_id_without_generics("felt_dup")
    }

    pub fn felt_jump_nz_libfunc_id(&self) -> sierra::ids::ConcreteLibFuncId {
        self.get_extension_id_without_generics("felt_jump_nz")
    }

    pub fn jump_libfunc_id(&self) -> sierra::ids::ConcreteLibFuncId {
        self.get_extension_id_without_generics("jump")
    }

    pub fn unwrap_nz_libfunc_id(&self, ty: semantic::TypeId) -> sierra::ids::ConcreteLibFuncId {
        // TODO(orizi): Propagate the diagnostics or extract `get_concrete_type_id` usage out of
        // this function.
        self.db.intern_concrete_lib_func(sierra::program::ConcreteLibFuncLongId {
            generic_id: sierra::ids::GenericLibFuncId::from_string("unwrap_nz"),
            generic_args: vec![sierra::program::GenericArg::Type(
                self.db.get_concrete_type_id(ty).expect("got unexpected diagnostics").unwrap(),
            )],
        })
    }

    pub fn generic_libfunc_id(
        &self,
        extern_id: defs::ids::ExternFunctionId,
    ) -> sierra::ids::ConcreteLibFuncId {
        self.get_extension_id_without_generics(extern_id.name(self.db.as_defs_group()))
    }

    /// Returns the [Diagnostics] object of the context.
    pub fn get_diagnostics(&mut self) -> &mut Diagnostics<Diagnostic> {
        &mut self.diagnostics
    }

    /// Add a SierraGenerator diagnostic to the list of diagnostics.
    pub fn add_diagnostic(
        &mut self,
        kind: SierraGeneratorDiagnosticKind,
        stable_ptr: SyntaxStablePtrId,
    ) {
        self.diagnostics.add(SierraGeneratorDiagnostic {
            stable_location: StableLocation::new(self.module_id, stable_ptr),
            kind,
        });
    }
}
