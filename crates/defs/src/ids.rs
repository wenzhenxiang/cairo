use filesystem::ids::CrateId;
use smol_str::SmolStr;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ModLongId {
    CrateRoot(CrateId),
    Submodule { parent: ModId, name: SmolStr },
}
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct ModId(salsa::InternId);
impl salsa::InternKey for ModId {
    fn from_intern_id(id: salsa::InternId) -> Self {
        Self(id)
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}

// Definitions.
macro_rules! item_id {
    ($id:ident, $long_id:ident) => {
        #[derive(Clone, Debug, Hash, PartialEq, Eq)]
        pub struct $long_id {
            pub parent: ModId,
            pub name: SmolStr,
        }
        #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
        pub struct $id(salsa::InternId);
        impl salsa::InternKey for $id {
            fn from_intern_id(id: salsa::InternId) -> Self {
                Self(id)
            }

            fn as_intern_id(&self) -> salsa::InternId {
                self.0
            }
        }
    };
}
item_id!(ModDefId, ModDefLongId);
item_id!(FuncDefId, FuncDefLongId);
item_id!(StructDefId, StructDefLongId);
item_id!(EnumDefId, EnumDefLongId);
item_id!(TraitDefId, TraitDefLongId);
item_id!(ImplDefId, ImplDefLongId);

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ItemDefId {
    ModDefId(ModDefId),
    FuncDefId(FuncDefId),
    StructDefId(StructDefId),
    EnumDefId(EnumDefId),
    TraitDefId(TraitDefId),
    ImplDefId(ImplDefId),
}
