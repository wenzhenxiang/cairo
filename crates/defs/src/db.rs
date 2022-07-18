use std::{collections::HashMap, sync::Arc};

use diagnostics::{Diagonstics, WithDiag};
use filesystem::{
    db::FilesGroup,
    ids::{CrateId, FileId, FileLongId},
    Upcast,
};
use parser::db::ParseGroup;
use smol_str::SmolStr;
use syntax::node::{
    ast::{ItemList, SyntaxFile},
    SyntaxNode,
};

use crate::ids::{
    EnumDefId, EnumDefLongId, FuncDefId, FuncDefLongId, ImplDefId, ImplDefLongId, ItemDefId,
    ModDefId, ModDefLongId, ModId, ModLongId, StructDefId, StructDefLongId, TraitDefId,
    TraitDefLongId,
};

// Salsa database interface.
#[salsa::query_group(DefsDatabase)]
pub trait DefsGroup: FilesGroup + Upcast<dyn FilesGroup> + ParseGroup {
    #[salsa::interned]
    fn intern_module(&self, mod_long_id: ModLongId) -> ModId;

    // Module file mapping.
    fn module_file(&self, module: ModId) -> Option<FileId>;

    // Backward mapping.
    fn crate_modules(&self, crt: CrateId) -> WithDiag<Option<Arc<HashMap<ModId, FileId>>>>;
    fn file_mapping(&self) -> Arc<HashMap<FileId, ModId>>;
    fn file_module(&self, file: FileId) -> ModId;

    fn module_syntax(&self, module: ModId) -> WithDiag<Option<ItemList>>;
    fn mod_defs(&self, module: ModId) -> Option<Arc<HashMap<SmolStr, ItemDefId>>>;
    fn mod_defs_modules(&self, module: ModId) -> Option<Arc<Vec<ModDefId>>>;

    // TODO: crate modules.

    #[salsa::interned]
    fn intern_mod_def(&self, mod_long_id: ModDefLongId) -> ModDefId;
    #[salsa::interned]
    fn intern_func_def(&self, func_long_id: FuncDefLongId) -> FuncDefId;
    #[salsa::interned]
    fn intern_struct_def(&self, struct_long_id: StructDefLongId) -> StructDefId;
    #[salsa::interned]
    fn intern_enum_def(&self, enum_long_id: EnumDefLongId) -> EnumDefId;
    #[salsa::interned]
    fn intern_trait_def(&self, trait_long_id: TraitDefLongId) -> TraitDefId;
    #[salsa::interned]
    fn intern_impl_def(&self, impl_long_id: ImplDefLongId) -> ImplDefId;
}

// // Question? How to store:
// // * names of defs.
// // * ItemIds (if needed?)
// // * Typed syntax nodes.
// // Should they be store in a hashp to enum, or muliple hashmaps? how to save code?
// pub struct ModuleData{
//     defs: HashMap<SmolStr, (ItemDefId)>
//     func_defs: HashMap
// }

// TODO: Handle missing modules / files.
fn module_file(db: &dyn DefsGroup, module: ModId) -> Option<FileId> {
    Some(match db.lookup_intern_module(module) {
        ModLongId::CrateRoot(crt) => db.crate_root(crt)?,
        ModLongId::Submodule { parent, name } => {
            let parent_file = module_file(db, parent)?;
            parent_file.subfile(db.upcast(), &name)
        }
    })
}
// Module file mapping.
fn crate_modules(
    db: &dyn DefsGroup,
    crt: CrateId,
) -> WithDiag<Option<Arc<HashMap<ModId, FileId>>>> {
    let mut res = HashMap::new();

    fn traverse_modules(
        db: &dyn DefsGroup,
        module: ModId,
        file: FileId,
        res: &mut HashMap<ModId, FileId>,
    ) {
        res.insert(module, file);
        // TODO: This should propogate an error for non existing modules.
        for submod_def in db.mod_defs_modules(module).unwrap_or_default().iter() {
            let submodule_name = db.lookup_intern_mod_def(*submod_def).name;
            let subfile = file.subfile(db.upcast(), &submodule_name);
            let submodule =
                db.intern_module(ModLongId::Submodule { parent: module, name: submodule_name });
            traverse_modules(db, submodule, subfile, res);
        }
    }

    if let Some(root) = db.crate_root(crt) {
        traverse_modules(db, db.intern_module(ModLongId::CrateRoot(crt)), root, &mut res);
        Some(Arc::new(res))
    } else {
        None
    }
    .into()
}
fn file_mapping(db: &dyn DefsGroup) -> Arc<HashMap<FileId, ModId>> {
    let mut res = HashMap::new();
    for crt in db.crates() {
        for (k, v) in db.crate_modules(crt).ignore_err().unwrap_or_default().iter() {
            res.insert(*v, *k);
        }
    }
    Arc::new(res)
}
fn file_module(db: &dyn DefsGroup, file: FileId) -> ModId {
    db.file_mapping()[&file]
}

fn module_syntax(db: &dyn DefsGroup, module: ModId) -> WithDiag<Option<ItemList>> {
    let mut diagnostics = Diagonstics::default();
    if let Some(file) = db.module_file(module) {
        let syntax = db.parse(file).unwrap(&mut diagnostics);
        diagnostics.wrap(syntax.map(|x| x.items(db.upcast())))
    } else {
        diagnostics.wrap(None)
    }
}

fn mod_defs(db: &dyn DefsGroup, module: ModId) -> Option<Arc<HashMap<SmolStr, ItemDefId>>> {
    let gdb = db.upcast();
    let syntax = db.module_syntax(module).ignore_err()?;
    let mut res = HashMap::new();
    for e in syntax.elements(gdb) {
        match e {
            syntax::node::ast::Item::ItemModule(item) => {
                // TODO: Handle missing token.
                let name = item.name(gdb).terminal(gdb).token(gdb).text(gdb);
                let def = db.intern_mod_def(ModDefLongId { parent: module, name: name.clone() });
                res.insert(name, ItemDefId::ModDefId(def));
            }
            syntax::node::ast::Item::ItemFunction(item) => {
                // TODO: Handle missing token.
                let name = item.name(gdb).terminal(gdb).token(gdb).text(gdb);
                let def = db.intern_func_def(FuncDefLongId { parent: module, name: name.clone() });
                res.insert(name, ItemDefId::FuncDefId(def));
            }
            syntax::node::ast::Item::ItemStruct(item) => {
                // TODO: Handle missing token.
                let name = item.name(gdb).terminal(gdb).token(gdb).text(gdb);
                let def =
                    db.intern_struct_def(StructDefLongId { parent: module, name: name.clone() });
                res.insert(name, ItemDefId::StructDefId(def));
            }
            syntax::node::ast::Item::ItemEnum(item) => {
                // TODO: Handle missing token.
                let name = item.name(gdb).terminal(gdb).token(gdb).text(gdb);
                let def = db.intern_enum_def(EnumDefLongId { parent: module, name: name.clone() });
                res.insert(name, ItemDefId::EnumDefId(def));
            }
            syntax::node::ast::Item::ItemTrait(item) => {
                // TODO: Handle missing token.
                let name = item.name(gdb).terminal(gdb).token(gdb).text(gdb);
                let def =
                    db.intern_trait_def(TraitDefLongId { parent: module, name: name.clone() });
                res.insert(name, ItemDefId::TraitDefId(def));
            }
            syntax::node::ast::Item::ItemImpl(item) => {
                // TODO: Handle missing token.
                let name = item.name(gdb).terminal(gdb).token(gdb).text(gdb);
                let def = db.intern_impl_def(ImplDefLongId { parent: module, name: name.clone() });
                res.insert(name, ItemDefId::ImplDefId(def));
            }
            // TODO: change to ItemMissing, or make it impossible somehow?
            syntax::node::ast::Item::Empty(_) => {}
        }
    }
    Some(Arc::new(res))
}
fn mod_defs_modules(db: &dyn DefsGroup, module: ModId) -> Option<Arc<Vec<ModDefId>>> {
    Some(Arc::new(
        db.mod_defs(module)?
            .values()
            .flat_map(|v| if let ItemDefId::ModDefId(mod_def) = v { Some(*mod_def) } else { None })
            .collect(),
    ))
}
