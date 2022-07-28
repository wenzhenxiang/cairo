use crate::{
    cursors::Cursors,
    edit_state::Error as EditError,
    effects::{Effects, Error as EffError},
    extensions::Error as ExtError,
    graph::{BlockId, Identifier, Type},
    ref_value::RefValue,
    resources::ResourceMap,
};

#[derive(Debug, PartialEq)]
pub enum Error {
    UsedTempMemoryInvalidated(Identifier),
    Extension(ExtError, String),
    TypeInfo(ExtError, Type),
    EditState(BlockId, EditError),
    EffectsConverge(BlockId, EffError),
    EffectsAdd(BlockId, EffError),
    UnusedBlock(BlockId),
    FunctionBlockOutOfBounds,
    FunctionBlockCursorsMismatch(BlockId, Cursors, Cursors),
    FunctionBlockEffectsMismatch(BlockId, Effects, Effects),
    FunctionBlockResourceMismatch(BlockId, ResourceMap, ResourceMap),
    FunctionBlockReturnTypesMismatch(BlockId, Vec<Type>, Vec<Type>),
    FunctionRemainingOwnedObjects(Vec<Identifier>),
    FunctionReturnTypeMismatch(BlockId, Identifier),
    FunctionReturnApChangeMismatch(String, Option<usize>),
    FunctionReturnResourceUsageMismatch(String, Identifier, i64),
    FunctionReturnLocationMismatch(BlockId, Identifier),
    FunctionReturnLocationNotEndOfTemp(BlockId, i64, usize),
    FunctionBlockIdentifiersMismatch(BlockId, Vec<Identifier>, Vec<Identifier>),
    FunctionBlockIdentifierTypeMismatch(BlockId, Identifier, Type, Type),
    FunctionBlockIdentifierLocationMismatch(BlockId, Identifier, RefValue, RefValue),
    ExtensionBranchesMismatch(String),
    ExtensionResultSizeMismatch(String),
    ExtensionFallthroughMismatch(String),
}
