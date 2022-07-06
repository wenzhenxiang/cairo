use crate::error::Error;
use crate::graph::*;
use std::collections::HashMap;
use Result::*;

mod arithmetic;
mod function_call;
mod gas_station;
mod jump_nz;
mod match_nullable;
mod tuple_obj;
mod unconditional_jump;

#[derive(Debug, PartialEq)]
pub(crate) struct ExtensionSignature {
    pub args: Vec<Type>,
    pub results: Vec<Vec<Type>>,
    pub fallthrough: Option<usize>,
}

fn simple_invoke_ext_sign(args: Vec<Type>, results: Vec<Type>) -> ExtensionSignature {
    ExtensionSignature {
        args: args,
        results: vec![results],
        fallthrough: Some(0),
    }
}

pub(crate) trait ExtensionImplementation {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error>;
}

type ExtensionBox = Box<dyn ExtensionImplementation + Sync + Send>;

pub(crate) type ExtensionRegistry = HashMap<String, ExtensionBox>;

pub(crate) fn get_registry(prog: &Program) -> ExtensionRegistry {
    chain!(
        arithmetic::extensions().into_iter(),
        function_call::extensions(prog).into_iter(),
        gas_station::extensions().into_iter(),
        jump_nz::extensions().into_iter(),
        match_nullable::extensions().into_iter(),
        tuple_obj::extensions().into_iter(),
        unconditional_jump::extensions().into_iter()
    )
    .collect()
}

pub(crate) fn get_signature(
    registry: &ExtensionRegistry,
    ext: &Extension,
) -> Result<ExtensionSignature, Error> {
    match registry.get(&ext.name) {
        None => Err(Error::UnsupportedLibCallName(ext.name.clone())),
        Some(e) => e.get_signature(&ext.tmpl_args),
    }
}
