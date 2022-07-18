use std::ops::Deref;

pub mod db;
pub mod ids;

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}
