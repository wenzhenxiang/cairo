use std::cmp::max;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    LocalMemoryAlreadyAllocated,
    LocalMemoryCantBeAllocated,
    LocalMemoryUsedBeforeAllocation,
    LocalAllocationMismatch,
}

// The context that can be changed by a running line, not including its direct inputs and outputs.
#[derive(Debug, Clone, PartialEq)]
pub struct Effects {
    // The number of writes to local in the considered scope.
    pub local_writes: usize,
    // Was local allocated in the considered scope.
    pub local_allocated: bool,
    // The number of writes to temp in the considered scope.
    pub ap_change: Option<usize>,
    pub temp_used: bool,
}

impl Effects {
    pub(crate) fn none() -> Effects {
        Effects {
            local_writes: 0,
            local_allocated: false,
            ap_change: Some(0),
            temp_used: false,
        }
    }
    pub(crate) fn local_writes(v: usize) -> Effects {
        Effects {
            local_writes: v,
            local_allocated: false,
            ap_change: Some(0),
            temp_used: false,
        }
    }
    pub(crate) fn allocate_locals() -> Effects {
        Effects {
            local_writes: 0,
            local_allocated: true,
            ap_change: Some(0),
            temp_used: false,
        }
    }
    pub(crate) fn ap_invalidation() -> Effects {
        Effects {
            local_writes: 0,
            local_allocated: false,
            ap_change: None,
            temp_used: true,
        }
    }
    pub(crate) fn ap_change(v: usize) -> Effects {
        Effects {
            local_writes: 0,
            local_allocated: false,
            ap_change: Some(v),
            temp_used: true,
        }
    }

    pub(crate) fn add(self: &Self, other: &Effects) -> Result<Effects, Error> {
        if self.local_allocated && other.local_allocated {
            return Err(Error::LocalMemoryAlreadyAllocated);
        }
        if self.temp_used && other.local_allocated {
            return Err(Error::LocalMemoryCantBeAllocated);
        }
        if self.local_writes > 0 && other.local_allocated {
            return Err(Error::LocalMemoryUsedBeforeAllocation);
        }
        Ok(Effects {
            local_writes: self.local_writes + other.local_writes,
            local_allocated: self.local_allocated || other.local_allocated,
            ap_change: match (&self.ap_change, &other.ap_change) {
                (Some(c1), Some(c2)) => Some(c1 + c2),
                _ => None,
            },
            temp_used: self.temp_used || other.temp_used,
        })
    }

    pub(crate) fn converge(self: Self, other: &Effects) -> Result<Effects, Error> {
        if self.local_allocated && !other.local_allocated && other.local_writes > 0
            || other.local_allocated && !self.local_allocated && self.local_writes > 0
        {
            return Err(Error::LocalAllocationMismatch);
        }
        Ok(Effects {
            local_writes: max(self.local_writes, other.local_writes),
            local_allocated: self.local_allocated || other.local_allocated,
            ap_change: match (&self.ap_change, &other.ap_change) {
                (Some(c1), Some(c2)) if c1 == c2 => Some(*c1),
                _ => None,
            },
            temp_used: self.temp_used || other.temp_used,
        })
    }
}

#[cfg(test)]
mod function {
    use super::*;
    #[test]
    fn test_add() {
        assert_eq!(
            Effects::local_writes(1).add(&Effects::local_writes(2)),
            Ok(Effects::local_writes(3))
        );
        assert_eq!(
            Effects::allocate_locals().add(&Effects::none()),
            Ok(Effects::allocate_locals())
        );
        assert_eq!(
            Effects::none().add(&Effects::allocate_locals()),
            Ok(Effects::allocate_locals())
        );
        assert_eq!(
            Effects::ap_change(1).add(&Effects::ap_change(5)),
            Ok(Effects::ap_change(6))
        );
        assert_eq!(
            Effects::ap_change(1).add(&Effects::ap_invalidation()),
            Ok(Effects::ap_invalidation())
        );
        assert_eq!(
            Effects::allocate_locals().add(&Effects::allocate_locals()),
            Err(Error::LocalMemoryAlreadyAllocated)
        );
        assert_eq!(
            Effects::ap_change(0).add(&Effects::allocate_locals()),
            Err(Error::LocalMemoryCantBeAllocated)
        );
        assert_eq!(
            Effects::local_writes(1).add(&Effects::allocate_locals()),
            Err(Error::LocalMemoryUsedBeforeAllocation)
        );
    }

    #[test]
    fn test_converge() {
        assert_eq!(
            Effects::local_writes(1).converge(&Effects::local_writes(2)),
            Ok(Effects::local_writes(2))
        );
        assert_eq!(
            Effects::allocate_locals()
                .add(&Effects::local_writes(1))
                .unwrap()
                .converge(&Effects::none()),
            Effects::allocate_locals().add(&Effects::local_writes(1))
        );
        assert_eq!(
            Effects::ap_change(0).converge(&Effects::ap_change(1)),
            Ok(Effects::ap_invalidation())
        );
        assert_eq!(
            Effects::ap_change(2).converge(&Effects::ap_change(2)),
            Ok(Effects::ap_change(2))
        );
        assert_eq!(
            Effects::allocate_locals()
                .add(&Effects::local_writes(2))
                .unwrap()
                .converge(&Effects::local_writes(2)),
            Err(Error::LocalAllocationMismatch)
        );
    }
}
