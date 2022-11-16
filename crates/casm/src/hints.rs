use std::fmt::{Display, Formatter};

use num_bigint::BigInt;

use crate::operand::{CellRef, DerefOrImmediate};

#[cfg(test)]
#[path = "hints_test.rs"]
mod test;

// Represents a cairo hint.
#[derive(Debug, Eq, PartialEq)]
pub enum Hint {
    AllocSegment {
        dst: CellRef,
    },
    AllocDict {
        dst: CellRef,
        default_value: CellRef,
    },
    DictRead {
        dict_ptr: CellRef,
        dict_offset: u16,
        key: CellRef,
        value_dst: CellRef,
    },
    DictWrite {
        dict_ptr: CellRef,
        dict_offset: u16,
        key: CellRef,
        value: CellRef,
        prev_value_dst: CellRef,
    },
    TestLessThan {
        lhs: DerefOrImmediate,
        rhs: DerefOrImmediate,
    },
}

impl Display for Hint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let fmt_access_or_const = |f: &mut Formatter<'_>, v: &DerefOrImmediate| match v {
            DerefOrImmediate::Deref(d) => write!(f, "memory{d}"),
            DerefOrImmediate::Immediate(i) => write!(f, "{i}"),
        };
        write!(f, "%{{ ")?;
        match self {
            Hint::AllocSegment { dst } => write!(f, "memory{dst} = segments.add()")?,
            Hint::AllocDict { dst, default_value } => {
                writeln!(f, "")?;
                writeln!(f, "if '__dict_manager' not in globals():")?;
                writeln!(f, "    from starkware.cairo.common.dict import DictManager")?;
                writeln!(f, "    __dict_manager = DictManager()")?;
                writeln!(
                    f,
                    "memory{dst} = __dict_manager.new_default_dict(segments, \
                     memory{default_value})"
                )?
            }
            // TODO(Gil): get the 3 from DictAccess or pass it as an argument.
            Hint::DictRead { dict_ptr, dict_offset, key, value_dst } => {
                writeln!(f, "")?;
                writeln!(
                    f,
                    "dict_tracker = __dict_manager.get_tracker(memory{dict_ptr} + {dict_offset})"
                )?;
                writeln!(f, "dict_tracker.current_ptr += 3")?;
                writeln!(f, "memory{value_dst} = dict_tracker.data[memory{key}]")?
            }
            Hint::DictWrite { dict_ptr, dict_offset, key, value, prev_value_dst } => {
                writeln!(f, "")?;
                writeln!(
                    f,
                    "dict_tracker = __dict_manager.get_tracker(memory{dict_ptr} + {dict_offset})"
                )?;
                writeln!(f, "dict_tracker.current_ptr += 3")?;
                writeln!(f, "memory{prev_value_dst} = dict_tracker.data[memory{key}]")?;
                writeln!(f, "dict_tracker.data[memory{key}] = memory{value}")?;
            }
            Hint::TestLessThan { lhs, rhs } => {
                write!(f, "memory[ap + 0] = ")?;
                fmt_access_or_const(f, lhs)?;
                write!(f, " < ")?;
                fmt_access_or_const(f, rhs)?;
            }
        }
        write!(f, " %}}")
    }
}
