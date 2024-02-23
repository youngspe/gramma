use core::{any::TypeId, fmt, hash::Hash};

use crate::{Rule, Token};

pub struct ExpectedParseObject {
    parse_id: fn() -> TypeId,
    name: fn() -> &'static str,
    display_name: fn() -> &'static str,
    fmt_name: fn(&mut fmt::Formatter) -> fmt::Result,
    fmt_display_name: fn(&mut fmt::Formatter) -> fmt::Result,
}

impl ExpectedParseObject {
    pub const fn from_token<T: Token>() -> ExpectedParse {
        &ExpectedParseObject {
            parse_id: TypeId::of::<T>,
            name: T::name,
            display_name: T::display_name,
            fmt_name: |f| f.write_str(T::name()),
            fmt_display_name: |f| f.write_str(T::display_name()),
        }
    }

    pub const fn from_rule<T: Rule>() -> ExpectedParse {
        &ExpectedParseObject {
            parse_id: TypeId::of::<T>,
            name: T::name,
            display_name: T::name,
            fmt_name: T::print_name,
            fmt_display_name: T::print_name,
        }
    }

    pub fn parse_id(&self) -> TypeId {
        (self.parse_id)()
    }

    pub fn name(&self) -> &'static str {
        (self.name)()
    }

    pub fn display_name(&self) -> &'static str {
        (self.display_name)()
    }
}

pub type ExpectedParse = &'static ExpectedParseObject;

impl fmt::Debug for ExpectedParse {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (self.fmt_name)(f)
    }
}

impl fmt::Display for ExpectedParse {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (self.fmt_display_name)(f)
    }
}

impl PartialEq for ExpectedParseObject {
    fn eq(&self, other: &Self) -> bool {
        self.parse_id() == other.parse_id()
    }
}

impl Eq for ExpectedParseObject {}

impl PartialOrd for ExpectedParseObject {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ExpectedParseObject {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.parse_id().cmp(&other.parse_id())
    }
}
impl Hash for ExpectedParseObject {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.parse_id().hash(state);
    }
}
