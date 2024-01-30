use core::fmt;

pub(crate) fn default<T: Default>() -> T {
    T::default()
}

pub struct DebugFn<F: Fn(&mut fmt::Formatter) -> fmt::Result>(pub F);

impl<F: Fn(&mut fmt::Formatter) -> fmt::Result> fmt::Debug for DebugFn<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0(f)
    }
}

impl<F: Fn(&mut fmt::Formatter) -> fmt::Result> fmt::Display for DebugFn<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0(f)
    }
}

pub(crate) fn simple_name<T: ?Sized>() -> &'static str {
    let mut name = core::any::type_name::<T>();
    if let Some((first, _)) = name.split_once('<') {
        name = first;
    }

    if let Some((_, last)) = name.rsplit_once("::") {
        name = last;
    }

    name
}
