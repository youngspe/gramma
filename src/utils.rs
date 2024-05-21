use core::fmt;
use core::ops::ControlFlow::{self, Break, Continue};

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

pub(crate) fn try_run<T, E>(f: impl FnOnce() -> Result<T, E>) -> Result<T, E> {
    f()
}

pub(crate) trait MyTryKind: MyTry<Continue = (), Kind = Self> {
    type WithContinue<T>: MyTry<Continue = T, Break = Self::Break, Kind = Self>;
}

pub(crate) trait MyTry: Sized {
    type Continue;
    type Break;
    type Kind: MyTryKind<Break = Self::Break, WithContinue<Self::Continue> = Self>;
    fn into_ctrl(self) -> ControlFlow<Self::Break, Self::Continue>;
    fn from_ctrl(ctrl: ControlFlow<Self::Break, Self::Continue>) -> Self;

    fn from_continue(c: Self::Continue) -> Self {
        Self::from_ctrl(Continue(c))
    }

    fn from_break(b: Self::Break) -> Self {
        Self::from_ctrl(Break(b))
    }

    fn map<C>(self, f: impl FnOnce(Self::Continue) -> C) -> WithContinue<Self, C> {
        match self.into_ctrl() {
            Continue(c) => MyTry::from_continue(f(c)),
            Break(b) => MyTry::from_break(b),
        }
    }

    fn flat_map<X>(self, f: impl FnOnce(Self::Continue) -> X) -> X
    where
        X: MyTry<Break = Self::Break>,
    {
        match self.into_ctrl() {
            Continue(c) => f(c),
            Break(b) => MyTry::from_break(b),
        }
    }

    fn continue_also(self, f: impl FnOnce(&mut Self::Continue)) -> Self {
        self.map(|mut c| {
            f(&mut c);
            c
        })
    }

    fn break_also(self, f: impl FnOnce(&mut Self::Break)) -> Self {
        let mut ctrl = self.into_ctrl();
        if let Break(ref mut b) = ctrl {
            f(b)
        }
        Self::from_ctrl(ctrl)
    }

    fn continue_also_with<R>(
        self,
        f: impl FnOnce(&mut Self::Continue) -> R,
    ) -> WithContinue<Self, (Self::Continue, R)> {
        self.map(|mut c| {
            let out = f(&mut c);
            (c, out)
        })
    }
    fn map_break<B>(self, f: impl FnOnce(Self::Break) -> B) -> Self::WithBreak<B>
    where
        Self: MapBreak,
    {
        match self.into_ctrl() {
            Continue(c) => MyTry::from_continue(c),
            Break(b) => MyTry::from_break(f(b)),
        }
    }

    fn flat_map_break<X>(self, f: impl FnOnce(Self::Break) -> X) -> X
    where
        X: MyTry<Continue = Self::Continue>,
    {
        match self.into_ctrl() {
            Continue(c) => X::from_continue(c),
            Break(b) => f(b),
        }
    }

    fn break_also_with<R>(
        self,
        f: impl FnOnce(&mut Self::Continue) -> R,
    ) -> WithContinue<Self, (Self::Continue, R)>
    where
        Self: MapBreak,
    {
        self.map(|mut c| {
            let out = f(&mut c);
            (c, out)
        })
    }
}

pub(crate) trait MapBreak: MyTry {
    type WithBreak<B>: MapBreak<WithBreak<Self::Break> = Self, Continue = Self::Continue, Break = B>;
}

pub(crate) type WithContinue<T, C> = <<T as MyTry>::Kind as MyTryKind>::WithContinue<C>;

impl MyTryKind for Option<()> {
    type WithContinue<T> = Option<T>;
}

impl<T> MyTry for Option<T> {
    type Continue = T;
    type Break = ();
    type Kind = Option<()>;

    fn into_ctrl(self) -> ControlFlow<Self::Break, Self::Continue> {
        self.map_or(Break(()), Continue)
    }

    fn from_ctrl(ctrl: ControlFlow<Self::Break, Self::Continue>) -> Self {
        match ctrl {
            Continue(x) => Some(x),
            Break(()) => None,
        }
    }

    fn map<C>(self, f: impl FnOnce(Self::Continue) -> C) -> WithContinue<Self, C> {
        self.map(f)
    }
}

impl<E> MyTryKind for Result<(), E> {
    type WithContinue<T> = Result<T, E>;
}

impl<T, E> MyTry for Result<T, E> {
    type Continue = T;
    type Break = E;
    type Kind = Result<(), E>;
    fn into_ctrl(self) -> ControlFlow<Self::Break, Self::Continue> {
        self.map_or_else(Break, Continue)
    }

    fn from_ctrl(ctrl: ControlFlow<Self::Break, Self::Continue>) -> Self {
        match ctrl {
            Continue(x) => Ok(x),
            Break(e) => Err(e),
        }
    }

    fn map<C>(self, f: impl FnOnce(Self::Continue) -> C) -> WithContinue<Self, C> {
        self.map(f)
    }
}

impl<T, E> MapBreak for Result<T, E> {
    type WithBreak<B> = Result<T, B>;
}

impl<B> MyTryKind for ControlFlow<B> {
    type WithContinue<C> = ControlFlow<B, C>;
}

impl<B, C> MyTry for ControlFlow<B, C> {
    type Continue = C;
    type Break = B;
    type Kind = ControlFlow<B>;

    fn into_ctrl(self) -> Self {
        self
    }

    fn from_ctrl(ctrl: Self) -> Self {
        ctrl
    }
}

impl<B, C> MapBreak for ControlFlow<B, C> {
    type WithBreak<B2> = ControlFlow<B2, C>;
}

pub(crate) trait FunctionalExt: Sized {
    fn then_do<R>(self, f: impl FnOnce(Self) -> R) -> R {
        f(self)
    }

    fn also_with<R>(mut self, f: impl FnOnce(&mut Self) -> R) -> (Self, R) {
        let value = f(&mut self);
        (self, value)
    }

    fn also(mut self, f: impl FnOnce(&mut Self)) -> Self {
        f(&mut self);
        self
    }
}

impl<T> FunctionalExt for T {}
