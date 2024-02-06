use core::fmt::{self, Debug, Formatter};

use either::Either;

use crate::utils::DebugFn;

use super::Rule;

pub struct PrintContext<'src> {
    src: &'src str,
    debug: bool,
}

enum IterSpecialCase<I: Iterator> {
    Zero,
    One(I::Item),
    Many(I),
}

fn iter_special_case<T>(
    iter: impl IntoIterator<Item = T>,
) -> IterSpecialCase<impl Iterator<Item = T>> {
    let mut iter = iter.into_iter();
    match iter.size_hint() {
        (_, Some(0)) => IterSpecialCase::Zero,
        (_, Some(1)) => match iter.next() {
            Some(item) => IterSpecialCase::One(item),
            None => IterSpecialCase::Zero,
        },
        (2, _) => IterSpecialCase::Many(Either::Left(iter)),
        _ => {
            let Some(first) = iter.next() else {
                return IterSpecialCase::Zero;
            };

            let Some(second) = iter.next() else {
                return IterSpecialCase::One(first);
            };

            IterSpecialCase::Many(Either::Right([first, second].into_iter().chain(iter)))
        }
    }
}

impl<'src> PrintContext<'src> {
    pub fn src(&self) -> &'src str {
        self.src
    }

    pub fn new(src: &'src str) -> Self {
        Self { src, debug: false }
    }

    pub fn debuggable<'lt, R: Rule + ?Sized>(&'lt self, ast: &'lt R) -> impl Debug + 'lt {
        DebugFn(move |f| ast.print_tree(self, f))
    }

    pub fn filter_ignored<'short, 'item, R: Rule + ?Sized>(
        &'short self,
        items: impl IntoIterator<Item = &'item R> + 'short,
    ) -> impl Iterator<Item = &'item R> + 'short {
        items
            .into_iter()
            .filter(|item| match item.print_visibility(self) {
                PrintVisibility::Never => false,
                PrintVisibility::DebugOnly => self.debug,
                PrintVisibility::Always => true,
            })
    }

    pub fn fold_printable<'item, T>(
        &self,
        items: impl IntoIterator<Item = &'item dyn Rule>,
        init: T,
        mut f: impl FnMut(T, &dyn Debug) -> T,
    ) -> T {
        items
            .into_iter()
            .fold(init, move |acc, ast| f(acc, &self.debuggable(ast)))
    }

    pub fn debug_tuple<'item>(
        &self,
        name: &str,
        f: &mut Formatter,
        items: impl IntoIterator<Item = &'item dyn Rule>,
    ) -> fmt::Result {
        match iter_special_case(self.filter_ignored(items)) {
            IterSpecialCase::Zero => f.write_str("()"),
            IterSpecialCase::One(item) => item.print_tree(self, f),
            IterSpecialCase::Many(items) => self
                .fold_printable(items, &mut f.debug_tuple(name), |d, item| d.field(item))
                .finish(),
        }
    }

    pub fn debug_list<'item>(
        &self,
        f: &mut Formatter,
        items: impl IntoIterator<Item = &'item dyn Rule>,
    ) -> fmt::Result {
        match iter_special_case(self.filter_ignored(items)) {
            IterSpecialCase::Zero => f.write_str("[]"),
            IterSpecialCase::One(item) => {
                f.write_str("[")?;
                item.print_tree(self, f)?;
                f.write_str("]")
            }
            IterSpecialCase::Many(items) => self
                .fold_printable(items, &mut f.debug_list(), |d, item| d.entry(item))
                .finish(),
        }
    }

    pub fn debug_rule<'item>(
        &self,
        f: &mut Formatter,
        items: impl IntoIterator<Item = &'item dyn Rule>,
    ) -> fmt::Result {
        match iter_special_case(self.filter_ignored(items)) {
            IterSpecialCase::Zero => f.write_str("{}"),
            IterSpecialCase::One(item) => item.print_tree(self, f),
            IterSpecialCase::Many(items) => self
                .fold_printable(items, &mut f.debug_set(), |d, item| d.entry(item))
                .finish(),
        }
    }

    pub fn is_debug(&self) -> bool {
        self.debug
    }

    pub fn set_debug(&mut self, debug: bool) -> &mut Self {
        self.debug = debug;
        self
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PrintVisibility {
    Never,
    DebugOnly,
    #[default]
    Always,
}

impl PrintVisibility {
    pub fn should_print(self, cx: &PrintContext) -> bool {
        match self {
            PrintVisibility::Never => false,
            PrintVisibility::DebugOnly => cx.is_debug(),
            PrintVisibility::Always => true,
        }
    }
}
