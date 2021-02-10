pub fn join<S: AsRef<str> + Into<String>>(sep: &str, iter: impl IntoIterator<Item = S>) -> String {
    let mut iter = iter.into_iter();
    match iter.next() {
        Some(first) => iter.fold(first.into(), |mut joined, item| {
            joined += sep;
            joined += item.as_ref();
            joined
        }),
        None => String::new(),
    }
}
