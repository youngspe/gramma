#[macro_use]
pub mod common;

use common::unwrap_display;
use gramma::lex::{Eof, RealDecimal, StringToken, Whitespace};
use gramma::{grammar, tokens};
use std::collections::BTreeMap;
use std::str::FromStr;

tokens! {
    pub struct LBrace => symbol("{");
    pub struct RBrace => symbol("}");
    pub struct LBracket => symbol("[");
    pub struct RBracket => symbol("]");
    pub struct Colon => symbol(":");
    pub struct Comma => symbol(",");
    pub struct True => symbol("true");
    pub struct False => symbol("false");
    pub struct Null => symbol("null");
    pub enum JsonToken => {
        | LBrace | RBrace
        | LBracket | RBracket
        | Colon
        | Comma
        | Whitespace
        | StringToken
        | RealDecimal
        | True
        | False
        | Null
        | Eof
    };
}

grammar! { JsonToken;
    pub struct Kvp => { key: StringToken, Colon, value: Value };
    pub struct List => { LBracket, vals: [(Value)(Comma)*], RBracket };
    pub struct Object => { LBrace, vals: [(Kvp)(Comma)*], RBrace };
    pub enum Bool => { True | False };
    pub enum Value => {
        | Object
        | List
        | String(StringToken)
        | Number(RealDecimal)
        | Bool
        | Null
    };
    pub struct Root => { value: Value, Eof };
}

#[derive(Clone, Debug, PartialEq)]
enum JsonValue {
    Object(BTreeMap<String, JsonValue>),
    List(Vec<JsonValue>),
    String(String),
    Number(f64),
    Bool(bool),
    Null,
}

fn parse(src: &str) -> JsonValue {
    fn parse_kvp(src: &str, tree: &Kvp) -> (String, JsonValue) {
        let key = tree.key.inner_str(src).to_string();
        let value = inner(src, &tree.value);
        (key, value)
    }

    fn inner(src: &str, tree: &Value) -> JsonValue {
        match tree {
            Value::Object(Object { vals, .. }) => {
                JsonValue::Object(vals.iter().map(|kvp| parse_kvp(src, kvp)).collect())
            }
            Value::List(List { vals, .. }) => {
                JsonValue::List(vals.iter().map(|v| inner(src, v)).collect())
            }
            Value::String(s) => JsonValue::String(s.inner_str(src).to_string()),
            Value::Number(n) => JsonValue::Number(f64::from_str(n.get_str(src)).unwrap()),
            Value::Bool(Bool::True(..)) => JsonValue::Bool(true),
            Value::Bool(Bool::False(..)) => JsonValue::Bool(false),
            Value::Null(..) => JsonValue::Null,
        }
    }

    let (_, ast) = unwrap_display(gramma::parse(src));
    inner(src, &ast)
}

macro_rules! object {
    ($($k:expr => $v:expr),*$(,)?) => {{
        #[allow(unused)]
        let mut map = BTreeMap::new();
        $(map.insert($k.into(), $v.into());)*
        JsonValue::Object(map)
    }};
}

macro_rules! list {
    ($($x:expr),*$(,)?) => {
        JsonValue::List(vec![$($x.into()),*])
    };
}

macro_rules! json {
    ({$($key:literal => $val:tt),*$(,)?}) => {
        object! { $($key => json!($val)),* }
    };
    ([$($val:tt),*$(,)?]) => {
        list![$(json!($val)),*]
    };
    (null) => { JsonValue::Null };
    ($lit:literal) => { JsonValue::from($lit) };
    ($val:expr) => { Jso nValue::from($val) };
}

impl From<&str> for JsonValue {
    fn from(src: &str) -> Self {
        Self::String(src.into())
    }
}
impl From<String> for JsonValue {
    fn from(src: String) -> Self {
        Self::String(src)
    }
}
impl From<f64> for JsonValue {
    fn from(src: f64) -> Self {
        Self::Number(src)
    }
}

impl From<i64> for JsonValue {
    fn from(src: i64) -> Self {
        Self::Number(src as f64)
    }
}

impl From<bool> for JsonValue {
    fn from(src: bool) -> Self {
        Self::Bool(src)
    }
}

impl<T: Into<JsonValue> + Clone> From<&[T]> for JsonValue {
    fn from(src: &[T]) -> Self {
        JsonValue::List(src.iter().cloned().map(Into::into).collect())
    }
}

impl From<Vec<JsonValue>> for JsonValue {
    fn from(src: Vec<JsonValue>) -> Self {
        JsonValue::List(src)
    }
}

impl<T: Into<JsonValue>> From<Option<T>> for JsonValue {
    fn from(src: Option<T>) -> Self {
        match src {
            Some(v) => v.into(),
            None => Self::Null,
        }
    }
}

#[test]
fn parse_json() {
    parse("{ }");
    let expected = json!({
        "x" => "x",
        "y" => ["z", [], { "a" => ["c"], "d" => 3.5 }],
        "z" => {},
        "w" => [true, null, 4.0, false],
    });

    let src = r#"{
        "x": "x",
        "y": ["z", [], {"a":["c"], "d": 3.5}],
        "z": {},
        "w": [true, null, 4, false]
    }"#;

    assert_eq!(parse(src), expected);
}
