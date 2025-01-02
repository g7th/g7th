// ©2024 Otmar Klenk
#![macro_use]

use crate::{character, string};
use crate::vpush;

use std::panic::panic_any;
use std::path::PathBuf;

use super::cmd::Cmd;
use super::cmd::value::Value;
use crate::interpreter::data::DataHolder;
use crate::interpreter::item::{Item, NamedCharacter, NamedTextual};

pub(crate) type FnChar      = fn(&mut DataHolder, &char);
pub(crate) type FnString    = fn(&mut DataHolder, &String);
pub const CHAR_PUSH:   FnChar       = |dh: &mut DataHolder, c: &char| { vpush!(dh, Value::from(*c)) };
pub const STRING_PUSH:   FnString   = |dh: &mut DataHolder, t: &String| { vpush!(dh, Value::from(t.to_string())) };

pub(crate) const CHAR_EXPECTED: &str    = "CHAR EXPECTED";
pub(crate) const STRING_EXPECTED: &str    = "STRING EXPECTED";

impl From<NamedCharacter> for Item {
    fn from(c: NamedCharacter) -> Item {
        character!(c.name, CHAR_PUSH, Value::Character(c.kind))
    }
}
impl From<NamedTextual> for Item {
    fn from(t: NamedTextual) -> Item {
        string!(t.name, STRING_PUSH, Value::Textual(t.kind))
    }
}

impl From<Item> for char {
    fn from(b: Item) -> char {
        if let Ok(c) = b.character() {
            c
        }
        else { panic_any(CHAR_EXPECTED); }
    }
}
impl From<Item> for String {
    fn from(b: Item) -> String {
        if let Ok(x) = b.string() {
            x
        }
        else { panic_any(STRING_EXPECTED); }
    }
}

impl From<char> for Value {
    fn from(c: char) -> Value {
        Value::Character(c)
    }
}
impl From<String> for Value {
    fn from(s: String) -> Value {
        Value::Textual(s)
    }
}
impl From<&String> for Value {
    fn from(s: &String) -> Value {
        Value::Textual(s.to_string())
    }
}

impl From<PathBuf> for Value {
    fn from(p: PathBuf) -> Value {
        Value::Textual(p.display().to_string())
    }
}

impl From<Value> for char {
    fn from(v: Value) -> char {
        if let Value::Character(c) = v { c } else { unreachable!() }
    }
}
impl From<Value> for String {
    fn from(v: Value) -> String {
        format!("{}", v)
    }
}
