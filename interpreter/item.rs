// ©2024 Otmar Klenk
#![allow(non_camel_case_types)]

use crate::disc;
use crate::{address, addr_arr, index, package};

pub mod branch;
pub mod cmd;
pub mod file;
pub mod matrix;
pub mod number;
pub mod string;
pub mod word;

use std::fmt;
use std::fs::File;
use std::io::{BufReader, BufWriter};
use std::panic::panic_any;
use std::sync::{Arc, Mutex};

use indextree::NodeId;
use nalgebra::DMatrix;

use super::Call;
use super::data::Data;
use super::item::branch::CtrlType;
use super::item::cmd::Cmd;
use super::item::cmd::callable::Immediate;
use super::item::cmd::value::Value;
use super::item::cmd::value::UNASSIGNED;
use super::item::number::Number;
use super::item::word::{ADDR_PUSH, ADDR_ARR_CODE, FN_PACKAGE, IDX_PUSH, NO_NAME};

pub struct NamedValue {
    pub name:   String,
    pub kind:   Value,
}

pub(crate) struct NamedBoolean {
    pub(crate) name:   String,
    pub(crate) kind:   bool,
}

pub(crate) struct NamedNumerical {
    pub(crate) name:   String,
    pub(crate) kind:   Number,
}

pub(crate) struct NamedVectorial {
    pub(crate) name:   String,
    pub(crate) kind:   DMatrix<f64>,
}

pub(crate) struct NamedCharacter {
    pub(crate) name:   String,
    pub(crate) kind:   char,
}

pub(crate) struct NamedTextual {
    pub(crate) name:   String,
    pub(crate) kind:   String,
}

pub(crate) struct NamedAddressable {
    pub(crate) name:   String,
    pub(crate) kind:   NodeId,
}

pub(crate) struct NamedAddressArray {
    pub(crate) ctrl_type: CtrlType,
    pub(crate) name:   String,
    pub(crate) kind:   Vec<Value>,
}

pub(crate) struct NamedPackable {
    pub(crate) ctrl_type: CtrlType,
    pub(crate) name:   String,
    pub(crate) kind:   Vec<NodeId>,
}

pub(crate) struct NamedIndexable {
    pub(crate) name:   String,
    pub(crate) b:      u8,  // 'd'ata, 'v'ariable
    pub(crate) kind:   usize,
}

pub(crate) struct NamedStrMatrix {
    pub(crate) name:   String,
    pub(crate) kind:   DMatrix<String>,
}

pub(crate) struct NamedBufReader {
    pub(crate) name:   String,
    pub(crate) kind:   Arc<Mutex<BufReader<File>>>,
}

pub(crate) struct NamedBufWriter {
    pub(crate) name:   String,
    pub(crate) kind:   Arc<Mutex<BufWriter<File>>>,
}

#[derive(Debug)]
pub enum Item {
    WORD {
        ctrl_type: CtrlType, cmd: (String, Call) },
    BOOL {
        cmd: (String, Call) },
    NUMBER {
        cmd: (String, Call) },
    MATRIX {
        cmd: (String, Call) },
    CHAR {
        cmd: (String, Call) },
    STRING {
        cmd: (String, Call) },
    VOCABULARY {
        cmd: (String, Call) },
    ADDR {
        cmd: (String, Call) },
    ADDR_ARR {
        ctrl_type: CtrlType, cmd: (String, Call) },
    PACKAGE {
        ctrl_type: CtrlType, cmd: (String, Call) },
    INDEX {
        cmd: (String, Call) },
    STR_MATRIX {
        cmd: (String, Call) },
    BUF_READER {
        cmd: (String, Call) },
    BUF_WRITER {
        cmd: (String, Call) },
}

impl PartialEq for Item {
    fn eq(&self, other: &Item) -> bool {
        disc!(self, other) && self == other
    }
}

/////////////////////////////////////////////////////////////////////////////////////////
pub trait GetCtrlType {
    fn is_ctrl_intro(&self) -> bool;
    fn is_ctrl_cont(&self) -> bool;
    fn get_ctrl_type(&self) -> CtrlType;
}

impl GetCtrlType for Item {
    #[inline]
    fn is_ctrl_intro(&self) -> bool {
        let CtrlType::Intro = self.get_ctrl_type() else { return false; };
        true
    }
    #[inline(always)]
    fn is_ctrl_cont(&self) -> bool {
        let CtrlType::Cont = self.get_ctrl_type() else { return false; };
        true
    }
    #[inline(always)]
    fn get_ctrl_type(&self) -> CtrlType {
        match self {
            Item::WORD { ctrl_type, .. }        => *ctrl_type,
            Item::ADDR_ARR { ctrl_type, .. }    => *ctrl_type,
            Item::PACKAGE { ctrl_type, .. }     => *ctrl_type,
            _                                   => CtrlType::Other,
        }
    }
}

/////////////////////////////////////////////////////////////////////////////////////////
pub trait MakeValue {
    fn make_value(&self) -> Value;
}

impl MakeValue for Item {
    fn make_value(&self) -> Value {
        match self {
            Item::WORD   { ctrl_type: _, cmd: (_w, c) }     => {
                Value::from(c.is_immediate())
            },
            Item::BOOL { cmd: (_w, _) }             => {
                Value::from(self.boolean().unwrap())
            },
            Item::NUMBER { cmd: (_w, _) }           => {
                Value::from(self.number().unwrap())
            },
            Item::MATRIX { cmd: (_w, _) }           => {
                Value::from(self.matrix().unwrap())
            },
            Item::CHAR { cmd: (_w, _) }             => {
                Value::from(self.character().unwrap())
            },
            Item::STRING { cmd: (_w, _) }           => {
                Value::from(self.string().unwrap())
            },
            Item::VOCABULARY { cmd: (_w, _) }       => {
                match self.address() {
                    Ok(Some(id))    => Value::from(id),
                    Ok(None)        => panic_any(UNASSIGNED),
                    Err(err)        => panic_any(err),
                }
            },
            Item::ADDR { cmd: (_w, _) }             => {  // Item
                match self.address() {
                    Ok(Some(id))    => Value::from(id),
                    Ok(None)        => Value::Unassigned,
                    Err(err)        => panic_any(err),
                }
            },
            Item::ADDR_ARR { ctrl_type: _, cmd: (_w, _) }         => {  // Item
                Value::AddressArray(self.addr_arr().unwrap())
            },
            Item::PACKAGE { ctrl_type: _, cmd: (_w, _) }         => {  // Item
                Value::from(&self.package().unwrap())
            },
            Item::INDEX { cmd: (_w, _) }            => {  // VARIABLE
                Value::from(self.index().unwrap())
            },
            Item::STR_MATRIX { cmd: (_w, _) }       => {
                Value::from(self.str_matrix().unwrap())
            },
            Item::BUF_READER { cmd: (_w, _) }       => {
                Value::from(self.buf_reader().unwrap())
            },
            Item::BUF_WRITER { cmd: (_w, _) }       => {
                Value::from(self.buf_writer().unwrap())
            },
        }
    }
}

impl From<Value> for Item {
    fn from(value: Value) -> Item {
        Item::from(NamedValue {name: NO_NAME.to_string(), kind: value})
    }    
}

impl From<NamedValue> for Item {
    fn from(value: NamedValue) -> Item {
        match value.kind {
            Value::Boolean(b)        => Item::from(NamedBoolean {name: value.name, kind: b}),
            Value::Numerical(n)      => Item::from(NamedNumerical{name: value.name, kind: n}),
            Value::Vectorial(m)      => Item::from(NamedVectorial {name: value.name, kind: m}),
            Value::Character(c)      => Item::from(NamedCharacter {name: value.name, kind: c}),
            Value::Textual(t)        => Item::from(NamedTextual {name: value.name, kind: t}),
            Value::Addressable(id)   => Item::from(NamedAddressable {name: value.name, kind: id}),
            Value::AddressArray(arr) => Item::from(NamedAddressArray {ctrl_type: CtrlType::Intro, name: value.name, kind: arr}),
            Value::Packable(a3)      => Item::from(NamedPackable {ctrl_type: CtrlType::Intro, name: value.name, kind: a3}),
            Value::Indexable(b, id)  => Item::from(NamedIndexable {name: value.name, b, kind: id}),  // value.kind.make_item(&value.name),
            Value::StrMatrix(m)      => Item::from(NamedStrMatrix {name: value.name, kind: m}),
            Value::Readable(rd)      => Item::from(NamedBufReader {name: value.name, kind: rd}),
            Value::Writeable(wr)     => Item::from(NamedBufWriter {name: value.name, kind: wr}),
            Value::Unassigned        => address!(value.name, ADDR_PUSH, Value::Unassigned),
        }
    }
}

impl From<NamedAddressable> for Item {
    fn from(nval: NamedAddressable) -> Item {
        address!(nval.name, ADDR_PUSH, Value::Addressable(nval.kind))
    }
}

impl From<NamedAddressArray> for Item {
    fn from(nval: NamedAddressArray) -> Item {
        addr_arr!(nval.ctrl_type, nval.name, ADDR_ARR_CODE, Value::AddressArray(nval.kind))
    }
}

impl From<NamedPackable> for Item {
    fn from(nval: NamedPackable) -> Item {
        package!(nval.ctrl_type, nval.name, FN_PACKAGE, Value::Packable(nval.kind))
    }
}

impl From<NamedIndexable> for Item {
    fn from(nval: NamedIndexable) -> Item {
        index!(nval.name, IDX_PUSH, Value::Indexable(nval.b, nval.kind))
    }
}

// impl Clone for Item --> word

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Item::WORD   { ctrl_type: _, cmd: (_w, _) }  => {
                write!(f, "{{{:#?}}}", self.name())
            },
            Item::BOOL { cmd: (_w, _) }                  => {
                let v = self.boolean().unwrap();
                write!(f, "{:?}", v)
            },
            Item::NUMBER { cmd: (_w, _) }                => {
                let v = self.number().unwrap();
                write!(f, "{:?}", v)
            },
            Item::MATRIX { cmd: (_w, _) }                => {
                let v = self.matrix().unwrap();
                write!(f, "{:?}", v)
            },
            Item::CHAR { cmd: (_w, _) }                  => {
                let v = self.character().unwrap();
                write!(f, "{:?}", v)
            },
            Item::STRING { cmd: (_w, _) }                => {
                let v = self.string().unwrap();
                write!(f, "{:?}", v)
            },
            Item::VOCABULARY { cmd: (w, _) }             => {
                write!(f, "{{{:?}}}", w)
            },
            Item::ADDR { cmd: (w, _) }                   => {  // Item
                write!(f, "{{{:?}}}", w)
            },
            Item::ADDR_ARR { ctrl_type: _, cmd: (w, _) } => {  // Item
                write!(f, "{{{:?}}}", w)
            },
            Item::PACKAGE { ctrl_type: _, cmd: (w, _) }  => {  // Item
                write!(f, "{{{:?}}}", w)
            },
            Item::INDEX { cmd: (w, _) }                  => {  // VARIABLE
                write!(f, "{{{:?}}}", w)
            },
            Item::STR_MATRIX { cmd: (_w, _) }            => {
                let v = self.str_matrix().unwrap();
                write!(f, "{:?}", v)
            },
            Item::BUF_READER { cmd: (_w, _) }            => {
                let v = self.buf_reader().unwrap();
                write!(f, "{:?}", v)
            },
            Item::BUF_WRITER { cmd: (_w, _) }            => {
                let v = self.buf_writer().unwrap();
                write!(f, "{:?}", v)
            },
        }
    }
}

impl From<Item> for Data {
    fn from(item: Item) -> Data {
        Data::ITEM(item)
    }    
}
