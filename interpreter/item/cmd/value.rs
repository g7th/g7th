// ©2024 Otmar Klenk
use crate::v;

use bigdecimal::BigDecimal;
use std::fmt;
use std::fs::File;
use std::io::{BufReader, BufWriter};
use std::ops::Neg;
use std::panic::panic_any;
use std::sync::{Arc, Mutex};

use indextree::NodeId;
use nalgebra::DMatrix;

use crate::interpreter::data::Data;
use crate::interpreter::item::file::FILE_EXPECTED;
use crate::interpreter::item::number::{BOOL_EXPECTED, NUMBER_EXPECTED, BIG_DECIMAL_EXPECTED, Number};
use crate::interpreter::item::matrix::MATRIX_EXPECTED;
use crate::interpreter::item::string::CHAR_EXPECTED;
use crate::interpreter::item::string::STRING_EXPECTED;

pub(crate) const ARRAY_EXPECTED: &str       = "ARRAY EXPECTED";  // Vectorial, StrMatrix
pub(crate) const ADDRESS_EXPECTED: &str     = "ADDRESS EXPECTED";
pub(crate) const ADDR_ARR_EXPECTED: &str    = "ADDRESS-ARRAY EXPECTED";
pub(crate) const PACKAGE_EXPECTED: &str     = "PACKAGE EXPECTED";
pub(crate) const INDEX_EXPECTED: &str       = "INDEX EXPECTED";
pub(crate) const UNASSIGNED: &str           = "UNASSIGNED";

#[macro_export]
macro_rules! disc {
    ($this: expr, $other: expr) => {{
        use std::mem::discriminant;
        discriminant($this) == discriminant($other)
    }}
}

#[derive(Debug)]  // Clone 171
pub enum Value {
    Boolean(bool),
    Numerical(Number),
    Vectorial(DMatrix<f64>),
    Character(char),
    Textual(String),
    Addressable(NodeId),
    AddressArray(Vec<Value>),
    Packable(Vec<NodeId>),
    Indexable(u8, usize),  // 'd'atastack, 'v'ariable
    StrMatrix(DMatrix<String>),
    Readable(Arc<Mutex<BufReader<File>>>),
    Writeable(Arc<Mutex<BufWriter<File>>>),
    Unassigned,
}

pub trait GetValue {
    fn is_assigned(&self)       -> bool;
    fn boolean(&self)           -> bool;
    fn number(&self)            -> Number;
    fn big_decimal(&self)       -> BigDecimal;
    fn matrix(&self)            -> DMatrix<f64>;
    fn character(&self)         -> char;
    fn string(&self)            -> String;
    fn address(&self)           -> Option<NodeId>;
    fn addr_arr(&self)          -> Vec<Value>;
    fn addr_arr_mut(&mut self)  -> &mut Vec<Value>;
    fn package(&self)           -> Vec<NodeId>;
    fn package_mut(&mut self)   -> &mut Vec<NodeId>;
    fn index(&self)             -> (u8, usize);
    fn str_matrix(&self)        -> DMatrix<String>;
    fn buf_reader(&self)        -> Arc<Mutex<BufReader<File>>>;
    fn buf_writer(&self)        -> Arc<Mutex<BufWriter<File>>>;
}

impl Neg for Value {
    type Output = Value;
    fn neg(self) -> <Value as Neg>::Output {
        if let Value::Numerical(n) = self { Value::Numerical(-n) } else { if self.is_assigned() { panic!("{NUMBER_EXPECTED}: {self}"); } else { panic!("{UNASSIGNED}"); } }
    }
}

impl GetValue for Value {
    fn is_assigned(&self)       -> bool {
        if let Value::Unassigned = self { false } else { true }
    }
    fn boolean(&self)           -> bool {
        if let Value::Boolean(b) = self { *b } else {  if self.is_assigned() { panic!("{BOOL_EXPECTED}: {self}"); } else { panic!("{UNASSIGNED}"); } }
    }
    fn number(&self)            -> Number {
        if let Value::Numerical(n) = self { n.clone() } else { if self.is_assigned() { panic!("{NUMBER_EXPECTED}: {self}"); } else { panic!("{UNASSIGNED}"); } }
    }
    fn big_decimal(&self)       -> BigDecimal {
        if let Value::Numerical(Number::BIGDEC(bd)) = self { bd.clone() } else { if self.is_assigned() { panic!("{BIG_DECIMAL_EXPECTED}: {self}"); } else { panic!("{UNASSIGNED}"); } }
    }
    fn matrix(&self)            -> DMatrix<f64> {
        if let Value::Vectorial(m) = self { m.clone() } else { panic_any(if self.is_assigned() { MATRIX_EXPECTED } else { UNASSIGNED }) }
    }
    fn character(&self)         -> char {
        if let Value::Character(c) = self { *c } else { if self.is_assigned() { panic!("{CHAR_EXPECTED}: {self}"); } else { panic!("{UNASSIGNED}"); } }
    }
    fn string(&self)            -> String {
        if let Value::Textual(t) = self { t.to_string() } else { if self.is_assigned() { panic!("{STRING_EXPECTED}: {self}"); } else { panic!("{UNASSIGNED}"); } }
    }
    fn address(&self)           -> Option<NodeId> {
        match self {
            Value::Addressable(id)  => Some(*id),
            Value::Unassigned       => None,
            _                       => panic_any(format!("{ADDRESS_EXPECTED} {self}" )),
        }
    }
    fn addr_arr(&self)          -> Vec<Value> {
        if let Value::AddressArray(arr) = self { arr.to_vec() } else { panic_any(if self.is_assigned() { ADDR_ARR_EXPECTED } else { UNASSIGNED }) }
    }
    fn addr_arr_mut(&mut self)  -> &mut Vec<Value> {
        if let Value::AddressArray(arr) = self { arr } else { panic_any(if self.is_assigned() { ADDR_ARR_EXPECTED } else { UNASSIGNED }) }
    }
    fn package(&self)           -> Vec<NodeId> {
        if let Value::Packable(a3) = self { a3.to_vec() } else { panic_any(if self.is_assigned() { PACKAGE_EXPECTED } else { UNASSIGNED }) }
    }
    fn package_mut(&mut self)   -> &mut Vec<NodeId> {
        if let Value::Packable(a3) = self { a3 } else { panic_any(if self.is_assigned() { PACKAGE_EXPECTED } else { UNASSIGNED }) }
    }
    fn index(&self)             -> (u8, usize) {
        if let Value::Indexable(b, idx) = self { (*b, *idx) } else { panic_any(if self.is_assigned() { INDEX_EXPECTED } else { UNASSIGNED }) }
    }
    fn str_matrix(&self)        -> DMatrix<String> {
        if let Value::StrMatrix(m) = self { m.clone() } else { panic_any(if self.is_assigned() { MATRIX_EXPECTED } else { UNASSIGNED }) }
    }
    fn buf_reader(&self)        -> Arc<Mutex<BufReader<File>>> {
        let Value::Readable(rd) = self else { panic_any(if self.is_assigned() { FILE_EXPECTED } else { UNASSIGNED }) };
        rd.clone()
    }
    fn buf_writer(&self)        -> Arc<Mutex<BufWriter<File>>> {
        let Value::Writeable(wr) = self else { panic_any(if self.is_assigned() { FILE_EXPECTED } else { UNASSIGNED }) };
        wr.clone()
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Value::Boolean(b)       => disc!(&Value::Boolean(*b), other) && *b == other.boolean(),
            Value::Numerical(n)     => disc!(&Value::Numerical(n.clone()), other) && *n == other.number(),
            Value::Vectorial(m)     => disc!(&Value::Vectorial(m.clone()), other) && DMatrix::eq(m, &other.matrix()),
            Value::Character(c)     => disc!(&Value::Character(*c), other) && *c == other.character(),
            Value::Textual(t)       => disc!(&Value::Textual(t.to_string()), other) && *t == other.string(),
            Value::Addressable(id)  => disc!(&Value::Addressable(*id), other) && match other {
                Value::Addressable(id2) => id == id2,
                _                       => false,
            },
            Value::AddressArray(a1) => disc!(&Value::AddressArray(a1.to_vec()), other) && {
                let it1 = a1.into_iter().map(|v| v.package()).flatten();
                let a2 = match other {
                    Value::AddressArray(arr) => arr,
                    _   => return false,
                };
                let mut it2 = a2.into_iter().map(|v| v.package()).flatten();
                if it1.clone().count() != it2.clone().count() { return false; }
                for a1 in it1 {
                    if Some(a1) != it2.next() {
                        return false;
                    }
                }
                true
            },
            Value::Indexable(b, idx)    => disc!(&Value::Indexable(*b, *idx), other) && {
                let (b_o, idx_o) = other.index();
                *b == b_o && *idx == idx_o
            },
            Value::StrMatrix(m)     => disc!(&Value::StrMatrix(m.clone()), other) && DMatrix::eq(m, &other.str_matrix()),
            Value::Unassigned       => match other {
                Value::Unassigned   => true,
                _                   => false,
            },
            _                       => unreachable!(),
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Value::Unassigned
    }
}

impl Clone for Value  {
    fn clone(&self) -> Value {
        match self {
            Value::Boolean(b)           => Value::Boolean(*b),
            Value::Numerical(n)         => Value::Numerical(n.clone()),
            Value::Vectorial(m)         => Value::Vectorial(m.clone()),
            Value::Character(c)         => Value::Character(*c),
            Value::Textual(t)           => Value::Textual(t.to_string()),
            Value::Addressable(id)      => Value::Addressable(*id),
            Value::AddressArray(arr)    => Value::AddressArray(arr.to_vec()),
            Value::Packable(a3)         => Value::Packable(a3.to_vec()),
            Value::Indexable(b, idx)    => Value::Indexable(*b, *idx),
            Value::StrMatrix(m)         => Value::StrMatrix(m.clone()),
            Value::Readable(rd)         => Value::Readable(rd.clone()),
            Value::Writeable(wr)        => Value::Writeable(wr.clone()),
            Value::Unassigned           => Value::Unassigned
        }
    }
}

impl IntoIterator for Value {
    type Item = Value;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Value::Vectorial(m)         => m.iter().map(|&f| v!(Number::FLOAT(f))).collect::<Vec<_>>().into_iter(),
            Value::Textual(t)           => t.chars().map(|c| v!(c)).collect::<Vec<_>>().into_iter(),
            Value::AddressArray(arr)    => arr.iter().map(|v| v.clone()).collect::<Vec<_>>().into_iter(),
            Value::Packable(a3)         => a3.iter().map(|&id| v!(id)).collect::<Vec<_>>().into_iter(),
            Value::StrMatrix(m)         => m.iter().map(|s| v!(s)).collect::<Vec<_>>().into_iter(),
            _                           => vec![Value::Unassigned].into_iter(),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Boolean(b)           => write!(f, "{}", *b),
            Value::Numerical(n)         => write!(f, "{}", *n),
            Value::Vectorial(m)         => write!(f, "{}", m.clone()),
            Value::Character(c)         => write!(f, "{}", *c),
            Value::Textual(t)           => write!(f, "{}", t.to_string()),
            Value::Addressable(id)      => write!(f, "[{:?}]", id),
            Value::AddressArray(arr)    => write!(f, "[{:?}]", arr),
            Value::Packable(a3)         => write!(f, "[{:?}]", a3),
            Value::Indexable(b, idx) if *b == b'd'  => write!(f, "[{:?}]", idx),
            Value::Indexable(_, idx)    => write!(f, "{{{:?}}}", idx),
            Value::StrMatrix(m)         => write!(f, "{}", m.clone()),
            Value::Readable(rd)         => write!(f, "{:?}", *rd),
            Value::Writeable(wr)        => write!(f, "{:?}", *wr),
            Value::Unassigned           => write!(f, "{UNASSIGNED}"),
        }
    }
}


impl From<(u8, usize)> for Value {
    fn from(tup: (u8, usize)) -> Value {
        Value::Indexable(tup.0, tup.1)  // 'd'/'v', idx
    }
}

impl From<NodeId> for Value {
    fn from(id: NodeId) -> Value {
        Value::Addressable(id)
    }
}
impl From<&Value> for Option<NodeId> {  // for NodeId
    fn from(v: &Value) -> Option<NodeId> {
        match v {
            Value::Addressable(id)  => Some(*id),
            _                       => None,
        }
    }
}

impl From<&Vec<NodeId>> for Value {
    fn from(a3: &Vec<NodeId>) -> Value {
        Value::Packable(a3.to_vec())
    }
}
impl From<&Value> for Vec<NodeId> {
    fn from(v: &Value) -> Vec<NodeId> {
        v.package()
    }
}

impl From<&Vec<Value>> for Value {
    fn from(arr: &Vec<Value>) -> Value {
        Value::AddressArray(arr.clone())
    }
}
impl From<&Value> for Vec<Value> {
    fn from(varr: &Value) -> Vec<Value> {
        varr.addr_arr()
    }
}

impl From<Value> for Data {
    fn from(value: Value) -> Data {
        Data::VALUE(value)
    }    
}
