// ©2024 Otmar Klenk
pub mod callable;
pub mod value;

use std::fmt;
use std::fs::File;
use std::io::{BufReader, BufWriter};
use std::sync::{Arc, Mutex};

use core::fmt::Debug;
use indextree::NodeId;
use nalgebra::DMatrix;

use crate::interpreter::item::Item;
use crate::interpreter::item::cmd::callable::Callable;
use crate::interpreter::tools::Dots;
use crate::interpreter::item::word::{BufReaderCode, BufWriterCode};
use super::cmd::value::Value;
use super::number::Number;
use super::word::{BooleanCode, NumberCode, MatrixCode, CharCode, StringCode,
                  AddrCode, AddrArrCode, PackageCode, IndexCode, StrMatrixCode};

pub const ADDRESS_MISSING: &str     = "ADDRESS MISSING";
pub const ADDR_ARR_MISSING: &str    = "ADDRESS-ARRAY MISSING";
pub const PACKAGE_MISSING: &str     = "PACKAGE MISSING";
const INDEX_MISSING: &str           = "INDEX MISSING";
const VALUE_MISSING: &str           = "VALUE MISSING";

pub(crate) trait Cmd {
    fn name(&self)          -> String;
    fn code(&self)          -> &dyn Callable;
    fn code_mut(&mut self)  -> &mut dyn Callable;
    fn boolean(&self)       -> Result<bool, String>;
    fn number(&self)        -> Result<Number, String>;
    fn matrix(&self)        -> Result<DMatrix<f64>, String>;
    fn character(&self)     -> Result<char, String>;
    fn string(&self)        -> Result<String, String>;
    fn address(&self)       -> Result<Option<NodeId>, String>;
    fn addr_arr(&self)      -> Result<Vec<Value>, String>;
    fn package(&self)       -> Result<Vec<NodeId>, String>;
    fn index(&self)         -> Result<(u8, usize), String>;
    fn str_matrix(&self)    -> Result<DMatrix<String>, String>;
    fn buf_reader(&self)    -> Result<Arc<Mutex<BufReader<File>>>, String>;
    fn buf_writer(&self)    -> Result<Arc<Mutex<BufWriter<File>>>, String>;
}

impl Cmd for Item {
    fn name(&self) -> String {
        let rslt = match self {
            Item::WORD   { ctrl_type: _, cmd: (s, _) }      => s.to_string(),
            Item::BOOL   { cmd: (s, _) }        => s.to_string(),
            Item::NUMBER { cmd: (s, _) }        => s.to_string(),
            Item::MATRIX { cmd: (s, _) }        => s.to_string(),
            Item::CHAR   { cmd: (s, _) }        => s.to_string(),
            Item::STRING { cmd: (s, _) }        => s.to_string(),
            Item::VOCABULARY { cmd: (s, _) }    => s.to_string(),
            Item::ADDR   { cmd: (s, _) }        => s.to_string(),
            Item::ADDR_ARR { ctrl_type: _, cmd: (s, _) }    => s.to_string(),
            Item::PACKAGE { ctrl_type: _, cmd: (s, _) }     => s.to_string(),
            Item::INDEX  { cmd: (s, _) }        => s.to_string(),
            Item::STR_MATRIX { cmd: (s, _) }    => s.to_string(),
            Item::BUF_READER { cmd: (s, _) }    => s.to_string(),
            Item::BUF_WRITER { cmd: (s, _) }    => s.to_string(),
        };
        rslt
    }
    fn code(&self) -> &dyn Callable {
        match self {
            Item::WORD      { ctrl_type: _, cmd: (_, code) }    => code.as_ref(),
            Item::BOOL   { cmd: (_, code) }     => code.as_ref(),
            Item::NUMBER { cmd: (_, code) }     => code.as_ref(),
            Item::MATRIX { cmd: (_, code) }     => code.as_ref(),
            Item::CHAR   { cmd: (_, code) }     => code.as_ref(),
            Item::STRING { cmd: (_, code) }     => code.as_ref(),
            Item::VOCABULARY { cmd: (_, code) } => code.as_ref(),
            Item::ADDR   { cmd: (_, code) }     => code.as_ref(),
            Item::ADDR_ARR { ctrl_type: _, cmd: (_, code) }     => code.as_ref(),
            Item::PACKAGE { ctrl_type: _, cmd: (_, code) }      => code.as_ref(),
            Item::INDEX  { cmd: (_, code) }     => code.as_ref(),
            Item::STR_MATRIX { cmd: (_, code) } => code.as_ref(),
            Item::BUF_READER { cmd: (_, code) } => code.as_ref(),
            Item::BUF_WRITER { cmd: (_, code) } => code.as_ref(),
        }
    }
    fn code_mut(&mut self) -> &mut dyn Callable {
        match self {
            Item::WORD   { ctrl_type: _, cmd: (_, code) }       => code.as_mut(),
            Item::BOOL   { cmd: (_, code) }     => code.as_mut(),
            Item::NUMBER { cmd: (_, code) }     => code.as_mut(),
            Item::MATRIX { cmd: (_, code) }     => code.as_mut(),
            Item::CHAR   { cmd: (_, code) }     => code.as_mut(),
            Item::STRING { cmd: (_, code) }     => code.as_mut(),
            Item::VOCABULARY { cmd: (_, code) } => code.as_mut(),
            Item::ADDR   { cmd: (_, code) }     => code.as_mut(),
            Item::ADDR_ARR { ctrl_type: _,  cmd: (_, code) }    => code.as_mut(),
            Item::PACKAGE { ctrl_type: _,  cmd: (_, code) }     => code.as_mut(),
            Item::INDEX  { cmd: (_, code) }     => code.as_mut(),
            Item::STR_MATRIX { cmd: (_, code) } => code.as_mut(),
            Item::BUF_READER { cmd: (_, code) } => code.as_mut(),
            Item::BUF_WRITER { cmd: (_, code) } => code.as_mut(),
        }
    }
    fn boolean(&self) -> Result<bool, String> {
        if let Item::BOOL { cmd: (_, code) } = self {
            if let Value::Boolean(b) = code.as_any().downcast_ref::<BooleanCode>().unwrap().value {
                return Ok(b);
            }
        }
        Err(VALUE_MISSING.dots())
    }
    fn number(&self) -> Result<Number, String> {
        if let Item::NUMBER { cmd: (_, code) } = self {
            if let Value::Numerical(n) = &code.as_any().downcast_ref::<NumberCode>().unwrap().value {
                return Ok(n.clone());
            }
        }
        Err(VALUE_MISSING.dots())
    }
    fn matrix(&self) -> Result<DMatrix<f64>, String> {
        if let Item::MATRIX { cmd: (_, code) } = self {
            if let Value::Vectorial(m) = &code.as_any().downcast_ref::<MatrixCode>().unwrap().value {
                return Ok(m.clone());
            }
        }
        Err(VALUE_MISSING.dots())
    }
    fn character(&self) -> Result<char, String> {
        if let Item::CHAR { cmd: (_, code) } = self {
            if let Value::Character(c) = &code.as_any().downcast_ref::<CharCode>().unwrap().value {
                return Ok(*c);
            }
        }
        Err(VALUE_MISSING.dots())
    }
    fn string(&self) -> Result<String, String> {
        if let Item::STRING { cmd: (_, code) } = self {
            if let Value::Textual(t) = &code.as_any().downcast_ref::<StringCode>().unwrap().value {
                return Ok(t.to_string());
            }
        }
        Err(VALUE_MISSING.dots())
    }
    fn address(&self) -> Result<Option<NodeId>, String> {
        if let Item::ADDR { cmd: (_, code) } = self {
            match &code.as_any().downcast_ref::<AddrCode>().unwrap().value {
                Value::Addressable(id)  => return Ok(Some(*id)),
                Value::Unassigned       => return Ok(None),
                _                       => unreachable!(),
            }
        }
        Err(ADDRESS_MISSING.dots())
    }
    fn addr_arr(&self) -> Result<Vec<Value>, String> {
        if let Item::ADDR_ARR { ctrl_type: _, cmd: (_, code) } = self {
            if let Value::AddressArray(arr) = &code.as_any().downcast_ref::<AddrArrCode>().unwrap().value {
                return Ok(arr.to_vec());
            }
        }
        Err(ADDR_ARR_MISSING.dots())
    }
    fn package(&self) -> Result<Vec<NodeId>, String> {
        if let Item::PACKAGE { ctrl_type: _, cmd: (_, code) } = self {
            if let Value::Packable(a3) = &code.as_any().downcast_ref::<PackageCode>().unwrap().value {
                return Ok(a3.to_vec());
            }
        }
        Err(PACKAGE_MISSING.dots())
    }
    fn index(&self) -> Result<(u8, usize), String> {
        if let Item::INDEX { cmd: (_, code) } = self {
            if let Value::Indexable(b, idx) = &code.as_any().downcast_ref::<IndexCode>().unwrap().value {
                return Ok((*b, *idx));
            }
        }
        Err(INDEX_MISSING.to_string())
    }
    fn str_matrix(&self) -> Result<DMatrix<String>, String> {
        if let Item::MATRIX { cmd: (_, code) } = self {
            if let Value::StrMatrix(m) = &code.as_any().downcast_ref::<StrMatrixCode>().unwrap().value {
                return Ok(m.clone());
            }
        }
        Err(VALUE_MISSING.dots())
    }
    fn buf_reader(&self) -> Result<Arc<Mutex<BufReader<File>>>, String> {
        if let Item::BUF_READER { cmd: (_, code) } = self {
            if let Value::Readable(rd) = &code.as_any().downcast_ref::<BufReaderCode>().unwrap().value {
                return Ok(rd.clone());
            }
        }
        Err(VALUE_MISSING.dots())
    }
    fn buf_writer(&self) -> Result<Arc<Mutex<BufWriter<File>>>, String> {
        if let Item::BUF_WRITER { cmd: (_, code) } = self {
            if let Value::Writeable(wr) = &code.as_any().downcast_ref::<BufWriterCode>().unwrap().value {
                return Ok(wr.clone());
            }
        }
        Err(VALUE_MISSING.dots())
    }
}

impl Debug for dyn Cmd {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), std::fmt::Error> {
        let rslt = match self {
            code    => code.name(),
        };
        write!(f, "ƒ{}", rslt)
    }
}

impl Debug for dyn Callable {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "value: {:?}", self.get_value().clone())
    }
}
