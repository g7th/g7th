// ©2024 Otmar Klenk
use crate::{buf_reader, buf_writer};
use crate::vpush;

use std::fs::File;
use std::io::{BufReader, BufWriter};
use std::sync::{Arc, Mutex};

use super::cmd::value::Value;
use crate::interpreter::data::DataHolder;
use crate::interpreter::item::{Item, NamedBufReader, NamedBufWriter};

pub(crate) const FILE_EXPECTED: &str        = "FILE EXPECTED";

pub type FnBufReader = fn(&mut DataHolder, Arc<Mutex<BufReader<File>>>);
pub(crate) const BUF_RD_PUSH:  FnBufReader = |dh: &mut DataHolder, rd: Arc<Mutex<BufReader<File>>>| { vpush!(dh, Value::from(rd)) };
pub type FnBufWriter = fn(&mut DataHolder, Arc<Mutex<BufWriter<File>>>);
pub(crate) const BUF_WR_PUSH:  FnBufWriter = |dh: &mut DataHolder, wr: Arc<Mutex<BufWriter<File>>>| { vpush!(dh, Value::from(wr)) };

impl From<NamedBufReader> for Item {
    fn from(rd: NamedBufReader) -> Item {
        buf_reader!(rd.name, BUF_RD_PUSH, Value::Readable(rd.kind))
    }
}

impl From<Arc<Mutex<BufReader<File>>>> for Value {
    fn from(rd: Arc<Mutex<BufReader<File>>>) -> Value {
        Value::Readable(rd)
    }
}


impl From<NamedBufWriter> for Item {
    fn from(wr: NamedBufWriter) -> Item {
        buf_writer!(wr.name, BUF_WR_PUSH, Value::Writeable(wr.kind))
    }
}

impl From<Arc<Mutex<BufWriter<File>>>> for Value {
    fn from(wr: Arc<Mutex<BufWriter<File>>>) -> Value {
        Value::Writeable(wr)
    }
}
