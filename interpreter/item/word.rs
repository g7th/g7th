// ©2024 Otmar Klenk
#![macro_use]

use crate::dt;
use crate::get_var_indexed;
use crate::get_fence_id;
use crate::{v, vpush};

use crate::immediate;
use crate::boolean;
use crate::number;
use crate::{matrix, str_matrix};
use crate::{character, string};
use crate::vocabulary;
use crate::{address, addr_arr, package};
use crate::index;
use crate::{buf_reader, buf_writer};

use indextree::NodeId;
use std::any::Any;

use super::cmd::callable::Callable;
use super::cmd::value::{GetValue, Value};
use crate::interpreter::PROG;
use crate::interpreter::data::{Data, DataHolder, Modes};
use crate::interpreter::dictionary::Dictionary;
use crate::interpreter::execute::Execute;
use crate::interpreter::item::Item;
use crate::interpreter::item::branch::{CtrlType, WithProgRefMut};
use crate::interpreter::item::cmd::Cmd;
use crate::interpreter::item::cmd::callable::Immediate;
use crate::interpreter::item::file::{FnBufReader, FnBufWriter};
use crate::interpreter::item::number::FnBool;
use crate::interpreter::item::number::FnNumber;
use crate::interpreter::item::matrix::{FnMatrix, FnStrMatrix};
use crate::interpreter::item::string::{FnChar, FnString};

pub const NO_NAME: &str = "";

pub(crate) type FnWord   = fn(&mut DataHolder, &Value);

pub(crate) type FnAddr      = fn(&mut DataHolder, Option<NodeId>);
pub(crate) const ADDR_PUSH: FnAddr  = |dh: &mut DataHolder, id_opt: Option<NodeId>| {
    match id_opt {
        Some(id)    => { vpush!(dh, v!(id)); vpush!(dh, v!(true)); }  // data -> ADDR_FETCH
        None        => vpush!(dh, v!(false)),
    }
};

pub(crate) type FnAddrArr   = fn(&mut DataHolder, &Value);
pub(crate) const ADDR_ARR_CODE: FnAddrArr  = |dh: &mut DataHolder, v: &Value| {
    vpush!(dh, v.clone());
};

pub(crate) type FnPackage   = fn(&mut DataHolder, &Value);
pub(crate) const FN_PACKAGE: FnPackage    = |dh: &mut DataHolder, v: &Value| {
    PROG.with(|prg| {
        dh.exec(&prg.borrow(), v.package().as_slice());  // Execute
    });
};

pub(crate) type FnIndex     = fn(&mut DataHolder, v: &Value);
pub(crate) const IDX_PUSH: FnIndex  = |dh: &mut DataHolder, v: &Value| {  // variable
    match v {
        index @ Value::Indexable(_b, _idx)  => dh.data.push(dt!(index.clone())),
        _                                   => unreachable!(),
    }
};
pub(crate) const VAL_PUSH: FnIndex  = |dh: &mut DataHolder, v: &Value| {  // constant
    match v {
        index @ Value::Indexable(_b, idx)   => {
            let var = get_var_indexed!(dh, *idx);
            if var == dt!(Value::Unassigned) {
                dh.data.push(dt!(index.clone()));
            }
            else {
                dh.data.push(dt!(var));
            }
        },
        _                                   => unreachable!(),
    }
};

/////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct WordCode {
    pub(crate) fn_word: FnWord,
    pub(crate) value: Value,
    pub(crate) immediate: bool,
}
#[derive(Clone)]
pub struct BooleanCode {
    fn_bool: FnBool,
    pub(crate) value: Value,
    pub(crate) immediate: bool,
}
#[derive(Clone)]
pub struct NumberCode {
    fn_number: FnNumber,
    pub(crate) value: Value,
    pub(crate) immediate: bool,
}
#[derive(Clone)]
pub struct MatrixCode {
    fn_matrix: FnMatrix,
    pub(crate) value: Value,
    pub(crate) immediate: bool,
}
#[derive(Clone)]
pub struct CharCode {
    fn_char: FnChar,
    pub(crate) value: Value,
    pub(crate) immediate: bool,
}
#[derive(Clone)]
pub struct StringCode {
    fn_string: FnString,
    pub(crate) value: Value,
    pub(crate) immediate: bool,
}
#[derive(Clone)]
pub struct AddrCode {
    fn_addr: FnAddr,
    pub(crate) value: Value,
    pub(crate) immediate: bool,
}
#[derive(Clone)]
pub struct AddrArrCode {
    pub(crate) fn_addr_arr: FnAddrArr,
    pub(crate) value: Value,
    pub(crate) immediate: bool,
}
#[derive(Clone, Debug)]
pub struct PackageCode {
    pub(crate) fn_package: FnPackage,
    pub(crate) value: Value,
}
#[derive(Clone)]
pub struct IndexCode {
    fn_index: FnIndex,
    pub(crate) value: Value,
    pub(crate) immediate: bool,
}
#[derive(Clone)]
pub struct StrMatrixCode {
    fn_str_matrix: FnStrMatrix,
    pub(crate) value: Value,
    pub(crate) immediate: bool,
}
#[derive(Clone)]
pub struct BufReaderCode {
    fn_buf_reader: FnBufReader,
    pub(crate) value: Value,
    pub(crate) immediate: bool,
}
#[derive(Clone)]
pub struct BufWriterCode {
    fn_buf_writer: FnBufWriter,
    pub(crate) value: Value,
    pub(crate) immediate: bool,
}

/////////////////////////////////////////////////////////////////////////////////////////
impl WordCode {
    pub fn new(fn_word: FnWord, value: Value) -> WordCode {
        Self {
            fn_word,
            value,
            immediate: false,
        }
    }
}
impl BooleanCode {
    pub fn new(fn_bool: FnBool, value: Value) -> BooleanCode {
        Self {
            fn_bool,
            value,
            immediate: false,
        }
    }
}
impl NumberCode {
    pub fn new(fn_number: FnNumber, value: Value) -> NumberCode {
        Self {
            fn_number,
            value,
            immediate: false,
        }
    }
}
impl MatrixCode {
    pub fn new(fn_matrix: FnMatrix, value: Value) -> MatrixCode {
        Self {
            fn_matrix,
            value,
            immediate: false,
        }
    }
}
impl CharCode {
    pub fn new(fn_char: FnChar, value: Value) -> CharCode {
        Self {
            fn_char,
            value,
            immediate: false,
        }
    }
}
impl StringCode {
    pub fn new(fn_string: FnString, value: Value) -> StringCode {
        Self {
            fn_string,
            value,
            immediate: false,
        }
    }
}
impl AddrCode {
    pub fn new(fn_addr: FnAddr, value: Value) -> AddrCode {
        Self {
            fn_addr,
            value,
            immediate: false,
        }
    }
}
impl AddrArrCode {
    pub fn new(fn_addr_arr: FnAddrArr, value: Value) -> AddrArrCode {
        Self {
            fn_addr_arr,
            value,
            immediate: false,
        }
    }
}
impl PackageCode {
    pub fn new(fn_package: FnAddrArr, value: Value) -> PackageCode {
        Self {
            fn_package,
            value,
        }
    }

    pub fn set_immediate(dh: &mut DataHolder) {  // last word id
        let fence_id = get_fence_id!(dh);
        if let Some(last_id) = latest_lib_id!(dh) {
            if last_id != fence_id {
                let code: &mut dyn Callable = dh.dictionary.library[last_id].get_mut().code_mut();
                let pkg: &mut PackageCode = code.as_any_mut().downcast_mut::<PackageCode>().unwrap();
                PROG.with(|prg| {
                    prg.borrow_mut().set_package_immediate(pkg);
                });
                return;
            }
        }  // tel fi
        eprintln!("immediate: NOT SET");
    }
}
impl IndexCode {
    pub fn new(fn_index: FnIndex, value: Value) -> IndexCode {
        Self {
            fn_index,
            value,
            immediate: false,
        }
    }
}
impl StrMatrixCode {
    pub fn new(fn_str_matrix: FnStrMatrix, value: Value) -> StrMatrixCode {
        Self {
            fn_str_matrix,
            value,
            immediate: false,
        }
    }
}
impl BufReaderCode {
    pub fn new(fn_buf_reader: FnBufReader, value: Value) -> BufReaderCode {
        Self {
            fn_buf_reader,
            value,
            immediate: false,
        }
    }
}
impl BufWriterCode {
    pub fn new(fn_buf_writer: FnBufWriter, value: Value) -> BufWriterCode {
        Self {
            fn_buf_writer,
            value,
            immediate: false,
        }
    }
}

/////////////////////////////////////////////////////////////////////////////////////////
impl Clone for Item {
    fn clone(&self) -> Self {
        match self {
            Item::WORD { ctrl_type: _, cmd: (w, code) }                 => {
                let opt = code.as_any().downcast_ref::<WordCode>().cloned().unwrap();
                immediate!(w, opt.fn_word, opt.value)
            },
            Item::BOOL { cmd: (w, code) }           => {
                let opt = code.as_any().downcast_ref::<BooleanCode>().cloned().unwrap();
                boolean!(w, opt.fn_bool, opt.value)
            },
            Item::NUMBER { cmd: (w, code) }         => {
                let opt = code.as_any().downcast_ref::<NumberCode>().cloned().unwrap();
                number!(w, opt.fn_number, opt.value)
            },
            Item::MATRIX { cmd: (w, code) }         => {
                let opt = code.as_any().downcast_ref::<MatrixCode>().cloned().unwrap();
                matrix!(w, opt.fn_matrix, opt.value)
            },
            Item::CHAR { cmd: (w, code) }           => {
                let opt = code.as_any().downcast_ref::<CharCode>().cloned().unwrap();
                character!(w, opt.fn_char, opt.value)
            },
            Item::STRING { cmd: (w, code) }         => {
                let opt = code.as_any().downcast_ref::<StringCode>().cloned().unwrap();
                string!(w, opt.fn_string, opt.value)
            },
            Item::VOCABULARY { cmd: (w, code) }     => {
                let opt = code.as_any().downcast_ref::<AddrCode>().cloned().unwrap();
                vocabulary!(w, opt.fn_addr, opt.value)
            },
            Item::ADDR { cmd: (w, code) }           => {
                let opt = code.as_any().downcast_ref::<AddrCode>().cloned().unwrap();
                address!(w, opt.fn_addr, opt.value)
            },
            Item::ADDR_ARR { ctrl_type, cmd: (w, code) }     => {
                let opt = code.as_any().downcast_ref::<AddrArrCode>().cloned().unwrap();
                addr_arr!(*ctrl_type, w, opt.fn_addr_arr, opt.value)
            },
            Item::PACKAGE { ctrl_type, cmd: (w, code) }     => {
                let opt = code.as_any().downcast_ref::<PackageCode>().cloned().unwrap();
                package!(*ctrl_type, w, opt.fn_package, opt.value)
            },
            Item::INDEX { cmd: (w, code) }          => {
                let opt = code.as_any().downcast_ref::<IndexCode>().cloned().unwrap();
                index!(w, opt.fn_index, opt.value)
            },
            Item::STR_MATRIX { cmd: (w, code) }         => {
                let opt = code.as_any().downcast_ref::<StrMatrixCode>().cloned().unwrap();
                str_matrix!(w, opt.fn_str_matrix, opt.value)
            },
            Item::BUF_READER { cmd: (w, code) }         => {
                let opt = code.as_any().downcast_ref::<BufReaderCode>().cloned().unwrap();
                buf_reader!(w, opt.fn_buf_reader, opt.value)
            },
            Item::BUF_WRITER { cmd: (w, code) }         => {
                let opt = code.as_any().downcast_ref::<BufWriterCode>().cloned().unwrap();
                buf_writer!(w, opt.fn_buf_writer, opt.value)
            },
        }
    }
}

/////////////////////////////////////////////////////////////////////////////////////////

impl Callable for WordCode {  // v -> hook
    fn call(&self, dh: &mut DataHolder, v: &Value) {
        let call: &dyn Callable = self;
        if call.is_immediate() {
            dh.do_interpret();
        }
        (self.fn_word)(dh, v)
    }
    fn get_value(&self) -> &Value {
        &self.as_any().downcast_ref::<WordCode>().unwrap().value
    }
    fn get_value_mut(&mut self) -> &mut Value {
        &mut self.as_any_mut().downcast_mut::<WordCode>().unwrap().value
    }
    fn set_value(&mut self, value: &Value) {
        *(&mut self.value) = value.clone();
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl Callable for BooleanCode {
    fn call(&self, dh: &mut DataHolder, v: &Value) {
        (self.fn_bool)(dh, &v.boolean())
    }
    fn get_value(&self) -> &Value {
        &self.as_any().downcast_ref::<BooleanCode>().unwrap().value
    }
    fn get_value_mut(&mut self) -> &mut Value {
        &mut self.as_any_mut().downcast_mut::<BooleanCode>().unwrap().value
    }
    fn set_value(&mut self, value: &Value) {
        *(&mut self.value) = value.clone();
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}
impl Callable for NumberCode {
    fn call(&self, dh: &mut DataHolder, v: &Value) {
        (self.fn_number)(dh, &v.number());
    }
    fn get_value(&self) -> &Value {
        &self.as_any().downcast_ref::<NumberCode>().unwrap().value
    }
    fn get_value_mut(&mut self) -> &mut Value {
        &mut self.as_any_mut().downcast_mut::<NumberCode>().unwrap().value
    }
    fn set_value(&mut self, value: &Value) {
        *(&mut self.value) = value.clone();
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}
impl Callable for MatrixCode {
    fn call(&self, dh: &mut DataHolder, v: &Value) {
        (self.fn_matrix)(dh, &v.matrix())
    }
    fn get_value(&self) -> &Value {
        &self.as_any().downcast_ref::<MatrixCode>().unwrap().value
    }
    fn get_value_mut(&mut self) -> &mut Value {
        &mut self.as_any_mut().downcast_mut::<MatrixCode>().unwrap().value
    }
    fn set_value(&mut self, value: &Value) {
        *(&mut self.value) = value.clone();
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}
impl Callable for CharCode {
    fn call(&self, dh: &mut DataHolder, v: &Value) {
        (self.fn_char)(dh, &v.character())
    }
    fn get_value(&self) -> &Value {
        &self.as_any().downcast_ref::<CharCode>().unwrap().value
    }
    fn get_value_mut(&mut self) -> &mut Value {
        &mut self.as_any_mut().downcast_mut::<CharCode>().unwrap().value
    }
    fn set_value(&mut self, value: &Value) {
        *(&mut self.value) = value.clone();
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}
impl Callable for StringCode {
    fn call(&self, dh: &mut DataHolder, v: &Value) {
        (self.fn_string)(dh, &v.string())
    }
    fn get_value(&self) -> &Value {
        &self.as_any().downcast_ref::<StringCode>().unwrap().value
    }
    fn get_value_mut(&mut self) -> &mut Value {
        &mut self.as_any_mut().downcast_mut::<StringCode>().unwrap().value
    }
    fn set_value(&mut self, value: &Value) {
        *(&mut self.value) = value.clone();
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}
impl Callable for AddrCode {
    fn call(&self, dh: &mut DataHolder, v: &Value) {
        (self.fn_addr)(dh, v.address())
    }
    fn get_value(&self) -> &Value {  // returns Value::Unassigned
        &self.as_any().downcast_ref::<AddrCode>().unwrap().value
    }
    fn get_value_mut(&mut self) -> &mut Value {
        &mut self.as_any_mut().downcast_mut::<AddrCode>().unwrap().value
    }
    fn set_value(&mut self, value: &Value) {
        *(&mut self.value) = value.clone();
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}
impl Callable for AddrArrCode {
    fn call(&self, dh: &mut DataHolder, v: &Value) {
        (self.fn_addr_arr)(dh, v)  // Vec<Vec<Call>>
    }
    fn get_value(&self) -> &Value {  // returns Value::Unassigned
        &self.as_any().downcast_ref::<AddrArrCode>().unwrap().value
    }
    fn get_value_mut(&mut self) -> &mut Value {
        &mut self.as_any_mut().downcast_mut::<AddrArrCode>().unwrap().value
    }
    fn set_value(&mut self, value: &Value) {
        *(&mut self.value) = value.clone();
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}
impl Callable for PackageCode {
    fn call(&self, dh: &mut DataHolder, v: &Value) {
        (self.fn_package)(dh, v);  // Vec<NodeId>
    }
    fn get_value(&self) -> &Value {  // returns Value::Unassigned
        &self.as_any().downcast_ref::<PackageCode>().unwrap().value
    }
    fn get_value_mut(&mut self) -> &mut Value {
        &mut self.as_any_mut().downcast_mut::<PackageCode>().unwrap().value
    }
    fn set_value(&mut self, value: &Value) {
        *(&mut self.value) = value.clone();
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}
impl Callable for IndexCode {
    fn call(&self, dh: &mut DataHolder, v: &Value) {
        (self.fn_index)(dh, v);
    }
    fn get_value(&self) -> &Value {
        &self.as_any().downcast_ref::<IndexCode>().unwrap().value
    }
    fn get_value_mut(&mut self) -> &mut Value {
        &mut self.as_any_mut().downcast_mut::<IndexCode>().unwrap().value
    }
    fn set_value(&mut self, value: &Value) {
        *(&mut self.value) = value.clone();
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}
impl Callable for StrMatrixCode {
    fn call(&self, dh: &mut DataHolder, v: &Value) {
        (self.fn_str_matrix)(dh, &v.str_matrix())
    }
    fn get_value(&self) -> &Value {
        &self.as_any().downcast_ref::<StrMatrixCode>().unwrap().value
    }
    fn get_value_mut(&mut self) -> &mut Value {
        &mut self.as_any_mut().downcast_mut::<StrMatrixCode>().unwrap().value
    }
    fn set_value(&mut self, value: &Value) {
        *(&mut self.value) = value.clone();
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}
impl Callable for BufReaderCode {
    fn call(&self, dh: &mut DataHolder, v: &Value) {
        (self.fn_buf_reader)(dh, v.buf_reader())
    }
    fn get_value(&self) -> &Value {
        &self.as_any().downcast_ref::<BufReaderCode>().unwrap().value
    }
    fn get_value_mut(&mut self) -> &mut Value {
        &mut self.as_any_mut().downcast_mut::<BufReaderCode>().unwrap().value
    }
    fn set_value(&mut self, value: &Value) {
        *(&mut self.value) = value.clone();
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}
impl Callable for BufWriterCode {
    fn call(&self, dh: &mut DataHolder, v: &Value) {
        (self.fn_buf_writer)(dh, v.buf_writer())
    }
    fn get_value(&self) -> &Value {
        &self.as_any().downcast_ref::<BufWriterCode>().unwrap().value
    }
    fn get_value_mut(&mut self) -> &mut Value {
        &mut self.as_any_mut().downcast_mut::<BufWriterCode>().unwrap().value
    }
    fn set_value(&mut self, value: &Value) {
        *(&mut self.value) = value.clone();
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}
