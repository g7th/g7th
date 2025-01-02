// ©2024 Otmar Klenk
#![allow(non_snake_case)]

use indextree::NodeId;
use std::any::Any;
use std::cell::Ref;
use std::ptr::addr_of;

use super::value::Value;
use crate::interpreter::{PROG, NOOP};
use crate::interpreter::Arena;
use crate::interpreter::data::DataHolder;
use crate::interpreter::item::branch::{GetCall, WithProgRefMut};
use crate::interpreter::item::cmd::value::PACKAGE_EXPECTED;
use crate::interpreter::tools::Dots;
use crate::interpreter::item::word::{WordCode, BooleanCode, NumberCode, MatrixCode, CharCode, StringCode, AddrCode,
                                     AddrArrCode, PackageCode, IndexCode, StrMatrixCode, BufReaderCode, BufWriterCode};

#[macro_export]
macro_rules! v {  // Value
    ($value: expr) => {{
        use crate::interpreter::item::cmd::value::Value;
        Value::from($value)
    }}
}

#[macro_export]
macro_rules! dt {  // Data
    ($source: expr) => {  // Value
        Data::from($source)
    }
}

#[macro_export]
macro_rules! immediate {  // Item
    ($name: expr, $fn_ptr: expr, $v: expr) => {{
        crate::interpreter::item::Item::WORD { ctrl_type: CtrlType::Other, cmd: ($name.to_string(), Box::new(
            crate::interpreter::WordCode {
                fn_word:    $fn_ptr,
                value:      $v,
                immediate:  true,
            }
        )) }
    }}
}

#[macro_export]
macro_rules! boolean {  // Item
    ($name: expr, $fn_ptr: expr, $v: expr) => {
        Item::BOOL { cmd: ($name.to_string(), Box::new(crate::interpreter::item::word::BooleanCode::new($fn_ptr, $v))) }
    }
}
#[macro_export]
macro_rules! number {  // Item
    ($name: expr, $fn_ptr: expr, $v: expr) => {
        Item::NUMBER { cmd: ($name.to_string(), Box::new(crate::interpreter::item::word::NumberCode::new($fn_ptr, $v))) }
    }
}
#[macro_export]
macro_rules! matrix {  // Item
    ($name: expr, $fn_ptr: expr, $v: expr) => {
        Item::MATRIX { cmd: ($name.to_string(), Box::new(crate::interpreter::item::word::MatrixCode::new($fn_ptr, $v))) }
    }
}
#[macro_export]
macro_rules! character {  // Item
    ($name: expr, $fn_ptr: expr, $v: expr) => {
        Item::CHAR { cmd: ($name.to_string(), Box::new(crate::interpreter::item::word::CharCode::new($fn_ptr, $v))) }
    }
}
#[macro_export]
macro_rules! string {  // Item
    ($name: expr, $fn_ptr: expr, $v: expr) => {
        Item::STRING { cmd: ($name.to_string(), Box::new(crate::interpreter::item::word::StringCode::new($fn_ptr, $v))) }
    }
}
#[macro_export]
macro_rules! vocabulary {  // Item
    ($name: expr, $fn_ptr: expr, $v: expr) => {
        Item::VOCABULARY { cmd: ($name.to_string(), Box::new(crate::interpreter::item::word::AddrCode::new($fn_ptr, $v))) }
    }
}
#[macro_export]
macro_rules! address {  // Item
    ($name: expr, $fn_ptr: expr, $v: expr) => {
        Item::ADDR { cmd: ($name.to_string(), Box::new(crate::interpreter::item::word::AddrCode::new($fn_ptr, $v))) }
    }
}
#[macro_export]
macro_rules! index {  // 'd'atastack, 'v'ariable
    ($name: expr, $fn_ptr: expr, $v: expr) => {
        Item::INDEX { cmd: ($name.to_string(), Box::new(crate::interpreter::item::word::IndexCode::new($fn_ptr, $v))) }
    }
}
#[macro_export]
macro_rules! str_matrix {  // Item
    ($name: expr, $fn_ptr: expr, $v: expr) => {
        Item::STR_MATRIX { cmd: ($name.to_string(), Box::new(crate::interpreter::item::word::StrMatrixCode::new($fn_ptr, $v))) }
    }
}
#[macro_export]
macro_rules! buf_reader {  // Item
    ($name: expr, $fn_ptr: expr, $v: expr) => {
        Item::BUF_READER { cmd: ($name.to_string(), Box::new(crate::interpreter::item::word::BufReaderCode::new($fn_ptr, $v))) }
    }
}
#[macro_export]
macro_rules! buf_writer {  // Item
    ($name: expr, $fn_ptr: expr, $v: expr) => {
        Item::BUF_WRITER { cmd: ($name.to_string(), Box::new(crate::interpreter::item::word::BufWriterCode::new($fn_ptr, $v))) }
    }
}
/////////////////////////////////////////////////////////////////////////////////////////
pub trait WithProgRef {
    fn get_call_at(&self, slot_id: NodeId) -> Option<Box<dyn Callable>>;
    fn is_package_immediate(&self, value: &Value) -> Result<bool, String>;
}

impl WithProgRef for Ref<'_, Arena<Box<(dyn Callable + 'static)>>> {
    fn is_package_immediate(&self, value: &Value) -> Result<bool, String> {
        let Value::Packable(a3) = value else { return Err(PACKAGE_EXPECTED.dots()); };
        match a3.get(0) {
            Some(&id0)  => match self.get_call_at(id0) {
                Some(call0)     => Ok(call0.is_immediate()),
                None            => Ok(false),
            },
            None        => Ok(false),
        }
    }

    #[inline(always)]
    fn get_call_at(&self, slot_id: NodeId) -> Option<Box<dyn Callable>> {
        Some(self.get_call(slot_id)?.clone_callable())
    }
}

/////////////////////////////////////////////////////////////////////////////////////////

pub trait Callable: CloneCallable + Send {
    fn call(&self, dh: &mut DataHolder, v: &Value);
    fn get_value(&self) -> &Value;
    fn get_value_mut(&mut self) -> &mut Value;  // AddressArray
    fn set_value(&mut self, value: &Value);
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

pub trait CloneCallable {
    fn clone_callable(&self) -> Box<dyn Callable>;
}

impl<T> CloneCallable for T where T: 'static + Callable + Clone {
    fn clone_callable(&self) -> Box<dyn Callable> {  // clone is ambiguous
        Box::new(self.clone())
    }
}

impl PartialEq for dyn Callable {
    fn eq(&self, other: &Self) -> bool {
        addr_of!(self) == addr_of!(other)
    }
}
/////////////////////////////////////////////////////////////////////////////////////////

pub trait Immediate {
    fn is_immediate(&self) -> bool;
    fn set_immediate(&mut self);
}

impl Immediate for dyn Callable {
    fn is_immediate(&self) -> bool {
        if let Some(WordCode { immediate, .. }) = self.as_any().downcast_ref::<WordCode>() {
            return *immediate;
        }
        if let Some(BooleanCode { immediate, .. }) = self.as_any().downcast_ref::<BooleanCode>() {
            return *immediate;
        }
        if let Some(NumberCode { immediate, .. }) = self.as_any().downcast_ref::<NumberCode>() {
            return *immediate;
        }
        if let Some(MatrixCode { immediate, .. }) = self.as_any().downcast_ref::<MatrixCode>() {
            return *immediate;
        }
        if let Some(CharCode { immediate, .. }) = self.as_any().downcast_ref::<CharCode>() {
            return *immediate;
        }
        if let Some(StringCode { immediate, .. }) = self.as_any().downcast_ref::<StringCode>() {
            return *immediate;
        }
        if let Some(AddrCode { immediate, .. }) = self.as_any().downcast_ref::<AddrCode>() {
            return *immediate;
        }
        if let Some(AddrArrCode { immediate, .. }) = self.as_any().downcast_ref::<AddrArrCode>() {
            return *immediate;
        }
        if let Some(PackageCode { value, .. }) = self.as_any().downcast_ref::<PackageCode>() {
            return PROG.with(|prg| {
                return prg.borrow().is_package_immediate(value).ok().unwrap();
            })
        }
        if let Some(IndexCode { immediate, .. }) = self.as_any().downcast_ref::<IndexCode>() {
            return *immediate;
        }
        if let Some(StrMatrixCode { immediate, .. }) = self.as_any().downcast_ref::<StrMatrixCode>() {
            return *immediate;
        }
        if let Some(BufReaderCode { immediate, .. }) = self.as_any().downcast_ref::<BufReaderCode>() {
            return *immediate;
        }
        if let Some(BufWriterCode { immediate, .. }) = self.as_any().downcast_ref::<BufWriterCode>() {
            return *immediate;
        }

        false
    }

    fn set_immediate(&mut self) {
        if let Some(WordCode { immediate, .. }) = self.as_any_mut().downcast_mut::<WordCode>() {
           *immediate = true;
        }
        if let Some(BooleanCode { immediate, .. }) = self.as_any_mut().downcast_mut::<BooleanCode>() {
            *immediate = true;
        }
        if let Some(NumberCode { immediate, .. }) = self.as_any_mut().downcast_mut::<NumberCode>() {
            *immediate = true;
        }
        if let Some(MatrixCode { immediate, .. }) = self.as_any_mut().downcast_mut::<MatrixCode>() {
            *immediate = true;
        }
        if let Some(CharCode { immediate, .. }) = self.as_any_mut().downcast_mut::<CharCode>() {
            *immediate = true;
        }
        if let Some(StringCode { immediate, .. }) = self.as_any_mut().downcast_mut::<StringCode>() {
            *immediate = true;
        }
        if let Some(AddrCode { immediate, .. }) = self.as_any_mut().downcast_mut::<AddrCode>() {
            *immediate = true;
        }
        if let Some(AddrArrCode { immediate, .. }) = self.as_any_mut().downcast_mut::<AddrArrCode>() {
            *immediate = true;
        }
        if let Some(pkg @ PackageCode { .. }) = self.as_any_mut().downcast_mut::<PackageCode>() {
            PROG.with(|prg| {
                prg.borrow_mut().set_package_immediate(pkg);
            });
        }
        if let Some(IndexCode { immediate, .. }) = self.as_any_mut().downcast_mut::<IndexCode>() {
            *immediate = true;
        }
        if let Some(StrMatrixCode { immediate, .. }) = self.as_any_mut().downcast_mut::<StrMatrixCode>() {
            *immediate = true;
        }
        if let Some(BufReaderCode { immediate, .. }) = self.as_any_mut().downcast_mut::<BufReaderCode>() {
            *immediate = true;
        }
        if let Some(BufWriterCode { immediate, .. }) = self.as_any_mut().downcast_mut::<BufWriterCode>() {
            *immediate = true;
        }
    }
}

impl Default for Box<dyn Callable> {
    fn default() -> Self {
        NOOP.with(|noop| noop.borrow().clone_callable())
    }
}

impl Clone for Box<dyn Callable> {
    fn clone(&self) -> Self {
        self.clone_callable()
    }
}
