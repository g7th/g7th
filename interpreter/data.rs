// ©2024 Otmar Klenk
#![allow(non_snake_case)]

use crate::{disc, dt};
use crate::reg_exit;
use crate::{variable, v, vpop, vpush};
use crate::idx;
use crate::modulo;

use std::f64::consts;
use std::cell::OnceCell;
use std::cmp::max;
use std::fs;
use std::io;
use std::mem;
use std::ops::Range;
use std::panic::panic_any;
use std::path::PathBuf;
use std::result::Result;
use std::sync::atomic::{AtomicU8, Ordering::SeqCst};
use std::sync::Arc;

use bigdecimal::{BigDecimal, FromPrimitive, One, Zero};
use bigdecimal::num_bigint::BigInt;
use indextree::NodeId;
use nalgebra::DMatrix;

use super::{BYE, EXIT, FREE, PROG};
use super::{PROMPT, PRMT, TOKEN};
use super::data::Mode::{Compile, Interpret, CleanOn, CleanOff};
use super::dictionary::{Dictionary, WithLibrary};
use super::finder::{CodeFinder, IdFinder, node_item};
use super::item::Item;
use super::item::branch::{CtrlType, GetCall, WithProgRefMut};
use super::item::cmd::Cmd;
use super::item::cmd::callable::Callable;
use super::item::cmd::value::ARRAY_EXPECTED;
use super::item::cmd::value::{GetValue, Value};
use super::item::MakeValue;
use super::item::number::{IntDiv, Number, precision};
use super::item::word::IDX_PUSH;
use super::leaf::Leaf;
use super::line_reader::LineReader;
use super::tools::Dots;
use super::tools::mul_div;
use super::vocabulary::UNKNOWN;
use super::vocabulary::Vocabulary;
use super::with_deque::WithDeque;

const ANGLE: (f64, f64, f64)    = (180.0, consts::PI, 200.0);

pub(crate) const EMPTY_STACK: &str          = "EMPTY STACK";
pub(crate) const EMPTY_RETURN_STACK: &str   = "EMPTY RETURN-STACK";
pub(crate) const NO_VALUE: &str             = "NO VALUE";
pub(crate) const NOT_FOUND: &str            = "NOT_FOUND";
const UNSUPPORTED_OP: &str                  = "UNSUPPORTED OP";

#[macro_export]
macro_rules! n {         // -> n
    ($len: expr, $n: expr) => {{
        let (r, b) = $len.overflowing_sub($n);
        if b { panic_any("INVALID STACK ACCESS"); }
        r
    }}
}
macro_rules! nth_get {   // -> Option<Data>
    ($data: expr, $n: expr) => {{
        let len = $data.len();
        $data.get(n!(len, $n))
    }}
}
//macro_rules! nth_get_mut {   // -> &mut Data
//    ($stack: expr, $n: expr) => {{
//        use crate::interpreter::tools::Dots;
//        let len = $stack.dh.data.len();
//        if let Some(b) = $stack.dh.data.get_mut(n!(len, $n)) {
//            &**b
//        }
//        else { panic_any(EMPTY_STACK.dots()); }
//    }}
//}
macro_rules! nth_rm {   // Data
    ($data: expr, $n: expr) => {
        $data.remove(n!($data.len(), $n))  // n! panics EMPTY_STACK
    }
}

#[macro_export]
macro_rules! ipush {  // push(ITEM)
    ($data: expr, $word: expr) => {
        $data.push(crate::interpreter::data::Data::ITEM($word))
    }
}
#[macro_export]
macro_rules! ipop {  // Data  -> nth_rm!(self, 0)
    ($dh: expr) => {{
        match $dh.pop() {
            use crate::interpreter::tools::Dots;
            Ok(data)    => match data {
                crate::interpreter::data::Data::ITEM(item)  => item,
                _       => std::panic::panic_any(crate::interpreter::data::NO_ITEM.dots()),
            }
            Err(err)    => { return $dh.error(&err); }
        }
    }}
}

#[macro_export]
macro_rules! vpeek {
    ($self: expr) => {{
        use crate::interpreter::tools::Dots;
        match $self.peek() {
            crate::interpreter::data::Data::VALUE(v)    => v,
            _       => panic_any(crate::interpreter::data::NO_VALUE.dots()),
        }
    }}
}
#[macro_export]
macro_rules! vpush {
    ($self: expr, $word: expr) => {{
        $self.push_data(crate::interpreter::data::Data::VALUE($word))
    }}
}
#[macro_export]
macro_rules! vpop {  // Data  -> nth_rm!(self, 0)
    ($self: expr) => {{
        use crate::interpreter::tools::Dots;
        match $self.pop() {
            Ok(data) => match data {
                crate::interpreter::data::Data::VALUE(v)    => v,
                _       => panic_any(crate::interpreter::data::NO_VALUE.dots()),
            }
            Err(err)    => { $self.error(&err); Value::Unassigned },
        }
    }}
}

#[macro_export]
macro_rules! pop_bool {  // bool  -> nth_rm!(self, 0)
    ($self: expr) => {{
        use crate::interpreter::item::MakeValue;
        use crate::interpreter::tools::Dots;
        match $self.pop() {
            Ok(crate::interpreter::data::Data::VALUE(v))    => match v {
                Value::Boolean(b)   => Ok(b),
                _                   => { crate::reg_exit!($self); Err(crate::interpreter::item::number::BOOL_EXPECTED.dots()) }
            }
            Ok(crate::interpreter::data::Data::ITEM(item))  => match item.make_value() {
                Value::Boolean(b)   => Ok(b),
                _                   => { crate::reg_exit!($self); Err(crate::interpreter::item::number::BOOL_EXPECTED.dots()) }
            }
            Err(err)                => { crate::reg_exit!($self); Err(err) }
        }
    }}
}

//#[macro_export]
//macro_rules! peek_i64 {  // i64  -> nth_rm!(self, 0)
//    ($dh: expr) => {{
//        use crate::interpreter::tools::Dots;
//        match DataHolder::peek($dh) {
//            crate::interpreter::data::Data::VALUE(v) => match v {
//                Value::Numerical(n) => i64::from(n),
//                _                   => panic_any(crate::interpreter::item::number::NUMBER_EXPECTED.dots()),
//            }
//            _                   => panic_any(crate::interpreter::item::number::NUMBER_EXPECTED.dots()),
//        }
//    }}
//}
#[macro_export]
macro_rules! pop_i64 {  // i64  -> nth_rm!(self, 0)
    ($dh: expr) => {{
        use crate::interpreter::tools::Dots;
        match $dh.pop() {
            Ok(crate::interpreter::data::Data::VALUE(v)) => match v {
                Value::Numerical(n) => i64::from(n),
                _                   => panic_any(crate::interpreter::item::number::NUMBER_EXPECTED.dots()),
            }
            Err(err)            => {eprintln!("{err}"); reg_exit!($dh); -1},
            _                   => panic_any(crate::interpreter::item::number::NUMBER_EXPECTED.dots()),
        }
    }}
}
#[macro_export]
macro_rules! pop_f64 {  // f64  -> nth_rm!(self, 0)
    ($dh: expr) => {{
        use crate::interpreter::tools::Dots;
        match $dh.pop() {
            Ok(crate::interpreter::data::Data::VALUE(v)) => match v {
                Value::Numerical(n) => f64::from(n),
                _                   => panic_any(crate::interpreter::item::number::NUMBER_EXPECTED.dots()),
            }
            Err(err)            => {eprintln!("{err}"); reg_exit!($dh); -1.0},
            _                   => panic_any(crate::interpreter::item::number::NUMBER_EXPECTED.dots()),
        }
    }}
}
#[macro_export]
macro_rules! pop_str {  // &str  -> nth_rm!(self, 0)
    ($dh: expr) => {
        &vpop!($dh).string()
    }
}
#[macro_export]
macro_rules! pop_bd {  // &BigDecimal -> nth_rm!(self, 0)
    ($dh: expr) => {
        &vpop!($dh).big_decimal()
    }
}

#[macro_export]
macro_rules! get_fence_id {
    ($dh: expr) => {
	   $dh.get_node_id("fence").unwrap()
    }
}

macro_rules! is_index_item {
    ($target: expr) => {  // &Item
        $target.index().is_ok()
    }
}

#[macro_export]
macro_rules! get_var_indexed {  // -> Data
    ($dh: expr, $idx: expr) => {  // usize, Value
        if let Some(var) = $dh.vars.get($idx) {
            var.clone()
        } else { unreachable!() }
    }
}

#[macro_export]
macro_rules! set_var_indexed {  //  -> bool
    ($dh: expr, $idx: expr, $data: expr) => {  // usize, Data
        if let Some(var) = $dh.vars.get_mut($idx) {
            *var = $data;
            true
        }
        else {
            false
        }
    }
}

/////////////////////////////////////////////////////////////////////////////////////////

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Mode {
    Interpret,
    Compile,
    CleanOn,
    CleanOff,
}

impl Mode {
    #[inline(always)]
    pub(crate) fn set(&mut self, mode: Mode) -> Mode {
        mem::replace(self, mode)
    }
}

pub trait Modes {
    fn in_interpret_mode(&self) -> bool;
    fn in_clean_prg_mode(&self) -> bool;
    fn do_compile(&mut self) -> Mode;       // returns former Mode
    fn do_interpret(&mut self) -> Mode;     // returns former Mode
    fn do_clean_prg(&mut self) -> Mode;     // returns former Mode
    fn dont_clean_prg(&mut self) -> Mode;   // returns former Mode
}

impl Modes for DataHolder<'_> {
    #[inline(always)]
    fn in_interpret_mode(&self) -> bool {
        self.compile_mode == Interpret && self.ctrl_level == 0
    }
    #[inline(always)]
    fn in_clean_prg_mode(&self) -> bool {
        self.clean_mode == CleanOn
    }
    #[inline(always)]
    fn do_compile(&mut self) -> Mode {
        self.compile_mode.set(Compile)
    }
    #[inline(always)]
    fn do_interpret(&mut self) -> Mode {
        self.compile_mode.set(Interpret)
    }
    #[inline(always)]
    fn do_clean_prg(&mut self) -> Mode {
        self.clean_mode.set(CleanOn)
    }
    #[inline(always)]
    fn dont_clean_prg(&mut self) -> Mode {
        self.clean_mode.set(CleanOff)
    }
}

/////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug, PartialEq)]
pub enum Data {
    ITEM(Item),
    VALUE(Value),
}

pub struct DataHolder<'a> {
    pub(crate) free_id:         NodeId,
    pub vocabulary:             Vocabulary,
    pub dictionary:             Dictionary,
    args:                       Vec<Data>,
    pub(crate) loop_arr:        Vec<[i64; 2]>,    // from to <- leave
    pub(crate) case_arr:        Vec<Data>,
    pub(crate) ret_arr:         Vec<Data>,
    bye:                        Arc<AtomicU8>,
    pub(crate) compile_mode:    Mode,
    pub(crate) clean_mode:      Mode,
    pub(crate) ctrl_level:      u32,
    angle:                      u8,      // Deg, Rad, Grad, d°m's"
    pi:                         Value,
    line_reader:                LineReader<'a>,
    #[allow(dead_code)]
    scale:                      usize,
    pub(crate) leaf:            Leaf,  // prog compile
    pub(crate) data:            Vec<Data>,
    pub(crate) vars:            Vec<Data>,  // VARIABLE
}

impl DataHolder<'_> {
    pub fn new() -> Self {
        let mut line_reader = LineReader::new(PROMPT, PRMT);
        let _ = line_reader.loadf("load.7th");

        let once_id = {
            let cell = OnceCell::new();
            *cell.get_or_init(|| {
                let call: &dyn Callable = &*FREE.with(|free| { free.borrow().clone_callable() });
                PROG.with(|prg| {
                    prg.borrow_mut().new_node(call.clone_callable())
                })
            })
        };
        let mut rslt = Self {
            free_id:        once_id,
            vocabulary:     Vocabulary::new(),
            dictionary:     Dictionary::new(),
            args:           vec![],
            loop_arr:       Vec::<[i64; 2]>::with_capacity(4),  // from to <- leave
            case_arr:       Vec::<Data>::with_capacity(2),
            ret_arr:        Vec::<Data>::with_capacity(8),
            bye:            Arc::new(AtomicU8::new(0)),
            compile_mode:   Mode::Compile,
            clean_mode:     Mode::CleanOff,
            ctrl_level:     0,
            angle:          b'R',     // Deg, Rad, Grad, d°m's"
            pi:             v!(BigDecimal::from_f64(consts::PI).unwrap()),
            line_reader,
            scale:          8,
            leaf:           Leaf::new(),
            data:           Vec::<Data>::with_capacity(128),
            vars:           vec![],
        };
        let id = rslt.dictionary.library.create_vocab("7th");
        rslt.vocabulary.ins_core(rslt.dictionary.library[id].get().clone());

        variable!(&mut rslt, "fence", IDX_PUSH);
        variable!(&mut rslt, "isexact", IDX_PUSH);  // trig functions
        variable!(&mut rslt, "iseqtype", IDX_PUSH);  // (2) Eq trait
        variable!(&mut rslt, "nonce$", IDX_PUSH);

        let bye = rslt.bye.clone();
        ctrlc::set_handler(move || {bye.fetch_or(BYE, SeqCst); println!("Ctrl-C");}).expect("no ctrl-C handler");

        rslt
    }

    #[inline]
    pub(crate) fn set_curr_id(&mut self, ctrl_type: CtrlType, name: &str, new_id: NodeId) {
        match ctrl_type {
            CtrlType::Intro => self.push_ctrl(ctrl_type, name, new_id),
            _               => {},
        }
        self.leaf.rstack_push(ctrl_type, new_id);  // rstack.push
        debug_assert_ne!(name, "\n");
    }

    #[inline]
    pub(crate) fn push_ctrl(&mut self, ctrl_type: CtrlType, name: &str, new_id: NodeId) {
        match ctrl_type {  // dependingly
            CtrlType::Other     => {
            },
            CtrlType::Cont      => {
                unreachable!();
            },
            CtrlType::Intro     => {
                WithDeque::push_ctrl_slot(&mut self.leaf.ctrls, (name.to_string(), new_id));  // ☕ ctrls.push()
            },
            CtrlType::End       => {  /* prepare new [0] line in set_curr_id() */
                unreachable!();
            },
        }
    }

    pub(crate) fn dot_s(&self) {
        if self.vocabulary.base == 10 {
            println!("{}---", self);
        }
        else {
            println!("{}-‹{}›-", self, self.vocabulary.base);
        }
    }

    #[inline(always)]
    pub(crate) fn has_next(&mut self) -> bool {
        self.line_reader.has_next()
    }
    #[inline(always)]
    pub(crate) fn next_tuple(&mut self, stdout: &mut io::StdoutLock) -> Option<(TOKEN, String)> {
        let reason = self.bye(EXIT);
        self.bye_unreg(reason);
        let rslt = self.line_reader.next_tuple(stdout, reason, self.in_interpret_mode());
        rslt
    }

    #[inline(always)]
    pub(crate) fn next_word(&mut self, stdout: &mut io::StdoutLock) -> String {
        let reason = self.bye(EXIT);
        self.bye_unreg(reason);
        let Some((_, rslt)) = self.line_reader.next_tuple(stdout, reason, self.in_interpret_mode()) else { panic_any("EOL") };
        rslt
    }
    #[inline]
    pub(crate) fn compile_inject(&mut self, name: &str) {
        let rslt = Dictionary::find_word_id(self, name);
        match rslt {
            Ok(id)      => {
                PROG.with(|prg| {  // ඞ
                    prg.borrow_mut().get_call_at_mut(id).expect("ENTRY").set_value(&Value::Addressable(id));
                });
            }
            Err(err)    => eprintln!("compile_inject: {} {name}", err.to_string()),
        }
    }
    #[inline]
    pub(crate) fn accept(&mut self) {  // <- expect$
        let input = self.line_reader.accept();
        vpush!(self, Value::Textual(input));
    }

    #[inline(always)]
    pub(crate) fn clear(&mut self) {
        self.data.clear();
    }

    #[inline(always)]
    pub(crate) fn pop(&mut self) -> Result<Data, String> {
        let Some(data) = self.data.pop() else { return Err(EMPTY_STACK.dots()); };
        Ok(data)
    }

    #[inline(always)]
    pub(crate) fn push_data(&mut self, data: Data) {
        Vec::push(&mut self.data, data);
    }

    #[inline]
    pub(crate) fn dup(&mut self) {
        let Some(data) = nth_get!(self.data, 1) else { panic_any(EMPTY_STACK.dots()) };
        self.push_data(data.clone());
    }
    #[inline]
    pub(crate) fn twodup(&mut self) {  // 2dup
        let Some(data) = nth_get!(self.data, 2) else { panic_any(EMPTY_STACK.dots()) };
        self.push_data(data.clone());
        self.push_data(nth_get!(self.data, 2).cloned().unwrap());
    }

    #[inline]
    pub(crate) fn over(&mut self) {
        let Some(data) = nth_get!(self.data, 2) else { panic_any(EMPTY_STACK.dots()) };
        self.push_data(data.clone());
    }
    #[inline]
    pub(crate) fn under(&mut self) {
        let Some(data) = nth_get!(self.data, 1) else { panic_any(EMPTY_STACK.dots()) };
        self.data.insert(self.data.len() - 2, data.clone());
    }
    #[inline]
    pub(crate) fn peek(&self) -> Data {
        if let Some(data) = self.data.last() {
            data.clone()
        } else {
            Data::VALUE(Value::Unassigned)
        }
    }
    #[inline]
    pub(crate) fn pick(&mut self, pos: i64) {
        let Some(data) = nth_get!(self.data, pos as usize) else { panic_any(EMPTY_STACK.dots()) };
        self.push_data(data.clone());
    }
    #[inline]
    pub(crate) fn mpick(&mut self, pos: i64) {
        let Some(data) = nth_get!(self.data, 1) else { panic_any(EMPTY_STACK.dots()) };
        self.data.insert(self.data.len() - pos as usize, data.clone());
    }
    #[inline]
    pub(crate) fn rpick(&mut self, pos: i64) {
        let Some(data) = nth_get!(self.ret_arr, pos as usize) else { panic_any(EMPTY_RETURN_STACK.dots()) };
        self.push_data(data.clone());
    }
    #[inline]
    pub(crate) fn roll(&mut self, pos: i64) {
        let rm = nth_rm!(self.data, pos as usize);
        self.push_data(rm);
    }
    #[inline]
    pub(crate) fn mroll(&mut self, pos: i64) {
        let rm = nth_rm!(self.data, 1);
        self.data.insert(self.data.len() - pos as usize, rm);
    }
    #[inline]
    pub(crate) fn rot(&mut self) {
        let rm = nth_rm!(self.data, 3);
        self.push_data(rm);
    }
    pub(crate) fn mrot(&mut self) {
        let n = n!(self.data.len(), 3);
        if let Some(data) = self.data.pop() {
            self.data.insert(n, data);
        } else { self.error(&EMPTY_STACK.dots()); return; };
    }

    pub(crate) fn swap(&mut self) {
        let pos1 = n!(self.data.len(), 1);
        let pos2 = n!(self.data.len(), 2);
        self.data.swap(pos1, pos2);
    }

    pub(crate) fn tuck(&mut self) {
        let n = n!(self.data.len(), 2);
        let data = self.peek();
        self.data.insert(n, data);
    }

    pub(crate) fn turn(&mut self) {
        let n = pop_i64!(self) as usize;
        self.arg1(n);
        for _ in 0..n {
            let data = self.argn();
            self.push_data(data);
        }
    }

    pub(crate) fn arg1(&mut self, u: usize) -> Data {
        self.args = self.data.drain(u..).collect();
        self.args.remove(0)
    }

    pub(crate) fn argn(&mut self) -> Data {
        self.args.remove(0)
    }

    pub(crate) fn as_range(&mut self) -> Range<i64> {
        let n2 = pop_i64!(self);
        let n1 = pop_i64!(self);
        if n1 > n2 { Range { start: n2, end: n1 } } else { Range { start: n1, end: n2 } }
    }

/////////////////////////////////////////////////////////////////////////////////////////
    pub(crate) fn store(&mut self, op: char) {
        match self.pop() {  // VARIABLE or MATRIX
           Ok(data)            => match data {
                Data::ITEM(target)                      => match self.pop() {
                    Ok(Data::VALUE(source_value)) if is_index_item!(target)         => {
                        let src = match source_value {
                            Value::Indexable(_b, idx)           => get_var_indexed!(self, idx),
                            _                                   => dt!(source_value),
                        };
                        return self.set_variable(&target.name(), dt!(src));  // (1.)
                    },
                    Ok(Data::VALUE(mut source_value))                               => {
                        match op {
                            '+' => {
                                match target {
                                    Item::STRING {..}           => source_value = v!(self.vfetch(&target.name()).string() + &source_value.string()),
                                    _                           => source_value = v!(self.vfetch(&target.name()).number() + source_value.number()),
                                }
                            },
                            '-'                                 => source_value = v!(self.vfetch(&target.name()).number() - source_value.number()),
                            '*'                                 => source_value = v!(self.vfetch(&target.name()).number() * source_value.number()),
                            '/'                                 => source_value = v!(self.vfetch(&target.name()).number() / source_value.number()),
                            _                                   => {},
                        }
                        return self.set_variable(&target.name(), dt!(source_value));  // (2.)
                    },  // eulav_ecruos
                    Ok(Data::ITEM(_))                       => unreachable!(),
                    Err(err)                                => {eprintln!("{err}"); reg_exit!(self);}
                },
                Data::VALUE(Value::Numerical(idx2))                                 => {
                    match self.pop() {
                        Ok(mut data2)    => match data2 {
////////////
                            Data::ITEM(ref mut mat)  => {
                                match mat.code().get_value() {
                                    Value::Vectorial(v)     => {
                                        let m = &mut v.clone();
                                        *m.index_mut((0, idx!(idx2))) = pop_f64!(self);
                                        mat.code_mut().set_value(&Value::Vectorial(m.clone()));
                                    }
                                    Value::StrMatrix(v)     => {
                                        let m = &mut v.clone();
                                        *m.index_mut((0, idx!(idx2))) = pop_str!(self).to_string();
                                        mat.code_mut().set_value(&Value::StrMatrix(m.clone()));
                                    }
                                    _   => unreachable!(),
                                }
                            }
////////////
                            Data::VALUE(Value::Vectorial(mut a))    => {
                                *a.index_mut((0, idx!(idx2))) = pop_f64!(self);
                                vpush!(self, v!(a));
                            }
                            Data::VALUE(Value::StrMatrix(mut m))    => {
                                *m.index_mut((0, idx!(idx2))) = pop_str!(self).to_string();
                                vpush!(self, v!(m));
                            }
                            Data::VALUE(Value::Numerical(ref idx1)) => {
                                match self.pop() {
                                    Ok(data)            => match data {
                                        Data::VALUE(Value::Vectorial(mut a))    => *a.index_mut((idx!(idx2), idx!(idx1.clone()))) = pop_f64!(self),
                                        Data::VALUE(Value::StrMatrix(mut m))    => *m.index_mut((idx!(idx2), idx!(idx1.clone()))) = pop_str!(self).to_string(),
                                        _               => panic_any(crate::interpreter::item::matrix::MATRIX_EXPECTED.dots()),
                                    }
                                    Err(err)            => {eprintln!("{err}"); reg_exit!(self);},
                                }
                            }
                            _           => panic_any(crate::interpreter::item::number::NUMBER_EXPECTED.dots()),
                        }
                        Err(err)    => {eprintln!("{err}"); reg_exit!(self);}
                    }
                },
                Data::VALUE(index @ Value::Indexable(b, idx))                       => {
                    match b {
                        b'd' => {
                            match self.pop() {
                                Ok(data)    => {
                                    let len = self.data.len();
                                    self.data[n!(len, idx) - 1] = data;
                                },
                                Err(err)    => {eprintln!("{err}"); reg_exit!(self);}
                            }
                        }
                        b'v' => {
                            if let Value::Indexable(_b, idx) = index {
                                match self.pop() {
                                    Ok(data)    => if ! set_var_indexed!(self, idx, data.clone()) {
                                        eprintln!("NOT STORED: {idx} {data:?}");
                                    },
                                    Err(err)    => {eprintln!("{err}"); reg_exit!(self);},
                                }
                            }
                        }
                        _    => unreachable!(),
                    }
                },
                _                                                                   => {
                    panic_any(crate::interpreter::item::number::NUMBER_EXPECTED.dots());
                },
            },
            Err(err)            => {eprintln!("{err}"); reg_exit!(self);}
        }
    }

    pub(crate) fn set_variable(&mut self, target_name: &str, source: Data) {  // VARIABLE
        match self.get_var_idx(target_name) {
            Ok(idx)     => {
                set_var_indexed!(self, idx, source);
            }
            Err(err)    => {
                if err.starts_with(NOT_FOUND) {
                    self.vars.push(source);
                }
                else {
                    eprintln!("{err}");
                }
            }
        }
    }

    #[inline]
    fn vfetch(&self, name: &str) -> Value {  // VARIABLE
        match self.get_var_idx(&name) {
            Ok(idx)     => if let Some(var) = self.vars.get(idx) {
                return match var {
                    Data::VALUE(v)      => v.clone(),
                    Data::ITEM(item)    => item.make_value(),
                };
            },
            Err(err)    => eprintln!("{err}"),
        } 
        Value::Unassigned
    }

    pub(crate) fn fetch(&mut self) {  // VARIABLE or MATRIX
        match self.pop() {
            Ok(data)            => match data {
                Data::ITEM(ref target) if is_index_item!(target)   => {
                    if let Some((b, idx)) = target.index().ok() {
                        match b {
                            b'd'    => {
                                let len = self.data.len();
                                if let Data::VALUE(value) = &self.data[n!(len, idx) - 1] {
                                    vpush!(self, value.clone());
                                }
                            }
                            b'v'    => self.push_data(get_var_indexed!(self, idx)),
                            _       => unreachable!(),
                        }
                    }
                },
                Data::ITEM(ref target)                      => {
                    vpush!(self, self.vfetch(&target.name()));
                },
                Data::VALUE(Value::Numerical(idx2))         => {
                    match self.pop() {
                        Ok(data)    => match data {
                            Data::VALUE(Value::Vectorial(m))    => vpush!(self, v!(*m.index((0, idx!(idx2))))),
                            Data::VALUE(Value::StrMatrix(m))    => vpush!(self, v!(m.index((0, idx!(idx2))))),
                            Data::VALUE(Value::Numerical(idx1)) => {
                                match self.pop() {
                                    Ok(data)                    => match data {
                                        Data::VALUE(Value::Vectorial(m))    => vpush!(self, v!(*m.index((idx!(idx2), idx!(idx1))))),
                                        Data::VALUE(Value::StrMatrix(m))    => vpush!(self, v!(m.index((idx!(idx2), idx!(idx1))))),
                                        _                                   => panic_any(crate::interpreter::item::number::NUMBER_EXPECTED.dots()),
                                    }
                                    Err(err)                => {eprintln!("{err}"); reg_exit!(self);},
                                }
                            },
                            _                               => {println!("{data:?}");
                                panic_any(crate::interpreter::item::number::NUMBER_EXPECTED.dots()) },
                        }
                        Err(_err)   => {eprintln!("{}...", ARRAY_EXPECTED); reg_exit!(self);},
                    }
                },
                Data::VALUE(Value::Indexable(b, idx))   => {
                    match b {
                        b'd'            => {
                            let len = self.data.len();
                            if let Data::VALUE(value) = &self.data[n!(len, idx) - 1] {
                                vpush!(self, value.clone());
                            }
                        }
                        b'v'            => {
                            let data = &self.vars[idx];
                            self.push_data(data.clone());
                        }
                        _               => unreachable!(),
                    }
                },
                _       => panic_any(crate::interpreter::item::number::NUMBER_EXPECTED.dots()),
            }
            Err(err)                => {eprintln!("{err}"); reg_exit!(self);},
        }
    }

    pub(crate) fn fetch_store(&mut self) {  // VARIABLE or MATRIX
        match self.pop() {
            Ok(ref mut target)  => {
                match self.pop() {
                    Ok(source)                  => {
                        let Data::VALUE(ref source_value) = source else { unreachable!(); };
                        match source_value {
                            Value::Numerical(idx2)          => {
                                match self.pop() {
                                    Ok(data)    => match data {
                                        Data::VALUE(Value::Vectorial(m))    => vpush!(self, v!(*m.index((0, idx!(idx2.clone()))))),
                                        Data::VALUE(Value::StrMatrix(m))    => vpush!(self, v!(m.index((0, idx!(idx2.clone()))))),
                                        Data::VALUE(Value::Numerical(idx1)) => {
                                            match self.pop() {
                                                Ok(data)                    => match data {
                                                    Data::VALUE(Value::Vectorial(m))    => vpush!(self, v!(*m.index((idx!(idx2.clone()), idx!(idx1))))),
                                                    Data::VALUE(Value::StrMatrix(m))    => vpush!(self, v!(m.index((idx!(idx2.clone()), idx!(idx1))))),
                                                    _                                   => panic_any(crate::interpreter::item::number::NUMBER_EXPECTED.dots()),
                                                }
                                                Err(err)                => {eprintln!("{err}"); reg_exit!(self);},
                                            }
                                        },
                                        _                               => {
                                            panic_any(crate::interpreter::item::number::NUMBER_EXPECTED.dots());
                                        },
                                    },
                                    Err(_err)   => {eprintln!("{}...", ARRAY_EXPECTED); reg_exit!(self);},
                                }
                            },
                            Value::Indexable(_b, idx)       => {
                                let Data::VALUE(target_value) = target else { unreachable!(); };
                                let src = dt!(get_var_indexed!(self, *idx));
                                let (_b, idx) = target_value.index();
                                set_var_indexed!(self, idx, src);  // (1.)
                            },
                            _                               => *target = source,
                        }
                    },
                    Err(err)                    => {eprintln!("{err}"); reg_exit!(self); return;},
                }
            },
            Err(err)            => {eprintln!("{err}"); reg_exit!(self); return;},
        }
    }

///////////////////////////////////////////////////////////////////////////////////OP's//
    pub(crate) fn add(&mut self) {
        if let Data::VALUE(item) = self.peek() {
            match item {
                Value::Character(_)     => {
                    let s2: String = String::from(vpop!(self).character());
                    if let Data::VALUE(item) = self.peek() {
                        match item {
                            Value::Character(_) => {
                                let s1: String = String::from(vpop!(self).character());
                                vpush!(self, Value::Textual(s1 + &s2));
                            },
                            Value::Textual(_)   => {
                                let s1: String = vpop!(self).string();
                                vpush!(self, Value::Textual(s1 + &s2));
                            },
                            _                   => unreachable!(),
                        }
                    }
                },
                Value::Textual(_)       => {
                    let s2: String = vpop!(self).string();
                    if let Data::VALUE(item) = self.peek() {
                        match item {
                            Value::Character(_) => {
                                let s1: String = String::from(vpop!(self).character());
                                vpush!(self, Value::Textual(s1 + &s2));
                            },
                            Value::Textual(_)   => {
                                let s1: String = vpop!(self).string();
                                vpush!(self, Value::Textual(s1 + &s2));
                            },
                            _                   => unreachable!(),
                        }
                    }
                },
                Value::Vectorial(_)     => {
                    let m2 = vpop!(self).matrix();
                    let m1 = vpop!(self).matrix();
                    vpush!(self, Value::Vectorial(m1 + &m2));
                },
                _                       => {
                    self.op(b'+');  // push in op()
                },
            };
        } else { std::panic::panic_any(NO_VALUE.dots()); }
    }
    pub(crate) fn sub(&mut self) {
        if let Data::VALUE(item) = self.peek() {
            if let Value::Vectorial(_) = item {
                let m2 = vpop!(self).matrix();
                let m1 = vpop!(self).matrix();
                vpush!(self, Value::Vectorial(m1 - &m2));
            } else {
                self.op(b'-');  // push in op()
            }
        } else { std::panic::panic_any(NO_VALUE.dots()); }
    }
    pub(crate) fn mul(&mut self) {
        if let Data::VALUE(item2) = self.peek() {
            match item2 {
                Value::Vectorial(_)         => {
                    let m2 = vpop!(self).matrix();
                    if let Data::VALUE(item1) = self.peek() {
                        match item1 {
                            Value::Vectorial(_)         => {
                                let m1 = vpop!(self).matrix();
                                vpush!(self, Value::Vectorial(m1 * &m2));
                            },
                            Value::Numerical(_)         => {
                                let n1: f64 = Number::into(vpop!(self).number());
                                vpush!(self, Value::Vectorial(n1 * m2));
                            },
                            _                                   => {
                                self.op(b'*');  // push in op()
                            },
                        };
                    } else { std::panic::panic_any(NO_VALUE.dots()); }
                },
                _                                   => {
                    self.op(b'*');  // push in op()
                },
            };
        } else { std::panic::panic_any(NO_VALUE.dots()); }
    }
    pub(crate) fn div(&mut self) {
        self.op(b'/');  // push in op()
    }
    pub(crate) fn intdiv(&mut self) {
        self.op(b'd');  // push in op()
    }
    pub(crate) fn modulo(&mut self) {
        self.op(b'%');  // push in op()
    }
    pub(crate) fn rem(&mut self) {
        self.op(b'r');  // push in op()
    }

    pub(crate) fn op(&mut self, c: u8) {
        let mut num2 = vpop!(self).number();
        if let Data::VALUE(item1) = self.peek() {
            match item1 {
                Value::Vectorial(_)         => {
                    let m1 = vpop!(self).matrix();
                    let n2 = f64::from(num2);
                    vpush!(self, Value::Vectorial(m1 * n2));
                },
                Value::Numerical(_)         => {
                    ///////////////////////////////////////////////////////////////////
                    let mut num1 = vpop!(self).number();
                    let p1 = precision(num1.clone());
                    let p2 = precision(num2.clone());
                    let p = max(p1, p2);
                    if p1 != p2 {
                        if p == p1 {
                            num2 = match p {
                                0   => num2,
                                2   => Number::FLOAT(num2.into()),
                                3   => Number::COMPLEX(num2.into(), 0.0),
                                4   => Number::BIGDEC(num2.into()),
                                1   => Number::BIGINT(num2.into()),
                                _   => unreachable!(),
                            };
                        }
                        else {
                            num1 = match p {
                                0   => num1,
                                2   => Number::FLOAT(num1.into()),
                                3   => Number::COMPLEX(num1.into(), 0.0),
                                4   => Number::BIGDEC(num1.into()),
                                1   => Number::BIGINT(num1.into()),
                                _   => unreachable!(),
                            }
                        }
                    }
                    let num = match c {
                        b'+' => num1 + num2,
                        b'-' => num1 - num2,
                        b'*' => num1 * num2,
                        b'/' => num1 / num2,
                        b'd' => num1.intdiv(num2),
                        b'%' => modulo!(num1, num2.clone()),
                        b'r' => num1 % num2,
                        _    => panic_any(UNSUPPORTED_OP.dots()),
                    };
                    vpush!(self, Value::Numerical(num));
                    ///////////////////////////////////////////////////////////////////
                },
                _                                   => {
                    self.op(b'*');  // push in op()
                },
            };
        } else { std::panic::panic_any(NO_VALUE.dots()); }
    }
///////////////////////////////////////////////////////////////////////////////////s'PO//

    pub(crate) fn binomial(&self, n: i64, k: i64) -> BigInt {  // coefficient
        let mut b = BigInt::from(n);
        let mut r: BigInt = BigInt::one();
        for i in 1..=k {
            r *= b.clone();
            b -= BigInt::one();
            r /= i;
        }
        r
    }
    pub(crate) fn fact(&self, n: i64,) -> BigInt {
        (1..=n).fold(BigInt::one(), |a, b| a * b)
    }
    pub(crate) fn fibo(&mut self) {  // returns 0 on overflow
        let n = pop_i64!(self) as u64;
        let mut rslt = 1u64;
        let mut prev = 0;
        for _ in 1..n {
            if let Some(u) = rslt.checked_add(prev) {
                prev = rslt;
                rslt = u;
            }
            else {
                vpush!(self, v!(0));
                return
            }
        }
        vpush!(self, v!(rslt as i64))
    }
    pub(crate) fn deg(&mut self) {
        println!("Deg");
        self.angle = b'D';
    }
    pub(crate) fn dms(&mut self) {
        println!("DMS");  // ° ' "
        self.angle = b'd';
    }
    pub(crate) fn rad(&mut self) {
        println!("Rad");
        self.angle = b'R';
    }
    pub(crate) fn grad(&mut self) {
        println!("Grad");
        self.angle = b'G';
    }

    #[inline]
    pub(crate) fn pi(&self) -> Value {
        self.pi.clone()
    }

    pub(crate) fn bd_p2(&mut self, f: &dyn Fn(&BigDecimal, i64) -> BigDecimal) {
        let tos = BigDecimal::from(vpop!(self));
        vpush!(self, v!((f)(&tos, 0)));
    }
    pub(crate) fn f_p1(&mut self, f: &dyn Fn(f64) -> f64) {
        let tos = f64::from(vpop!(self));
        vpush!(self, v!((f)(tos)));
    }
    pub(crate) fn b_p2(&mut self, f: &dyn Fn(bool, bool) -> bool) {
        let p2 = bool::from(vpop!(self).boolean());
        let p1 = bool::from(vpop!(self).boolean());
        vpush!(self, v!((f)(p1, p2)));
    }
    pub(crate) fn u_p2(&mut self, f: &dyn Fn(i64, u32) -> i64) {
        let p2 = pop_i64!(self) as u32;
        let p1 = pop_i64!(self);
        let n = (f)(p1, p2);
        vpush!(self, v!(n));
    }
    pub(crate) fn i_p2(&mut self, f: &dyn Fn(i64, i64) -> i64) {
        let p2 = pop_i64!(self);
        let p1 = pop_i64!(self);
        vpush!(self, v!((f)(p1, p2)));
    }
    pub(crate) fn f_p2(&mut self, f: &dyn Fn(f64, f64) -> f64) {
        let p2 = f64::from(vpop!(self));
        let p1 = f64::from(vpop!(self));
        vpush!(self, v!((f)(p1, p2)));
    }
    pub(crate) fn f_p3(&mut self, f: &dyn Fn(f64, f64, f64) -> f64) {
        let p3 = f64::from(vpop!(self));
        let p2 = f64::from(vpop!(self));
        let p1 = f64::from(vpop!(self));
        vpush!(self, v!((f)(p1, p2, p3)));
    }
    pub(crate) fn abs(&mut self) {
        let tos = vpop!(self).number();
        match tos {
            Number::INTEGER(i)      => vpush!(self, v!(i.abs())),
            Number::FLOAT(r)        => vpush!(self, v!(r.abs())),
            Number::COMPLEX(re, im) => vpush!(self, v!((re*re + im*im).sqrt())),
            Number::BIGDEC(bd)      => vpush!(self, v!(if bd < BigDecimal::zero() {-bd} else {bd})),
            Number::BIGINT(bi)      => vpush!(self, v!(if bi < BigInt::zero() {-bi} else {bi})),
        }
    }
    pub(crate) fn not(&mut self) {
        let tos: Value = vpop!(self);
        match tos {
            Value::Boolean(_)           =>
                vpush!(self, v!(! tos.boolean())),
            Value::Numerical(_)         =>
                if let Number::INTEGER(i) = tos.number() {
                    vpush!(self, v!(! i));
                },
            _                           => panic_any(UNSUPPORTED_OP.dots()),
        }
    }
    pub(crate) fn xor(&mut self) {
        let p2 = vpop!(self);
        let p1 = vpop!(self);
        match p2 {
            Value::Boolean(_)           =>
                vpush!(self, v!(p1.boolean() ^ p2.boolean())),
            Value::Numerical(_)         => {
                    if let Number::INTEGER(i1) = p1.number() {
                        if let Number::INTEGER(i2) = p2.number() {
                            vpush!(self, v!(i1 ^ i2));
                        }
                    }
                },
            _                           => panic_any(UNSUPPORTED_OP.dots()),
        }
    }
    pub(crate) fn nand(&mut self) {
        let p2 = vpop!(self);
        let p1 = vpop!(self);
        match p2 {
            Value::Boolean(_)           =>
                vpush!(self, v!(! (p1.boolean() & p2.boolean()))),
            Value::Numerical(_)         => {
                    if let Number::INTEGER(i1) = p1.number() {
                        if let Number::INTEGER(i2) = p2.number() {
                            vpush!(self, v!(! (i1 & i2)));
                        }
                    }
                },
            _                           => panic_any(UNSUPPORTED_OP.dots()),
        }
    }
    pub(crate) fn nor(&mut self) {
        let p2 = vpop!(self);
        let p1 = vpop!(self);
        match p2 {
            Value::Boolean(_)           =>
                vpush!(self, v!(! (p1.boolean() | p2.boolean()))),
            Value::Numerical(_)         => {
                    if let Number::INTEGER(i1) = p1.number() {
                        if let Number::INTEGER(i2) = p2.number() {
                            vpush!(self, v!(! i1 | i2));
                        }
                    }
                },
            _                           => panic_any(UNSUPPORTED_OP.dots()),
        }
    }
    pub(crate) fn pow(&mut self) {
        let p2 = pop_i64!(self);
        let p1 = f64::from(vpop!(self));
            vpush!(self, v!(f64::powi(p1, p2 as i32)));
    }
    pub(crate) fn powb(&mut self) {
        let e = pop_i64!(self) as u32;
        let b = vpop!(self).number();
        let rslt = BigInt::from(b).pow(e);
        vpush!(self, v!(rslt))
    }
    pub(crate) fn equals(&mut self) {
        let p2: Value = vpop!(self);
        let p1: Value = vpop!(self);
        vpush!(self, v!(p1 == p2));
        let _b = set_var_indexed!(self, 2, dt!(v!(disc!(&p1, &p2))));  // (2) iseqtype
    }
    pub(crate) fn cmp(&mut self, f: &dyn Fn(&Number, &Number) -> bool) {
        let p2 = vpop!(self).number();
        let p1 = vpop!(self).number();
        vpush!(self, v!((f)(&p1, &p2)));
    }
    pub(crate) fn trig(&mut self, chk: &dyn Fn(&f64) -> bool,
            f: &dyn Fn(f64) -> f64) {
        let a = match self.angle {
            b'D' => ANGLE.0,
            b'R' => ANGLE.1,
            b'G' => ANGLE.2,
            _    => unreachable!(),
        };
        let tos = f64::from(vpop!(self));
        let pi = consts::PI;
        let rslt = (f)(mul_div(tos, pi, a));
        self.set_variable("isexact", dt!(v!(chk(&rslt))));
        vpush!(self, v!(rslt));
    }

    #[inline(always)]
    pub(crate) fn get_node_id(&self, name: &str) -> Option<NodeId> {
        return self.dictionary.find_secondary_id(&name);
    }

    #[inline]
    pub(crate) fn find_secondary(&self, w: &str) -> Option<Item> {
        let dict = &self.dictionary;
        let found = dict.find_secondary_id(w)?;
        let node = dict.library.get(found)?;
        node_item(node, w).cloned()
    }

    pub(crate) fn get_var_idx(&self, var_name: &str) -> Result<usize, String> {  // VARIABLE
        let Some(word_item) = self.find_secondary(&var_name) else {
            return Err(format!("{NOT_FOUND}: {var_name}"));
        };
        match Cmd::package(&word_item) {
            Ok(a3)      => {
                debug_assert!(a3.get(0).is_some());
                PROG.with(|prg| {
                    let p = prg.borrow_mut();
                    return match p.get_value_at(a3[0]) {
                        Some(Value::Indexable(_b, idx)) => Ok(idx),
                        _                               => Err(format!("NO VARIABLE: {var_name}")),
                    };
                })
            },
            Err(err)    => Err(err),
        }
    }

    pub(crate) fn dir(&self, rec: bool, files: &mut Vec<String>, path: &PathBuf) {
         if let Ok(entries) = fs::read_dir(path) {
            for entry in entries {
                if let Ok(entry) = entry {
                    let path = entry.path();
                    if rec && path.is_dir() {
                        self.dir(rec, files, &path);
                    }
                    files.push(path.display().to_string());
                }
            }
        }
    }

    pub(crate) fn words(&self) -> DMatrix<String> {
        let mut vec = self.vocabulary.core.keys().cloned().collect::<Vec<String>>();
        vec.sort();
        let descs = self.dictionary.context.children(&self.dictionary.library);
        for id in descs {
            if let Some(node) = &self.dictionary.library.get(id) {
                vec.push(node.get().name());
            }
        }
        DMatrix::from_vec(1, vec.len(), vec)
    }

    #[inline(always)]
    pub(crate) fn load(&mut self, fname: &str) -> Result<(), String> {
        self.line_reader.loadf(fname)
    }

/////////////////////////////////////////////////////////////////////////////////////////
    pub(crate) fn forget(&mut self, word_name: &str) -> Result<Value /*bool*/, String> {
        let descs = self.dictionary.context.reverse_children(&self.dictionary.library);
        let id1 = Dictionary::find_word_id(self, word_name)?;  // -> Err
        let fence_id = get_fence_id!(self);
        let mut below_fence: bool = false;
        for id2 in descs {
            if id2 == fence_id {
                below_fence = true;
            }
            if below_fence && id1 == id2 {
                return Err(format!("BEYOND FENCE: {word_name}"));
            }
            let node = self.dictionary.library.get(id2).unwrap();
            if node.get().name() == word_name && ! node.is_removed() {
                let mut fence_set = false;
                for voc in id2.preceding_siblings(&self.dictionary.library).collect::<Vec<_>>().into_iter() {
                    if ! fence_set {
                        fence_set = true;
                        if let Some(node) = &self.dictionary.library.get(voc) {
                            self.set_variable("fence", dt!(v!(node.get().name())));
                        }
                    }
                    if let Item::VOCABULARY { cmd: (_, _) } = self.dictionary.library[voc].get() {
                        self.dictionary.current = voc;
                        break;
                    }
                }
                debug_assert!(fence_set);
                id2.detach(&mut self.dictionary.library);
                id2.remove_subtree(&mut self.dictionary.library);
                return Ok(v!(true));
            }
        }
        Err(if Vocabulary::find_item(&self, word_name).is_some() {
            format!("{word_name} CORE-WORD")
        }
        else {
            format!("{word_name} {}", UNKNOWN.dots())
        })
    }

    pub(crate) fn error(&mut self, err: &str) {
        eprintln!("{err}"); self.bye_reg(EXIT);
    }

    #[inline(always)]
    pub(crate) fn bye(&mut self, reason: u8) -> u8 {
        self.bye.load(SeqCst) & reason
    }

    #[inline(always)]
    pub(crate) fn bye_reg(&mut self, reason: u8) {
        self.bye.fetch_or(reason, SeqCst);
    }

    #[inline(always)]
    pub(crate) fn bye_unreg(&mut self, reason: u8) {
        self.bye.fetch_and(! reason, SeqCst);
    }
}
