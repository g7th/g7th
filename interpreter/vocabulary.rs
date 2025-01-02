// ©2024 Otmar Klenk
#![macro_use]

use crate::n;
use crate::{addr_arr, caller};
use crate::{reg_break, reg_bye, reg_exit};
use crate::{v, vpeek, vpush, vpop, pop_bool, pop_bd, pop_i64, pop_str};
use crate::{eflush, flush, immediate, word};
use crate::indexed;

use bigdecimal::{BigDecimal, One, Zero};
use bigdecimal::num_bigint::Sign;
use once_cell::sync::Lazy;
use std::cell::RefCell;
use std::cmp::min;
use std::collections::{HashMap, HashSet};

use std::env;
use std::f64::consts;
use std::fs::{self, File, OpenOptions};
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::ops::{BitAnd, BitOr};
use std::panic::panic_any;
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::{Arc, Mutex};

use indextree::Arena;
use nalgebra::DMatrix;
use rand::distributions::{Alphanumeric, DistString};
use rand::Rng;
use regex::Regex;

use super::{PROG, EXIT};
use super::Call;
use super::data::{EMPTY_STACK, EMPTY_RETURN_STACK, NOT_FOUND};
use super::data::{Data, DataHolder};
use super::dictionary::WithLibrary;
use super::execute::Execute;
use super::finder::CodeFinder;
use super::item::{GetCtrlType, Item};
use super::item::branch::CtrlType;
use super::item::cmd::Cmd;
use super::item::cmd::callable::Callable;
use super::item::cmd::value::{GetValue, Value};
use super::item::number::{Number, is_bigdec};
use super::item::word::{IDX_PUSH, VAL_PUSH};
use super::item::word::PackageCode;
use super::line_reader::wheel;
use super::parser::{CPLX_FORMAT, Parser};
use super::tools::Dots;

pub(crate) const SECONDARIES: &[&str] = &[
    "true isexact !",  // trig functions
    "true iseqtype !",  // Eq trait
    ": 2>r swap >r >r ;",
    ": 2r> r> r> swap ;",
    ": 2r@ 2 rpick r@ ;",
    ": .r >r . ;",
    ": nip swap drop ;",
    ": 2nip 2swap 2drop ;",
    ": -2rot 2rot 2rot ;",
    ": 2tuck 2swap 2over ;",
    ": cr \"\" println ;",
    ": space bl print ;",
    ": spaces 0 do space loop ;",
    ": div // ;",
    ": /mod 2dup // -rot % ;",
    ": mod % ;",
    ": = == ;",
    ": 0= 0 == ;",
    ": 0!= 0 != ;",
    ": 0< 0 < ;",
    ": 0> 0 > ;",
    ": 0<= 0 <= ;",
    ": 0>= 0 >= ;",
    ": 1+ 1 + ;",
    ": 1- 1 - ;",
    ": 5+ 5 + ;",
    ": 5- 5 - ;",
    ": 5* 5 * ;",
    ": 5/ 5 / ;",
    ": 2* 2 * ;",
    ": 2/ 2 / ;",
    ": 10* 10 * ;",
    ": 10/ 10 / ;",
    ": incr dup @ 1 + swap ! ;",
    ": decr dup @ 1 - swap ! ;",
    ": x^y pow ;",
    "' x^y fence !",
];

const INT_FORMAT: &str              = "INTEGER FORMAT";
pub(crate) const UNKNOWN: &str      = "UNKNOWN";

#[macro_export]
macro_rules! indexed {
    ($name: expr, $vec: expr, $idx: expr) => {{
        let data_opt = rget!($vec, $idx).cloned();
        if let Some(data) = data_opt {
            Vec::push(&mut $vec, data);
        }
        else {
            Vec::push(&mut $vec, Data::VALUE(v!(0f64)));
        }
    }}
}

#[macro_export]
macro_rules! rget {
    ($vec: expr, $idx: expr) => {{
        let len = $vec.len();
        if len > $idx {  // prevent overflow
            $vec.get(len - $idx - 1)
        } else {
            None
        }
    }}
}

macro_rules! spop {
    ($dh: expr, $vec: expr, $err: expr) => {{
        match $vec.pop() {
            Some(entry) => entry,
            None        => return $dh.error($err),
        }
    }}
}

macro_rules! core {  // Item
    ($name: expr, $fn_ptr: expr) => {
        word!(CtrlType::Other, $name, $fn_ptr, Value::Unassigned)
    }
}

#[macro_export]
macro_rules! ctrl {  // Item
    ($type: expr, $name: expr, $fn_ptr: expr) => {
        addr_arr!($type, $name, $fn_ptr, crate::interpreter::Value::AddressArray(vec![]))
    }
}

#[macro_export]
macro_rules! variable {
    ($dh: expr, $var_name: expr, $push: expr) => {{
        use crate::dt;
        use crate::new_package_item;
        use crate::interpreter::HACK;
        use crate::interpreter::item::branch::append_to_a3;
        use crate::interpreter::item::cmd::Cmd;
        use crate::interpreter::item::word::FN_PACKAGE;
        let idx = $dh.vars.len();
        let index_item = crate::index!($var_name, $push, Value::Indexable(b'v', idx));
        Vec::push(&mut $dh.vars, dt!(Value::Unassigned));
        let mut package_item = new_package_item!($dh, $var_name);
        append_to_a3($dh, CtrlType::Other, &$var_name, index_item.code());
//// =======⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯
        let Some((hack, slot)) = $dh.leaf.ctrls.pop() else { unreachable!(); };
        debug_assert_eq!(hack, HACK);

        PROG.with(|prg| {
            package_item.code_mut().set_value(prg.borrow()[slot].get().get_value());
        });
        // Item into dictionary
        $dh.dictionary.ins_current(package_item);  // dir - sibn, sibn-1... sib2, sib1
    }}
}

macro_rules! is_curr_vocab {
    ($dict: expr) => {
        if let Some(_) = $dict.library.get($dict.current) {
            match &$dict.library[$dict.current].get().code().get_value() {
                Value::Addressable(_)   => true,
                _                       => false,
            }
        } else {
            unreachable!();
        }
    }
}

macro_rules! bd_fract {
    ($dh: expr) => {{
        let bd = pop_bd!($dh);
        vpush!($dh, v!(bd % BigDecimal::one()));
    }}
}

macro_rules! bd_trunc {
    ($dh: expr) => {{
        let bd = pop_bd!($dh);
        vpush!($dh, v!(bd - bd % BigDecimal::one()));
    }}
}

macro_rules! bd_floor {
    ($dh: expr) => {{
        let bd = pop_bd!($dh);
        let fract = bd % BigDecimal::one();
        let r = if bd.sign() == Sign::Minus && fract != BigDecimal::zero() {
            bd - fract - BigDecimal::one()
        }
        else {
            bd - fract
        };
        vpush!($dh, v!(r));
    }}
}

macro_rules! bd_ceil {
    ($dh: expr) => {{
        let bd = pop_bd!($dh);
        let fract = bd % BigDecimal::one();
        let r = if bd.sign() == Sign::Plus && fract != BigDecimal::zero() {
            bd - fract + BigDecimal::one()
        }
        else {
            bd - fract
        };
        vpush!($dh, v!(r));
    }}
}

pub(crate) fn fix_idx(len: usize, idx: i64) -> usize {
    let i = idx as usize;
    if len > i && idx >= 0 {
        return i;
    }
    panic_any("INDEX");
}


macro_rules! dot {  // fails silently
    ($dh: expr) => {{
        match $dh.pop() {
            Ok(Data::VALUE(value))  => {
                match value {
                   Value::Indexable(b, idx)     => match b {
                        b'd'    => if let Data::VALUE(value) = &$dh.data[idx] {
                            println!("[{value}]"); flush!();
                        },
                        b'v'    => {
                            let var = &$dh.vars[idx];
                            println!("{{{var}}}"); flush!();
                        },
                        _       => unreachable!(),
                    },
                    _                           => { print!("{value}"); flush!(); },
                }
            },
            Ok(Data::ITEM(item))    => { print!("{{{item}}}"); flush!(); },
            Err(err)                => { eprintln!("{err}"); eflush!(); },
        }
    }}
}


macro_rules! fappend {
    ($fname: expr) => {
        OpenOptions::new().append(true).open($fname)
    }
}

#[macro_export]
macro_rules! flog {
    ($text: expr) => {
        if let Ok(mut file) = std::fs::OpenOptions::new().create(true).append(true).open("log.txt") {
            let _ = write!(file, "{}\n", $text);
        }
    }
}
/////////////////////////////////////////////////////////////////////////////////////////
static ALLOC: Lazy<Mutex<Vec<Value>>> = Lazy::new(|| { Mutex::new(vec![]) });

/////////////////////////////////////////////////////////////////////////////////////////
fn chk_sin(x: &f64) -> bool {
    (x - 1.0).abs() < 1e-10
}
fn chk_cos(x: &f64) -> bool {
    (x - 1.0).abs() < 1e-10
}
fn chk_tan(x: &f64) -> bool {
    (x - 1.0).abs() < 1e-14
}
fn chk_asin(x: &f64) -> bool {
    (x.sin() - x).abs() < 1e-10
}
fn chk_acos(x: &f64) -> bool {
    (x.cos() - x).abs() < 1e-10
}
fn chk_atan(x: &f64) -> bool {
    (x.tan() - 1.0).abs() < 1e-10
}
fn chk_sinh(x: &f64) -> bool {
    let e = consts::E;
    (x - ((e * e) - 1.0) / (2.0 * e)).abs() < 1e-10
}
fn chk_cosh(x: &f64) -> bool {
    let e = consts::E;
    (x - ((e * e) + 1.0) / (2.0 * e)).abs() < 1e-10
}
fn chk_tanh(x: &f64) -> bool {
    let e = consts::E;
    (x - (1.0 - e.powi(-2)) / (1.0 + e.powi(-2))).abs() < 1e-10
}
fn chk_asinh(x: &f64) -> bool {
    (x.sinh() - x).abs() < 1e-10
}
fn chk_acosh(x: &f64) -> bool {
    (x.cosh() - x).abs() < 1e-10
}
fn chk_atanh(x: &f64) -> bool {
    let e = consts::E;
    (x.tanh() - e).abs() < 1e-10
}

/////////////////////////////////////////////////////////////////////////////////////////
#[derive(Clone, Debug)]
pub struct Vocabulary {
    pub base:           u32,
    pub core:           HashMap<String, Item>,
    pub syntax:         HashMap<String, (CtrlType, String)>,  // ctrl: ctrl_type & end
    pub ctrl_ends:      HashSet<String>,
    cplx_i:             Regex,
}

impl Vocabulary {
    pub(crate) fn new() -> Self {
        Self {
            base:           10,
            core:           HashMap::new(),
            syntax:         HashMap::new(),  // ctrl: ctrl_type & end
            ctrl_ends:      HashSet::new(),
            cplx_i:         Regex::new(r"[^A-Za-z]i([^A-Za-z]|)").unwrap(),
        }
    }

    pub(crate) fn core(&mut self, _flow: Arena<Call>) {
        self.ins_core(core!("true",             |dh: &mut DataHolder, _v: &Value| {vpush!(dh, v!(true));}));
        self.ins_core(core!("false",            |dh: &mut DataHolder, _v: &Value| {vpush!(dh, v!(false));}));
        self.ins_core(core!("&&",               |dh: &mut DataHolder, _v: &Value| {dh.b_p2(&bool::bitand);}));
        self.ins_core(core!("||",               |dh: &mut DataHolder, _v: &Value| {dh.b_p2(&bool::bitor);}));
        self.ins_core(core!("not",              |dh: &mut DataHolder, _v: &Value| {dh.not();}));
        self.ins_core(core!("&",                |dh: &mut DataHolder, _v: &Value| {dh.i_p2(&i64::bitand);}));
        self.ins_core(core!("|",                |dh: &mut DataHolder, _v: &Value| {dh.i_p2(&i64::bitor);}));
        self.ins_core(core!("xor",              |dh: &mut DataHolder, _v: &Value| {dh.xor();}));
        self.ins_core(core!("nand",             |dh: &mut DataHolder, _v: &Value| {dh.nand();}));
        self.ins_core(core!("nor",              |dh: &mut DataHolder, _v: &Value| {dh.nor();}));
        self.ins_core(core!("<<",               |dh: &mut DataHolder, _v: &Value| {dh.u_p2(&i64::rotate_left);}));
        self.ins_core(core!(">>",               |dh: &mut DataHolder, _v: &Value| {dh.u_p2(&i64::rotate_right);}));
        self.ins_core(core!("decimal",          |dh: &mut DataHolder, _v: &Value| {dh.vocabulary.base = 10;}));
        self.ins_core(core!("bin",              |dh: &mut DataHolder, _v: &Value| {dh.vocabulary.base =  2;}));
        self.ins_core(core!("oct",              |dh: &mut DataHolder, _v: &Value| {dh.vocabulary.base =  8;}));
        self.ins_core(core!("hex",              |dh: &mut DataHolder, _v: &Value| {dh.vocabulary.base = 16;}));
        self.ins_core(core!("!",                |dh: &mut DataHolder, _v: &Value| {dh.store('\x20');}));
        self.ins_core(core!("+!",               |dh: &mut DataHolder, _v: &Value| {dh.store('+');}));
        self.ins_core(core!("-!",               |dh: &mut DataHolder, _v: &Value| {dh.store('-');}));
        self.ins_core(core!("*!",               |dh: &mut DataHolder, _v: &Value| {dh.store('*');}));
        self.ins_core(core!("/!",               |dh: &mut DataHolder, _v: &Value| {dh.store('/');}));
        self.ins_core(core!("@",                |dh: &mut DataHolder, _v: &Value| {dh.fetch();}));
        self.ins_core(core!("@!",               |dh: &mut DataHolder, _v: &Value| {dh.fetch_store();}));
        self.ins_core(core!("-rot",             |dh: &mut DataHolder, _v: &Value| {dh.mrot();}));
        self.ins_core(core!("rot",              |dh: &mut DataHolder, _v: &Value| {dh.rot();}));
        self.ins_core(core!("roll",             |dh: &mut DataHolder, _v: &Value| {let n = pop_i64!(dh); dh.roll(n);}));
        self.ins_core(core!("-roll",            |dh: &mut DataHolder, _v: &Value| {let n = pop_i64!(dh); dh.mroll(n);}));
        self.ins_core(core!("pick",             |dh: &mut DataHolder, _v: &Value| {let n = pop_i64!(dh); dh.pick(n);}));
        self.ins_core(core!("-pick",            |dh: &mut DataHolder, _v: &Value| {let n = pop_i64!(dh); dh.mpick(n);}));
        self.ins_core(core!("rpick",            |dh: &mut DataHolder, _v: &Value| {let n = pop_i64!(dh); dh.rpick(n);}));
        self.ins_core(core!("peek",             |dh: &mut DataHolder, _v: &Value| {dh.push_data(dh.peek());}));
        self.ins_core(core!("over",             |dh: &mut DataHolder, _v: &Value| {dh.over();}));
        self.ins_core(core!("under",            |dh: &mut DataHolder, _v: &Value| {dh.under();}));
        self.ins_core(core!("tuck",             |dh: &mut DataHolder, _v: &Value| {dh.tuck();}));
        self.ins_core(core!("turn",             |dh: &mut DataHolder, _v: &Value| {dh.turn();}));
        self.ins_core(core!("swap",             |dh: &mut DataHolder, _v: &Value| {dh.swap();}));
        self.ins_core(core!("dup",              |dh: &mut DataHolder, _v: &Value| {dh.dup();}));
        self.ins_core(core!("drop",             |dh: &mut DataHolder, _v: &Value| {if let Err(e) = dh.pop() { dh.error(&e); return; };}));
        self.ins_core(core!("2swap",            |dh: &mut DataHolder, _v: &Value| {dh.roll(4); dh.roll(4);}));
        self.ins_core(core!("2over",            |dh: &mut DataHolder, _v: &Value| {dh.pick(4); dh.pick(4);}));
        self.ins_core(core!("2dup",             |dh: &mut DataHolder, _v: &Value| {dh.twodup();}));
        self.ins_core(core!("2drop",            |dh: &mut DataHolder, _v: &Value| {if let Err(e) = dh.pop() { dh.error(&e); return; } else {
                                                                           if let Err(e) = dh.pop() { dh.error(&e); return; }};}));
        self.ins_core(core!("2rot",             |dh: &mut DataHolder, _v: &Value| {dh.roll(6); dh.roll(6);}));
        self.ins_core(core!("X",                |dh: &mut DataHolder, _v: &Value| {indexed!("X", dh.data, 0);}));
        self.ins_core(core!("Y",                |dh: &mut DataHolder, _v: &Value| {indexed!("Y", dh.data, 1);}));
        self.ins_core(core!("Z",                |dh: &mut DataHolder, _v: &Value| {indexed!("Z", dh.data, 2);}));
        self.ins_core(core!("T",                |dh: &mut DataHolder, _v: &Value| {indexed!("T", dh.data, 3);}));

        self.ins_core(core!("abs",              |dh: &mut DataHolder, _v: &Value| {dh.abs();}));
        self.ins_core(core!("==",               |dh: &mut DataHolder, _v: &Value| {dh.equals();}));
        self.ins_core(core!("!=",               |dh: &mut DataHolder, _v: &Value| {dh.equals(); dh.not();}));
        self.ins_core(core!(">",                |dh: &mut DataHolder, _v: &Value| {dh.cmp(&Number::gt);}));
        self.ins_core(core!("<",                |dh: &mut DataHolder, _v: &Value| {dh.cmp(&Number::lt);}));
        self.ins_core(core!(">=",               |dh: &mut DataHolder, _v: &Value| {dh.cmp(&Number::ge);}));
        self.ins_core(core!("<=",               |dh: &mut DataHolder, _v: &Value| {dh.cmp(&Number::le);}));
        self.ins_core(core!("+",                |dh: &mut DataHolder, _v: &Value| {dh.add();}));
        self.ins_core(core!("-",                |dh: &mut DataHolder, _v: &Value| {dh.sub();}));
        self.ins_core(core!("*",                |dh: &mut DataHolder, _v: &Value| {dh.mul();}));
        self.ins_core(core!("/",                |dh: &mut DataHolder, _v: &Value| {dh.div();}));
        self.ins_core(core!("//",               |dh: &mut DataHolder, _v: &Value| {dh.intdiv();}));
        self.ins_core(core!("%",                |dh: &mut DataHolder, _v: &Value| {dh.modulo();}));
        self.ins_core(core!("rem",              |dh: &mut DataHolder, _v: &Value| {dh.rem();}));
        self.ins_core(core!("fact",             |dh: &mut DataHolder, _v: &Value| {let n = pop_i64!(dh); let r = dh.fact(n); vpush!(dh, v!(r));}));
        self.ins_core(core!("fibo",             |dh: &mut DataHolder, _v: &Value| {dh.fibo();}));
        self.ins_core(core!("cls",              |dh: &mut DataHolder, _v: &Value| {print!("\x1b[2J\x1b[00f");dh.clear();}));
        self.ins_core(core!("clst",             |dh: &mut DataHolder, _v: &Value| {dh.clear();}));
        self.ins_core(core!("cbrt",             |dh: &mut DataHolder, _v: &Value| {dh.f_p1(&f64::cbrt);}));
        self.ins_core(core!("ceil",             |dh: &mut DataHolder, _v: &Value| {if is_bigdec(vpeek!(dh)) { bd_ceil!(dh); } else { dh.f_p1(&f64::ceil); }}));
        self.ins_core(core!("exp",              |dh: &mut DataHolder, _v: &Value| {dh.f_p1(&f64::exp);}));
        self.ins_core(core!("exp2",             |dh: &mut DataHolder, _v: &Value| {dh.f_p1(&f64::exp2);}));
        self.ins_core(core!("exp-1",            |dh: &mut DataHolder, _v: &Value| {dh.f_p1(&f64::exp_m1);}));
        self.ins_core(core!("floor",            |dh: &mut DataHolder, _v: &Value| {if is_bigdec(vpeek!(dh)) { bd_floor!(dh); } else { dh.f_p1(&f64::floor); }}));
        self.ins_core(core!("ln",               |dh: &mut DataHolder, _v: &Value| {dh.f_p1(&f64::ln);}));
        self.ins_core(core!("ln(1+n)",          |dh: &mut DataHolder, _v: &Value| {dh.f_p1(&f64::ln_1p);}));
        self.ins_core(core!("log",              |dh: &mut DataHolder, _v: &Value| {dh.f_p2(&f64::log);}));
        self.ins_core(core!("log10",            |dh: &mut DataHolder, _v: &Value| {dh.f_p1(&f64::log10);}));
        self.ins_core(core!("log2",             |dh: &mut DataHolder, _v: &Value| {dh.f_p1(&f64::log2);}));
        self.ins_core(core!("max",              |dh: &mut DataHolder, _v: &Value| {dh.f_p2(&f64::max);}));
        self.ins_core(core!("min",              |dh: &mut DataHolder, _v: &Value| {dh.f_p2(&f64::min);}));
        self.ins_core(core!("*+",               |dh: &mut DataHolder, _v: &Value| {dh.f_p3(&f64::mul_add);}));
        self.ins_core(core!("1/x",              |dh: &mut DataHolder, _v: &Value| {dh.f_p1(&f64::recip);}));
        self.ins_core(core!("powf",             |dh: &mut DataHolder, _v: &Value| {dh.f_p2(&f64::powf);}));
        self.ins_core(core!("pow",              |dh: &mut DataHolder, _v: &Value| {dh.pow();}));  // x^i
        self.ins_core(core!("powb",             |dh: &mut DataHolder, _v: &Value| {dh.powb();})); // bi^i
        self.ins_core(core!("round",            |dh: &mut DataHolder, _v: &Value| {if is_bigdec(vpeek!(dh)) { dh.bd_p2(&BigDecimal::round); } else { dh.f_p1(&f64::round); }}));
        self.ins_core(core!("signum",           |dh: &mut DataHolder, _v: &Value| {dh.f_p1(&f64::signum);}));
        self.ins_core(core!("negate",           |dh: &mut DataHolder, _v: &Value| {let v = -vpop!(dh); vpush!(dh, v);}));
        self.ins_core(core!("sqrt",             |dh: &mut DataHolder, _v: &Value| {dh.f_p1(&f64::sqrt);}));
        self.ins_core(core!("fp",               |dh: &mut DataHolder, _v: &Value| {if is_bigdec(vpeek!(dh)) { bd_fract!(dh); } else { dh.f_p1(&f64::fract); }}));
        self.ins_core(core!("ip",               |dh: &mut DataHolder, _v: &Value| {if is_bigdec(vpeek!(dh)) { bd_trunc!(dh); } else { dh.f_p1(&f64::trunc); }}));
        self.ins_core(core!("deg",              |dh: &mut DataHolder, _v: &Value| {dh.deg();}));
        self.ins_core(core!("dms",              |dh: &mut DataHolder, _v: &Value| {dh.dms();}));
        self.ins_core(core!("rad",              |dh: &mut DataHolder, _v: &Value| {dh.rad();}));
        self.ins_core(core!("grad",             |dh: &mut DataHolder, _v: &Value| {dh.grad();}));
        self.ins_core(core!("pi",               |dh: &mut DataHolder, _v: &Value| {vpush!(dh, dh.pi());}));
        self.ins_core(core!("sin",              |dh: &mut DataHolder, _v: &Value| {dh.trig(&chk_sin, &f64::sin);}));
        self.ins_core(core!("cos",              |dh: &mut DataHolder, _v: &Value| {dh.trig(&chk_cos, &f64::cos);}));
        self.ins_core(core!("tan",              |dh: &mut DataHolder, _v: &Value| {dh.trig(&chk_tan, &f64::tan);}));
        self.ins_core(core!("asin",             |dh: &mut DataHolder, _v: &Value| {dh.trig(&chk_asin, &f64::asin);}));
        self.ins_core(core!("acos",             |dh: &mut DataHolder, _v: &Value| {dh.trig(&chk_acos, &f64::acos);}));
        self.ins_core(core!("atan",             |dh: &mut DataHolder, _v: &Value| {dh.trig(&chk_atan, &f64::atan);}));
        self.ins_core(core!("sinh",             |dh: &mut DataHolder, _v: &Value| {dh.trig(&chk_sinh, &f64::sinh);}));
        self.ins_core(core!("cosh",             |dh: &mut DataHolder, _v: &Value| {dh.trig(&chk_cosh, &f64::cosh);}));
        self.ins_core(core!("tanh",             |dh: &mut DataHolder, _v: &Value| {dh.trig(&chk_tanh, &f64::tanh);}));
        self.ins_core(core!("asinh",            |dh: &mut DataHolder, _v: &Value| {dh.trig(&chk_asinh, &f64::asinh);}));
        self.ins_core(core!("acosh",            |dh: &mut DataHolder, _v: &Value| {dh.trig(&chk_acosh, &f64::acosh);}));
        self.ins_core(core!("atanh",            |dh: &mut DataHolder, _v: &Value| {dh.trig(&chk_atanh, &f64::atanh);}));
        self.ins_core(core!("rand",             |dh: &mut DataHolder, _v: &Value| {let r = dh.as_range(); vpush!(dh, v!(rand::thread_rng().gen_range(r)));}));
        self.ins_core(core!("nk",               |dh: &mut DataHolder, _v: &Value| {let k = pop_i64!(dh); let n = pop_i64!(dh); vpush!(dh, v!(dh.binomial(n, k)));}));
        self.ins_core(core!("break",            |dh: &mut DataHolder, _v: &Value| {reg_break!(dh);}));
        self.ins_core(core!("bye",              |dh: &mut DataHolder, _v: &Value| {reg_bye!(dh);}));
        self.ins_core(core!("exit",             |dh: &mut DataHolder, _v: &Value| {reg_exit!(dh);}));
        self.ins_core(core!("leave",            |dh: &mut DataHolder, _v: &Value| {
            let Some(v) = &mut dh.loop_arr.last_mut() else { return; };
            v[0] = v[1];
            reg_break!(dh);
        }));
        self.ins_core(core!("forget",           |dh: &mut DataHolder, v: &Value| {
            let rslt = match dh.forget(&v.string()) {
                Ok(ok)      => ok,
                Err(err)    => { eprintln!("{err}"); v!(false) }
            };
            vpush!(dh, rslt);
        }));
        self.ins_core(core!("words",            |dh: &mut DataHolder, _v: &Value| {block_print(dh.words().as_slice());}));
        self.ins_core(core!(",",                |dh: &mut DataHolder, _v: &Value| {Vec::push(&mut ALLOC.lock().unwrap(), vpop!(dh));}));
        self.ins_core(core!("cell",             |dh: &mut DataHolder, _v: &Value| {let n = pop_i64!(dh); let v = &mut ALLOC.lock().unwrap(); vpush!(dh, v.get(fix_idx(v.len(), n)).expect("CELL").clone());}));
        self.ins_core(immediate!(";",           |_dh: &mut DataHolder, _v: &Value| {eprintln!("';' w/o ':'");}, Value::Unassigned));
        self.ins_core(core!("vocabulary",       |dh: &mut DataHolder, v: &Value| {
            let name = v.string();
            let id = dh.dictionary.library.create_vocab(&name);
            dh.dictionary.ins_current(dh.dictionary.library[id].get().clone()); }));
        self.ins_core(core!("definitions", |dh: &mut DataHolder, _v: &Value| { dh.dictionary.current = dh.dictionary.context;
            debug_assert!(is_curr_vocab!(dh.dictionary));
        }));
//////////////////////////////////////////////////////////////////////////////////PRINT//
        self.ins_core(core!("depth",            |dh: &mut DataHolder, _v: &Value| {vpush!(dh, v!(Number::INTEGER(dh.data.len() as i64)));}));
        self.ins_core(core!(".s",               |dh: &mut DataHolder, _v: &Value| {dh.dot_s();}));
        self.ins_core(core!(".",                |dh: &mut DataHolder, _v: &Value| {dot!(dh);}));
        self.ins_core(core!("?",                |dh: &mut DataHolder, _v: &Value| {
            match dh.peek() {
                Data::VALUE(Value::Indexable(b, idx))   => match b {
                    b'd'    => if let Data::VALUE(value) = &dh.data[idx] {
                        println!("{} [{value}]", n!(dh.data.len(), idx) - 1);
                    }
                    b'v'    => {
                        let var = &dh.vars[idx];
                        println!("{idx} {{{var}}}");
                    }
                    _       => unreachable!(),
                },
                _                                       => println!("{}", dh.peek()),
            }
        }));
        self.ins_core(core!(".c",               |_dh: &mut DataHolder, _: &Value| {println!("{:#?}", ALLOC.lock().unwrap()); flush!();}));
        self.ins_core(core!("bl",               |dh: &mut DataHolder, _v: &Value| {vpush!(dh, Value::Character(' '));}));
        self.ins_core(core!("emit",             |dh: &mut DataHolder, _v: &Value| {print!("{}", char::from_u32(pop_i64!(dh) as u32).unwrap()); flush!();}));
        self.ins_core(core!("print",            |dh: &mut DataHolder, _v: &Value| {print!("{}", vpop!(dh)); flush!();}));
        self.ins_core(core!("println",          |dh: &mut DataHolder, _v: &Value| {println!("{}", vpop!(dh)); flush!();}));
///////////////////////////////////////////////////////////////////////////////////TNIRP//
        self.ins_core(core!("expect$",          |dh: &mut DataHolder, _v: &Value| {dh.accept();}));
        self.ins_core(core!(">str",             |dh: &mut DataHolder, _v: &Value| {let tos = String::from(vpop!(dh)); vpush!(dh, v!(tos));}));
        self.ins_core(core!("len",              |dh: &mut DataHolder, _v: &Value| {let tos = vpop!(dh).string().len() as i64; vpush!(dh, v!(tos));}));
        self.ins_core(core!("left$",            |dh: &mut DataHolder, _v: &Value| {let i = pop_i64!(dh) as usize; let t = String::from(vpop!(dh)); vpush!(dh, v!(t.chars().take(min(i, t.len())).collect::<String>()));}));
        self.ins_core(core!("right$",           |dh: &mut DataHolder, _v: &Value| {let i = pop_i64!(dh) as usize; let t = String::from(vpop!(dh)); vpush!(dh, v!(t.chars().rev().take(min(i, t.len())).collect::<String>().chars().rev().collect::<String>()));}));
        self.ins_core(core!("mid$",             |dh: &mut DataHolder, _v: &Value| {let j = pop_i64!(dh) as usize; let i = pop_i64!(dh) as usize; let t = String::from(vpop!(dh)); vpush!(dh, v!(t.chars().skip(i).take(min(j, t.len() - i)).collect::<String>()));}));
        self.ins_core(core!("sub$",             |dh: &mut DataHolder, _v: &Value| {let j = pop_i64!(dh) as usize; let i = pop_i64!(dh) as usize; let t = String::from(vpop!(dh)); vpush!(dh, v!(t.chars().skip(i).take(min(j - i, t.len() - i)).collect::<String>()));}));
        self.ins_core(core!("split$",           |dh: &mut DataHolder, _v: &Value| {
            let s = pop_str!(dh);
            let vec = s.split_whitespace().map(|s| s.to_string()).collect::<Vec<String>>();
            vpush!(dh, v!(DMatrix::from_vec(0, vec.len(), vec)));
        }));
        self.ins_core(core!("salt$",            |dh: &mut DataHolder, _v: &Value| {let i = pop_i64!(dh).abs() as usize; vpush!(dh, v!(Alphanumeric.sample_string(&mut rand::thread_rng(), i)));}));
        self.ins_core(core!(">num",             |dh: &mut DataHolder, _v: &Value| {let t = pop_str!(dh); match dh.vocabulary.parse_number(t) {
            Ok(num) => vpush!(dh, v!(num)),
            Err(_)  => panic_any("INVALID NUMBER: ".to_string() + t),
        };}));
        self.ins_core(core!("wpcs$",            |dh: &mut DataHolder, _v: &Value| {vpush!(dh, v!("K♔Q♕R♖B♗N♘P♙".to_string()));}));
        self.ins_core(core!("bpcs$",            |dh: &mut DataHolder, _v: &Value| {vpush!(dh, v!("k♚q♛r♜b♝n♞p♟".to_string()));}));
        self.ins_core(core!("wheel$",           |dh: &mut DataHolder, _v: &Value| {vpush!(dh, v!(wheel()));}));
        self.ins_core(core!("words$",           |dh: &mut DataHolder, _v: &Value| {vpush!(dh, v!(dh.words()));}));
/////////////////////////////////////////////////////////////////////////////////////////
        self.ins_core(core!("dir$",             |dh: &mut DataHolder, v: &Value| {let mut files = vec![]; let p = v.string(); dh.dir(false, &mut files, &PathBuf::from(&p)); vpush!(dh, v!(DMatrix::from_vec(1, files.len(), files)));}));
        self.ins_core(core!("dirs$",            |dh: &mut DataHolder, v: &Value| {let mut files = vec![]; let p = v.string(); dh.dir(true, &mut files, &PathBuf::from(&p)); vpush!(dh, v!(DMatrix::from_vec(1, files.len(), files)));}));
        self.ins_core(core!("dir",              |dh: &mut DataHolder, v: &Value| {let mut files = vec![]; let p = v.string(); dh.dir(false, &mut files, &PathBuf::from(&p)); block_print(files.as_slice());}));
        self.ins_core(core!("dirs",             |dh: &mut DataHolder, v: &Value| {let mut files = vec![]; let p = v.string(); dh.dir(true, &mut files, &PathBuf::from(&p)); block_print(files.as_slice());}));
        self.ins_core(core!("wd",               |_dh: &mut DataHolder, _v: &Value| {println!("{}", env::current_dir().unwrap().display());}));
        self.ins_core(core!("wd$",              |dh: &mut DataHolder, _v: &Value| {vpush!(dh, v!(env::current_dir().unwrap()))}));
        self.ins_core(core!("cd",               |_dh: &mut DataHolder, v: &Value| {
            let rslt = env::set_current_dir(&PathBuf::from(v.string()));
            if let Err(err) = rslt { eprintln!("{err}"); }
        }));
        self.ins_core(core!("cat$",             |dh: &mut DataHolder, v: &Value| {
            match fs::read_to_string(v.string()) {
                Ok(s)       => vpush!(dh, v!(s)),
                Err(e)      => { dh.error(&e.to_string()); return; }
            }
        }));
        self.ins_core(core!("cat",              |dh: &mut DataHolder, v: &Value| {
            match fs::read_to_string(v.string()) {
                Ok(s)       => println!("{s}"),
                Err(e)      => { dh.error(&e.to_string()); return; }
            }
        }));
        self.ins_core(core!("load",             |dh: &mut DataHolder, v: &Value| {let fname = v.string(); println!("loading {fname}..."); if let Err(err) = dh.load(&fname) {eprintln!("{err}")};}));
        self.ins_core(core!("loop_indices",     |dh: &mut DataHolder, _v: &Value| {
            dh.loop_arr.iter().rev().map(|arr| Data::VALUE(v!(arr[0]))).for_each(|data| dh.data.push(data));
            vpush!(dh, v!(Number::INTEGER(dh.loop_arr.len().try_into().unwrap())));
        }));
        self.ins_core(core!("r>",               |dh: &mut DataHolder, _v: &Value| {let r = spop!(dh, dh.ret_arr, &EMPTY_RETURN_STACK.dots()); dh.push_data(r);}));
        self.ins_core(core!(">r",               |dh: &mut DataHolder, _v: &Value| {let d = spop!(dh, dh.data, EMPTY_STACK); dh.ret_arr.push(d);}));
        self.ins_core(core!("r@",               |dh: &mut DataHolder, _v: &Value| {dh.rpick(1);}));
        self.ins_core(core!(".rs",              |dh: &mut DataHolder, _v: &Value| {
            dh.ret_arr.iter().rev().for_each(|d| println!("{d}")); println!("---"); }));
        self.ins_core(core!("clrst",            |dh: &mut DataHolder, _v: &Value| {dh.ret_arr.clear();}));
        self.ins_core(ctrl!(CtrlType::Other, "does>",               |dh: &mut DataHolder, varr: &Value| {dh.doing(varr);}));
///////////////////////////////////////////leaf//////////////////////////////////////////
        self.ins_ctrl(("endwhile", ctrl!(CtrlType::Intro, "begin",  |dh: &mut DataHolder, varr: &Value| {
            dh.repeating(varr);
        })));
        self.ins_ctrl(("endwhile", ctrl!(CtrlType::Cont, "while",   |_dh: &mut DataHolder, _vpkg: &Value| {
        })));
        self.ins_ctrl(("until", ctrl!(CtrlType::Intro, "repeat",    |dh: &mut DataHolder, varr: &Value| {
            let rslt = pop_bool!(dh);
            let Ok(ok) = rslt else { dh.error(&rslt.err().unwrap()); return; };
            if ok {
                dh.execute_vhook(varr);
            }
        })));
        self.ins_ctrl(("endcase", ctrl!(CtrlType::Intro, "case",    |dh: &mut DataHolder, varr: &Value| {
            if dh.data.is_empty() {
                dh.case_arr.push(Data::VALUE(Value::Unassigned));
            }
            else {
                match dh.pop().ok() {
                    Some(data)  => dh.case_arr.push(data),
                    None        => dh.case_arr.push(Data::VALUE(Value::Unassigned)),
                }
            }
            dh.distinguishing(varr);
        })));
        self.ins_ctrl(("endof", ctrl!(CtrlType::Cont, "of",         |_dh: &mut DataHolder, _varr: &Value| {
            // EMPTY
        })));
        self.ins_ctrl(("endcase", ctrl!(CtrlType::Cont, "endof",    |_dh: &mut DataHolder, _varr: &Value| {
            // EMPTY
        })));
        self.ctrl_ends.insert("loop".to_string());
        self.ins_ctrl(("+loop", ctrl!(CtrlType::Intro, "do",        |dh: &mut DataHolder, varr: &Value| {
            dh.looping(varr, |dh| { pop_i64!(dh) });
        })));
        self.ins_ctrl(("endif", ctrl!(CtrlType::Intro, "if",        |dh: &mut DataHolder, varr: &Value| {
            dh.forking(varr);
        })));
        self.ins_ctrl(("endif", ctrl!(CtrlType::Cont, "else",       |_dh: &mut DataHolder, _vpkg: &Value| {
            // EMPTY
        })));
        self.ins_ctrl(("each", ctrl!(CtrlType::Intro, "for",        |dh: &mut DataHolder, varr: &Value| {
            dh.iterating(varr);
        })));
/////////////////////////////////////////////////////////////////////////////////////////
        self.ins_core(core!("'",                |dh: &mut DataHolder, v: &Value| {
            let tick = v.string();
            match Vocabulary::find_item(&dh, &tick) {
                Some(item)  => dh.push_data(Data::ITEM(item.clone())),
                None        => {
                    eprintln!("{NOT_FOUND}: {}", tick);
                    dh.bye_reg(EXIT);
                }
            }
        }));
        self.ins_core(core!("isfound",          |dh: &mut DataHolder, v: &Value| {
            let s = v.string();
            match Vocabulary::find_item(&dh, &s) {
                Some(item)  => {
                    dh.push_data(Data::ITEM(item.clone()));
                    vpush!(dh, v!(true));
                },
                None        => vpush!(dh, v!(false)),
            }
        }));
        self.ins_core(core!("execute",          |dh: &mut DataHolder, _v: &Value| {
            match dh.pop() {
                Ok(Data::VALUE(value))  => { 
                    PROG.with(|prg: &RefCell<Arena<Box<dyn Callable>>>| {
                        let p = prg.borrow();
                        if let Value::Packable(pkg) = value {
                             dh.exec(&p, &pkg);
                        }
                    })
                },
                Ok(Data::ITEM(item))    => {
                    caller!(dh, item.code());
                },
                Err(err)                => { eprintln!("{err}"); eflush!(); },
            }
        }));
        self.ins_core(immediate!("immediate",   |dh: &mut DataHolder, _v: &Value| {PackageCode::set_immediate(dh);}, Value::Unassigned));
//        self.ins_ctrl(Vocabulary::put_in("[char]", &flow));
//        self.ins_ctrl(Vocabulary::put_in("[compile]", &flow));
//        self.ins_ctrl(Vocabulary::put_in("fork", &flow));
//        self.ins_ctrl(Vocabulary::put_in("literal", &flow));
/////////////////////////////////////////////////////////////////////////////////////////
        self.ins_core(core!("fread",            |dh: &mut DataHolder, v: &Value| {
            let fname = &v.string();
            match File::open(fname) {
                Ok(file)    => vpush!(dh, v!(Arc::new(Mutex::new(BufReader::new(file))))),
                Err(e)      => { dh.error(&format!("{fname}: {e}")); return; }
            }
        }));
        self.ins_core(core!("fwrite",           |dh: &mut DataHolder, v: &Value| {
            let fname = &v.string();
            match OpenOptions::new().write(true).create(true).truncate(true).open(fname) {
                Ok(file)    => vpush!(dh, v!(Arc::new(Mutex::new(BufWriter::new(file))))),
                Err(e)      => { dh.error(&format!("{fname}: {e}")); return; }
            }
        }));
        self.ins_core(core!("fappend",          |dh: &mut DataHolder, v: &Value| {
            let fname = &v.string();
            match fappend!(fname) {
                Ok(file)    => vpush!(dh, v!(Arc::new(Mutex::new(BufWriter::new(file))))),
                Err(e)      => { dh.error(&format!("{fname}: {e}")); return; }
            }
        }));
        self.ins_core(core!("readln",           |dh: &mut DataHolder, _v: &Value| {
            let mut line = String::new();
            dh.dup();
            let rd = vpop!(dh).buf_reader();
            let rslt = rd.lock().unwrap().read_line(&mut line);
            let Ok(n) = rslt else { dh.error(&rslt.err().unwrap().to_string()); return; };
            if n > 0 {
                vpush!(dh, v!(line)); vpush!(dh, v!(true));
            }
            else {
                let _ = dh.pop(); vpush!(dh, v!(false));
            }
        }));
        self.ins_core(core!("writeln",          |dh: &mut DataHolder, _v: &Value| {
            dh.over();
            let wr = vpop!(dh).buf_writer();
            let mut bwr = wr.lock().unwrap();
            let rslt = write!(bwr, "{}\n", vpop!(dh).string());
            let Ok(_) = rslt else { dh.error(&rslt.err().unwrap().to_string()); return; };
            let _ = bwr.flush();
        }));
        self.ins_core(core!("write",            |dh: &mut DataHolder, _v: &Value| {
            dh.over();
            let wr = vpop!(dh).buf_writer();
            let mut bwr = wr.lock().unwrap();
            let rslt = write!(bwr, "{}", vpop!(dh).string());
            let Ok(_) = rslt else { dh.error(&rslt.err().unwrap().to_string()); return; };
            let _ = bwr.flush();
        }));
        self.ins_core(core!("flog",             |dh: &mut DataHolder, _v: &Value| {
            flog!(pop_str!(dh));
        }));
///////////////////////////////////////////////////////////////////////////////////////////
        self.ins_core(immediate!("constant",    |dh: &mut DataHolder, v: &Value| {
            if dh.get_var_idx(&v.string()).is_ok() {
                eprintln!("WRN: '{}' already exists...", &v.string());
            }
            variable!(dh, &v.string(), VAL_PUSH);
        }, Value::Unassigned));
        self.ins_core(immediate!("variable",    |dh: &mut DataHolder, v: &Value| {  // VARIABLE
            if dh.get_var_idx(&v.string()).is_ok() {
                eprintln!("WRN: '{}' already exists...", &v.string());
            }
            variable!(dh, &v.string(), IDX_PUSH);
        }, Value::Unassigned));
        self.ins_core(core!("beep",             |_dh: &mut DataHolder, _v: &Value| {print!("\x07");}));
        self.ins_core(core!("nop",              |_dh: &mut DataHolder, _v: &Value| {}));
        self.ins_core(core!("I",                |dh: &mut DataHolder, _v: &Value| {
            let Some(arr) = dh.loop_arr.iter().rev().nth(0) else { eprintln!("I outside loop"); return; };
            vpush!(dh, v!(arr[0]));
        }));
        self.ins_core(core!("J",                |dh: &mut DataHolder, _v: &Value| {
            let Some(arr) = dh.loop_arr.iter().rev().nth(1) else { eprintln!("J outside loop"); return; };
            vpush!(dh, v!(arr[0]));
        }));

        debug_assert!(self.ctrl_ends.contains("each"));
        debug_assert!(self.ctrl_ends.contains("endcase"));
        debug_assert!(self.ctrl_ends.contains("endif"));
        debug_assert!(self.ctrl_ends.contains("loop"));
        debug_assert!(self.ctrl_ends.contains("+loop"));
        debug_assert!(self.ctrl_ends.contains("until"));
    }

    pub(crate) fn ins_core(&mut self, branch: Item) {
        debug_assert!({let b = self.core.get(&branch.name()).is_none();
            if ! b { eprintln!("DOUBLE {}", branch.name()) }; b});  // no double entries allowed
        self.core.insert(branch.name(), branch);
    }

    fn ins_ctrl(&mut self, tup: (&str, Item)) {
        if ! tup.0.is_empty() {
            let end = tup.0;
            self.ctrl_ends.insert(end.to_string());
            self.syntax.insert(tup.1.name(), (tup.1.get_ctrl_type(), end.to_string()));
        }   //                 start                                 end
        self.ins_core(tup.1);
    }

    pub(crate) fn parse_number(&self, mut word: &str) -> Result<Number, String> {
        let mut base = self.base;
        if word.starts_with("0x") {
            base = 16;
            word = &word[2..];
        }
        else if word.starts_with("0b") {
            base = 2;
            word = &word[2..];
        }
        let rslt = i64::from_str_radix(&word, base as u32);
        match rslt {
            Ok(i) => {
                let num = Number::INTEGER(i);
                Ok(num)
            },
            Err(_) => {
                if self.base == 10 {
                    let rslt = f64::from_str(&word);
                    match rslt {
                        Ok(f) => {
                            let num = Number::FLOAT(f);
                            Ok(num)
                        },
                        Err(_) => {
                            if let Some(cplx) = Parser::parse_complex(&word) {
                                Ok(cplx)
                            }
                            else {
                                if self.cplx_i.is_match(&word) {
                                    Err(CPLX_FORMAT.dots())
                                }
                                else {
                                    Err(UNKNOWN.dots())
                                }
                            }
                        }
                    }
                }
                else {
                    Err(INT_FORMAT.dots())
                }
            }
        }
    }
}  // yralubacoV

fn block_print(slice: &[String]) {
    let mut acc = "".to_string();
    for w in slice {
        if acc.len() + w.len() > 80 {
            println!("{acc}");
            acc = "".to_string();
        }
        acc = format!("{acc}{w}");
        acc += " ";
    }
    if acc.len() > 0 {
        println!("{acc}");
    }
}
