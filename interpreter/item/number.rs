// ©2024 Otmar Klenk
#![macro_use]

use crate::{boolean, number};
use crate::vpush;

use bigdecimal::BigDecimal;
use std::cmp::Ordering;
use std::fmt;
use std::fmt::{Debug, Display};
use std::ops::{Add, Sub, Mul, Div, Neg, Rem};
use std::panic::panic_any;
use num_complex::Complex64;
use bigdecimal::{num_bigint::BigInt, FromPrimitive, ToPrimitive, Signed};

use super::cmd::value::Value;
use crate::interpreter::data::DataHolder;
use crate::interpreter::item::{Item, NamedBoolean, NamedNumerical};
use crate::interpreter::item::cmd::Cmd;
use crate::interpreter::item::word::NO_NAME;
use crate::interpreter::tools::{Dots, Strip};

pub(crate) const BOOL_EXPECTED: &str        = "BOOL EXPECTED";
pub(crate) const NUMBER_EXPECTED: &str      = "NUMBER EXPECTED";
pub(crate) const BIG_DECIMAL_EXPECTED: &str = "BIG DECIMAL EXPECTED";

pub type FnNumber = fn(&mut DataHolder, &Number);
pub(crate) const NUMBER_PUSH:   FnNumber = |dh: &mut DataHolder, n: &Number| { vpush!(dh, Value::from(n.clone())) };

#[macro_export]
macro_rules! idx {
    ($num: expr) => {
        i64::from($num) as usize
    }
}

#[macro_export]
macro_rules! modulo {
    ($k: expr, $n: expr) => {
        ($k % $n + $n) % $n
    }
}

pub(crate) fn is_bigdec(value: Value) -> bool {
    match value {
        Value::Numerical(Number::BIGDEC(_bd))   => true,
        _                                       => false,
    }
}

/////////////////////////////////////////////////////////////////////////////////////////
fn flawless_df(bd: &BigDecimal) -> bool {
    let eps: BigDecimal = BigDecimal::from_f64(f64::EPSILON).unwrap();
    BigDecimal::from_f64(bd.to_f64().unwrap()).unwrap().abs_sub(bd) <= eps
}

fn flawless_if(bi: &BigInt) -> bool {
    *bi == BigInt::from_f64(bi.to_f64().unwrap()).unwrap()
}

fn flawless_i(bi: &BigInt) -> bool {
    *bi == BigInt::from_i64(bi.to_i64().unwrap()).unwrap()
}

/////////////////////////////////////////////////////////////////////////////////////////
const LOSS_OF_ACCURACY: &str = "LOSS OF ACCURACY";

pub type FnBool   = fn(&mut DataHolder, &bool);
const BOOL_PUSH:   FnBool = |dh: &mut DataHolder, b: &bool| { vpush!(dh, Value::from(*b)) };

#[derive(Clone, Debug)]
pub enum Number {
    INTEGER(i64),
    FLOAT(f64),
    COMPLEX(f64, f64),
    BIGDEC(BigDecimal),
    BIGINT(BigInt),
}

impl From<i64> for Number {
    fn from(i: i64) -> Number {
        Number::INTEGER(i)
    }
}
impl From<f64> for Number {
    fn from(f: f64) -> Number {
        Number::FLOAT(f)
    }
}
impl From<Complex64> for Number {
    fn from(cmpl: Complex64) -> Number {
        Number::COMPLEX(cmpl.re, cmpl.im)
    }
}

impl From<Number> for i64 {
    fn from(n: Number) -> i64 {
        match n {
            Number::INTEGER(i)      => i,
            Number::FLOAT(r) if r.fract() == 0.0 => r as i64,
            Number::COMPLEX(re, im) if im == 0.0 && re.fract() == 0.0 => re as i64,
            Number::BIGINT(bi)      if flawless_i(&bi) => bi.to_i64().unwrap(),
            _                       => panic_any(LOSS_OF_ACCURACY.dots()),
        }
    }
}

impl From<Number> for f64 {
    fn from(n: Number) -> f64 {
        match n {
            Number::INTEGER(i)      => i as f64,
            Number::FLOAT(r)        => r,
            Number::COMPLEX(re, im) if im == 0.0 => re,
            Number::BIGDEC(bd)      if flawless_df(&bd) => bd.to_f64().unwrap(),
            Number::BIGINT(bi)      if flawless_if(&bi) => bi.to_f64().unwrap(),
            _                       => panic_any(LOSS_OF_ACCURACY.dots()),
        }
    }
}
impl From<Number> for Complex64 {
    fn from(n: Number) -> Complex64 {
        match n {
            Number::INTEGER(i)      => Complex64::new(i as f64, 0.0),
            Number::FLOAT(r)        => Complex64::new(r, 0.0),
            Number::COMPLEX(re, im) => Complex64::new(re, im),
            Number::BIGDEC(bd)      => Complex64::new(bd.to_f64().unwrap(), 0.0),
            Number::BIGINT(bi)      => Complex64::new(bi.to_f64().unwrap(), 0.0),
        }
    }
}
impl From<Number> for BigDecimal {
    fn from(n: Number) -> BigDecimal {
        match n {
            Number::INTEGER(i)      => BigDecimal::from(i),
            Number::FLOAT(r)        => BigDecimal::from_f64(r).unwrap(),
            Number::COMPLEX(re, im) if im == 0.0 => BigDecimal::from_f64(re).unwrap(),
            Number::BIGDEC(bd)      => bd,
            Number::BIGINT(bi)      => BigDecimal::from(bi),
            _                       => panic_any(LOSS_OF_ACCURACY.dots()),
        }
    }
}
impl From<Number> for BigInt {
    fn from(n: Number) -> BigInt {
        match n {
            Number::INTEGER(i)      => BigInt::from(i),
            Number::FLOAT(r) if r.fract() == 0.0 => BigInt::from(r as i64),
            Number::COMPLEX(re, im) if im == 0.0 && re.fract() == 0.0 => BigInt::from(re as i64),
            Number::BIGINT(bi)      => bi,
            _                       => panic_any(LOSS_OF_ACCURACY.dots()),
        }
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        f64::from(self.clone()).partial_cmp(&f64::from(other.clone()))
    }
}
impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Number::INTEGER(i)      => *i == i64::from(other.clone()),
            Number::FLOAT(r)        => *r == f64::from(other.clone()),
            Number::COMPLEX(re, im) => {
                let cplx = Complex64::from(other.clone());
                *re == cplx.re && *im == cplx.im
            },
            Number::BIGDEC(bd)      => *bd == BigDecimal::from(other.clone()),
            Number::BIGINT(bi)      => *bi == BigInt::from(other.clone()),
        }
    }
}

impl From<NamedBoolean> for Item {
    fn from(b: NamedBoolean) -> Item {
        boolean!(b.name, BOOL_PUSH, Value::Boolean(b.kind))
    }
}
impl From<NamedNumerical> for Item {
    fn from(num: NamedNumerical) -> Item {
        number!(num.name, NUMBER_PUSH, Value::Numerical(num.kind))
    }
}
impl From<i64> for Item {
    fn from(i: i64) -> Item {
        number!(NO_NAME, NUMBER_PUSH, Value::from(i))
    }
}
impl From<f64> for Item {
    fn from(f: f64) -> Item {
        number!(NO_NAME, NUMBER_PUSH, Value::from(f))
    }
}
impl From<Complex64> for Item {
    fn from(cmpl: Complex64) -> Item {
        number!(NO_NAME, NUMBER_PUSH, Value::from(cmpl))
    }
}

impl From<Item> for bool {
    fn from(b: Item) -> bool {
        if let Ok(x) = b.boolean() {
            x
        }
        else { panic_any(BOOL_EXPECTED.dots()); }
    }
}
impl From<Item> for i64 {
    fn from(b: Item) -> i64 {
        if let Ok(x) = b.number() {
            i64::from(x)
        }
        else { panic_any(NUMBER_EXPECTED.dots()); }
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), std::fmt::Error> {
        get(self, f)
    }
}

fn get(n: &Number, f: &mut fmt::Formatter) -> Result<(), std::fmt::Error> {
    match n {
        Number::INTEGER(i)      => write!(f, "{}", i),
        Number::FLOAT(r)        => write!(f, "{}", r.strip(8)),
        Number::COMPLEX(re, im) => write!(f, "({}, {}i)", re.strip(8), im.strip(8)),
        Number::BIGDEC(bd)      => write!(f, "{}", bd.strip(8)),
        Number::BIGINT(bi)      => write!(f, "{}", bi),
    }
}

pub(crate) fn precision(n: Number) -> i8 {
    match n {
        Number::INTEGER(_)      => 0,
        Number::FLOAT(_)        => 2,
        Number::COMPLEX(_, _)   => 3,
        Number::BIGDEC(_)       => 4,
        Number::BIGINT(_)       => 1,
    }
}

impl Neg for Number {
    type Output = Number;
    fn neg(self) -> <Number as Neg>::Output {
        match self {
            Number::INTEGER(i)      => Number::INTEGER(-i),
            Number::FLOAT(r)        => Number::FLOAT(-r),
            Number::COMPLEX(re, im) => Number::COMPLEX(-re, -im),
            Number::BIGDEC(bd)      => Number::BIGDEC(-bd),
            Number::BIGINT(bi)      => Number::BIGINT(-bi),
        }
    }
}

impl Add<Number> for Number {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        match precision(self.clone()) {
            0   => Number::INTEGER(i64::from(self)       + i64::from(other)),
            2   => Number::FLOAT(f64::from(self)         + f64::from(other)),
            3   => Number::from(Complex64::add(Complex64::from(self), Complex64::from(other))),
            4   => Number::BIGDEC(BigDecimal::from(self) + BigDecimal::from(other)),
            1   => Number::BIGINT(BigInt::from(self)     + BigInt::from(other)),
            _   => unreachable!(),
        }
    }
}
impl Sub<Number> for Number {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        match precision(self.clone()) {
            0   => Number::INTEGER(i64::from(self)       - i64::from(other)),
            2   => Number::FLOAT(f64::from(self)         - f64::from(other)),
            3   => Number::from(Complex64::sub(Complex64::from(self), Complex64::from(other))),
            4   => Number::BIGDEC(BigDecimal::from(self) - BigDecimal::from(other)),
            1   => Number::BIGINT(BigInt::from(self)     - BigInt::from(other)),
            _   => unreachable!(),
        }
    }
}
impl Mul<Number> for Number {
    type Output = Self;
    fn mul(self, other: Self) -> Self {
        match precision(self.clone()) {
            0   => Number::INTEGER(i64::from(self)       * i64::from(other)),
            2   => Number::FLOAT(f64::from(self)         * f64::from(other)),
            3   => Number::from(Complex64::mul(Complex64::from(self), Complex64::from(other))),
            4   => Number::BIGDEC(BigDecimal::from(self) * BigDecimal::from(other)),
            1   => Number::BIGINT(BigInt::from(self)     * BigInt::from(other)),
            _   => unreachable!(),
        }
    }
}
impl Div<Number> for Number {
    type Output = Self;
    fn div(self, other: Self) -> Self {
        match precision(self.clone()) {
            0|2 => Number::FLOAT(f64::from(self)         / f64::from(other)),
            3   => Number::from(Complex64::div(Complex64::from(self), Complex64::from(other))),
            4|1 => Number::BIGDEC(BigDecimal::from(self) / BigDecimal::from(other)),
            _   => unreachable!(),
        }
    }
}

impl Rem<Number> for Number {
    type Output = Self;
    fn rem(self, other: Self) -> Self {
        match precision(self.clone()) {
            0|2 => Number::FLOAT(f64::from(self)         % f64::from(other)),
            3   => Number::from(Complex64::from(self) % Complex64::from(other)),
            4|1 => Number::BIGDEC(BigDecimal::from(self) % BigDecimal::from(other)),
            _   => unreachable!(),
        }
    }
}

pub trait IntDiv<Rhs = Self> {
    type Output;

    fn intdiv(self, rhs: Rhs) -> Self::Output;
}
impl IntDiv<Number> for Number {
    type Output = Self;
    fn intdiv(self, other: Self) -> Self {
        match precision(self.clone()) {
            0   => Number::INTEGER(i64::from(self).div_euclid(i64::from(other))),
            2   => Number::FLOAT(f64::from(self).div_euclid(f64::from(other))),
            3   => Number::from({
                    let cplx = Complex64::div(Complex64::from(self), Complex64::from(other));
                    Complex64::new(cplx.re.trunc(), cplx.im.trunc())
                }),
            4   => Number::BIGDEC((BigDecimal::from(self) / BigDecimal::from(other)).with_scale(0)),
            1   => Number::BIGINT(BigInt::from(self) / BigInt::from(other)),
            _   => unreachable!(),
        }
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Value {
        Value::Boolean(b)
    }
}

impl From<Number> for Value {
    fn from(n: Number) -> Value {
        Value::Numerical(n)
    }
}
impl From<i64> for Value {
    fn from(i: i64) -> Value {
        Value::Numerical(Number::INTEGER(i))
    }
}
impl From<f64> for Value {
    fn from(f: f64) -> Value {
        Value::Numerical(Number::FLOAT(f))
    }
}
impl From<Complex64> for Value {
    fn from(cmpl: Complex64) -> Value {
       Value::Numerical(Number::COMPLEX(cmpl.re, cmpl.im))
    }
}
impl From<BigDecimal> for Value {
    fn from(bd: BigDecimal) -> Value {
        Value::Numerical(Number::BIGDEC(bd))
    }
}
impl From<BigInt> for Value {
    fn from(bi: BigInt) -> Value {
        Value::Numerical(Number::BIGINT(bi))
    }
}

impl From<Value> for i64 {
    fn from(v: Value) -> i64 {
        match v {
            Value::Numerical(Number::INTEGER(i))    => i,
            Value::Numerical(Number::BIGINT(bi))  if flawless_i(&bi) => bi.to_i64().unwrap(),
            _                                       => unreachable!(),
        }
    }
}
impl From<Value> for f64 {
    fn from(v: Value) -> f64 {
        match v {
            Value::Numerical(Number::FLOAT(f))                          => f,
            Value::Numerical(Number::BIGDEC(bd)) if flawless_df(&bd)    => bd.to_f64().unwrap(),
            Value::Numerical(Number::BIGINT(bi)) if flawless_if(&bi)    => bi.to_f64().unwrap(),
            Value::Numerical(Number::INTEGER(i))                        => { let bi = BigInt::from_i64(i);
                                                                             bi.unwrap().to_f64().expect(LOSS_OF_ACCURACY) }
            _                                                           =>  unreachable!(),
        }
    }
}
impl From<Value> for Complex64 {
    fn from(v: Value) -> Complex64 {
        if let Value::Numerical(Number::COMPLEX(re, im)) = v { Complex64::new(re, im) } else { unreachable!() }
    }
}
impl From<Value> for BigDecimal {
    fn from(v: Value) -> BigDecimal {
        if let Value::Numerical(Number::BIGDEC(bd)) = v { bd } else { unreachable!() }
    }
}
