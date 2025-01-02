// ©2024 Otmar Klenk
use bigdecimal::{BigDecimal, FromPrimitive, ToPrimitive};

pub trait Strip {
    fn strip(&self, scale: usize) -> String;
}

impl Strip for f64 {  // strips rc_flowing 0 from float's
    fn strip(&self, scale: usize) -> String {
        let fmt = format!("{:.1$}", self, scale);
        if fmt.contains('.') {
            let mut rslt = fmt.trim_end_matches(|c: char| c == '0');
            rslt = rslt.trim_end_matches(|c: char| c == '.');
            rslt.to_string()
        }
        else {
            fmt
        }
    }
}   

impl Strip for BigDecimal {  // strips rc_flowing 0 from float's
    fn strip(&self, scale: usize) -> String {
        let fmt = format!("{:.1$}", self, scale);
        if fmt.contains('.') {
            let mut rslt = fmt.trim_end_matches(|c: char| c == '0');
            rslt = rslt.trim_end_matches(|c: char| c == '.');
            rslt.to_string()
        }
        else {
            fmt
        }
    }
}   

pub trait Dots {
    fn dots(&self) -> String;
}

impl Dots for &str {
    fn dots(&self) -> String {
        self.to_string() + "..."
    }
}   

#[inline]
pub(crate) fn mul_div(a: f64, b: f64, c: f64) -> f64 {
    let bd0 = BigDecimal::from_f64(a);
    let bd1 = BigDecimal::from_f64(b);
    let bd2 = BigDecimal::from_f64(c);
    ((bd0.unwrap() * bd1.unwrap()) / bd2.unwrap()).to_f64().unwrap()
}

pub(crate) fn conv(mut n: u64, base: u32) -> String {
    if base == 10 {
        format!("{}", n)
    }
    else { 
        let mut rslt = String::new();
        let b = base as u64;
        while {
            let r = n % b;
            n = n / b;
            rslt = format!("{}{}", std::char::from_digit(r as u32, base).unwrap(), rslt);
            n > 0
        } {}
        format!("{}‹{}›", rslt, base)
    }
}
