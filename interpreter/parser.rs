// ©2024 Otmar Klenk
#![macro_use]

use crate::v;

use nalgebra::DMatrix;
use core::slice::Windows;
use std::cmp;
use std::mem;
use std::str::FromStr;
use std::sync::Mutex;
use TOKEN::*;

use super::Number;
use super::item::cmd::value::Value;
use super::tools::Dots;

pub type Tokens = Vec<(TOKEN, String)>;

pub(crate) const CPLX_FORMAT:      &str = "CPLX FORMAT";

macro_rules! make_f64_matrix {
    ($word: expr, $rslt: expr) => {{
        let mut row_cnt = 0;
        for line in $word.lines() {
            row_cnt += 1;
            let Ok(mut f_vec) = Parser::parse_csv(true, &line) else {
                row_cnt = usize::MAX;
                break;
            };
            $rslt.append(&mut f_vec);
        }
        row_cnt
    }}
}

macro_rules! make_str_matrix {
    ($word: expr, $rslt: expr) => {{
        let mut row_cnt = 0;
        for line in $word.lines() {
            row_cnt += 1;
            match Parser::parse_csv(false, &line) {
                Ok(_)           => unreachable!(),
                Err(mut s_vec)  => $rslt.append(&mut s_vec),
            };
        }
        row_cnt
    }}
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TOKEN {
    UNDEF,
    WORD,       // if else then
    AOL,        // (1+2)
    MATRIX,     // [1 2 3]
    STRING,     // "str"
    CHAR,       // 'c'
    TICK,       // '
    STACK,      // ( -- )
    COMMENT,    // (**)
    IMMEDIATE,  // { word } [1 2 3]
    BEGCOMPILE, // :
    ENDCOMPILE, // ;
    PRINT,      // ."" .''
}

/////////////////////////////////////////////////////////////////////////////////////////
const TOK_IN:  [char; 3] = ['(', '[', '{'];
const TOK_OUT: [char; 3] = [')', ']', '}'];
/////////////////////////////////////////////////////////////////////////////////////////
pub struct Parser {
    result: Mutex<Tokens>,
    backsl_cnt: u32,
    token: TOKEN,
}

/////////////////////////////////////////////////////////////////////////////////////////

impl Parser {
    pub fn new() -> Self {
        Self {
            result: Mutex::new(Vec::with_capacity(32)),
            backsl_cnt: 0,
            token: UNDEF,
        }
    }

    fn is_escaped(&self) -> bool {
        (self.backsl_cnt & 1) == 1
    }

    fn joining(&self, joined: &mut String, s: &str) {
        if joined.is_empty() {
            *joined = s.into();
        }
        else {
            *joined = [joined.as_str(), &s].join("\n");
        }
    }

    fn push_rslt(&mut self, s: &mut String) {
        Vec::push(&mut self.result.lock().unwrap(), (self.token, s.to_string()));
        (*s).clear();
        self.token = UNDEF;
    }

    fn terminate(&mut self, open: &mut usize, t_in: &str, t_out: &str,
            wnd: &mut Windows<char>, joined: &mut String) -> Option<String> {
        self.backsl_cnt = 0;
        if t_in == "[" { *open += 1; }
        let mut s = String::new();
        while let Some([a, b]) = wnd.next() {
            match [a, b] {
                // ',' handled in parse_csv
                [_, '\\'] => {
                    self.backsl_cnt += 1;
                    if t_out == "\"" {
                        s.push('\\');
                    }
                    continue;
                }
                [_, '"'] | [_, '\''] if *open > 0 => {
                    s.push(*b);
                    continue;
                }
                [_, b] if *b != '"' && t_out == "\"" => {
                    s.push(*b);
                    continue;
                }
                [_, '"'] | [_, '\''] if t_in == "(" => {
                    s.push(*b);
                }
                [_, '"'] | [_, '\''] => {
                    return Some(s);
                }
                [a, b] => {
                    if self.is_escaped() {
                        self.backsl_cnt = 0;
                        s.push(*b);
                        continue;
                    }
                    if *a == '\'' {  // TICK
                        if b.is_whitespace() && t_out == "'" {
                            s.push_str("' ");  // with trailing blank for TICK
                            return Some(s);
                        }
                    }
                    if [*a, *b] == ['(', '*'] {  // blank
                        while let Some([a, b]) = wnd.next() {
                            if [*a, *b] == ['*', ')'] {
                                return None;
                            }
                        }
                    }
                    if b.is_whitespace() {
                        if (t_out == "\"" || ! s.ends_with(|c: char| c.is_whitespace()))
                                && ! s.is_empty() {
                            s.push(' ');
                            if t_out == "'" {
                                return Some(s);  // with trailing blank for TICK
                            }
                        }
                    }
                    else if TOK_IN.contains(b) {
                        if let Some(t) = self.terminate(open, t_in, t_out, wnd, joined) {
                            self.joining(joined, &t);
                        }
                        else {
                            unreachable!();
                        }
                    }
                    else {
                        if TOK_OUT.contains(b) {
                            if *b == ']' {
                                debug_assert_ne!(*open, 0);
                                *open = open.saturating_sub(1);
                                self.token = MATRIX;  // IMMEDIATE
                                if *open == 1 {
                                    return Some(s);  // vertical vector
                                }
                                if *open == 0 {
                                    if ! joined.is_empty() {
                                        return None;  // vertical vector
                                    }
                                    return Some(s);
                                }
                                if s.starts_with(',') || s == ", " {
                                    continue;
                                }
                            }
                            else {
                                self.token = IMMEDIATE;
                            }
                            return Some(s);
                        }
                        else {
                            s.push(*b);
                        }
                    }
                }
            }
        }
        None
    }

    fn parse_csv(mut try_parse: bool, line: &str) -> Result<Vec<f64>, Vec<String>> {
        let mut rslt: Vec<f64> = Vec::with_capacity(8);
        let mut err: Vec<String> = Vec::with_capacity(8);
'error: for s in line.split(',').into_iter() {
            if ! s.is_empty() {
                let mut it = s.split(' ');
                if try_parse {
                    while let Some(t) = it.next() {
                        if ! t.is_empty() {
                            match t.parse::<f64>() {
                                Ok(f)   => {
                                    rslt.push(f);
                                },
                                Err(_)  => {
                                    try_parse = false;
                                    continue 'error;
                                },
                            }
                        }  // fi
                    }
                }
                else {
                    while let Some(t) = it.next() {
                        if ! t.is_empty() {
                            err.push(t.to_string());
                        }  // fi
                    }
                }  // esrap_yrt
            }
        }
        if try_parse {
            Ok(rslt)
        }
        else {
            Err(err)
        }
    }

    pub(crate) fn as_matrix(try_parse: bool, word: &str) -> Result<Value, Value> {
        if try_parse {
            let rslt: &mut Vec<f64> = &mut Vec::with_capacity(8);
            let row_cnt = make_f64_matrix!(word, rslt);
            if row_cnt != usize::MAX {
                let mat = DMatrix::from_row_slice(row_cnt, rslt.len() / cmp::max(row_cnt, 1), &rslt);
                return Ok(v!(mat));
            }
        }
        let rslt: &mut Vec<String> = &mut Vec::with_capacity(8);
        let row_cnt = make_str_matrix!(word, rslt);
        let mat = DMatrix::from_row_slice(row_cnt, rslt.len() / cmp::max(row_cnt, 1), &rslt);
        Err(v!(mat))
    }

    pub fn parse(&mut self, line: &str) -> Tokens {
        let mut joined = &mut String::new();
        let vc = format!(" {line}").chars().collect::<Vec<char>>();  // dummy ' '
        let wnd = &mut vc.windows(2);
        let mut open = &mut 0;
        let mut last_t = '\0';
        let mut last_a;
        self.result.lock().unwrap().clear();
 
        while let Some([a, b]) = {let arr_opt = wnd.next();
            last_a = last_t;
            match arr_opt {
                Some(arr)   => last_t = arr[0],
                None        => last_t = '\0',
            };
            arr_opt
        } {
            match [a, b] {
                [a, '('] if a.is_whitespace() => {
                    self.token = AOL;  // CPLX
                    if let Some(mut t) = self.terminate(&mut open, "(", ")", wnd, &mut joined) {
                        if t == "*" {
                            self.token = COMMENT;
                        }
                        else if t.contains("--") {
                            self.token = STACK;
                        }
                        else if t.contains(',') {
                            self.token = WORD;  // CPLX
                        }
                        self.push_rslt(&mut t);
                    }
                }
                [a, '['] if a.is_whitespace() => {
                    if let Some(mut t) = self.terminate(&mut open, "[", "]", wnd, &mut joined) {
                        self.token = MATRIX;  // IMMEDIATE
                        self.backsl_cnt = 0;
                        self.push_rslt(&mut t);
                    }
                    else {
                        self.push_rslt(&mut joined);  // vertical vec
                    }
                }
                [_a, '{']   => {
                    if let Some(mut t) = self.terminate(&mut open, "{", "}", wnd, &mut joined) {
                        self.token = IMMEDIATE;
                        self.push_rslt(&mut t);
                    }
                }
                [a, b] if b.is_whitespace() || *b == ',' => {
                    let tok = self.token;
                    if *a == ',' {
                        self.token = WORD;
                        *joined = String::from(',');
                        self.push_rslt(joined);
                        continue;
                    }
                    if tok != UNDEF {
                        if ! joined.is_empty() {
                            match joined.as_str() {
                                ":" => self.token = BEGCOMPILE,
                                ";" => self.token = ENDCOMPILE,
                                &_  => {}
                            }
                            self.push_rslt(joined);
                        }
                        self.token = UNDEF;
                        *joined = "".to_string();
                    }
                }
                [a, '"']    => {
                    if let Some(mut t) = self.terminate(&mut open, "", &*b.to_string(), wnd, &mut joined) {
                        if *a == ' ' {
                            self.token = STRING;
                            self.push_rslt(&mut t);
                        } else if *a == '.' && last_a.is_whitespace() {
                            self.token = PRINT;
                            self.push_rslt(&mut t);
                            *joined = "".to_string();  //.
                        }
                    }
                }
                [a, '\'']   => {
                    match self.terminate(&mut open, "", &*b.to_string(), wnd, &mut joined) {
                        Some(mut t) if *a == ' '                    => {
                            self.token = if t.len() > 1 {
                                t = t.trim().to_string();  // remove trailing blank
                                TICK
                            }
                            else {
                                CHAR
                            };
                            self.push_rslt(&mut t);
                        }
                        Some(mut t) if *a == '.' && last_a.is_whitespace()  => {
                            self.token = PRINT;
                            self.push_rslt(&mut t);
                        }
                        Some(_t)                                    => {}
                        None                                        => {}
                    }
                }
                _           => {
                    if ! b.is_whitespace() && *b != ',' {
                        if *a == ',' {
                            self.token = WORD;
                            *joined = String::from(',');
                            self.push_rslt(joined);
                        }
                        *joined = format!("{joined}{}", *b);
                    }
                    match joined.is_empty() {
                        true    => self.token = UNDEF,
                        false   => self.token = WORD,
                    }
                }
            };
        }
        let mut rslt = Vec::with_capacity(self.result.lock().unwrap().len());
        mem::swap(self.result.get_mut().unwrap(), &mut rslt);
        rslt
    }

//  fn parse_number(&self, mut word: &str) -> Result<Number, String>  --> vocabulary

    pub(crate) fn parse_complex(word: &str) -> Option<Number> {
        let mut re = 0.0;
        let mut imag = word.into();
        if let Some(_comma) = word.find(',') {
            let mut it = word.split(',').rev();
            if let Some(x) = it.next() {
                imag = x.to_string();
            }
            if let Ok(x) = f64::from_str(it.next().expect(&CPLX_FORMAT.dots()).trim()) {
                re = x;
            }
            else {
                return None;
            }
        }
        if let Some(n) = imag.find('i') {
            imag.remove(n);
            if let Ok(im) = f64::from_str(imag.trim()) {
                return Some(Number::COMPLEX(re, im));
            }
        }
        None
    }
}
