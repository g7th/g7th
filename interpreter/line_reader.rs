// ©2024 Otmar Klenk
use std::cell::OnceCell;
use std::fs::File;
use std::io::{self, stdin, BufRead, BufReader, Lines, Write};
use std::iter::Peekable;
use std::mem;
use std::vec::IntoIter;

use super::Parser;
use super::{FILE, TOKEN};
use super::vocabulary::SECONDARIES;

const WHEEL: &[u8] = b"|/-\\";
static mut WPOS: usize = 0;

#[macro_export]
macro_rules! flush {  // (-> core) Box<item::Item>
    () => {{
        use std::io::Write;
        let _ = std::io::stdout().flush();
    }}
}
#[macro_export]
macro_rules! eflush {  // (-> core) Box<item::Item>
    () => {{
        use std::io::Write;
        let _ = std::io::stderr().flush();
    }}
}

macro_rules! parse_line {
    ($line: expr, $v: expr) => {{
        if ! $line.is_empty() {
            for tuples in Parser::new().parse(push_eol!($line)) {
                $v.push(tuples);
            }
        }
    }}
}

macro_rules! split2_pos {
    ($s: expr) => {
        $s.chars().position(|c| c == '\r' || c == '\n')
    }
}

macro_rules! push_eol {
    ($s: expr) => {{
        &format!("{}\n", $s)
    }}
}

pub(crate) fn wheel() -> String {
    unsafe {
        if WPOS < 3 {WPOS += 1;} else {WPOS = 0};
        format!("{}{}", WHEEL[WPOS] as char, '\u{8}')
    }
}

pub struct LineReader<'a> {
    secondaries:    OnceCell<()>,
    prompt:         &'a str,
    prmt:           &'a str,  // prompt while compiling
    input:          String,
    iter:           Peekable<IntoIter<(TOKEN, String)>>,
}

impl LineReader<'_> {
    pub fn new(prompt: &'static str, prmt: &'static str) -> Self {
        Self {
            secondaries:    OnceCell::new(),
            prompt:         prompt,
            prmt:           prmt,
            input:          String::new(),
            iter:           vec![].into_iter().peekable(),
        }
    }

    #[inline(always)]
    pub(crate) fn has_next(&mut self) -> bool {
        self.iter.peek().is_some()
    }

    #[inline(always)]
    pub(crate) fn next_tuple(&mut self, stdout: &mut io::StdoutLock, reason: u8, in_interpret_mode: bool) -> Option<(TOKEN, String)> {
        if reason == 0 && ! self.has_next() {
            self.look_ahead(stdout, in_interpret_mode);
        }
        self.iter.next()
    }

    #[inline(always)]
    fn look_ahead(&mut self, stdout: &mut io::StdoutLock, in_interpret_mode: bool) {
        let mut line = self.readline(stdout, if in_interpret_mode { self.prompt } else { self.prmt });
        if let Some(pos) = split2_pos!(line) {
            line.truncate(pos);
        }
        self.iter = IntoIterator::into_iter(Parser::new().parse(push_eol!(line))).peekable();
    }

    pub fn loadf(&mut self, fname: &str) -> Result<(), String> {
        let mut v = vec![];
        self.secondaries.get_or_init(|| {
            for line in SECONDARIES.iter() {
                parse_line!(line, v);
            }
        });
        if let Err(err) = FILE.with(|f| {
            let rslt = File::open(fname);
            let Ok(ok) = rslt else {*f.borrow_mut() = None; return Err(rslt.err().unwrap().to_string());};
            *f.borrow_mut() = Some(BufReader::new(ok));
            Ok(())
        }) {
            self.iter = v.into_iter().peekable();
            return Err(err);
        }
        if let Some(lines) = self.readlines() {
            for line in lines.flatten() {
                let mut iter = line.split_whitespace();
                if let Some(split) = iter.next() {
                    if split.starts_with('#') {
                        continue;
                    }
                }
                parse_line!(line, v);
            }
        };
        self.iter = v.into_iter().peekable();
        Ok(())
    }

    fn readlines(&self) -> Option<Lines<BufReader<File>>> {
        FILE.with(|r| {
            if r.borrow().is_some() {
                let Some(brd) = mem::replace(&mut *r.borrow_mut(), None) else { unreachable!() };
                Some(brd.lines())
            }
            else {
                None
            }
        })
    }

    fn readline(&mut self, stdout: &mut io::StdoutLock, prompt: &str) -> String {
        eflush!();
        let _ = write!(stdout, "{prompt} ");
        let _ = stdout.flush();
        self.accept()
    }

    #[inline(always)]
    pub fn accept(&mut self) -> String {
        let _ = mem::replace(&mut self.input, "".to_string());
        let mut stdin = stdin().lock();
        match stdin.read_line(&mut self.input) {
            Ok(_)       => {
                let mut line = self.input.to_string();
                if let Some(pos) = split2_pos!(line) {
                    line.truncate(pos);
                }
                line
            }
            Err(err)    => format!("ERROR: {}", err.to_string().to_uppercase()),
        }
    }
}
