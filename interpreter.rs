// ©2024 Otmar Klenk
#![macro_use]
use crate::caller;
use crate::{v, vpush};

mod data;
mod dictionary;
mod execute;
mod finder;
mod item;
mod leaf;
mod line_reader;
mod parser;
mod stack;
mod tools;
mod vocabulary;
mod with_deque;

use indextree::Arena;
use std::cell::RefCell;
use std::collections::HashSet;
use std::fs::File;
use std::io::{self, BufReader};
use std::panic::panic_any;

/////////////////////////////////////////////////////////////////////////////////////////
use item::Item;
use item::word::{IDX_PUSH, FN_PACKAGE};
use item::word::{AddrArrCode, IndexCode, WordCode, NumberCode, MatrixCode, CharCode, StringCode};
use item::number::{Number, NUMBER_PUSH};
use item::matrix::MATRIX_PUSH;
use item::string::{CHAR_PUSH, STRING_PUSH};
/////////////////////////////////////////////////////////////////////////////////////////
use Value::AddressArray;
use data::Data::{self, VALUE};
use data::{DataHolder, Modes};
use execute::Execute;
use finder::CodeFinder;
use item::GetCtrlType;
use item::branch::{CtrlType, GetCall, WithProgRefMut};
use item::branch::{append_to_a3, free_nodes, make_new_node};
use item::cmd::Cmd;
use item::cmd::callable::{Callable, Immediate};
use item::cmd::value::{GetValue, Value::{self, Numerical, Character, Textual}};
use item::word::PackageCode;
use leaf::Ctrl;
use parser::{Parser, TOKEN};
use stack::Stack;
use vocabulary::Vocabulary;
use with_deque::WithDeque;

pub type Call   = Box<dyn Callable>;
pub type Prog   = indextree::Arena<Call>;

/////////////////////////////////////////////////////////////////////////////////////////

pub const BREAK: u8 = 1;
pub const BYE: u8   = 4;
pub const EXIT: u8  = 2;

/////////////////////////////////////////////////////////////////////////////////////////

pub(crate) const EMPTY_NAME: &str   = "";
pub(crate) const PROMPT: &str       = "ok>";
pub(crate) const PRMT: &str         = ">";  // prompt while compiling
pub(crate) const HACK: &str         = "🍺";

#[macro_export]
macro_rules! word {  // (-> core!) Box<item::Item>
    ($type: expr, $name: expr, $fn_ptr: expr, $v: expr) => {
        crate::interpreter::item::Item::WORD { ctrl_type: $type, cmd: ($name.to_string(), Box::new(crate::interpreter::item::word::WordCode::new($fn_ptr, $v))) }
    }
}
#[macro_export]
macro_rules! addr_arr {  // (-> ctrl!) Item
    ($type: expr, $name: expr, $fn_addr_arr: expr, $v: expr) => {
        crate::interpreter::item::Item::ADDR_ARR { ctrl_type: $type, cmd: ($name.to_string(), Box::new(crate::interpreter::item::word::AddrArrCode::new($fn_addr_arr, $v))) }
    }
}
#[macro_export]
macro_rules! package {
    ($type: expr, $name: expr, $fn_package: expr, $v: expr) => {
        crate::interpreter::item::Item::PACKAGE { ctrl_type: $type, cmd: ($name.to_string(), Box::new(crate::interpreter::item::word::PackageCode::new($fn_package, $v))) }
    }
}

#[macro_export]
macro_rules! is_core {
    ($callable: expr) => {
        $callable.as_any().downcast_ref::<crate::interpreter::item::word::WordCode>().is_some()
    }
}

#[macro_export]
macro_rules! reg_break {
    ($dh: expr) => {
        use crate::interpreter::BREAK;
        $dh.bye_reg(BREAK);
    }
}
#[macro_export]
macro_rules! reg_bye {
    ($dh: expr) => {
        use crate::interpreter::BYE;
        $dh.bye_reg(BYE);
    }
}
#[macro_export]
macro_rules! reg_exit {
    ($dh: expr) => {{
        use crate::interpreter::EXIT;
        use crate::interpreter::data::Modes;
        $dh.do_interpret();
        $dh.bye_reg(EXIT);
    }}
}

#[macro_export]
macro_rules! is_inject {
    ($self: expr, $word: expr) => {
        INJECTS.with(|injects| {
            let b = injects.borrow().contains($word);
//            if b {
//                println!("{}", $word);
//            }
            b
        })
    }
}

#[macro_export]
macro_rules! new_package_item  {
    ($dh: expr, $word_name: expr) => {{
        use crate::package;
        use crate::interpreter::HACK;
        use crate::interpreter::item::branch::make_new_node;
        use crate::interpreter::with_deque::WithDeque;
        let mut package_item = package!(CtrlType::Other, $word_name, FN_PACKAGE, Value::Packable(vec![]));
        let new_id = make_new_node($dh, CtrlType::Other, $word_name, package_item.code_mut());
        $dh.leaf.ctrls.push_ctrl_slot((HACK.to_string(), new_id));  // ☕ ctrls.push()
        package_item
    }}
}

macro_rules! dh {
    ($self: expr) => {
        $self.stack.dh
    }
}

thread_local! {
    static PROG: RefCell<Prog> = RefCell::new(Prog::new());
    static NOOP: RefCell<Call> = RefCell::new(Box::new(WordCode::new(|_dh: &mut DataHolder, _v: &Value| {}, v!(Value::Unassigned))));
    static FREE: RefCell<Call> = RefCell::new(Box::new(WordCode::new(|_dh: &mut DataHolder, _v: &Value| { unreachable!() }, v!(Value::Unassigned))));
    static LOOP: RefCell<Call> = RefCell::new(Box::new(WordCode::new(|dh: &mut DataHolder, varr: &Value| {
        dh.looping(varr, |_dh| {1});
    }, v!(Value::Unassigned))));
    static FILE: RefCell<Option<BufReader<File>>> = RefCell::new(None);
    static INJECTS: RefCell<HashSet<&'static str>> = RefCell::new(HashSet::from(
        ["'", "cat", "cat$", "cd", "constant", "dir", "dirs", "dir$", "dirs$", "expect$",
         "forget", "fread", "fwrite", "fappend", "isfound", "load", "variable", "vocabulary"]));
}

/////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////

pub struct Interpreter<'a> {
    pub(crate)stack:    Stack<'a>,
    do_inject:          bool,  // <- compile
    run:                u32,  // depth
}

impl Interpreter<'_> {
    pub fn new() -> Self {
        let stack = Stack::new();
        let mut rslt = Self {
            stack:          stack,
            do_inject:      false,  // <- compile
            run:            0,
        };
        let flow = Arena::new();
        dh!(rslt).vocabulary.core(flow);
        rslt
    }

    fn is_ctrl_end(dh: &DataHolder, vocab: &Vocabulary, w: &str) -> Result<bool, String> {
        debug_assert!(! w.is_empty());
        if ! vocab.ctrl_ends.contains(w) { return Ok(false); };
        let v: &Vec<Ctrl> = dh.leaf.ctrls.get_ctrl_slot_vec();
        let Some((name, _id)) = rget!(v, 0) else { return Ok(false); };
        let (_ctrl_type, end) = Self::get_ctrl_syntax(vocab, name);
        match w {
            end if end == w             => return Ok(true),
            "loop" if end == "+loop"    => return Ok(true),
            _                           => Err(format!("UNEXPECTED \"{}\"", name)),

        }
    }
    #[inline(always)]
    fn get_ctrl_syntax<'a>(vocab: &'a Vocabulary, w: &'a str) -> &'a (CtrlType, String) {
        vocab.syntax.get(w).expect("SYNTAX")
    }

    #[inline]
    pub fn run(&mut self) {
        dh!(self).do_interpret();  // sic!
        std::env::args().into_iter().rev().for_each(|s| dh!(self).ret_arr.push(VALUE(Value::from(v!(s)))));
        let mut depth;
        while dh!(self).bye(BYE) == 0 {
            depth = self._run(CtrlType::Other);
            debug_assert_eq!(depth, 0);
        }  // pool
    }

    fn _run(&mut self, ctrl_type: CtrlType) -> u32 {
        self.run += 1;
        let stdout = &mut io::stdout().lock();
        while let Some((tok, w)) = {
            if dh!(self).bye(BREAK|BYE|EXIT) != 0 {
                self.run -= 1;
                return self.run;
            }
            dh!(self).next_tuple(stdout)
        } {
            match tok {
                TOKEN::BEGCOMPILE   => {  // ➀
                    dh!(self).dont_clean_prg();
                    dh!(self).leaf.clear_all();
                    let word = dh!(self).next_word(stdout);
                    dh!(self).do_compile();
                    self.colon_compile(&word);  // <- do_interpret()
                },
                TOKEN::ENDCOMPILE   => {
                    dh!(self).do_clean_prg();
                    dh!(self).do_interpret();
                    break; // from colon_compile or rstack
                },
                TOKEN::IMMEDIATE    => {
                    dh!(self).do_interpret();
                },
                TOKEN::WORD | TOKEN::TICK if {
                    match dh!(self).find_secondary(&w) {
                        Some(item)                                      => {
                            match item {
                                Item::PACKAGE { ctrl_type: _, cmd: (_, ref code) }              => {
                                    match code.as_any().downcast_ref::<PackageCode>() {
                                        Some(PackageCode {..}) if dh!(self).in_interpret_mode()                 => {
                                            caller!(&mut dh!(self), item.code());
                                        },
                                        Some(PackageCode {..}) if item.code().clone_callable().is_immediate()   => {
                                            caller!(&mut dh!(self), item.code());
                                        },
                                        Some(PackageCode {..})                                                  => {
//\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
                                            append_to_a3(&mut dh!(self), ctrl_type, &w, item.code());
/////////////////////////////////////////////////////////////////////////////////////////
                                        },
                                        None                                                                    => unreachable!(),
                                    };
                                    true
                                },
                                Item::INDEX { cmd: (_, ref code) }                              => {
                                    match code.as_any().downcast_ref::<IndexCode>() {
                                        Some(IndexCode {..}) if dh!(self).in_interpret_mode()                   => {
                                            dh!(self).data.push(Data::ITEM(item));
                                        },
                                        Some(IndexCode {..}) if item.code().clone_callable().is_immediate()     => {
                                            dh!(self).data.push(Data::ITEM(item));
                                        },
                                        Some(IndexCode {..})                                                    => {
//\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
                                            append_to_a3(&mut dh!(self), ctrl_type, &w, item.code());
/////////////////////////////////////////////////////////////////////////////////////////
                                        },
                                        None                                                                    => {},
                                    };
                                    true
                                },
                                _                                                               => {
                                    match item.code().get_value() {
                                        index @ Value::Indexable(_b, _idx)                                                => {
                                            IDX_PUSH(&mut dh!(self), index);
                                            true
                                        },
                                        Value::Addressable(id)                                                  => {
//\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
                                            let mut b_c: Box<dyn Callable> = PROG.with(|prg: &RefCell<Arena<Box<dyn Callable>>>| {
                                                prg.borrow().get_call(*id).unwrap().clone_callable()
                                            });
                                            self.compile(stdout, ctrl_type, &w, b_c.is_immediate(), &mut *b_c);
                                            true
/////////////////////////////////////////////////////////////////////////////////////////
                                        },
                                        _                                                                       => unreachable!(),
                                    }
                                }
                            }  // meti hctam
                        },
                        None                                            => false,
                    }
                } => {}  // ➂
                TOKEN::WORD | TOKEN::TICK => {
                    let vocab: &Vocabulary = &dh!(self).vocabulary;
                    let item_opt = Vocabulary::find_core_item(&vocab, &w);
                    match item_opt {
                        Some(item) if item.is_ctrl_intro()  => {  // ➁ <-- rstack.push
                            dh!(self).ctrl_level += 1;
                            debug_assert!(! dh!(self).in_interpret_mode());
                            debug_assert_eq!(CtrlType::Intro, Interpreter::get_ctrl_syntax(&vocab, &w).0);  // CtrlType, String
                            let b_c = item.code().clone_callable();
                            self.compile_ctrl_intro(CtrlType::Intro, &w, &b_c);
                            continue;
                        }
                        Some(item) if item.is_ctrl_cont()   => {
                            debug_assert_eq!(CtrlType::Cont, Interpreter::get_ctrl_syntax(&vocab, &w).0);  // CtrlType, String
                            let b_c = item.code().clone_callable();
                            self.compile_ctrl_cont(CtrlType::Cont, &w, &b_c);  // <-- next arm
                            continue;
                        }
                        Some(item)                          => {  // "'"
                            debug_assert!(CtrlType::Other == item.get_ctrl_type());  // no CtrlType::End
                            let mut b_c = item.code().clone_callable();
                            if w == "does>" {
                                self.compile_ctrl_intro(CtrlType::Intro, EMPTY_NAME, &b_c);
                            }
                            else {
                                self.compile(stdout, ctrl_type, &w, b_c.is_immediate(), &mut *b_c);
                            }
                        }
                        _                                   => {  // secondaries & values
                            match Self::is_ctrl_end(&dh!(self), vocab, &w) {
                                Ok(b) if b  => {  // ➃ END
                                    if w == "loop" {
                                        dh!(self).leaf.ctrls.patch_loop();
                                    }
                                    self.compile_ctrl_end(CtrlType::End, &w);
                                    let lev = dh!(self).ctrl_level;
                                    dh!(self).ctrl_level -= 1;
                                    if lev == 1 {
                                        if dh!(self).in_interpret_mode() {
                                            if let Some((s, id)) = dh!(self).leaf.ctrls.pop_ctrl_slot() {
                                                debug_assert_ne!(s, HACK);
                                                dh!(self).execute_id(id);
                                            }
                                            break;
                                        }
                                        continue;
                                    }
                                    let ctrl: Option<Ctrl> = WithDeque::pop_ctrl_slot(&mut dh!(self).leaf.ctrls);
                                    debug_assert_eq!(ctrl.unwrap().0, EMPTY_NAME);
                                    continue;
                                }
                                Ok(_)       => {},
                                Err(err)    => panic_any(err),
                            }
                            if &w == ")" { eprintln!("SYNTAX: unexpected ')'"); continue; }
                            // boolean is handled as core-word
                            let num = vocab.parse_number(&w);
                            match num {
                                Ok(num) => {
                                    let num_code = &mut NumberCode::new(NUMBER_PUSH, Numerical(num));
                                    let c_mut: &mut dyn Callable = num_code;
//\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
                                    self.compile(stdout, ctrl_type, EMPTY_NAME, c_mut.is_immediate(), c_mut);
/////////////////////////////////////////////////////////////////////////////////////////
                                },
                                Err(e) => eprintln!("{w} {e}"),
                            }
                        }
                    }
                }  // DROW::NEKOT
                TOKEN::MATRIX       => {
                   let v = match Parser::as_matrix(true, &w) {
                        Ok(value)   => value,  // [f64]
                        Err(value)  => value,  // [String]
                    };
                    let mat_code = MatrixCode::new(MATRIX_PUSH, v);
                    vpush!(dh!(self), mat_code.value);
                },  // XIRTAM::NEKOT
                TOKEN::CHAR         => {
                    match w.chars().take(1).last() {
                        Some(ch)    => {
                            let char_code = &mut CharCode::new(CHAR_PUSH, Character(ch));
                            let c_ref: &mut dyn Callable = char_code;
                            self.compile(stdout, ctrl_type, EMPTY_NAME, c_ref.is_immediate(), c_ref);
                        }
                        _           => eprintln!("CHAR {w}"),
                    }
                },  // RAHC::NEKOT
                TOKEN::STRING       => {
                    let str_code = &mut StringCode::new(STRING_PUSH, Textual(String::from(w)));
                    let c_ref: &mut dyn Callable = str_code;
                    self.compile(stdout, ctrl_type, EMPTY_NAME, c_ref.is_immediate(), c_ref);
                },  // GNIRTS::NEKOT
                TOKEN::PRINT        => {
                    let str_code = &mut StringCode::new(STRING_PUSH, Textual(String::from(w)));
                    let c_ref: &mut dyn Callable = str_code;
                    self.compile(stdout, ctrl_type, EMPTY_NAME, c_ref.is_immediate(), c_ref);  // sic!
                    self.compile(stdout, ctrl_type, EMPTY_NAME, false, &mut *Vocabulary::find_core_code(&dh!(self).vocabulary, "print").unwrap());
                },  // GNIRTS::NEKOT
                TOKEN::STACK        => {},
                TOKEN::COMMENT      => {},
                _                   => { unreachable!() },
            }  // hctam
        }  // elihw
        debug_assert!(self.run >= 1);
        self.run -= 1;
        self.run
    }

    fn do_finalize(&mut self) -> bool {
        let mut rslt = true;
        if let Err(err) = dh!(self).leaf.finished(&dh!(self).vocabulary) {
            eprintln!("{err}");
            free_nodes(&mut dh!(self).leaf.root_id);
            rslt = false;
        }
        dh!(self).leaf.root_id = None;
        rslt
    }

    pub(crate) fn colon_compile(&mut self, word_name: &str) {  // isolated
        // create new Item
        let mut package_item = new_package_item!(&mut dh!(self), word_name);
        // create prog
        self._run(CtrlType::Other);
//// =======⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯⚯
        let (_hack, slot) = dh!(self).leaf.ctrls.remove(0);

        if self.do_finalize() {
            PROG.with(|prg| {
                package_item.code_mut().set_value(prg.borrow()[slot].get().get_value());
            });
            // Item into dictionary
            dh!(self).dictionary.ins_current(package_item);  // dir - sibn, sibn-1... sib2, sib1
        }
        dh!(self).do_interpret();
    }

    #[inline]
    fn compile_ctrl_intro(&mut self, ctrl_type: CtrlType, ctrl_name: &str, b_c_ctrl: &Call) {
        let code_opt: Option<&AddrArrCode> = b_c_ctrl.as_any().downcast_ref::<AddrArrCode>();
        match code_opt {
            Some(AddrArrCode { fn_addr_arr, .. })  => {  // "repeat" .. until
                let mut item_branch = addr_arr!(ctrl_type, ctrl_name.to_string(), *fn_addr_arr, AddressArray(vec![]));
                append_to_a3(&mut dh!(self), ctrl_type, ctrl_name, item_branch.code_mut());  // link_hook
            }
            None                            => {
                eprintln!("UNSUPPORTED CTRL {ctrl_name}");
            }
        };
    }

    #[inline(always)]
    pub(crate) fn compile_ctrl_cont(&mut self, new_ctrl_type: CtrlType, new_name: &str, b_c_ctrl: &Call) {
        PROG.with(|prg| {
            let Some((_s, at_id)): Option<Ctrl> = dh!(self).leaf.ctrls.peek_rearmost() else { unreachable!(); };
            debug_assert_ne!(at_id, dh!(self).free_id);
            let new_id = make_new_node(&mut dh!(self), new_ctrl_type, new_name, b_c_ctrl.as_ref());
//---------------------------------------------------------------------------------------
            let mut p = prg.borrow_mut();
            let value: &mut Value = p.get_value_at_mut(at_id).expect("COMPILE_CTRL_CONT");
            let a3: &mut Vec<Value> = value.addr_arr_mut();
            a3.push(Value::Packable(vec![new_id]));
        })
    }

    #[inline]
    pub(crate) fn compile_ctrl_end(&mut self, ctrl_type: CtrlType, _ctrl_name: &str) {
        WithDeque::pop_ctrl_slot(&mut dh!(self).leaf.ctrls);
        debug_assert_eq!(CtrlType::End, ctrl_type);
    }

    fn compile(&mut self, stdout: &mut io::StdoutLock, ctrl_type: CtrlType, name: &str, is_immediate: bool, c_mut: &mut dyn Callable) {  // handle ctrl and core
        if dh!(self).in_interpret_mode() || is_immediate {
            if is_inject!(self, name) {
                if (! dh!(self).has_next())
                    && (name == "dir" || name == "dir$" || name == "dirs" || name == "dirs$") {  // set default
                    c_mut.set_value(&v!(".".to_string()));
                }
                else {
                    c_mut.set_value(&v!(dh!(self).next_word(stdout)));
                }
            }
            c_mut.call(&mut dh!(self), c_mut.get_value());  // <<<<<<<<<<<<<<<<<<<<<<<
        }
        else {
            if self.do_inject {
                dh!(self).compile_inject(name);  // ඞ
            }
            else {
                append_to_a3(&mut dh!(self), ctrl_type, EMPTY_NAME, c_mut);
                self.do_inject = is_inject!(self, name);
            }
        }
    }
}
