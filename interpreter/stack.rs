// ©2024 Otmar Klenk
#![macro_use]

use crate::{n, pop_i64};
use crate::reg_exit;
use crate::flush;

use std::cell::RefCell;
use std::fmt::{self, Display};
use std::panic::panic_any;

use crate::interpreter::Prog;
use crate::interpreter::data::{Data, DataHolder};
use crate::interpreter::tools::conv;
use crate::interpreter::vocabulary::fix_idx;
use super::item::cmd::value::Value;
use super::item::number::Number;

pub struct Stack<'a> {
    pub(crate) dh:          DataHolder<'a>,
}

impl Stack<'_> {
    pub(crate) fn new() -> Self {
        Self {
            dh:             DataHolder::new(),
        }
    }

    #[allow(dead_code)]
    pub(crate) fn params(&mut self, _prg: &RefCell<Prog>, _v: &Value) -> &[Data] {
        let n = pop_i64!(self.dh); self.dh.data.split_at(fix_idx(self.dh.data.len(), n)).1
    }
}
/////////////////////////////////////////////////////////////////////////////////////////

impl Display for Data {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Data::VALUE(value)  => write!(f, "{value}"),
            Data::ITEM(item)    => write!(f, "{item}"),
        }
    }    
}

impl Display for DataHolder<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), std::fmt::Error> {
         for item in &self.data {
            flush!();
            match item {
                Data::VALUE(value) => {
                    loop {  // Structure
                        match value {
                            Value::Numerical(_)     => {
                                if let Value::Numerical(Number::INTEGER(i)) = value {
                                    if self.vocabulary.base != 10 {
                                        let r = conv(*i as u64, self.vocabulary.base);
                                        writeln!(f, "{r:?} ({value})")?;
                                        break;
                                    }
                                }
                                let big = match value {
                                    Value::Numerical(Number::BIGDEC(_)) => "`",
                                    Value::Numerical(Number::BIGINT(_)) => "`",
                                    _                                   => "",
                                };
                                match value {
                                    Value::Textual(_)   => writeln!(f, "\"{value}\"{big}"),
                                    Value::Character(_) => writeln!(f, "'{value}'{big}"),
                                    _                   => writeln!(f, "{value}{big}"),
                                }?;
                                break;
                            },
                            Value::Addressable(id)  => {
                                let boxed_item = self.dictionary.library[*id].get();   
                                writeln!(f, "{id} {{{boxed_item}}}")?;
                                break;
                            },
                            Value::Indexable(b, idx)  => {
                                match *b {
                                    b'd'    => if let Data::VALUE(value) = &self.data[*idx] {
                                        writeln!(f, "{} [{value}]", n!(self.data.len(), *idx) - 1)?;
                                    }
                                    b'v'    => {
                                        let var = &self.vars[*idx];
                                        writeln!(f, "{idx} {{{var}}}")?;
                                    }
                                    _       => unreachable!(),
                                }
                                break;
                            },
                            _                       => {
                                writeln!(f, "{value}")?;
                                break;
                            },
                        }
                    }  // pool
                },
                Data::ITEM(item) => {
                    writeln!(f, "{item}")?;
                },
            }
        }
        Ok(())  // ==> EXIT
    }
}
