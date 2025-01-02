// ©2024 Otmar Klenk
use crate::get_fence_id;
use crate::{pop_bool, pop_i64};
use crate::reg_exit;

use indextree::{Node, NodeId};
use std::ops::ControlFlow;
use std::panic::panic_any;

use super::{BREAK, BYE, EXIT, PROG};
use super::Prog;
use super::data::{Data, DataHolder};
use super::dictionary::Dictionary;
use super::item::branch::GetCall;
use super::item::cmd::Cmd;
use super::item::cmd::callable::Callable;
use super::item::cmd::value::{GetValue, Value};
use super::item::word::PackageCode;

#[macro_export]
macro_rules! caller {
    ($dh: expr, $call: expr) => {{
        $call.call($dh, &$call.get_value());  // ඞ ➆ <<<<<<<<<<<<<<<<<<<<<<<
    }}
}


pub trait Execute {  // -> DataHolder
    fn doing(&mut self, varr: &Value);
    fn forking(&mut self, varr: &Value);
    fn case_found(&mut self, p: &Prog, a: &[NodeId]) -> bool;
    fn distinguishing(&mut self, varr: &Value);
    fn repeating(&mut self, varr: &Value);
    fn looping(&mut self, varr: &Value, step: fn(&mut DataHolder) -> i64);
    fn iterating(&mut self, varr: &Value);
    fn execute_id(&mut self, at_id: NodeId);
    fn execute_vhook(&mut self, vhook: &Value);
    fn exec(&mut self, p: &Prog, a: &[NodeId]) -> bool;
}

impl Execute for DataHolder<'_> {
    #[inline]
    fn doing(&mut self, varr: &Value) {  // AddressArray
        let arr = <Vec<Value>>::from(varr);  // does>
        if arr.is_empty() { return; }
        let fence_id = get_fence_id!(self);
        if let Some(last_id) = latest_lib_id!(self) {
            if last_id != fence_id {
                let code: &mut dyn Callable = self.dictionary.library[last_id].get_mut().code_mut();
                if let Some(PackageCode { value, .. }) = code.as_any_mut().downcast_mut::<PackageCode>() {
                    let a3 = value.package_mut();
                    if let Value::Packable(does) = &arr[0] {
                        a3.extend_from_slice(does);
                        return;
                    }
                }
            }
        }
        eprintln!("does>: ERROR");
    }

    #[inline]
    fn forking(&mut self, varr: &Value) {  // AddressArray
        PROG.with(|prg| {
            let vh_rslt = pop_bool!(self);
            let arr = <Vec<Value>>::from(varr);
            if arr.is_empty() { return; }
            let p = prg.borrow();
            let Ok(ok) = vh_rslt else { panic!("{}", vh_rslt.err().unwrap()) };
            if ok {  // if
                self.exec(&p, arr[0].package().as_slice());
            }
            else if arr.len() > 1 {  // else
                self.exec(&p, arr[1].package().as_slice());
            }
        });
    }

    #[inline(always)]
    fn case_found(&mut self, p: &Prog, a: &[NodeId]) -> bool {  // disc! ?
        self.exec(p, a);
        let case_data = self.case_arr.last().unwrap().clone();
        match self.pop() {
            Ok(Data::VALUE(value))  => match case_data {
                Data::VALUE(case)       => value == case,
                Data::ITEM(_)           => false,
            }
            Ok(Data::ITEM(item))    => match case_data {
                Data::ITEM(case)        => item == case,
                Data::VALUE(_)          => false,
            }
            Err(err)                => panic_any(err),
        }
    }

    #[inline]
    fn distinguishing(&mut self, varr: &Value) {
        PROG.with(|prg| {
            let p = prg.borrow();
            let arr: Vec<Value> = <Vec<Value>>::from(varr);
            (0..arr.len()).step_by(2).try_for_each(|n| {
                if self.case_found(&p, arr[n].package().as_slice()) {
                    if let Some(id) = arr.get(n + 1) {
                        self.exec(&p, id.package().as_slice());
                    }
                    return ControlFlow::Break(());
                }
                ControlFlow::Continue(())
            });
        });
    }

    #[inline]
    fn repeating(&mut self, varr: &Value) {  // AddressArray
        PROG.with(|prg| {
            let arr = <Vec<Value>>::from(varr);
            if arr.is_empty() { return; }
            let p = prg.borrow();
            while {
                if self.exec(&p, arr[0].package().as_slice()) {
                    false
                }
                else {
                    let va_rslt = pop_bool!(self);
                    let Ok(ok) = va_rslt else { panic!("{}", va_rslt.err().unwrap()) };
                    ok
                }
            } {
                let reason = self.bye(BREAK|BYE|EXIT);
                if reason != 0 {
                    break;
                }
                self.exec(&p, arr[1].package().as_slice());
            }
        });
    }

    #[inline]
    fn looping(&mut self, varr: &Value, step: fn(&mut DataHolder) -> i64) {  // AddressArray
        let arr = <Vec<Value>>::from(varr);
        if arr.is_empty() { return; }
        let from = pop_i64!(self);
        let to = pop_i64!(self);
        self.loop_arr.push([from, to]);
        PROG.with(|prg| {
            let p = prg.borrow();
            loop {
                let reason = self.bye(BREAK|BYE|EXIT);
                if reason != 0 {
                    break;
                }
                if self.exec(&p, arr.last().unwrap().package().as_slice()) {
                    break;
                }
                let step = (step)(self);
                let Some(i2): Option<&mut [i64;2]> = self.loop_arr.last_mut() else { panic_any("LOOPING"); };
                i2[0] += step;
                let Some(i2): Option<&[i64;2]> = self.loop_arr.last() else { panic_any("LOOPING"); };
                if i2[0] >= i2[1] {
                    break;
                }
            }
        });
        self.loop_arr.pop();
    }

    #[inline]
    fn iterating(&mut self, varr: &Value) {  // AddressArray
        let opt_data = self.data.pop();
        if let Some(Data::VALUE(value)) = opt_data {
            let mut it = value.into_iter();
            PROG.with(|prg| {
                let arr = <Vec<Value>>::from(varr);
                if arr.is_empty() { return; }
                let p = prg.borrow();
                let v: Vec<NodeId> = arr[0].package();
                let pkg: &[NodeId] = v.as_slice();
                while let Some(e) = it.next() {
                    let reason = self.bye(BREAK|BYE|EXIT);
                    if reason != 0 {
                        break;
                    }
                    self.push_data(Data::VALUE(e));
                    self.exec(&p, pkg);
                }
            });
        }
    }

    fn execute_id(&mut self, at_id: NodeId) {
        PROG.with(|prg| {
            debug_assert_ne!(at_id, self.free_id);
            let node: &Node<Box<dyn Callable>> = &prg.borrow()[at_id];
            let b_c: &Box<dyn Callable> = node.get();
            caller!(self, b_c);
        });
    }

    #[inline(always)]
    fn execute_vhook(&mut self, vhook: &Value) {
        let Some(at_id) = vhook.address() else { return };
        self.execute_id(at_id)
    }

    #[inline(always)]  // get_call_at()
    fn exec(&mut self, p: &Prog, a: &[NodeId]) -> bool {
        a.iter().try_for_each(|at_id: &NodeId| {
            let reason = self.bye(BREAK|BYE|EXIT);
            if reason != 0 {
                return ControlFlow::Break(());
            }
            if let Some(call) = p.get_call(*at_id) {
                caller!(self, call);
            };
            ControlFlow::Continue(())
        }).is_break()
    }
}
