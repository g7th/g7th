// ©2024 Otmar Klenk
use std::panic::panic_any;

use super::item::branch::WithProgRefMut;
use super::item::cmd::callable::Callable;
use super::item::cmd::value::Value;
use super::leaf::Ctrl;
use crate::interpreter::{LOOP, PROG};

pub trait WithDeque {
    fn push_ctrl_slot(&mut self, ctrl_slot: Ctrl);
    fn peek_rearmost(&self) -> Option<Ctrl>;  // ☕
//    fn peek_last_ctrl(&self) -> Option<Ctrl>;  // ☕ peek_ctrl
    fn pop_ctrl_slot(&mut self) -> Option<Ctrl>;  // ☕
    fn get_ctrl_slot_vec(&self) -> &Vec<Ctrl>;  // ctrl vec
    fn patch_loop(&self);
}

impl WithDeque for Vec<Ctrl> {  // ctrls Vec<Ctrl>
    #[inline(always)]
    fn push_ctrl_slot(&mut self, ctrl_slot: Ctrl) {  // push_ctrl!
        self.push(ctrl_slot);
    }
    #[inline(always)]
    fn peek_rearmost(&self) -> Option<Ctrl> {  // ☕
        self.last().cloned()
    }
//    #[inline(always)]
//    fn peek_last_ctrl(&self) -> Option<Ctrl> {  // ☕ peek_ctrl!
//        self.iter().rev().find(|&(s, _slot_id)| ! s.is_empty()).cloned()
//    }
    #[inline(always)]
    fn pop_ctrl_slot(&mut self) -> Option<Ctrl> {  // ☕
        self.pop()
    }
    #[inline(always)]
    fn get_ctrl_slot_vec(&self) -> &Vec<Ctrl> {
        self
    }
    fn patch_loop(&self) {
        let Some((_s, do_id)) = self.peek_rearmost() else { panic_any("SYNTAX") };
        PROG.with(|prg| {
            let mut p = prg.borrow_mut();
            debug_assert!(p.is_addr_arr_at(do_id));
            let old_call: Box<dyn Callable> = p.get_call_at(do_id).unwrap();
            let value: &Value = old_call.get_value();
            let b_c_mut: &mut Box<dyn Callable> = p.get_mut(do_id).unwrap().get_mut();
            let new_call: &dyn Callable = &*LOOP.with(|lop| { lop.borrow().clone_callable() });
            *b_c_mut = new_call.clone_callable();
            b_c_mut.set_value(&value);
        });
    }
}
