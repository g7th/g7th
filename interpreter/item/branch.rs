// ©2024 Otmar Klenk
use indextree::{Arena, Node, NodeId};
use std::cell::{RefCell, RefMut};
use std::fmt;
use std::panic::panic_any;

use super::cmd::callable::Callable;
use super::cmd::value::GetValue;
use crate::interpreter::{EMPTY_NAME, PROG};
use crate::interpreter::{Call, Value};
use crate::interpreter::data::{DataHolder, Modes};
use crate::interpreter::item::Item;
use crate::interpreter::item::cmd::callable::Immediate;
use crate::interpreter::item::cmd::value::{ADDRESS_EXPECTED};
use crate::interpreter::item::word::FN_PACKAGE;
use crate::interpreter::item::word::PackageCode;
use crate::interpreter::leaf::Ctrl;
use crate::interpreter::tools::Dots;
use crate::interpreter::with_deque::WithDeque;

const APPEND_TO_A3: &str            = "APPEND TO A3";

/////////////////////////////////////////////////////////////////////////////////////////
pub trait WithProgRefMut {
    fn get_call_at(&self, slot_id: NodeId) -> Option<Box<dyn Callable>>;
    fn get_value_at(&self, slot_id: NodeId) -> Option<Value>;
    fn get_value_at_mut(&mut self, slot_id: NodeId) -> Option<&mut Value>;
    fn set_value_at(&mut self, slot_id: NodeId, value: Value);
    fn init_addr_arr_at(&mut self, slot_id: NodeId);  // a
    fn is_addr_arr_at(&self, slot_id: NodeId) -> bool;
    fn is_package_at(&self, slot_id: NodeId) -> bool;
    fn set_package_immediate(&mut self, pkg: &mut PackageCode);
}

impl WithProgRefMut for RefMut<'_, Arena<Box<(dyn Callable + 'static)>>> {
    #[inline(always)]
    fn get_call_at(&self, slot_id: NodeId) -> Option<Box<dyn Callable>> {
        Some(self.get_call(slot_id)?.clone_callable())
    }
    #[inline(always)]
    fn get_value_at(&self, slot_id: NodeId) -> Option<Value> {
        Some(self.get_call_at(slot_id)?.get_value().clone())
    }
    #[inline(always)]
    fn get_value_at_mut(&mut self, slot_id: NodeId) -> Option<&mut Value> {
        Some(self.get_mut(slot_id)?.get_mut().as_mut().get_value_mut())
    }

    fn set_value_at(&mut self, slot_id: NodeId, value: Value) {
        match self.get_value_at_mut(slot_id) {
            Some(v) => *v = value,
            None    => {
                let call: &mut dyn Callable = self.get_mut(slot_id).expect("SET VALUE AT").get_mut().as_mut();
                call.set_value(&value);
            },
        }
    }

    fn init_addr_arr_at(&mut self, slot_id: NodeId) {  // a
        self.set_value_at(slot_id, Value::AddressArray(vec![]));
    }

    fn is_addr_arr_at(&self, slot_id: NodeId) -> bool {
        let Some(Value::AddressArray(_a3)) = self.get_value_at(slot_id) else { return false; };
        true
    }
    #[inline(always)]
    fn is_package_at(&self, slot_id: NodeId) -> bool {
        let Some(Value::Packable(_a3)) = self.get_value_at(slot_id) else { return false; };
        true
    }

    fn set_package_immediate(&mut self, pkg: &mut PackageCode) {
        let PackageCode {value, ..} = pkg;
        let a3 = value.package_mut();
        if let Some(id0) = a3.get(0) {
            self.get_call_at_mut(*id0).expect("ENTRY").set_immediate();
        }
    }
}

/////////////////////////////////////////////////////////////////////////////////////////
#[inline]
pub(crate) fn make_new_node(dh: &mut DataHolder, ctrl_type: CtrlType, name: &str, call: &dyn Callable) -> NodeId {
    PROG.with(|prg: &RefCell<Arena<Box<dyn Callable>>>| {
        let new_id = prg.borrow_mut().new_node(call.clone_callable());
        if dh.leaf.root_id.is_none() {
            dh.leaf.root_id = Some(new_id);
        }
        if dh.in_clean_prg_mode() {
            dh.leaf.clean_list.push(new_id);
        }
        dh.set_curr_id(ctrl_type, name, new_id);
        new_id
    })
}
#[inline]
pub(crate) fn append_to_a3(dh: &mut DataHolder, new_ctrl_type: CtrlType, new_name: &str, call: &dyn Callable) {
    PROG.with(|prg| {
//\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        if dh.leaf.ctrls.is_empty() {  // loose id for interpreting
            let loose_id = make_new_node(dh, CtrlType::Other, EMPTY_NAME, &PackageCode {
                fn_package: FN_PACKAGE,
                value: Value::Packable(vec![]),
            });
            dh.leaf.ctrls.push_ctrl_slot((EMPTY_NAME.to_string(), loose_id));  // ☕ ctrls.push()
        }
/////////////////////////////////////////////////////////////////////////////////////////
        let Some((s, at_id)): Option<Ctrl> = dh.leaf.ctrls.peek_rearmost() else { unreachable!(); };
        debug_assert!(dh.leaf.root_id.is_some());
        debug_assert_ne!(at_id, dh.free_id);
//---------------------------------------------------------------------------------------
        match new_ctrl_type {
            CtrlType::Other     => {
                let ctrl_type = match s.as_str() {
                    "case"  => {
                        match dh.leaf.rstack_peek() {
                            Some(&(_ctrl_type, id)) if at_id == id  => CtrlType::Cont,
                            _                                       => new_ctrl_type,
                        }
                    },
                    _       => new_ctrl_type,
                };
                let new_id = make_new_node(dh, ctrl_type, new_name, call);
                let mut p = prg.borrow_mut();
                if p.is_package_at(at_id) {
                    let value: &mut Value = p.get_value_at_mut(at_id).expect(APPEND_TO_A3);
                    let pkg: &mut Vec<NodeId> = value.package_mut();
                    pkg.push(new_id);
                }
                else {
                    let value: &mut Value = p.get_value_at_mut(at_id).expect(APPEND_TO_A3);
                    let a3: &mut Vec<Value> = value.addr_arr_mut();
                    match a3.last_mut() {
                        Some(value) => value.package_mut().push(new_id),
                        None        => a3.push(Value::Packable(vec![new_id])),
                    }
                }
                debug_assert!(! p.is_addr_arr_at(new_id));
            },
            CtrlType::Cont      => {
                let new_id = make_new_node(dh, new_ctrl_type, new_name, call);
                let mut p = prg.borrow_mut();
                let value: &mut Value = p.get_value_at_mut(at_id).expect(APPEND_TO_A3);
                let a3: &mut Vec<Value> = value.addr_arr_mut();
                let value: &mut Value = a3.last_mut().expect(APPEND_TO_A3);
                value.package_mut().push(new_id);
                debug_assert!(p.is_package_at(new_id));
            },
            CtrlType::Intro     => {
                if { let p = prg.borrow_mut(); p.is_package_at(at_id) } {
                    let new_id = make_new_node(dh, new_ctrl_type, new_name, call);
                    let mut p = prg.borrow_mut();
                    let value: &mut Value = p.get_value_at_mut(at_id).expect(APPEND_TO_A3);
                    let a: &mut Vec<NodeId> = value.package_mut();
                    a.push(new_id);
                    debug_assert!(p.is_addr_arr_at(new_id));
//                  println!("*{{{new_name} {new_id}}} {:#?}", p.get_value_at(new_id));
                }
                else {
// entering next level ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    let next_id = {
                        let nxt_id = make_new_node(dh, CtrlType::Other, EMPTY_NAME, &PackageCode {
                            fn_package: FN_PACKAGE,
                            value: Value::Packable(vec![]),
                        });
                        let mut p = prg.borrow_mut();
                        let value: &mut Value = p.get_value_at_mut(at_id).expect(APPEND_TO_A3);
                        let a3: &mut Vec<Value> = value.addr_arr_mut();
                        match a3.last_mut() {
                            Some(value) => value.package_mut().push(nxt_id),
                            None        => a3.push(Value::Packable(vec![nxt_id])),
                        }
                        nxt_id
                    };
                    dh.leaf.ctrls.push_ctrl_slot((EMPTY_NAME.to_string(), next_id));  // ☕ ctrls.push()
                    append_to_a3(dh, new_ctrl_type, new_name, call);
// level txen gniretne ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                }
            },
            CtrlType::End       => {  // compile_ctrl_end() <- pop_ctrl_slot()
                unreachable!();
            },
        }
    });
}
#[inline]
pub(crate) fn free_nodes(id_opt: &mut Option<NodeId>) {
    if let Some(id) = id_opt {
        PROG.with(|prg| {
            id.remove_subtree(&mut prg.borrow_mut());
        });
        *id_opt = None;
    }
}

/////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////
pub(crate) trait GetCall {
    fn get_call(&self, id: NodeId) -> Option<&Call>;
    fn get_call_at_mut(&mut self, id: NodeId) -> Option<&mut Call>;
}

impl GetCall for Arena<Call> {
    #[inline(always)]
    fn get_call(&self, id: NodeId) -> Option<&Call> {  // GetCall
        let Some(node): Option<&Node<Box<dyn Callable>>> = self.get(id) else { panic_any("GET CALL {id}"); };
        Some(node.get())
    }
    #[inline(always)]
    fn get_call_at_mut(&mut self, id: NodeId) -> Option<&mut Call> {  // GetCall
        Some(self.get_mut(id)?.get_mut())
    }
}

/////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CtrlType {
    Other,
    Intro,
    Cont,
    End,
}

impl fmt::Display for CtrlType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            CtrlType::Other     => "Other",
            CtrlType::Intro     => "Intro",
            CtrlType::Cont      => "Cont",
            CtrlType::End       => "End",
        })
    }
}

impl From<&Item> for Option<NodeId> {
    fn from(x: &Item) -> Option<NodeId> {
        match x {
            Item::ADDR { cmd }  => (cmd.1).get_value().address(),
            _                   => panic_any(ADDRESS_EXPECTED.dots()),
        }
    }
}
