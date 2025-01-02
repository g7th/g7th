// ©2024 Otmar Klenk
use indextree::NodeId;

use super::PROG;
use super::item::branch::{CtrlType, WithProgRefMut};
use super::vocabulary::Vocabulary;

pub type Ctrl               = (String, NodeId);

/////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////
#[derive(Clone, Debug)]
pub struct Leaf {
    pub(crate) root_id:     Option<NodeId>,  // prog compile
    rstack:                 Vec<(CtrlType, NodeId)>,
    pub(crate) clean_list:  Vec<NodeId>,
    pub(crate) ctrls:       Vec<Ctrl>,  // (name, slot); ctrl vec
}

impl Leaf {
    #[inline]
    pub(crate) fn new() -> Self {
        Self {
            root_id:        None,
            rstack:         vec![],
            clean_list:     vec![],
            ctrls:          vec![],  // slot of Ctrl in Vec
        }
    }

    #[inline(always)]
    pub(crate) fn rstack_push(&mut self, ctrl_type: CtrlType, id: NodeId) {
        Vec::push(&mut self.rstack, (ctrl_type, id));
    }

    #[inline(always)]
    pub(crate) fn rstack_peek(&self) -> Option<&(CtrlType, NodeId)> {
        self.rstack.last()
    }

//    #[inline(always)]
//    pub(crate) fn rstack_entry(&self) -> Option<&(CtrlType, NodeId)> {
//        self.rstack.get(0)
//    }

    #[inline]
    pub(crate) fn finished(&mut self, vocab: &Vocabulary) -> Result<(), String> {
        let mut it = self.ctrls.drain(..);
        match it.next() {
            Some((ctrl_name, _)) if ! ctrl_name.is_empty()  => {
                let mut err = ctrl_name;
                if let Some((_ctrl, end)) = vocab.syntax.get(&err) {
                    err += &format!(" - {} expected", end);
                }
                Err(format!("UNCLOSED CTRL {err}"))
            },
            _                                               => Ok(()),
        }
    }

    #[inline]
    fn clean_prg(&mut self) {  // siblings
        PROG.with(|prg| {
            while ! self.clean_list.is_empty() {
                let at_id = self.clean_list.pop().unwrap();
                prg.borrow_mut().init_addr_arr_at(at_id);  // init
                at_id.remove(&mut prg.borrow_mut());
            }
        });
    }

    #[inline(always)]
    pub(crate) fn clear_all(&mut self) {
        self.clean_prg();
        self.rstack.clear();
        self.ctrls.clear();
    }
}
