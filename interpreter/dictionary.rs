// ©2024 Otmar Klenk
#![macro_use]

use crate::v;

use indextree::{Arena, NodeId};

use super::data::DataHolder;
use super::item::Item;
use super::item::cmd::Cmd;
use super::item::word::FnAddr;

const VOCAB_SET: FnAddr  = |dh: &mut DataHolder, id_opt: Option<NodeId>| {
    dh.dictionary.context = id_opt.expect("VOCAB_SET");
};

#[macro_export]
macro_rules! latest_lib_id {  // Option<NodeId>
    ($dh: expr) => {{
        let dict: &mut Dictionary = &mut $dh.dictionary;
        dict.current.descendants(&dict.library).last()
    }}
}

/////////////////////////////////////////////////////////////////////////////////////////

pub(crate) trait WithLibrary {
    fn create_vocab(&mut self, name: &str) -> NodeId;
}

impl WithLibrary for Arena<Item> {
    fn create_vocab(&mut self, name: &str) -> NodeId {
        let addr_code = crate::interpreter::item::word::AddrCode::new(VOCAB_SET, v!(Value::Unassigned));
        let vocab = crate::interpreter::item::Item::VOCABULARY { cmd: (name.to_string(), Box::new(addr_code)) };
        let id = self.new_node(vocab);  //          ^^^^^^^^^^
        let code = self[id].get_mut().code_mut();
        code.set_value(&v!(id));
        id
    }
}

/////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug)]
pub struct Dictionary {
    pub(crate) library:     Arena<Item>,
    pub(crate) context:     NodeId,  // is where words are searched for execution
    pub(crate) current:     NodeId,  // is where words are compiled to
}

impl Dictionary {
    pub(crate) fn new() -> Self {
        let mut library = Arena::new();
        let first_id = library.create_vocab("7th");
        let rslt = Self {
            library,
            context:        first_id,   // dictionary / is where words are searched for execution
            current:        first_id,   // dictionary / is where words are compiled to
        };
        rslt
    }

    pub(crate) fn ins_current(&mut self, word: Item) -> NodeId {  // dir - sibn, sibn-1... sib2, sib1
        let new_node_id = self.library.new_node(word);
        self.current.append(new_node_id, &mut self.library);  // descendants
        new_node_id
    }
}
