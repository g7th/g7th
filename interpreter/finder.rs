// ©2024 Otmar Klenk
use indextree::{Node, NodeId};

use super::Call;
use super::data::NOT_FOUND;
use super::data::DataHolder;
use super::dictionary::Dictionary;
use super::item::Item;
use super::item::cmd::Cmd;
use super::vocabulary::Vocabulary;

pub trait IdFinder {
    fn find_secondary_id(&self, word_name: &str) -> Option<NodeId>;
    fn find_word_id(dh: &DataHolder, word_name: &str) -> Result<NodeId, String>;
}

pub trait CodeFinder {
    fn find_item<'a>(dh: &'a DataHolder, word_name: &'a str) -> Option<Item>;
    fn find_core_item<'a>(vocab: &'a Vocabulary, word_name: &'a str) -> Option<&'a Item>;
    fn find_core_code(vocab: &Vocabulary, word_name: &str) -> Option<Call>;
}

impl IdFinder for Dictionary {
    fn find_secondary_id(&self, word_name: &str) -> Option<NodeId> {
        if word_name.is_empty() { return None; };
        let arena = &self.library;
        let mut descs = self.context.reverse_children(arena).inspect(|id| debug_assert!(! id.is_removed(arena)));  // find;
        while let Some(id) = descs.next() {
            if let Some(node) = arena.get(id) {
                if node_item(node, word_name).is_some() {
                    return Some(id);
                }
            };
        }
        for node in arena.iter().collect::<Vec<_>>().iter().rev() {  // hidden words (not in current directory path) finds the last first
            if (! node.is_removed()) && node_item(node, word_name).is_some() {
                return arena.get_node_id(node);
            }
        }  // sdrow neddih

        None  // don't search in core map
    }

    #[inline]
    fn find_word_id(dh: &DataHolder, word_name: &str) -> Result<NodeId, String> {
        match dh.dictionary.find_secondary_id(word_name) {
            Some(id)    => Ok(id),
            None        => {
                match dh.vocabulary.core.get(word_name) {
                    Some(_item) => Err(format!("IN CORE: {}", word_name)),
                    None        => Err(format!("{NOT_FOUND}: {}", word_name)),
                }
            }
        }
    }
}

impl CodeFinder for Vocabulary {
    fn find_item<'a>(dh: &'a DataHolder, word_name: &'a str) -> Option<Item> {
        if word_name.is_empty() { return None; };
        let arena = &dh.dictionary.library;
        let descs = &mut dh.dictionary.context.reverse_children(arena).inspect(|id| debug_assert!(! id.is_removed(arena)));  // find;
        while let Some(id) = descs.next() {
            if let Some(node) = arena.get(id) {
                if let Some(item) = node_item(node, word_name) {
                    return Some(item).cloned();
                }
            }
        }
        dh.vocabulary.core.get(word_name).cloned()
    }

    #[inline]
    fn find_core_item<'a>(vocab: &'a Vocabulary, word_name: &'a str) -> Option<&'a Item> {
        if word_name.is_empty() { return None; };
        vocab.core.get(word_name)
    }

    fn find_core_code(vocab: &Vocabulary, word_name: &str) -> Option<Call> {
        let item = Vocabulary::find_core_item(vocab, word_name)?;
        Some(item.code().clone_callable())
    }
}


#[inline]
pub(crate) fn node_item<'a>(node: &'a Node<Item>, word_name: &str) -> Option<&'a Item> {
    let item = node.get();
    match item {
        Item::WORD       { ctrl_type: _, cmd: (s, _code) }      => if s.eq(word_name) {return Some(&item)} else {return None},
        Item::BOOL       { cmd: (s, _code) }    => if s.eq(word_name) {return Some(&item)} else {return None},
        Item::NUMBER     { cmd: (s, _code) }    => if s.eq(word_name) {return Some(&item)} else {return None},
        Item::MATRIX     { cmd: (s, _code) }    => if s.eq(word_name) {return Some(&item)} else {return None},
        Item::CHAR       { cmd: (s, _code) }    => if s.eq(word_name) {return Some(&item)} else {return None},
        Item::STRING     { cmd: (s, _code) }    => if s.eq(word_name) {return Some(&item)} else {return None},
        Item::VOCABULARY { cmd: (s, _code) }    => if s.eq(word_name) {return Some(&item)} else {return None},        
        Item::ADDR       { cmd: (s, _code) }    => if s.eq(word_name) {return Some(&item)} else {return None},
        Item::ADDR_ARR   { ctrl_type: _, cmd: (s, _code) }    => if s.eq(word_name) {return Some(&item)} else {return None},
        Item::PACKAGE    { ctrl_type: _, cmd: (s, _code) }    => if s.eq(word_name) {return Some(&item)} else {return None},
        Item::INDEX      { cmd: (s, _code) }    => if s.eq(word_name) {return Some(&item)} else {return None},
        Item::STR_MATRIX { cmd: (s, _code) }    => if s.eq(word_name) {return Some(&item)} else {return None},
        Item::BUF_READER { cmd: (s, _code) }    => if s.eq(word_name) {return Some(&item)} else {return None},
        Item::BUF_WRITER { cmd: (s, _code) }    => if s.eq(word_name) {return Some(&item)} else {return None},
    }
}
