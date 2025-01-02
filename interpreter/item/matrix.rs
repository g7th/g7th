// ©2024 Otmar Klenk
use crate::{matrix, str_matrix};
use crate::vpush;

use nalgebra::DMatrix;

use super::cmd::value::Value;
use crate::interpreter::data::DataHolder;
use crate::interpreter::item::{Item, NamedVectorial, NamedStrMatrix};

pub(crate) type FnMatrix = fn(&mut DataHolder, &DMatrix<f64>);
pub(crate) const MATRIX_PUSH:       FnMatrix = |dh: &mut DataHolder, m: &DMatrix<f64>| { vpush!(dh, Value::from(m.clone())) };

pub(crate) type FnStrMatrix = fn(&mut DataHolder, &DMatrix<String>);
pub(crate) const STR_MATRIX_PUSH:   FnStrMatrix = |dh: &mut DataHolder, m: &DMatrix<String>| { vpush!(dh, Value::from(m.clone())) };

pub(crate) const MATRIX_EXPECTED: &str    = "MATRIX EXPECTED";

impl From<NamedVectorial> for Item {
    fn from(m: NamedVectorial) -> Item {
        matrix!(m.name, MATRIX_PUSH, Value::Vectorial(m.kind))
    }
}

impl From<DMatrix<f64>> for Value  {
    fn from(m: DMatrix<f64>) -> Value  {
        Value::Vectorial(m)
    }
}


impl From<NamedStrMatrix> for Item {
    fn from(m: NamedStrMatrix) -> Item {
        str_matrix!(m.name, STR_MATRIX_PUSH, Value::StrMatrix(m.kind))
    }
}

impl From<DMatrix<String>> for Value  {
    fn from(m: DMatrix<String>) -> Value  {
        Value::StrMatrix(m)
    }
}
