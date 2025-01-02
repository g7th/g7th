// ©2024 Otmar Klenk
use crate::interpreter::Interpreter;

mod interpreter;

fn main() {
    Interpreter::new().run();
}
