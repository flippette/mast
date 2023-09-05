#![no_std]
#![forbid(unsafe_code)]

extern crate alloc;

pub mod ast;
pub mod lex;
pub mod value;

pub mod prelude {
    pub use crate::ast::Node;
    pub use crate::lex::{Lexer, Literal, Token};
    pub use crate::value::Number;
}
