use crate::value::Number;
use alloc::boxed::Box;

pub mod expr;

///
/// An evaluatable AST node.
///
/// Functions that take variables (`Token::Ident`) are currently unsupported.
///
pub trait Node {
    fn eval(&self) -> Number;
}

impl<T: Node + ?Sized> Node for Box<T> {
    fn eval(&self) -> Number {
        (**self).eval()
    }
}

#[cfg(test)]
mod test {
    use super::{expr::*, Node};
    use crate::value::Number;
    use alloc::boxed::Box;

    #[test]
    fn expr() {
        let expr = Scope::new(Add::new(
            Number::real(1.0),
            Mul::new(Number::complex(2.0, 1.0), Number::complex(2.0, 1.3)),
        ));

        assert_eq!(expr.eval(), Number::complex(3.7, 4.6));
    }
}
