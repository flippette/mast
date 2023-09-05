//
// Mathematical operations represented in the AST.
//

use super::Node;

macro_rules! binary_op {
    ($vis:vis $name:ident) => {
        #[allow(dead_code)]
        $vis struct $name {
            lhs: ::alloc::boxed::Box<dyn Node>,
            rhs: ::alloc::boxed::Box<dyn Node>,
        }

        impl $name {
            pub fn new(lhs: impl Node + 'static, rhs: impl Node + 'static) -> Self {
                Self {
                    lhs: ::alloc::boxed::Box::new(lhs),
                    rhs: ::alloc::boxed::Box::new(rhs),
                }
            }
        }
    }
}

macro_rules! unary_op {
    ($vis:vis $name:ident) => {
        #[allow(dead_code)]
        $vis struct $name {
            arg: ::alloc::boxed::Box<dyn Node>,
        }

        impl $name {
            pub fn new(arg: impl Node + 'static) -> Self {
                Self { arg: ::alloc::boxed::Box::new(arg) }
            }
        }
    }
}

macro_rules! binary_op_node_impl {
    ($name:ident, $op:expr) => {
        impl crate::ast::Node for $name {
            fn eval(&self) -> crate::value::Number {
                #[allow(clippy::redundant_closure_call)]
                $op(&self.lhs, &self.rhs)
            }
        }
    };
}

macro_rules! unary_op_node_impl {
    ($name:ident, $op:expr) => {
        impl crate::ast::Node for $name {
            fn eval(&self) -> crate::value::Number {
                #[allow(clippy::redundant_closure_call)]
                $op(&self.arg)
            }
        }
    };
}

binary_op!(pub Add);
binary_op!(pub Sub);
binary_op!(pub Mul);
binary_op!(pub Div);
binary_op!(pub Pow);

binary_op_node_impl!(Add, |a: &dyn Node, b: &dyn Node| a.eval() + b.eval());
binary_op_node_impl!(Sub, |a: &dyn Node, b: &dyn Node| a.eval() - b.eval());
binary_op_node_impl!(Mul, |a: &dyn Node, b: &dyn Node| a.eval() * b.eval());
binary_op_node_impl!(Div, |a: &dyn Node, b: &dyn Node| a.eval() / b.eval());
binary_op_node_impl!(Pow, |a: &dyn Node, b: &dyn Node| a.eval().pow(&b.eval()));

unary_op!(pub Neg);
unary_op!(pub Scope);

unary_op_node_impl!(Neg, |arg: &dyn Node| -arg.eval());
unary_op_node_impl!(Scope, Node::eval);
