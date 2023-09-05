use crate::ast::Node;
use core::{cmp, ops};
use derive_more::{
    Add, AddAssign, Display, Div, DivAssign, From, Mul, MulAssign, Sub,
    SubAssign,
};
use num_complex::Complex;

///
/// A number.
///
// holy wow look at those macros
#[rustfmt::skip]
#[derive(Debug, Clone, Copy, PartialEq)]
#[derive(Add, Sub, Mul, Div, AddAssign, SubAssign, MulAssign, DivAssign)]
#[derive(From)]
#[derive(Display)]
#[mul(forward)]
#[div(forward)]
#[from(f64, Complex<f64>)]
pub struct Number(Complex<f64>);

impl Number {
    #[must_use]
    pub const fn real(f: f64) -> Self {
        Self(Complex::new(f, 0.0))
    }

    #[must_use]
    pub const fn complex(re: f64, im: f64) -> Self {
        Self(Complex::new(re, im))
    }

    // `derive_more::Pow` when
    #[must_use]
    pub fn pow(&self, rhs: &Self) -> Self {
        self.0.powc(rhs.0).into()
    }
}

//
// PartialOrd is only defined for real numbers; as in
// numbers that do not contain an imaginary part.
//
impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        if libm::fabs(self.0.im) >= f64::EPSILON
            || libm::fabs(other.0.im) >= f64::EPSILON
        {
            return None;
        }

        self.0.re.partial_cmp(&other.0.re)
    }
}

impl ops::Neg for Number {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self::from(-self.0)
    }
}

impl Node for Number {
    fn eval(&self) -> Number {
        *self
    }
}
