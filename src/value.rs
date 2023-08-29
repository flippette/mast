///
/// A number.
///
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Number {
    Real(f64),
    Complex(Complex),
}

///
/// A complex number.
///
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Complex {
    pub re: f64,
    pub im: f64,
}

impl Complex {
    #[must_use]
    pub const fn new(re: f64, im: f64) -> Self {
        Self { re, im }
    }
}
