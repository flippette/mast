use nom::bytes::complete::take_till1;

use crate::value::{Complex, Number};

pub type Result<'src, O> = nom::IResult<&'src str, O>;

///
/// A syntactic mathematical token.
///
#[derive(Debug, Clone, PartialEq)]
pub enum Token<'src> {
    Literal(Literal),

    Add,
    Sub,
    Mul,
    Div,
    Pow,

    LParen,
    RParen,

    Ident(&'src str),

    Unknown(&'src str),
}

///
/// A literal inside an expression.
///
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Literal {
    Real(f64),
    Constant(char),
}

#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    src: &'src str,
}

impl<'src> Token<'src> {
    ///
    /// Parse a [`Token`] from an input slice, assuming no preceding whitespace.
    ///
    #[allow(clippy::missing_errors_doc)]
    pub fn parse(src: &'src str) -> Result<Self> {
        use nom::{
            branch::alt,
            bytes::complete::{tag, take_while1},
            combinator::map,
        };

        fn sign(src: &str) -> Result<Token> {
            alt((
                map(tag("+"), |_| Token::Add),
                map(tag("-"), |_| Token::Sub),
                map(tag("*"), |_| Token::Mul),
                map(tag("/"), |_| Token::Div),
                map(tag("^"), |_| Token::Pow),
            ))(src)
        }

        fn paren(src: &str) -> Result<Token> {
            alt((
                map(tag("("), |_| Token::LParen),
                map(tag(")"), |_| Token::RParen),
            ))(src)
        }

        alt((
            map(Literal::parse, Token::Literal),
            sign,
            paren,
            map(take_while1(|c: char| c.is_ascii_alphabetic()), Token::Ident),
            map(
                take_till1(|c: char| c.is_ascii_whitespace()),
                Token::Unknown,
            ),
        ))(src)
    }
}

impl Literal {
    ///
    /// Mathematical constants.
    ///
    /// Supported constants are always 1 character long.
    ///
    pub const CONSTANTS: &'static [(char, Number)] = &[
        ('π', Number::Real(core::f64::consts::PI)),
        ('e', Number::Real(core::f64::consts::E)),
        ('i', Number::Complex(Complex::new(0.0, 1.0))),
    ];

    ///
    /// Parse a [`Literal`] from an input slice, assuming no preceding whitespace.
    ///
    #[allow(clippy::missing_errors_doc)]
    pub fn parse(src: &str) -> Result<Self> {
        use nom::{bytes::complete::take, combinator::map};

        if let Some((name, _)) = Self::CONSTANTS
            .iter()
            .find(|(name, _)| src.starts_with(*name))
        {
            // return a constant if we know that constant
            Ok((take(1_usize)(src)?.0, Literal::Constant(*name)))
        } else {
            // return a real
            map(util::real, Literal::Real)(src)
        }
    }
}

impl<'src> Lexer<'src> {
    #[must_use]
    pub fn new(src: &'src str) -> Self {
        Self { src }
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Token<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        let (src, _) =
            nom::bytes::complete::take_while::<_, _, nom::error::Error<_>>(
                char::is_whitespace,
            )(self.src)
            .ok()?;
        if self.src.is_empty() {
            return None;
        }
        let (src, tok) = Token::parse(src).ok()?;
        self.src = src;
        Some(tok)
    }
}

mod util {
    use super::Result;
    use core::num::IntErrorKind;

    ///
    /// Parse a real number from an input slice, assuming no preceding whitespace.
    ///
    /// Note that both the integer and fractional parts of the number
    /// must fit inside a [`u32`] value.
    ///
    pub fn real(src: &str) -> Result<f64> {
        use nom::{
            branch::alt,
            bytes::complete::{tag, take_while, take_while1},
            combinator::{map, opt},
            error::{make_error, ErrorKind},
            sequence::preceded,
            Err,
        };

        let (src, sign) = map(opt(alt((tag("+"), tag("-")))), |op| match op {
            Some("+") | None => Some(1.0),
            Some("-") => Some(-1.0),
            _ => None,
        })(src)?;
        let sign =
            sign.ok_or_else(|| Err::Error(make_error(src, ErrorKind::Tag)))?;

        let (src_int_part, int_part) =
            opt(take_while1(|c: char| c.is_ascii_digit()))(src)?;
        let src = src_int_part;

        let mut fract_part_parser =
            preceded(tag("."), take_while(|c: char| c.is_ascii_digit()));

        let (src, int_part, fract_part, fract_len) = {
            let (src, fract_part) = if int_part.is_some() {
                map(opt(fract_part_parser), |op| op.unwrap_or("0"))(src)?
            } else {
                fract_part_parser(src)?
            };

            (
                src,
                match int_part {
                    Some(s) => match s.parse::<u32>() {
                        Ok(i) => i,
                        Err(e) if *e.kind() == IntErrorKind::PosOverflow => {
                            Err(Err::Error(make_error(
                                src_int_part,
                                ErrorKind::Float,
                            )))?
                        }
                        _ => 0,
                    },
                    _ => 0,
                },
                fract_part
                    .parse::<u32>()
                    .map_err(|_| Err::Error(make_error(src, ErrorKind::Tag)))?,
                u32::try_from(fract_part.len()).expect("fract part too big!"),
            )
        };

        Ok((
            src,
            libm::copysign(
                f64::from(int_part)
                    + f64::from(fract_part)
                        / libm::pow(10.0, f64::from(fract_len)),
                sign,
            ),
        ))
    }
}

#[cfg(test)]
mod test {
    use super::{Lexer, Literal, Token};

    ///
    /// Declares a test case with a name, given input and expected output.
    ///
    macro_rules! test_case {
        ($name:ident, $input:expr, $expected:expr $(,)?) => {
            #[test]
            fn $name() {
                // this is _way_ faster than just passing $expected for some reason
                let expected = $expected;

                let mut tokens = 0;
                for (i, token) in Lexer::new($input).enumerate() {
                    assert_eq!(token, expected[i]);
                    tokens += 1;
                }

                assert_eq!(tokens, expected.len());
            }
        };
    }

    ///
    /// Concatenates literals, separated by a space.
    ///
    macro_rules! space_separated {
        ($($term:literal $(,)?)+) => {
            concat!($($term, ' ',)+)
        }
    }

    test_case!(
        general,
        "(e + 2i) * 3x^-4.5π - 5 / 6.7",
        [
            Token::LParen,
            Token::Literal(Literal::Constant('e')),
            Token::Add,
            Token::Literal(Literal::Real(2.0)),
            Token::Literal(Literal::Constant('i')),
            Token::RParen,
            Token::Mul,
            Token::Literal(Literal::Real(3.0)),
            Token::Ident("x"),
            Token::Pow,
            Token::Literal(Literal::Real(-4.5)),
            Token::Literal(Literal::Constant('π')),
            Token::Sub,
            Token::Literal(Literal::Real(5.0)),
            Token::Div,
            Token::Literal(Literal::Real(6.7)),
        ],
    );

    test_case!(
        large_real,
        space_separated!(
            4294967295.4294967295,
            4294967296.4294967296,
            -4294967295.4294967295,
            -4294967296.4294967296,
        ),
        [
            Token::Literal(Literal::Real(4294967295.4294967295)),
            Token::Unknown("4294967296.4294967296"),
            Token::Literal(Literal::Real(-4294967295.4294967295)),
            // this is expected behavior, "4294967296" does not fit in a [`u32`].
            Token::Sub,
            Token::Unknown("4294967296.4294967296"),
        ],
    );
}
