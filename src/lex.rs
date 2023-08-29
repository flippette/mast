use crate::value::{Complex, Number};

pub type Result<'src, O> = nom::IResult<&'src str, O>;

///
/// A syntactic mathematical token.
///
#[derive(Debug, Clone, PartialEq)]
pub enum Token<'src> {
    Add,
    Sub,
    Mul,
    Div,

    LParen,
    RParen,

    Literal(Literal),
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
            ))(src)
        }

        fn paren(src: &str) -> Result<Token> {
            alt((
                map(tag("("), |_| Token::LParen),
                map(tag(")"), |_| Token::RParen),
            ))(src)
        }

        alt((
            sign,
            paren,
            map(Literal::parse, Token::Literal),
            map(take_while1(|c: char| c.is_ascii_alphabetic()), Token::Ident),
            |i| Ok(("", Token::Unknown(i))), // unknown consumes the entire island
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

    pub fn real(src: &str) -> Result<f64> {
        use nom::{
            branch::alt,
            bytes::complete::{tag, take_while},
            combinator::{map, opt},
            error::{make_error, ErrorKind},
            sequence::tuple,
            Err,
        };

        let (src, sign) = map(opt(alt((tag("+"), tag("-")))), |op| match op {
            Some("+") => Some(1.0),
            Some("-") => Some(-1.0),
            _ => None,
        })(src)?;
        let sign = sign.unwrap_or(1.0);

        let (src, int_part) =
            opt(take_while(|c: char| c.is_ascii_digit()))(src)?;

        let fract_len: u32;
        let err = |src| move |_| Err::Error(make_error(src, ErrorKind::Tag));

        #[allow(clippy::cast_possible_truncation)]
        let (src, int_part, fract_part) = if let Some(s) = int_part {
            let (src, fract_part) = map(
                opt(tuple((
                    tag("."),
                    take_while(|c: char| c.is_ascii_digit()),
                ))),
                |op| op.unwrap_or((".", "0")).1,
            )(src)?;

            // we are absolutely not having 2^64 character-long string slices
            fract_len = fract_part.len() as u32;

            (
                src,
                s.parse().map_err(err(src))?,
                fract_part.parse::<u32>().map_err(err(src))?,
            )
        } else {
            let (src, fract_part) = map(
                tuple((tag("."), take_while(|c: char| c.is_ascii_digit()))),
                |op: (&str, &str)| op.1,
            )(src)?;

            // we are absolutely not having 2^64 character-long string slices
            fract_len = fract_part.len() as u32;

            (src, 0, fract_part.parse().map_err(err(src))?)
        };

        Ok((
            src,
            f64::from(int_part) * sign
                + f64::from(fract_part) / libm::pow(10.0, f64::from(fract_len)),
        ))
    }
}

#[cfg(test)]
mod test {
    use super::{Lexer, Literal, Token};

    #[test]
    fn lexing() {
        let input = "(1 + 1) - 2 / 3 + 4+2i / π - 5e + 6π";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next(), Some(Token::LParen));
        assert_eq!(lexer.next(), Some(Token::Literal(Literal::Real(1.0))));
        assert_eq!(lexer.next(), Some(Token::Add));
        assert_eq!(lexer.next(), Some(Token::Literal(Literal::Real(1.0))));
        assert_eq!(lexer.next(), Some(Token::RParen));
        assert_eq!(lexer.next(), Some(Token::Sub));
        assert_eq!(lexer.next(), Some(Token::Literal(Literal::Real(2.0))));
        assert_eq!(lexer.next(), Some(Token::Div));
        assert_eq!(lexer.next(), Some(Token::Literal(Literal::Real(3.0))));
        assert_eq!(lexer.next(), Some(Token::Add));
        assert_eq!(lexer.next(), Some(Token::Literal(Literal::Real(4.0))));
        assert_eq!(lexer.next(), Some(Token::Add));
        assert_eq!(lexer.next(), Some(Token::Literal(Literal::Real(2.0))));
        assert_eq!(lexer.next(), Some(Token::Literal(Literal::Constant('i'))));
        assert_eq!(lexer.next(), Some(Token::Div));
        assert_eq!(lexer.next(), Some(Token::Literal(Literal::Constant('π'))));
        assert_eq!(lexer.next(), Some(Token::Sub));
        assert_eq!(lexer.next(), Some(Token::Literal(Literal::Real(5.0))));
        assert_eq!(lexer.next(), Some(Token::Literal(Literal::Constant('e'))));
        assert_eq!(lexer.next(), Some(Token::Add));
        assert_eq!(lexer.next(), Some(Token::Literal(Literal::Real(6.0))));
        assert_eq!(lexer.next(), Some(Token::Literal(Literal::Constant('π'))));
        assert_eq!(lexer.next(), None);
    }
}
