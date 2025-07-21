/// Parser for slvr files.
use std::num::ParseIntError;

use crate::algebra::{Data, Expr};
use crate::{Def, Hyp, IntType};
use nom::branch::alt;
use nom::bytes::{is_not, tag};
use nom::character::complete::{alpha1, digit1, line_ending, multispace0};
use nom::combinator::{eof, fail, map_res, value};
use nom::error::{Error, ParseError};
use nom::multi::{many0, many1};
use nom::sequence::{delimited, pair, separated_pair, terminated};
use nom::{IResult, Parser};
use nom_language::precedence::*;

type ParserExpr = Expr<String>;
type ParserHyp = Hyp<String>;
type ParserDef = Def<String>;

trait VarMap {
    fn map_variables(from: Self) -> usize;
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
fn ws<'a, O, E: ParseError<&'a str>, F>(inner: F) -> impl Parser<&'a str, Output = O, Error = E>
where
    F: Parser<&'a str, Output = O, Error = E>,
{
    delimited(multispace0, inner, multispace0)
}

/// Parse an algebraic expression
fn parse_expr(i: &str) -> IResult<&str, ParserExpr> {
    precedence(
        // Prefix
        unary_op(1, tag("-")),
        // Postfix
        fail(),
        // Binary
        alt((
            binary_op(2, Assoc::Left, tag("*")),
            binary_op(2, Assoc::Left, tag("/")),
            binary_op(3, Assoc::Left, tag("+")),
            binary_op(3, Assoc::Left, tag("-")),
        )),
        // Operands
        alt((parse_literal, parse_variable, parse_subexpr)),
        // Fold
        |op: Operation<&str, &str, &str, ParserExpr>| {
            use nom_language::precedence::Operation::*;
            match op {
                Prefix("-", o) => Ok(Expr::Neg(Box::new(o))),
                Binary(lhs, "*", rhs) => Ok(Expr::Mul(Box::new(lhs), Box::new(rhs))),
                Binary(lhs, "/", rhs) => Ok(Expr::Div(Box::new(lhs), Box::new(rhs))),
                Binary(lhs, "-", rhs) => Ok(Expr::Add(Box::new(lhs), Box::new(rhs))),
                Binary(lhs, "+", rhs) => Ok(Expr::Add(Box::new(lhs), Box::new(rhs))),
                _ => Err("Invalid combination"),
            }
        },
    )(i)
}

fn parse_subexpr(i: &str) -> IResult<&str, ParserExpr> {
    delimited(tag("("), parse_expr, tag(")")).parse(i)
}

/// Like [`parse_data`], but wrap the result in [`Expr::Literal`]
fn parse_literal<T>(i: &str) -> IResult<&str, Expr<T>> {
    let (rem, data) = parse_data.parse(i)?;
    Ok((rem, Expr::Literal(data)))
}

/// Parse a literal value into an unwrapped [`Data`]
fn parse_data(i: &str) -> IResult<&str, Data> {
    // FIXME Support non-int literals
    alt((parse_int, fail())).parse(i)
}

/// Parse an arbitrary integer into an Expr
fn parse_int(i: &str) -> IResult<&str, Data> {
    map_res(digit1, |s: &str| -> Result<Data, ParseIntError> {
        let val = s.parse::<IntType>()?;
        Ok(Data::Integer(val))
    })
    .parse(i)
}

fn parse_name(i: &str) -> IResult<&str, &str> {
    alpha1.parse(i)
}

/// Parse an arbitrary integer into an Expr
fn parse_variable(i: &str) -> IResult<&str, ParserExpr> {
    map_res(parse_name, |s: &str| -> Result<ParserExpr, ()> {
        Ok(Expr::Variable(s.to_string()))
    })
    .parse(i)
}

fn parse_comment(i: &str) -> IResult<&str, ()> {
    value((), pair(tag("#"), is_not("\n\r"))).parse(i)
}

fn parse_eol(i: &str) -> IResult<&str, ()> {
    value((), alt((parse_comment, value((), line_ending)))).parse(i)
}

fn parse_def(i: &str) -> IResult<&str, Def<String>> {
    let (rem, (name, expr)) =
        separated_pair(parse_name, |s| tag("==").parse(s), parse_expr).parse(i)?;
    Ok((
        rem,
        Def {
            variable: name.to_string(),
            def: expr,
        },
    ))
}

fn parse_hyp(i: &str) -> IResult<&str, ParserHyp> {
    let (rem, (name, expr)) =
        separated_pair(parse_name, |s| tag("=").parse(s), parse_data).parse(i)?;
    Ok((
        rem,
        Hyp {
            variable: name.to_string(),
            value: expr,
        },
    ))
}

fn parse_ignored_line(i: &str) -> IResult<&str, ()> {
    value((), many0((parse_comment, line_ending))).parse(i)
}

#[derive(Debug)]
pub enum Element {
    Def(ParserDef),
    Hyp(ParserHyp),
}

pub fn parse_one(i: &str) -> IResult<&str, Element> {
    alt((
        parse_def.map(|d| Element::Def(d)),
        parse_hyp.map(|h| Element::Hyp(h)),
    ))
    .parse(i)
}

/// Parse input into a Solvr.  
pub fn parse(i: &str) -> Result<Vec<Element>, nom::Err<Error<&str>>> {
    Ok(terminated(many0(terminated(parse_one, parse_eol)), eof)
        .parse(i)?
        .1)
}
