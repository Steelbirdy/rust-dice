pub mod ast;
mod lexer;
mod parser;

/// Attempts to parse a string into a dice expression.
///
/// # Examples
/// ```
/// # use parse::parse;
///
/// let expr = parse("2d20kh1   D20 with Advantage").unwrap();
/// assert_eq!(&expr.roll.to_string(), "2d20kh1");
/// assert_eq!(expr.comment.as_deref(), Some("D20 with Advantage"));
/// ```
pub fn parse(s: &str) -> Result<ast::Expression, parser::ParseError> {
    parser::Parser::new(s).parse()
}
