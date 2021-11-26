pub mod ast;
mod lexer;
mod parser;

pub(crate) use parser::ParseError;

pub(crate) fn parse(s: &str) -> Result<ast::Expression, ParseError> {
    parser::Parser::new(s).parse()
}
