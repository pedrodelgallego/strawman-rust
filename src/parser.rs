use crate::lexer::{tokenize, Token};

#[derive(Debug, PartialEq)]
pub enum Expr {
    Number(f64),
    Symbol(String),
    StringLit(String),
    Boolean(bool),
    List(Vec<Expr>),
}

pub fn parse(input: &str) -> Result<Expr, String> {
    let tokens = tokenize(input)?;
    if tokens.is_empty() {
        return Err("empty input".to_string());
    }
    let mut pos = 0;
    let result = parse_expr(&tokens, &mut pos)?;
    Ok(result)
}

pub fn parse_all(input: &str) -> Result<Vec<Expr>, String> {
    let tokens = tokenize(input)?;
    if tokens.is_empty() {
        return Err("empty input".to_string());
    }
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse_expr(&tokens, &mut pos)?);
    }
    Ok(exprs)
}

fn parse_expr(tokens: &[Token], pos: &mut usize) -> Result<Expr, String> {
    if *pos >= tokens.len() {
        return Err("unexpected end of input".to_string());
    }
    match &tokens[*pos] {
        Token::Number(n) => {
            let val = *n;
            *pos += 1;
            Ok(Expr::Number(val))
        }
        Token::Symbol(s) => {
            let val = s.clone();
            *pos += 1;
            Ok(Expr::Symbol(val))
        }
        Token::StringLit(s) => {
            let val = s.clone();
            *pos += 1;
            Ok(Expr::StringLit(val))
        }
        Token::Boolean(b) => {
            let val = *b;
            *pos += 1;
            Ok(Expr::Boolean(val))
        }
        Token::LParen => {
            *pos += 1; // consume '('
            let mut elements = Vec::new();
            loop {
                if *pos >= tokens.len() {
                    return Err("unexpected end of input".to_string());
                }
                if tokens[*pos] == Token::RParen {
                    *pos += 1; // consume ')'
                    break;
                }
                elements.push(parse_expr(tokens, pos)?);
            }
            Ok(Expr::List(elements))
        }
        Token::Quote => {
            *pos += 1; // consume quote
            let quoted = parse_expr(tokens, pos)?;
            Ok(Expr::List(vec![
                Expr::Symbol("quote".to_string()),
                quoted,
            ]))
        }
        Token::RParen => Err("unexpected closing paren".to_string()),
    }
}
