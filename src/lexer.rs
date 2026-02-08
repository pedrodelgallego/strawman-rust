#[derive(Debug, PartialEq)]
pub enum Token {
    Number(f64),
    StringLit(String),
    Boolean(bool),
    Symbol(String),
    LParen,
    RParen,
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        let ch = chars[i];

        if ch.is_whitespace() {
            i += 1;
            continue;
        }

        // Comment: skip from `;` to end of line
        if ch == ';' {
            while i < chars.len() && chars[i] != '\n' {
                i += 1;
            }
            continue;
        }

        // Parentheses
        if ch == '(' {
            tokens.push(Token::LParen);
            i += 1;
            continue;
        }
        if ch == ')' {
            tokens.push(Token::RParen);
            i += 1;
            continue;
        }

        // Number: digits, optional leading minus, optional decimal point
        if ch.is_ascii_digit() || (ch == '-' && i + 1 < chars.len() && chars[i + 1].is_ascii_digit()) {
            let start = i;
            if ch == '-' {
                i += 1;
            }
            while i < chars.len() && chars[i].is_ascii_digit() {
                i += 1;
            }
            if i < chars.len() && chars[i] == '.' {
                i += 1;
                while i < chars.len() && chars[i].is_ascii_digit() {
                    i += 1;
                }
            }
            let num_str: String = chars[start..i].iter().collect();
            let num: f64 = num_str.parse().expect("invalid number");
            tokens.push(Token::Number(num));
            continue;
        }

        // String literal: "..."
        if ch == '"' {
            i += 1; // skip opening quote
            let mut s = String::new();
            while i < chars.len() && chars[i] != '"' {
                if chars[i] == '\\' && i + 1 < chars.len() {
                    // escape sequence: consume backslash, push next char
                    i += 1;
                    s.push(chars[i]);
                } else {
                    s.push(chars[i]);
                }
                i += 1;
            }
            if i >= chars.len() {
                return Err("unterminated string".to_string());
            }
            i += 1; // skip closing quote
            tokens.push(Token::StringLit(s));
            continue;
        }

        // Boolean: #t or #f
        if ch == '#' && i + 1 < chars.len() && (chars[i + 1] == 't' || chars[i + 1] == 'f') {
            tokens.push(Token::Boolean(chars[i + 1] == 't'));
            i += 2;
            continue;
        }

        // Symbol: identifiers and operators
        if is_symbol_char(ch) {
            let start = i;
            while i < chars.len() && is_symbol_char(chars[i]) {
                i += 1;
            }
            let sym: String = chars[start..i].iter().collect();
            tokens.push(Token::Symbol(sym));
            continue;
        }

        i += 1;
    }

    Ok(tokens)
}

fn is_symbol_char(ch: char) -> bool {
    !ch.is_whitespace() && ch != '(' && ch != ')' && ch != '"' && ch != ';'
}
