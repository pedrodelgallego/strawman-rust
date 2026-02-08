use strawman::lexer::{tokenize, Token};

#[test]
fn empty_input_returns_empty_list() {
    let tokens = tokenize("").unwrap();
    assert!(tokens.is_empty(), "Expected empty vec for empty input, got {:?}", tokens);
}

#[test]
fn single_number() {
    let tokens = tokenize("42").unwrap();
    assert_eq!(tokens, vec![Token::Number(42.0)]);
}

#[test]
fn negative_number() {
    let tokens = tokenize("-3").unwrap();
    assert_eq!(tokens, vec![Token::Number(-3.0)]);
}

#[test]
fn float_number() {
    let tokens = tokenize("3.14").unwrap();
    assert_eq!(tokens, vec![Token::Number(3.14)]);
}

#[test]
fn string_literal() {
    let tokens = tokenize("\"hello\"").unwrap();
    assert_eq!(tokens, vec![Token::StringLit("hello".to_string())]);
}

#[test]
fn string_with_escape() {
    let tokens = tokenize("\"a\\\"b\"").unwrap();
    assert_eq!(tokens, vec![Token::StringLit("a\"b".to_string())]);
}

#[test]
fn symbol() {
    let tokens = tokenize("foo").unwrap();
    assert_eq!(tokens, vec![Token::Symbol("foo".to_string())]);
}

#[test]
fn operator_symbol() {
    let tokens = tokenize("+").unwrap();
    assert_eq!(tokens, vec![Token::Symbol("+".to_string())]);
}

#[test]
fn boolean_true() {
    let tokens = tokenize("#t").unwrap();
    assert_eq!(tokens, vec![Token::Boolean(true)]);
}

#[test]
fn boolean_false() {
    let tokens = tokenize("#f").unwrap();
    assert_eq!(tokens, vec![Token::Boolean(false)]);
}

#[test]
fn parens() {
    let tokens = tokenize("()").unwrap();
    assert_eq!(tokens, vec![Token::LParen, Token::RParen]);
}

#[test]
fn mixed_expression() {
    let tokens = tokenize("(+ 1 2)").unwrap();
    assert_eq!(
        tokens,
        vec![
            Token::LParen,
            Token::Symbol("+".to_string()),
            Token::Number(1.0),
            Token::Number(2.0),
            Token::RParen,
        ]
    );
}

#[test]
fn comment_skipped() {
    let tokens = tokenize("42 ; ignore").unwrap();
    assert_eq!(tokens, vec![Token::Number(42.0)]);
}

#[test]
fn comment_skipped_with_newline() {
    let tokens = tokenize("42 ; this is ignored\n7").unwrap();
    assert_eq!(tokens, vec![Token::Number(42.0), Token::Number(7.0)]);
}

#[test]
fn whitespace_only_returns_empty_list() {
    let tokens = tokenize("   \n\t  ").unwrap();
    assert!(tokens.is_empty(), "Expected empty vec for whitespace-only input, got {:?}", tokens);
}

#[test]
fn unterminated_string_returns_error() {
    let result = tokenize("\"abc");
    assert!(result.is_err(), "Expected Err for unterminated string, got {:?}", result);
    let err_msg = result.unwrap_err();
    assert!(err_msg.contains("unterminated string"), "Error message should contain 'unterminated string', got: {}", err_msg);
}

#[test]
fn nested_parens() {
    let tokens = tokenize("((a))").unwrap();
    assert_eq!(
        tokens,
        vec![
            Token::LParen,
            Token::LParen,
            Token::Symbol("a".to_string()),
            Token::RParen,
            Token::RParen,
        ]
    );
}
