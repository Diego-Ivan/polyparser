#[derive(PartialEq, Debug)]
pub enum Value {
    Constant(f64),
    X(f64, u32),
    Y(f64, u32),
    Xy(f64, u32, u32),
}

use super::parser::Token;

fn tokenize(char: char) -> Option<Token> {
    let point = char as u32;
    match char {
        '0'..='9' => Some(Token::Number(point - 0x30)),
        'x' | 'X' => Some(Token::X),
        'y' | 'Y' => Some(Token::Y),
        '+' => Some(Token::Plus),
        '-' => Some(Token::Minus),
        ' ' => Some(Token::Whitespace),
        '^' => Some(Token::Exp),
        '.' => Some(Token::DecimalSeparator),
        _ => None,
    }
}

#[derive(PartialEq)]
enum Variable {
    X,
    Y,
}

#[derive(PartialEq)]
enum NumberParseMode {
    Integer,
    Decimal,
    Exponent(Variable),
    Idle,
}

#[derive(Debug)]
enum Exp {
    Unit,
    Integer(u32),
}

pub fn parse_value(str: &str) -> Value {
    let mut out_index = 0;
    let mut constant = 0.0;
    let mut x_exp = Exp::Integer(0);
    let mut y_exp = Exp::Integer(0);
    let mut n_decimals = 0;
    let mut parse_mode = NumberParseMode::Integer;

    for char in str.chars() {
        let token = tokenize(char).unwrap();
        match token {
            Token::Number(num) => match &parse_mode {
                NumberParseMode::Integer | NumberParseMode::Idle => {
                    constant *= 10.0;
                    constant += num as f64;
                }
                NumberParseMode::Decimal => {
                    n_decimals += 1;
                    constant += (num as f64) * 10.0f64.powi(-n_decimals);
                }
                NumberParseMode::Exponent(Variable::X) => {
                    x_exp = match x_exp {
                        Exp::Unit => Exp::Integer(num),
                        Exp::Integer(int) => Exp::Integer(int * 10 + num),
                    };
                }
                NumberParseMode::Exponent(Variable::Y) => {
                    y_exp = match y_exp {
                        Exp::Unit => Exp::Integer(num),
                        Exp::Integer(int) => Exp::Integer(int * 10 + num),
                    };
                }
            },
            Token::DecimalSeparator => {
                if parse_mode == NumberParseMode::Decimal {
                    panic!("Found a decimal separator after parsing decimals");
                }
                parse_mode = NumberParseMode::Decimal;
            }
            Token::X => {parse_mode = NumberParseMode::Exponent(Variable::X); x_exp = Exp::Unit},
            Token::Y => {parse_mode = NumberParseMode::Exponent(Variable::Y); y_exp = Exp::Unit},
            Token::Exp => match parse_mode {
                NumberParseMode::Exponent(_) => {}
                _ => panic!("Parse mode was not exponent reaching an exp token. Please verify the integrity of the str")
            },

            Token::Whitespace | Token::Plus | Token::Minus => {
                break;
            },
        }
    }

    let constant = if constant == 0.0 { 1.0 } else { constant };

    let value = match (x_exp, y_exp) {
        /* Constant variant */
        (Exp::Integer(0), Exp::Integer(0)) => Value::Constant(constant),

        /* X variants */
        (Exp::Unit, Exp::Integer(0)) => Value::X(constant, 1),
        (Exp::Integer(x_exp), Exp::Integer(0)) => Value::X(constant, x_exp),

        /* Y Variants */
        (Exp::Integer(0), Exp::Unit) => Value::Y(constant, 1),
        (Exp::Integer(0), Exp::Integer(y_exp)) => Value::Y(constant, y_exp),

        /* XY variants */
        (Exp::Unit, Exp::Unit) => Value::Xy(constant, 1, 1),
        (Exp::Unit, Exp::Integer(y_exp)) => Value::Xy(constant, 1, y_exp),
        (Exp::Integer(x_exp), Exp::Unit) => Value::Xy(constant, x_exp, 1),
        (Exp::Integer(x_exp), Exp::Integer(y_exp)) => Value::Xy(constant, x_exp, y_exp),
    };

    value
}

#[test]
fn test_parse_value() {
    let val = "3x^2";
    assert_eq!(parse_value(val), Value::X(3.0, 2));

    let val = "6.4x^3";
    assert_eq!(parse_value(val), Value::X(6.4, 3));

    let val = "101.54y^3";
    assert_eq!(parse_value(val), Value::Y(101.54, 3));

    let val = "10xy";
    assert_eq!(parse_value(val), Value::Xy(10.0, 1, 1));

    let val = "10x^10y^6";
    assert_eq!(parse_value(val), Value::Xy(10.0, 10, 6));
    let val = "10y^6x^10";
    assert_eq!(parse_value(val), Value::Xy(10.0, 10, 6));

    let val = "xy";
    assert_eq!(parse_value(val), Value::Xy(1.0, 1, 1));
    let val = "5xy^2";
    assert_eq!(parse_value(val), Value::Xy(5.0, 1, 2));
}
