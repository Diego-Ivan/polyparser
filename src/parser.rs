pub struct Parser<'a> {
    bytes: &'a [u8],
    current_index: usize,
}

#[derive(Debug)]
pub enum Token {
    Number(u32),
    DecimalSeparator,
    X,
    Y,
    Plus,
    Minus,
    Exp,
    Whitespace,
}

#[derive(Debug, PartialEq)]
pub struct Expression {
    pub coefficient: f64,
    pub x_exp: u32,
    pub y_exp: u32,
}

#[derive(PartialEq, Eq, Debug)]
enum SignParseMode {
    Positive,
    Negative,
    Unset,
}

impl SignParseMode {
    pub fn unwrap(self) -> f64 {
        match self {
            Self::Positive => 1.0,
            Self::Negative => -1.0,
            Self::Unset => panic!("Unwrapped called on Unset variant of SignParseMode"),
        }
    }
}

enum Exp {
    Unit,
    Number(u32),
}

impl Exp {
    pub fn unwrap(self) -> u32 {
        match self {
            Self::Number(num) => num,
            Self::Unit => 1,
        }
    }
}

#[derive(PartialEq, Debug)]
enum Constant {
    Unit,
    Number(f64),
}

impl Constant {
    pub fn multiply_assign(&mut self, num: f64) {
        match self {
            Self::Unit => *self = Self::Number(num),
            Self::Number(internal) => *internal *= num,
        }
    }

    pub fn unwrap(self) -> f64 {
        match self {
            Self::Unit => 1.0,
            Self::Number(num) => num,
        }
    }

    pub fn add_assign(&mut self, num: f64) {
        match self {
            Self::Unit => *self = Self::Number(num + 1.0),
            Self::Number(internal) => *internal += num,
        }
    }
}

#[test]
fn test_const_multiply() {
    let mut constant = Constant::Unit;
    constant.multiply_assign(5.0);
    assert_eq!(constant, Constant::Number(5.0));

    constant = Constant::Number(9.0);
    constant.multiply_assign(9.0);
    assert_eq!(constant, Constant::Number(81.0));
}

#[test]
fn test_const_add() {
    let mut constant = Constant::Unit;
    constant.add_assign(5.0);
    assert_eq!(constant, Constant::Number(6.0));

    constant = Constant::Number(9.0);
    constant.add_assign(9.0);
    assert_eq!(constant, Constant::Number(18.0));
}

enum Variable {
    X,
    Y,
}

enum ParseMode {
    Integer,
    Decimal,
    Exponent(Variable),
}

impl<'a> Parser<'a> {
    pub fn new(str: &'a str) -> Self {
        Self {
            bytes: str.as_bytes(),
            current_index: 0,
        }
    }

    pub fn parse(&mut self) -> Vec<Expression> {
        let mut values = Vec::new();
        while let Some(current_token) = self.get_current() {
            match current_token {
                Token::Whitespace => self.consume(),
                Token::Number(_) => match self.parse_value() {
                    Some(value) => values.push(value),
                    None => panic!("Failed to parse value"),
                },
                Token::Plus | Token::Minus | Token::X | Token::Y | Token::DecimalSeparator => {
                    match self.parse_value() {
                        Some(value) => values.push(value),
                        None => panic!("Failed to parse value"),
                    };
                }
                Token::Exp => panic!("Found '^' outside of value expression"),
            }
        }

        values
    }

    fn parse_value(&mut self) -> Option<Expression> {
        let mut sign = SignParseMode::Unset;
        let mut constant = Constant::Unit;
        let mut x_exp = Exp::Number(0);
        let mut y_exp = Exp::Number(0);
        let mut n_decimals = 0;
        let mut parse_mode = ParseMode::Integer;

        while let Some(token) = self.get_current() {
            match token {
                Token::Whitespace => {
                    self.consume();
                }
                Token::DecimalSeparator => {
                    if sign == SignParseMode::Unset {
                        sign = SignParseMode::Positive;
                    }
                    match parse_mode {
                        ParseMode::Exponent(_) => {
                            eprintln!("Decimals are not supported in exponents");
                            return None;
                        }
                        _ => {}
                    }
                    parse_mode = ParseMode::Decimal;
                    self.consume();
                }
                Token::Exp => {
                    if sign == SignParseMode::Unset {
                        eprintln!("'^' was found, but no previous expression to work with");
                        return None;
                    }
                    match parse_mode {
                        ParseMode::Exponent(_) => {}
                        _ => {
                            eprintln!("'^' was found, but no previous expression to work with");
                            return None;
                        }
                    }
                    self.consume();
                }
                Token::Number(num) => {
                    if sign == SignParseMode::Unset {
                        sign = SignParseMode::Positive;
                    }
                    match &parse_mode {
                        ParseMode::Integer => {
                            constant = match constant {
                                Constant::Unit => {
                                    let mut new_const = Constant::Number(0.0);
                                    new_const.multiply_assign(10.0);
                                    new_const.add_assign(num as f64);

                                    new_const
                                }
                                Constant::Number(_) => {
                                    constant.multiply_assign(10.0);
                                    constant.add_assign(num as f64);
                                    constant
                                }
                            }
                        }
                        ParseMode::Decimal => {
                            n_decimals += 1;
                            constant.add_assign(num as f64 * 10f64.powi(-n_decimals));
                        }
                        ParseMode::Exponent(Variable::X) => {
                            x_exp = match x_exp {
                                Exp::Unit => Exp::Number(num),
                                Exp::Number(previous) => Exp::Number(previous * 10 + num),
                            }
                        }
                        ParseMode::Exponent(Variable::Y) => {
                            y_exp = match y_exp {
                                Exp::Unit => Exp::Number(num),
                                Exp::Number(previous) => Exp::Number(previous * 10 + num),
                            }
                        }
                    }
                    self.consume();
                }
                Token::X => {
                    if sign == SignParseMode::Unset {
                        sign = SignParseMode::Positive;
                    }
                    parse_mode = ParseMode::Exponent(Variable::X);
                    x_exp = Exp::Unit;
                    self.consume();
                }
                Token::Y => {
                    if sign == SignParseMode::Unset {
                        sign = SignParseMode::Positive;
                    }
                    parse_mode = ParseMode::Exponent(Variable::Y);
                    y_exp = Exp::Unit;
                    self.consume();
                }
                Token::Minus => {
                    if sign != SignParseMode::Unset {
                        println!("End of expression found");
                        break;
                    }
                    sign = SignParseMode::Negative;
                    self.consume();
                }
                Token::Plus => {
                    if sign != SignParseMode::Unset {
                        println!("End of expression found");
                        break;
                    }
                    sign = SignParseMode::Positive;
                    self.consume();
                }
            }
        }

        Some(Expression {
            x_exp: x_exp.unwrap(),
            y_exp: y_exp.unwrap(),
            coefficient: constant.unwrap() * sign.unwrap(),
        })
    }

    fn get_current(&self) -> Option<Token> {
        match self.bytes.get(self.current_index) {
            Some(char) => tokenize(*char),
            None => None,
        }
    }

    fn consume(&mut self) {
        self.current_index += 1;
    }
}

fn tokenize(char: u8) -> Option<Token> {
    match char {
        b'0'..=b'9' => Some(Token::Number(char as u32 - 0x30)),
        b'x' | b'X' => Some(Token::X),
        b'y' | b'Y' => Some(Token::Y),
        b'+' => Some(Token::Plus),
        b'-' => Some(Token::Minus),
        b' ' | b'\t' => Some(Token::Whitespace),
        b'^' => Some(Token::Exp),
        b'.' => Some(Token::DecimalSeparator),
        _ => None,
    }
}

#[test]
fn test_positives() {
    let input = "3x^2";
    let mut parser = Parser::new(&input);
    let output = parser.parse();
    assert_eq!(
        output,
        vec![Expression {
            coefficient: 3.0,
            x_exp: 2,
            y_exp: 0
        }]
    );

    let input = "5y^54";
    let mut parser = Parser::new(&input);
    let output = parser.parse();
    assert_eq!(
        output,
        vec![Expression {
            coefficient: 5.0,
            x_exp: 0,
            y_exp: 54
        }]
    );

    let input = "+5y^54";
    let mut parser = Parser::new(&input);
    let output = parser.parse();
    assert_eq!(
        output,
        vec![Expression {
            coefficient: 5.0,
            x_exp: 0,
            y_exp: 54
        }]
    );
}

#[test]
fn test_negatives() {
    let input = "-3x^6";
    let mut parser = Parser::new(&input);
    let output = parser.parse();
    assert_eq!(
        output,
        vec![Expression {
            coefficient: -3.0,
            x_exp: 6,
            y_exp: 0
        }]
    );

    let input = "-3y^34";
    let mut parser = Parser::new(&input);
    let output = parser.parse();
    assert_eq!(
        output,
        vec![Expression {
            coefficient: -3.0,
            x_exp: 0,
            y_exp: 34
        }]
    );
}

#[test]
fn test_complex() {
    let input = "3x^2 + 64x^6y^2 - 3x^2y^5";
    let mut parser = Parser::new(&input);
    let output = parser.parse();
    assert_eq!(
        output,
        vec![
            Expression {
                coefficient: 3.0,
                x_exp: 2,
                y_exp: 0
            },
            Expression {
                coefficient: 64.0,
                x_exp: 6,
                y_exp: 2,
            },
            Expression {
                coefficient: -3.0,
                x_exp: 2,
                y_exp: 5,
            }
        ]
    );
}

#[test]
fn test_no_exp() {
    let input = "3x2y87 - 4x54y2 + 5";
    let mut parser = Parser::new(&input);
    let output = parser.parse();
    assert_eq!(
        output,
        vec![
            Expression {
                coefficient: 3.0,
                x_exp: 2,
                y_exp: 87
            },
            Expression {
                coefficient: -4.0,
                x_exp: 54,
                y_exp: 2,
            },
            Expression {
                coefficient: 5.0,
                x_exp: 0,
                y_exp: 0
            }
        ]
    );
}

#[test]
fn test_decimals() {
    let input = "3.1416x3 - 4.35x2y5 - 4.3y";
    let mut parser = Parser::new(&input);
    let output = parser.parse();
    assert_eq!(
        output,
        vec![
            Expression {
                coefficient: 3.1416,
                x_exp: 3,
                y_exp: 0
            },
            Expression {
                coefficient: -4.35,
                x_exp: 2,
                y_exp: 5
            },
            Expression {
                coefficient: -4.3,
                x_exp: 0,
                y_exp: 1
            },
        ]
    );
}

#[test]
#[should_panic]
fn panic_orfan_exp() {
    let input = "3.1416x3 - 5^ - 4.3y";
    let mut parser = Parser::new(&input);
    let _output = parser.parse();

    let input = "3.1416x3 - 5^6 - 4.3y";
    let mut parser = Parser::new(&input);
    let _output = parser.parse();
}

#[test]
#[should_panic]
fn panic_decimals_exp() {
    let input = "3x^5.4";
    let mut parser = Parser::new(&input);
    let _output = parser.parse();

    let input = "3x5.4";
    let mut parser = Parser::new(&input);
    let _output = parser.parse();
}

#[test]
fn test_unit_coefficients() {
    let input = "xy + yx + x + y";
    let mut parser = Parser::new(&input);
    let output = parser.parse();

    assert_eq!(
        output,
        vec![
            Expression {
                coefficient: 1.0,
                x_exp: 1,
                y_exp: 1,
            },
            Expression {
                coefficient: 1.0,
                x_exp: 1,
                y_exp: 1,
            },
            Expression {
                coefficient: 1.0,
                x_exp: 1,
                y_exp: 0,
            },
            Expression {
                coefficient: 1.0,
                x_exp: 0,
                y_exp: 1,
            }
        ]
    );
}
