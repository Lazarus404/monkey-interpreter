use std::fmt;
pub type Program = BlockStmt;

#[derive(PartialEq, Debug, Eq, Clone)]
pub struct Identity(pub String);

#[derive(PartialEq, Debug, Clone)]
pub enum Stmt {
  Let(Identity, Expr),
  Return(Expr),
  Expr(Expr),
}

pub type BlockStmt = Vec<Stmt>;

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
  Ident(Identity),
  Lit(Literal),
  Prefix(Prefix, Box<Expr>),
  Infix(Infix, Box<Expr>, Box<Expr>),
  Index(Box<Expr>, Box<Expr>),
  Hash(Vec<(Literal, Expr)>),
  If {
    cond: Box<Expr>,
    consequence: BlockStmt,
    alternative: Option<BlockStmt>,
  },
  Function { params: Vec<Identity>, body: BlockStmt },
  Call {
    name: Option<String>,
    function: Box<Expr>,
    arguments: Vec<Expr>,
  },
}

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
  Int(i64),
  Bool(bool),
  String(String),
  Array(Vec<Expr>),
  Hash(Vec<(Expr, Expr)>),
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Precedence {
  PLowest,
  PEquals, // ==
  PLessGreater, // > or <
  PSum, // +
  PProduct, // *
  PPrefix, // !x -x +x
  PCall, // func(x)
  PIndex,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Prefix {
  PrePlus,
  PreMinus,
  Not,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Infix {
  Plus,
  Minus,
  Divide,
  Multiply,
  Equal,
  NotEqual,
  GreaterThanEqual,
  LessThanEqual,
  GreaterThan,
  LessThan,
}

impl fmt::Display for Infix {
  fn fmt(&self, formatting: &mut fmt::Formatter) -> fmt::Result {
    match *self {
      Infix::Plus => write!(formatting, "+"),
      Infix::Minus => write!(formatting, "-"),
      Infix::Divide => write!(formatting, "/"),
      Infix::Multiply => write!(formatting, "*"),
      Infix::Equal => write!(formatting, "=="),
      Infix::NotEqual => write!(formatting, "!="),
      Infix::GreaterThanEqual => write!(formatting, ">="),
      Infix::LessThanEqual => write!(formatting, "<="),
      Infix::GreaterThan => write!(formatting, ">"),
      Infix::LessThan => write!(formatting, "<"),
    }
  }
}