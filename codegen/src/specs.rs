use serde::Deserialize;
use std::collections::HashMap;

#[derive(Debug, Deserialize)]
pub struct Specs {
  pub keywords: HashMap<String, String>,
  pub reserved: HashMap<String, String>,
  pub operators: HashMap<String, Operator>,
  pub prefix_operators: HashMap<String, String>,
  pub postfix_operators: HashMap<String, String>,
  pub punctuation: HashMap<String, String>,
  pub delimiters: HashMap<String, Delimiter>,
}

#[derive(Debug, Deserialize)]
pub struct Operator {
  pub symbol: String,
  pub precedence: i8,
}

#[derive(Debug, Deserialize)]
pub struct Delimiter {
  pub open: String,
  pub close: String,
}
