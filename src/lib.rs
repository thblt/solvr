pub mod algebra;
// pub mod expr;

pub mod parser;
use algebra::*;
use bimap::BiMap;


use crate::parser::parse;

type IntType = i64;
type FloatType = f64;

struct Variable {
    /// Contents of this variable
    data: Data,
    /// Whether this variable is user-editable.
    editable: bool,
    /// Definitions
    defs: Vec<Expr<usize>>,
}

pub struct Solvr {
    obarray: BiMap<String, usize>,
    variables: Vec<Variable>,
    compiled: bool,
}

#[derive(Debug)]
pub struct Def<T> {
    variable: T,
    def: Expr<T>,
}

#[derive(Debug)]
pub struct Hyp<T> {
    variable: T,
    value: Data,
}

impl Solvr {
    pub fn new() -> Solvr {
        Solvr {
            obarray: BiMap::new(),
            variables: vec![],
            compiled: false,
        }
    }

    /// Insert a new variable into this solver.   Return the identifier of this
    /// variable in a Result.  If Err, a variable with that name was already present and
    /// wasn't modified.
    pub fn insert_variable(&mut self, name: &str, data: Data) -> Result<usize, usize> {
        if let Some(vid) = self.obarray.get_by_left(name) {
            return Err(*vid);
        }

        let vid = self.variables.len();
        self.obarray.insert(name.to_string(), vid);
        self.variables.push(Variable {
            data,
            editable: true,
            defs: vec![],
        });
        Ok(vid)
    }

    pub fn parse(&mut self, input: &str) {
        match parse(input) {
            Ok(_) => todo!(),
            Err(e) => println!("{e:?}"),
        }
    }

    pub fn intern(&mut self, name: &str) -> usize {
        match self.insert_variable(name, Data::None) {
            Ok(i) => i,
            Err(i) => i,
        }
    }
}
