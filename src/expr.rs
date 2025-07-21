/// The expression tree

/// An expression.  This is fundamentally a tree.
pub enum Expr<O, T> {
    Term(T),
    Op(O, Vec<Self>),
}

pub enum Term<D, V> {
    Literal(D),
    Variable(V),
}

impl<O, T> Expr<O, T> {
    fn all(&self, pred: fn(&Self) -> bool) -> bool {
        pred(self)
            && if let Self::Op(_, a) = self {
                a.iter().all(pred)
            } else {
                true
            }
    }

    fn any(&self, pred: fn(&Self) -> bool) -> bool {
        pred(self)
            || if let Self::Op(_, a) = self {
                a.iter().any(pred)
            } else {
                true
            }
    }
}
