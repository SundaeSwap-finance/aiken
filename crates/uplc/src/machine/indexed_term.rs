use std::{fmt::Display, rc::Rc};

use pretty::RcDoc;

use crate::{
    ast::{Constant, Term},
    builtins::DefaultFunction,
    flat::Binder,
};

#[derive(Debug, Clone, PartialEq)]
pub enum IndexedTerm<T> {
    Var {
        index: Option<u64>,
        name: Rc<T>,
    },
    Delay {
        index: Option<u64>,
        then: Rc<IndexedTerm<T>>,
    },
    Lambda {
        index: Option<u64>,
        parameter_name: Rc<T>,
        body: Rc<IndexedTerm<T>>,
    },
    Apply {
        index: Option<u64>,
        function: Rc<IndexedTerm<T>>,
        argument: Rc<IndexedTerm<T>>,
    },
    Constant {
        index: Option<u64>,
        value: Rc<Constant>,
    },
    Force {
        index: Option<u64>,
        then: Rc<IndexedTerm<T>>,
    },
    Error {
        index: Option<u64>,
    },
    Builtin {
        index: Option<u64>,
        func: DefaultFunction,
    },
    Constr {
        index: Option<u64>,
        tag: usize,
        fields: Vec<IndexedTerm<T>>,
    },
    Case {
        index: Option<u64>,
        constr: Rc<IndexedTerm<T>>,
        branches: Vec<IndexedTerm<T>>,
    },
}

impl<T: Clone> IndexedTerm<T> {
    pub fn from(term: Term<T>) -> Self {
        let mut current = 0;
        Self::build(&term, &mut current)
    }

    fn build(term: &Term<T>, current: &mut u64) -> Self {
        let index = Some(*current);
        *current += 1;
        match term {
            Term::Var(name) => {
                let name = name.clone();
                Self::Var { index, name }
            }
            Term::Delay(then) => {
                let then = Self::build(then, current).into();
                Self::Delay { index, then }
            }
            Term::Lambda {
                parameter_name,
                body,
            } => {
                let parameter_name = parameter_name.clone();
                let body = Self::build(body, current).into();
                Self::Lambda {
                    index,
                    parameter_name,
                    body,
                }
            }
            Term::Apply { function, argument } => {
                let function = Self::build(function, current).into();
                let argument = Self::build(argument, current).into();
                Self::Apply {
                    index,
                    function,
                    argument,
                }
            }
            Term::Constant(value) => {
                let value = value.clone();
                Self::Constant { index, value }
            }
            Term::Force(then) => {
                let then = Self::build(then, current).into();
                Self::Force { index, then }
            }
            Term::Error => Self::Error { index },
            Term::Builtin(func) => {
                let func = *func;
                Self::Builtin { index, func }
            }
            Term::Constr { tag, fields } => {
                let tag = *tag;
                let fields = fields.iter().map(|f| Self::build(f, current)).collect();
                Self::Constr { index, tag, fields }
            }
            Term::Case { constr, branches } => {
                let constr = Self::build(constr, current).into();
                let branches = branches.iter().map(|b| Self::build(b, current)).collect();
                Self::Case {
                    index,
                    constr,
                    branches,
                }
            }
        }
    }

    pub fn index(&self) -> Option<u64> {
        match self {
            Self::Var { index, .. }
            | Self::Delay { index, .. }
            | Self::Lambda { index, .. }
            | Self::Apply { index, .. }
            | Self::Constant { index, .. }
            | Self::Force { index, .. }
            | Self::Error { index }
            | Self::Builtin { index, .. }
            | Self::Constr { index, .. }
            | Self::Case { index, .. } => *index,
        }
    }

    pub fn force(self) -> Self {
        Self::Force {
            index: self.index(),
            then: Rc::new(self),
        }
    }

    pub fn apply(self, arg: Self) -> Self {
        Self::Apply {
            index: self.index(),
            function: Rc::new(self),
            argument: Rc::new(arg),
        }
    }
}

impl<'a, T> Display for IndexedTerm<T>
where
    T: Binder<'a>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_pretty(5))
    }
}

impl<T> From<&IndexedTerm<T>> for Term<T> {
    fn from(value: &IndexedTerm<T>) -> Self {
        match value {
            IndexedTerm::Var { index: _, name } => {
                let name = name.clone();
                Term::Var(name)
            }
            IndexedTerm::Delay { index: _, then } => {
                let then = Rc::new(then.as_ref().into());
                Term::Delay(then)
            }
            IndexedTerm::Lambda {
                index: _,
                parameter_name,
                body,
            } => {
                let parameter_name = parameter_name.clone();
                let body = Rc::new(body.as_ref().into());
                Term::Lambda {
                    parameter_name,
                    body,
                }
            }
            IndexedTerm::Apply {
                index: _,
                function,
                argument,
            } => {
                let function = Rc::new(function.as_ref().into());
                let argument = Rc::new(argument.as_ref().into());
                Term::Apply { function, argument }
            }
            IndexedTerm::Constant { index: _, value } => {
                let value = value.clone();
                Term::Constant(value)
            }
            IndexedTerm::Force { index: _, then } => {
                let then = Rc::new(then.as_ref().into());
                Term::Force(then)
            }
            IndexedTerm::Error { index: _ } => Term::Error,
            IndexedTerm::Builtin { index: _, func } => {
                let func = *func;
                Term::Builtin(func)
            }
            IndexedTerm::Constr {
                index: _,
                tag,
                fields,
            } => {
                let tag = *tag;
                let fields = fields.iter().map(|f| f.into()).collect();
                Term::Constr { tag, fields }
            }
            IndexedTerm::Case {
                index: _,
                constr,
                branches,
            } => {
                let constr = Rc::new(constr.as_ref().into());
                let branches = branches.iter().map(|b| b.into()).collect();
                Term::Case { constr, branches }
            }
        }
    }
}

impl<'a, T> IndexedTerm<T>
where
    T: Binder<'a>,
{
    pub fn to_pretty(&self, depth: usize) -> String {
        let mut w = Vec::new();

        self.to_doc(depth).render(80, &mut w).unwrap();

        String::from_utf8(w)
            .unwrap()
            .lines()
            // This is a hack to deal with blank newlines
            // that end up with a bunch of useless whitespace
            // because of the nesting
            .map(|l| {
                if l.chars().all(|c| c.is_whitespace()) {
                    "".to_string()
                } else {
                    l.to_string()
                }
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn to_doc(&self, depth: usize) -> RcDoc<'_, ()> {
      if depth <= 0 {
        return RcDoc::text("...")
      }
      match self {
            IndexedTerm::Var { name, .. } => RcDoc::text(name.text()),
            IndexedTerm::Delay { then, .. } => RcDoc::text("(")
                .append(
                    RcDoc::text("delay")
                        .append(RcDoc::line())
                        .append(then.to_doc(depth - 1))
                        .nest(2),
                )
                .append(RcDoc::line_())
                .append(RcDoc::text(")")),
            IndexedTerm::Lambda {
                parameter_name,
                body,
                ..
            } => RcDoc::text("(")
                .append(
                    RcDoc::text("lam")
                        .append(RcDoc::line())
                        .append(RcDoc::text(parameter_name.text()))
                        .append(RcDoc::line())
                        .append(body.to_doc(depth - 1))
                        .nest(2),
                )
                .append(RcDoc::line_())
                .append(RcDoc::text(")")),
            IndexedTerm::Apply {
                function, argument, ..
            } => RcDoc::text("[")
                .append(
                    RcDoc::line()
                        .append(
                            function
                                .to_doc(depth - 1)
                                .append(RcDoc::line())
                                .append(argument.to_doc(depth - 1))
                                .group(),
                        )
                        .nest(2),
                )
                .append(RcDoc::line())
                .append(RcDoc::text("]")),
            IndexedTerm::Constant { value, .. } => RcDoc::text("(")
                .append(
                    RcDoc::text("con")
                        .append(RcDoc::line())
                        .append(value.to_doc(depth - 1))
                        .nest(2),
                )
                .append(RcDoc::line_())
                .append(RcDoc::text(")")),
            IndexedTerm::Force { then, .. } => RcDoc::text("(")
                .append(
                    RcDoc::text("force")
                        .append(RcDoc::line())
                        .append(then.to_doc(depth - 1))
                        .nest(2),
                )
                .append(RcDoc::line_())
                .append(RcDoc::text(")")),
            IndexedTerm::Error { .. } => RcDoc::text("(")
                .append(RcDoc::text("error").nest(2))
                .append(RcDoc::line())
                .append(RcDoc::line_())
                .append(RcDoc::text(")")),
            IndexedTerm::Builtin { func, .. } => RcDoc::text("(")
                .append(
                    RcDoc::text("builtin")
                        .append(RcDoc::line())
                        .append(RcDoc::text(func.to_string()))
                        .nest(2),
                )
                .append(RcDoc::line_())
                .append(RcDoc::text(")")),
            IndexedTerm::Constr { tag, fields, .. } => RcDoc::text("(")
                .append(
                    RcDoc::text("constr")
                        .append(RcDoc::line())
                        .append(RcDoc::as_string(tag))
                        .nest(2),
                )
                .append(RcDoc::line_())
                .append(RcDoc::intersperse(
                    fields.iter().map(|f| f.to_doc(depth - 1)),
                    RcDoc::line_(),
                ))
                .append(RcDoc::text(")")),
            IndexedTerm::Case {
                constr, branches, ..
            } => RcDoc::text("(")
                .append(
                    RcDoc::text("case")
                        .append(RcDoc::line())
                        .append(constr.to_doc(depth - 1))
                        .nest(2),
                )
                .append(RcDoc::line_())
                .append(RcDoc::intersperse(
                    branches.iter().map(|f| f.to_doc(depth - 1)),
                    RcDoc::line_(),
                ))
                .append(RcDoc::text(")")),
        }
        .group()
    }
}
