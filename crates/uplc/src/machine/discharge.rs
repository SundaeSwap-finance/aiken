use crate::ast::{DeBruijn, NamedDeBruijn};

use super::{
    indexed_term::IndexedTerm,
    value::{Env, Value},
};

pub fn value_as_term(value: Value) -> IndexedTerm<NamedDeBruijn> {
    match value {
        Value::Con(value) => IndexedTerm::Constant { index: None, value },
        Value::Builtin { runtime, fun } => {
            let mut term = IndexedTerm::Builtin {
                index: None,
                func: fun,
            };

            for _ in 0..runtime.forces {
                term = term.force();
            }

            for arg in runtime.args {
                term = term.apply(value_as_term(arg));
            }

            term
        }
        Value::Delay(body, env) => with_env(
            0,
            env,
            IndexedTerm::Delay {
                index: None,
                then: body,
            },
        ),
        Value::Lambda {
            parameter_name,
            body,
            env,
        } => with_env(
            0,
            env,
            IndexedTerm::Lambda {
                index: None,
                parameter_name: NamedDeBruijn {
                    text: parameter_name.text.clone(),
                    index: 0.into(),
                }
                .into(),
                body,
            },
        ),
        Value::Constr { tag, fields } => IndexedTerm::Constr {
            index: None,
            tag,
            fields: fields.into_iter().map(value_as_term).collect(),
        },
    }
}

fn with_env(
    lam_cnt: usize,
    env: Env,
    term: IndexedTerm<NamedDeBruijn>,
) -> IndexedTerm<NamedDeBruijn> {
    match term {
        IndexedTerm::Var { index, name } => {
            let idx: usize = name.index.into();

            if lam_cnt >= idx {
                IndexedTerm::Var { index, name }
            } else {
                let idx = NamedDeBruijn {
                    text: name.text.clone(),
                    index: DeBruijn::new(idx - lam_cnt),
                };
                env.get(&idx)
                    .map_or(IndexedTerm::Var { index, name }, value_as_term)
            }
        }
        IndexedTerm::Lambda {
            index,
            parameter_name,
            body,
        } => {
            let body = with_env(lam_cnt + 1, env, body.as_ref().clone());

            IndexedTerm::Lambda {
                index,
                parameter_name,
                body: body.into(),
            }
        }
        IndexedTerm::Apply {
            index,
            function,
            argument,
        } => {
            let function = with_env(lam_cnt, env.clone(), function.as_ref().clone());
            let argument = with_env(lam_cnt, env, argument.as_ref().clone());

            IndexedTerm::Apply {
                index,
                function: function.into(),
                argument: argument.into(),
            }
        }

        IndexedTerm::Delay { index, then } => {
            let then = with_env(lam_cnt, env, then.as_ref().clone());

            IndexedTerm::Delay {
                index,
                then: then.into(),
            }
        }
        IndexedTerm::Force { index, then } => {
            let then = with_env(lam_cnt, env, then.as_ref().clone());

            IndexedTerm::Force {
                index,
                then: then.into(),
            }
        }
        rest => rest,
    }
}
