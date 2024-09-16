use std::{borrow::Borrow, collections::BTreeMap, rc::Rc};

use serde::Serialize;
use uplc::{
    ast::{Constant, DeBruijn, Name, Program, Term},
    builtins::DefaultFunction,
};

#[derive(Debug, PartialEq, Default, Clone, Serialize)]
#[serde(transparent)]
pub struct SourceMap {
    locations: BTreeMap<u64, String>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn extract(program: Program<DeBruijn>) -> (Self, Program<DeBruijn>) {
        let name_program: Program<Name> = program.try_into().unwrap();

        let mut trace = vec![];
        let mut index = 0;
        let mut locations = BTreeMap::new();
        let new_term = traverse(&name_program.term, &mut trace, &mut index, &mut locations);

        let new_program = Program {
            version: name_program.version,
            term: new_term,
        };

        (Self { locations }, new_program.try_into().unwrap())
    }
}

fn traverse(
    term: &Term<Name>,
    trace: &mut Vec<Rc<Name>>,
    index: &mut u64,
    locations: &mut BTreeMap<u64, String>,
) -> Term<Name> {
    // If this is our source-location trace statement, track the location

    // We are trying to match [[<trace> (con string "__sourcemap blahblahblah")] <body>],
    // where <trace> is either the "trace" builtin or an alias for it
    let Term::Apply {
        function: outer_func,
        argument: outer_arg,
    } = term
    else {
        return base_traverse(term, trace, index, locations);
    };

    // Sometimes aiken uses lambdas to make an alias for builtin functions like trace.
    // Keep track of any of those aliases for later.
    if is_trace(outer_arg, trace) {
        if let Term::Lambda { parameter_name, .. } = outer_func.borrow() {
            trace.push(parameter_name.clone());
        }
    }

    let Term::Apply { function, argument } = outer_func.borrow() else {
        return base_traverse(term, trace, index, locations);
    };

    if !is_trace(function, trace) {
        return base_traverse(term, trace, index, locations);
    }
    let Term::Constant(constant) = argument.borrow() else {
        return base_traverse(term, trace, index, locations);
    };
    let Constant::String(str) = constant.borrow() else {
        return base_traverse(term, trace, index, locations);
    };
    let Some(source_location) = str.strip_prefix("__sourcemap ") else {
        return base_traverse(term, trace, index, locations);
    };

    // this is definitely a sourcemap trace statement
    locations.insert(*index, source_location.to_string());
    base_traverse(outer_arg, trace, index, locations)
}

fn base_traverse(
    term: &Term<Name>,
    trace: &mut Vec<Rc<Name>>,
    index: &mut u64,
    locations: &mut BTreeMap<u64, String>,
) -> Term<Name> {
    *index += 1;
    match term {
        Term::Delay(then) => {
            let then = traverse(then, trace, index, locations).into();
            Term::Delay(then)
        }
        Term::Lambda {
            parameter_name,
            body,
        } => {
            let parameter_name = parameter_name.clone();
            let body = traverse(body, trace, index, locations).into();
            Term::Lambda {
                parameter_name,
                body,
            }
        }
        Term::Apply { function, argument } => {
            let function = traverse(function, trace, index, locations).into();
            let argument = traverse(argument, trace, index, locations).into();
            Term::Apply { function, argument }
        }
        Term::Force(then) => {
            let then = traverse(then, trace, index, locations).into();
            Term::Force(then)
        }
        Term::Constr { tag, fields } => {
            let tag = *tag;
            let fields = fields
                .iter()
                .map(|f| traverse(f, trace, index, locations))
                .collect();
            Term::Constr { tag, fields }
        }
        Term::Case { constr, branches } => {
            let constr = traverse(constr, trace, index, locations).into();
            let branches = branches
                .iter()
                .map(|b| traverse(b, trace, index, locations))
                .collect();
            Term::Case { constr, branches }
        }
        Term::Var(_) | Term::Constant(_) | Term::Error | Term::Builtin(_) => term.clone(),
    }
}

fn is_trace<T: Eq>(term: &Term<T>, trace: &Vec<Rc<T>>) -> bool {
    match term {
        Term::Force(then) => is_trace(then, trace),
        Term::Builtin(DefaultFunction::Trace) => true,
        Term::Var(name) => trace.contains(name),
        _ => false,
    }
}
