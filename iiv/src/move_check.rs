use crate::{
    builder::Block,
    fun::{self, Function},
    ty::{Type, TypeRef},
    Elem, Instruction, Prop, RawValue,
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum State {
    Dead,
    Live,
    Kindof(Vec<State>),
}

impl State {
    fn collapse(&mut self) {
        if let State::Kindof(fields) = self {
            for field in &mut *fields {
                field.collapse();
            }
            if fields.iter().all(|state| state == &State::Live) {
                *self = State::Live;
            } else if fields.iter().all(|state| state == &State::Dead) {
                *self = State::Dead;
            }
        }
    }

    fn merge(&mut self, other: Self) -> bool {
        match (self, other) {
            (this, State::Dead) => {
                *this = State::Dead;
                true
            }
            (State::Dead, _) | (State::Live, State::Live) => false,
            (State::Kindof(_), State::Live) => false,
            (this @ State::Live, other @ State::Kindof(_)) => {
                *this = other;
                true
            }
            (State::Kindof(left), State::Kindof(right)) => {
                let mut changed = false;
                for (l, r) in left.iter_mut().zip(right.into_iter()) {
                    changed = changed || l.merge(r);
                }
                changed
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct VarState {
    flags: Vec<State>,
    next_inst_idx: usize,
}

impl VarState {
    fn set_next(&mut self) {
        self.flags[self.next_inst_idx] = State::Live;
        println!("making {} live", self.next_inst_idx);
        self.next_inst_idx += 1;
    }

    fn next_is_dead(&self) -> bool {
        &self.flags[self.next_inst_idx] == &State::Dead
    }

    fn unset(&mut self, val: RawValue) {
        self.flags[val.0 as usize] = State::Dead;
    }

    fn is_valid(&self, val: RawValue) -> bool {
        match &self.flags[val.0 as usize] {
            State::Live => true,
            _ => false,
        }
    }

    fn is_partially_valid(&self, val: RawValue, path: &[Elem]) -> bool {
        let mut state = &self.flags[val.0 as usize];
        for elem in path {
            match (elem, state) {
                (_, State::Live) => return true,
                (_, State::Dead) => return false,
                (Elem::Prop(prop), State::Kindof(states)) => {
                    state = &states[prop.0 as usize];
                }
                _ => panic!("OOF"),
            }
        }
        match &self.flags[val.0 as usize] {
            State::Live => true,
            _ => false,
        }
    }

    fn partial_unset(&mut self, val: RawValue, mut ty: TypeRef<'_>, path: &[Elem]) {
        println!("PARTIAL UNSETTING {:?} {:?}", ty, path);

        let mut state = &mut self.flags[val.0 as usize];
        for elem in path {
            match (elem, &*ty) {
                (Elem::Prop(prop), Type::Struct(fields)) => {
                    let break_up = match state {
                        State::Live => true,
                        State::Kindof(_) => false,
                        State::Dead => break,
                    };
                    if break_up {
                        *state = State::Kindof(vec![State::Live; fields.len()]);
                    }
                    ty = fields[prop.0 as usize].1;
                    let State::Kindof(fields) = state else {
                        unreachable!()
                    };
                    state = &mut fields[prop.0 as usize];
                }
                (Elem::Prop(prop), Type::Variant(fields)) => {
                    let break_up = match state {
                        State::Live => true,
                        State::Kindof(_) => false,
                        State::Dead => break,
                    };
                    if break_up {
                        *state = State::Kindof(vec![State::Live]);
                    }
                    ty = fields[prop.0 as usize].1;
                    let State::Kindof(fields) = state else {
                        unreachable!()
                    };
                    state = &mut fields[0];
                }
                (_, _) => {
                    panic!("OOF {}", ty);
                }
            }
        }
        *state = State::Dead;
        self.flags[val.0 as usize].collapse();
    }

    fn partial_set(&mut self, val: RawValue, mut ty: TypeRef<'_>, path: &[Elem]) {
        println!("PARTIAL SETTING {:?} {:?}", ty, path);

        let mut state = &mut self.flags[val.0 as usize];
        for elem in path {
            match (elem, &*ty) {
                (Elem::Prop(prop), Type::Struct(fields)) => {
                    let break_up = match state {
                        State::Live => break,
                        State::Kindof(_) => false,
                        State::Dead => true,
                    };
                    if break_up {
                        *state = State::Kindof(vec![State::Dead; fields.len()]);
                    }
                    ty = fields[prop.0 as usize].1;
                    let State::Kindof(fields) = state else {
                        unreachable!()
                    };
                    state = &mut fields[prop.0 as usize];
                }
                (Elem::Prop(prop), Type::Variant(fields)) => {
                    let break_up = match state {
                        State::Live => break,
                        State::Kindof(_) => false,
                        State::Dead => true,
                    };
                    if break_up {
                        *state = State::Kindof(vec![State::Dead]);
                    }
                    ty = fields[prop.0 as usize].1;
                    let State::Kindof(fields) = state else {
                        unreachable!()
                    };
                    state = &mut fields[0];
                }
                (_, _) => {
                    panic!("OOF {}", ty);
                }
            }
        }
        *state = State::Live;
        self.flags[val.0 as usize].collapse();
    }

    fn process_inst(&mut self, inst: &Instruction<'_>) {
        match inst {
            Instruction::Int(_) | Instruction::Bool(_) => {}
            Instruction::Add(lhs, rhs)
            | Instruction::Sub(lhs, rhs)
            | Instruction::Mul(lhs, rhs)
            | Instruction::Div(lhs, rhs)
            | Instruction::Eq(lhs, rhs)
            | Instruction::Neq(lhs, rhs)
            | Instruction::Gt(lhs, rhs)
            | Instruction::Lt(lhs, rhs)
            | Instruction::GtEq(lhs, rhs)
            | Instruction::LtEq(lhs, rhs) => {
                self.unset(*lhs);
                self.unset(*rhs);
            }

            // not sure
            Instruction::Assign(lhs, ty, elems, rhs) => {
                self.unset(*rhs);
                self.partial_set(*lhs, *ty, &elems);
            }
            Instruction::GetElemRef(_val, _) | Instruction::CopyElem(_val, _) => {}

            Instruction::MoveElem(val, ty, elems) => {
                self.partial_unset(*val, *ty, &elems);
            }

            // don't
            Instruction::Discriminant(_val) => {}
            Instruction::Ty(_) => {}
            Instruction::Null => {}

            // do
            Instruction::Name(_, val)
            | Instruction::Return(val)
            | Instruction::Neg(val)
            | Instruction::Variant(_, _, val)
            | Instruction::VariantCast(_, val)
            | Instruction::Drop(val)
            | Instruction::Not(val) => {
                self.unset(*val);
            }

            // do
            Instruction::Jump(_, vals)
            | Instruction::Call(_, vals)
            | Instruction::Tuple(vals, _) => {
                for val in vals {
                    self.unset(*val);
                }
            }
            Instruction::Branch(lhs, _, yes_args, _, no_args) => {
                self.unset(*lhs);
                for val in yes_args {
                    self.unset(*val);
                }
                for val in no_args {
                    self.unset(*val);
                }
            }

            Instruction::Invalidate(_, _) | Instruction::CallDrop(_, _) => {
                panic!("invalid instruction")
            }
        }
        if inst.creates_value() {
            println!("set_next by {:?}", inst);
            self.set_next();
        }
    }

    fn merge(&mut self, other: Self) -> bool {
        let mut changed = false;
        for (state, other) in self.flags.iter_mut().zip(other.flags.into_iter()) {
            changed = changed || state.merge(other);
        }
        changed
    }
}

pub fn check(func: &Function) {
    // return;
    let mut states = vec![None; func.body.len()];

    let starting_idx: Vec<usize> = func
        .body
        .iter()
        .scan(func.sig.params.len(), |state, block| {
            let current: usize = *state;

            *state += block.params.len()
                + block
                    .instructions
                    .iter()
                    .filter(|inst| inst.creates_value())
                    .count();

            Some(current)
        })
        .collect();

    dbg!(&starting_idx);

    let mut first = VarState {
        flags: vec![State::Dead; func.value_count],
        next_inst_idx: func.sig.params.len(),
    };

    for i in 0..func.sig.params.len() {
        first.flags[i] = State::Live;
    }
    states[0] = Some(first);

    let mut stack = vec![0];
    while let Some(next) = stack.pop() {
        println!("processing block b{}", next);
        let old_state = states[next].clone().unwrap();
        let mut new_state = process(&func.body[next], old_state);
        for successor in func.body[next].successors() {
            new_state.next_inst_idx = starting_idx[successor];

            if let Some(state) = &mut states[successor] {
                if state.merge(new_state.clone()) {
                    stack.push(successor);
                }
            } else {
                states[successor] = Some(new_state.clone());
                stack.push(successor);
            }
        }
    }

    let mut block_idx = 0;
    for block in &states {
        println!("at entry of block {block_idx}:");
        let mut i = 0;

        for flag in &block.as_ref().unwrap().flags {
            print_state(flag, i, 1);
            i += 1;
        }
        block_idx += 1;
        println!("");
    }
    let mut block_idx = 0;
    for (block, state) in func.body.iter().zip(states.iter()) {
        println!(
            "verifying block {block_idx}, at {}",
            state.as_ref().unwrap().next_inst_idx
        );
        let after = verify(block, state.as_ref().unwrap().clone(), &states);

        if let Some(Instruction::Return(_)) = block.instructions.last() {
            for (i, flag) in after.flags.iter().enumerate() {
                if flag != &State::Dead {
                    eprintln!("undropped value {}", i);
                    panic!("undropped values");
                }
            }
        }

        block_idx += 1;
    }
}

fn print_state(state: &State, i: usize, depth: usize) {
    for _ in 0..depth {
        print!("    ");
    }
    match state {
        State::Dead => {
            println!("%{i} = dead")
        }
        State::Live => {
            println!("%{i} = live")
        }
        State::Kindof(fields) => {
            println!("%{i} = kindof {}", fields.len());
            for (i, field) in fields.iter().enumerate() {
                print_state(field, i, depth + 1)
            }
        }
    }
}

fn process(block: &Block<'_>, mut state: VarState) -> VarState {
    for _ in &block.params {
        println!("set_next by block_param");
        state.set_next();
    }
    for inst in &block.instructions {
        state.process_inst(inst);
    }
    state
}

fn verify(block: &Block<'_>, mut state: VarState, states: &[Option<VarState>]) -> VarState {
    for _ in &block.params {
        println!("set_next by block_param2");
        state.set_next();
    }

    for inst in &block.instructions {
        let valid = match inst {
            Instruction::Int(_) | Instruction::Bool(_) => true,
            Instruction::Add(lhs, rhs)
            | Instruction::Sub(lhs, rhs)
            | Instruction::Mul(lhs, rhs)
            | Instruction::Div(lhs, rhs)
            | Instruction::Eq(lhs, rhs)
            | Instruction::Neq(lhs, rhs)
            | Instruction::Gt(lhs, rhs)
            | Instruction::Lt(lhs, rhs)
            | Instruction::GtEq(lhs, rhs)
            | Instruction::LtEq(lhs, rhs) => state.is_valid(*lhs) && state.is_valid(*rhs),

            // not sure
            Instruction::Assign(_, _, _, rhs) => state.is_valid(*rhs),

            Instruction::GetElemRef(val, path)
            | Instruction::CopyElem(val, path)
            | Instruction::MoveElem(val, _, path) => state.is_partially_valid(*val, path),

            // don't
            Instruction::Discriminant(val) => state.is_valid(*val),
            Instruction::Ty(_) => true,
            Instruction::Null => true,

            // do
            Instruction::Name(_, val)
            | Instruction::Return(val)
            | Instruction::Neg(val)
            | Instruction::Variant(_, _, val)
            | Instruction::VariantCast(_, val)
            | Instruction::Not(val) => state.is_valid(*val),

            Instruction::Drop(_val) => true,

            // do
            Instruction::Jump(_, vals)
            | Instruction::Call(_, vals)
            | Instruction::Tuple(vals, _) => {
                let mut valid = true;
                for val in vals {
                    valid = valid && state.is_valid(*val);
                }
                valid
            }
            Instruction::Branch(lhs, _, yes_args, _, no_args) => {
                let mut valid = state.is_valid(*lhs);
                for val in yes_args {
                    valid = valid && state.is_valid(*val);
                }
                for val in no_args {
                    valid = valid && state.is_valid(*val);
                }
                valid
            }
            Instruction::Invalidate(_, _) | Instruction::CallDrop(_, _) => {
                panic!("invalid instruction")
            }
        };
        if !valid {
            if inst.creates_value() {
                eprintln!("use after move -> %{} = {:?}", state.next_inst_idx, inst);
            } else {
                eprintln!("use after move -> {:?}", inst);
            }
            panic!("use after move.");
        }
        if inst.creates_value() && !state.next_is_dead() {
            eprintln!(
                "reinit of undropped value -> %{} = {:?}",
                state.next_inst_idx, inst
            );
            panic!("invalid reinit");
        }
        state.process_inst(inst);
    }

    for successor in block.successors() {
        if &states[successor].as_ref().unwrap().flags != &state.flags {
            println!("jumping to: {}", successor);
            let mut i = 0;
            for state in &state.flags {
                print_state(&state, i, 0);
                i += 1;
            }
            println!("");
            i = 0;
            for state in &states[successor].as_ref().unwrap().flags {
                print_state(&state, i, 0);
                i += 1;
            }
            panic!("conditional move!");
        }
    }

    state
}

fn resolve_drops(block: &mut Block<'_>, mut state: VarState) {
    let mut new_instructions = vec![];
    for inst in &block.instructions {
        let valid = match inst {
            Instruction::Int(_) | Instruction::Bool(_) => {}
            Instruction::Add(lhs, rhs)
            | Instruction::Sub(lhs, rhs)
            | Instruction::Mul(lhs, rhs)
            | Instruction::Div(lhs, rhs)
            | Instruction::Eq(lhs, rhs)
            | Instruction::Neq(lhs, rhs)
            | Instruction::Gt(lhs, rhs)
            | Instruction::Lt(lhs, rhs)
            | Instruction::GtEq(lhs, rhs)
            | Instruction::LtEq(lhs, rhs) => {
                new_instructions.push(Instruction::Invalidate(*lhs, vec![]));
                new_instructions.push(Instruction::Invalidate(*rhs, vec![]));
            }

            // not sure
            Instruction::Assign(lhs, _, path, rhs) => {
                new_instructions.push(Instruction::Invalidate(*rhs, vec![]));
            }

            Instruction::GetElemRef(val, path) | Instruction::CopyElem(val, path) => {}

            Instruction::MoveElem(val, ty, path) => {
                let mut offset = 0;
                for (i, elem) in path.iter().enumerate() {
                    match *ty {
                        Type::Struct(props) => {}
                        Type::Variant(elems) => {}
                    }
                }

                unimplemented!();
                new_instructions.push(Instruction::Invalidate(*val, path.clone()));
            }

            // don't
            Instruction::Discriminant(val) => {}
            Instruction::Ty(_) => {}
            Instruction::Null => {}

            // do
            Instruction::Name(_, val)
            | Instruction::Return(val)
            | Instruction::Neg(val)
            | Instruction::Variant(_, _, val)
            | Instruction::VariantCast(_, val)
            | Instruction::Not(val) => {
                new_instructions.push(Instruction::Invalidate(*val, vec![]));
            }

            // do
            Instruction::Jump(_, vals)
            | Instruction::Call(_, vals)
            | Instruction::Tuple(vals, _) => {
                for val in vals {
                    new_instructions.push(Instruction::Invalidate(*val, vec![]));
                }
            }
            Instruction::Branch(lhs, _, yes_args, _, no_args) => {
                for val in yes_args {
                    new_instructions.push(Instruction::Invalidate(*val, vec![]));
                }
                for val in no_args {
                    new_instructions.push(Instruction::Invalidate(*val, vec![]));
                }
            }
            Instruction::Drop(val) => {
                new_instructions.push(Instruction::Invalidate(*val, vec![]));
            }
            Instruction::Invalidate(_, _) | Instruction::CallDrop(_, _) => {
                panic!("invalid instruction")
            }
        };

        new_instructions.push(inst.clone());
    }
}
