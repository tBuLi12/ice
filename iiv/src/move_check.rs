use crate::{
    builder::{Block, UnsealedBlock},
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

    fn is_partially_valid(&self, val: RawValue, path: &[u8]) -> bool {
        let mut state = &self.flags[val.0 as usize];
        for elem in path {
            match (elem, state) {
                (_, State::Live) => return true,
                (_, State::Dead) => return false,
                (prop, State::Kindof(states)) => {
                    state = &states[*prop as usize];
                }
                _ => panic!("OOF"),
            }
        }
        match &self.flags[val.0 as usize] {
            State::Live => true,
            _ => false,
        }
    }

    fn partial_unset(&mut self, val: RawValue, mut ty: TypeRef<'_>, path: &[u8]) {
        println!("PARTIAL UNSETTING {:?} {:?}", ty, path);

        let mut state = &mut self.flags[val.0 as usize];
        for &elem in path {
            match &*ty {
                Type::Struct(fields) => {
                    let break_up = match state {
                        State::Live => true,
                        State::Kindof(_) => false,
                        State::Dead => break,
                    };
                    if break_up {
                        *state = State::Kindof(vec![State::Live; fields.len()]);
                    }
                    ty = fields[elem as usize].1;
                    let State::Kindof(fields) = state else {
                        unreachable!()
                    };
                    state = &mut fields[elem as usize];
                }
                Type::Variant(fields) => {
                    let break_up = match state {
                        State::Live => true,
                        State::Kindof(_) => false,
                        State::Dead => break,
                    };
                    if break_up {
                        *state = State::Kindof(vec![State::Live]);
                    }
                    ty = fields[elem as usize].1;
                    let State::Kindof(fields) = state else {
                        unreachable!()
                    };
                    state = &mut fields[0];
                }
                _ => {
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
                (_, Type::Ref(_)) => return,
                (_, _) => {
                    panic!("OOF {}", ty);
                }
            }
        }
        *state = State::Live;
        self.flags[val.0 as usize].collapse();
    }

    fn process_inst(&mut self, inst: &Instruction<'_>, ty_idx: &[TypeRef<'_>]) {
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
            Instruction::Assign(lhs, elems, rhs) => {
                let ty = ty_idx[lhs.0 as usize];
                self.unset(*rhs);
                self.partial_set(*lhs, ty, &elems);
            }
            Instruction::GetElemRef(_val, _) | Instruction::CopyElem(_val, _) => {}

            Instruction::MoveElem(val, elems) => {
                let ty = ty_idx[val.0 as usize];
                self.partial_unset(*val, ty, &elems);
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
            Instruction::Switch(idx, targets) => {
                self.unset(*idx);
                for (_, args) in targets {
                    for val in args {
                        self.unset(*val);
                    }
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

pub fn check(func: &mut Function) {
    let body = match &mut func.body {
        fun::Body::Sealed(_) => panic!("move check on sealed body"),
        fun::Body::Unsealed(unsealed) => unsealed,
    };

    // return;
    let mut states = vec![None; body.len()];

    let starting_idx: Vec<usize> = body
        .iter()
        .scan(func.sig.params.len(), |state, block| {
            let current: usize = *state;

            *state += block.params.len()
                + block
                    .instructions
                    .iter()
                    .filter(|(inst, _)| inst.creates_value())
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
        let mut new_state = process(&body[next], old_state, &func.ty_cache);
        for successor in body[next].successors() {
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
    for (block, state) in body.iter().zip(states.iter()) {
        println!(
            "verifying block {block_idx}, at {}",
            state.as_ref().unwrap().next_inst_idx
        );
        let after = verify(
            block,
            state.as_ref().unwrap().clone(),
            &states,
            &func.ty_cache,
        );

        if let Some((Instruction::Return(_), _)) = block.instructions.last() {
            for (i, flag) in after.flags.iter().enumerate() {
                if flag != &State::Dead {
                    eprintln!("undropped value {}", i);
                    panic!("undropped values");
                }
            }
        }

        block_idx += 1;
    }

    for (block, state) in body.iter_mut().zip(states.iter()) {
        DropResolver {
            state: state.as_ref().unwrap().clone(),
            instructions: vec![],
        }
        .resolve(block, &func.ty_cache);
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

fn process(block: &UnsealedBlock<'_>, mut state: VarState, ty_idx: &[TypeRef<'_>]) -> VarState {
    for _ in &block.params {
        println!("set_next by block_param");
        state.set_next();
    }
    for (inst, _) in &block.instructions {
        state.process_inst(inst, ty_idx);
    }
    state
}

fn verify(
    block: &UnsealedBlock<'_>,
    mut state: VarState,
    states: &[Option<VarState>],
    ty_idx: &[TypeRef<'_>],
) -> VarState {
    for _ in &block.params {
        println!("set_next by block_param2");
        state.set_next();
    }

    for (inst, _) in &block.instructions {
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
            Instruction::Assign(_, _, rhs) => state.is_valid(*rhs),

            Instruction::GetElemRef(val, path) | Instruction::CopyElem(val, path) => state
                .is_partially_valid(
                    *val,
                    &path
                        .iter()
                        .map(|elem| match elem {
                            Elem::Prop(prop) => prop.0,
                            _ => panic!("OOF"),
                        })
                        .collect::<Vec<_>>(),
                ),

            Instruction::MoveElem(val, path) => state.is_partially_valid(*val, path),

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
            Instruction::Switch(idx, targets) => {
                let mut valid = state.is_valid(*idx);
                for (_, args) in targets {
                    for val in args {
                        valid = valid && state.is_valid(*val);
                    }
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
        state.process_inst(inst, ty_idx);
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

struct DropResolver<'i> {
    state: VarState,
    instructions: Vec<Instruction<'i>>,
}

impl<'i> DropResolver<'i> {
    fn invalidate(&mut self, val: RawValue, path: Option<Vec<u8>>, ty_idx: &[TypeRef<'_>]) {
        self.instructions
            .push(Instruction::Invalidate(val, path.clone()));

        let mut base_ty = ty_idx[val.0 as usize];
        let (mut path, mut ty) = if let Some(path) = path {
            let mut ty = base_ty;
            for elem in &path {
                ty = ty.elem(*elem).unwrap();
            }
            (path, ty)
        } else {
            if let Type::Variant(_) = &*base_ty {
                self.instructions
                    .push(Instruction::Invalidate(val, Some(vec![])));
            }
            (vec![], base_ty)
        };

        match &*ty {
            Type::Struct(fields) => {}
            Type::Variant(elems) => {}
            _ => {}
        }
    }

    fn invalidate_if_needed(&mut self, val: RawValue, path: Vec<u8>, ty: TypeRef<'i>) {
        match &*ty {
            Type::Struct(fields) => {
                for (i, field) in fields.iter().enumerate() {
                    let i = i as u8;
                    let mut path = path.clone();
                    path.push(i);
                    self.invalidate_if_needed(val, path, ty.elem(i).unwrap());
                }
            }
            Type::Variant(elems) => {
                self.instructions
                    .push(Instruction::Invalidate(val, Some(path.clone())));

                let discriminant = RawValue(unimplemented!());
                for (i, field) in elems.iter().enumerate() {
                    let i = i as u8;
                    let mut path = path.clone();
                    path.push(i);
                    self.invalidate_if_needed(val, path, ty.elem(i).unwrap());
                }
            }
            _ => {}
        }
    }

    fn resolve(&mut self, block: &mut UnsealedBlock, ty_idx: &[TypeRef<'i>]) {
        let mut new_instructions = vec![];
        for (inst, _) in &block.instructions {
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
                    self.invalidate(*lhs, None, ty_idx);
                    self.invalidate(*rhs, None, ty_idx);
                }

                // not sure
                Instruction::Assign(lhs, path, rhs) => {
                    self.invalidate(*rhs, None, ty_idx);
                }

                Instruction::GetElemRef(val, path) | Instruction::CopyElem(val, path) => {}

                Instruction::MoveElem(val, path) => {
                    let ty = ty_idx[val.0 as usize];
                    let mut invalidation_path = None;
                    let mut offset = 0;
                    for (i, elem) in path.iter().enumerate() {
                        match &*ty {
                            Type::Struct(props) => {
                                offset += 1;
                            }
                            Type::Variant(elems) => {
                                invalidation_path = Some(&path[..offset]);
                                offset += 1;
                            }
                            _ => {
                                panic!("oof");
                            }
                        }
                    }

                    self.invalidate(*val, invalidation_path.map(|path| path.to_vec()), ty_idx);
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
                    self.invalidate(*val, None, ty_idx);
                }

                // do
                Instruction::Jump(_, vals)
                | Instruction::Call(_, vals)
                | Instruction::Tuple(vals, _) => {
                    for val in vals {
                        self.invalidate(*val, None, ty_idx);
                    }
                }
                Instruction::Branch(lhs, _, yes_args, _, no_args) => {
                    self.invalidate(*lhs, None, ty_idx);
                    for val in yes_args {
                        self.invalidate(*val, None, ty_idx);
                    }
                    for val in no_args {
                        self.invalidate(*val, None, ty_idx);
                    }
                }
                Instruction::Switch(idx, targets) => {
                    self.invalidate(*idx, None, ty_idx);
                    for (_, args) in targets {
                        for val in args {
                            self.invalidate(*val, None, ty_idx);
                        }
                    }
                }
                Instruction::Drop(val) => {
                    self.invalidate(*val, None, ty_idx);
                }
                Instruction::Invalidate(_, _) | Instruction::CallDrop(_, _) => {
                    panic!("invalid instruction")
                }
            };

            new_instructions.push((inst.clone(), 0));
        }

        block.instructions = new_instructions;
    }
}
