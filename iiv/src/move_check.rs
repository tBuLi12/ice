use crate::{
    builder::{self, BlockRef, Cursor, UnsealedBlock},
    fun::{self, Function},
    impl_tree::ImplForest,
    ty::{TraitRef, Type, TypeRef},
    Ctx, Elem, Instruction, RawValue,
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
}

impl VarState {
    fn set(&mut self, idx: usize) {
        self.flags[idx] = State::Live;
    }

    fn is_dead(&self, idx: usize) -> bool {
        &self.flags[idx] == &State::Dead
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
            }
        }
        match state {
            State::Live => true,
            _ => false,
        }
    }

    fn partial_unset(&mut self, val: RawValue, mut ty: TypeRef<'_>, path: &[u8]) {
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
                (_, Type::Ref(_)) | (_, Type::Ptr(_)) => return,
                (_, _) => {
                    panic!("OOF {}", ty);
                }
            }
        }
        *state = State::Live;
        self.flags[val.0 as usize].collapse();
    }

    fn process_inst(&mut self, inst: &Instruction<'_>, idx: usize, ty_idx: &[TypeRef<'_>]) {
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
            | Instruction::Call(_, vals, _)
            | Instruction::TraitCall(_, _, vals, _)
            | Instruction::Tuple(vals, _) => {
                for val in vals {
                    self.unset(*val);
                }
            }
            Instruction::Branch(lhs, _, _, args) => {
                self.unset(*lhs);
                for val in args {
                    self.unset(*val);
                }
            }
            Instruction::Switch(idx, _, args) => {
                self.unset(*idx);
                for arg in args {
                    self.unset(*arg);
                }
            }

            Instruction::Invalidate(_, _) | Instruction::CallDrop(_, _) => {
                panic!("invalid instruction")
            }
        }
        if inst.creates_value() {
            self.set(idx);
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

pub fn check<'i>(func: &Function<'i>) {
    eprintln!("{}", func);
    let body = match &func.body {
        fun::Body::Sealed(_) => panic!("move check on sealed body"),
        fun::Body::Unsealed(unsealed) => unsealed,
        _ => return,
    };

    let mut states = vec![None; body.len()];

    let mut first = VarState {
        flags: vec![State::Dead; func.ty_cache.len()],
    };

    for i in 0..func.sig.params.len() {
        first.flags[i] = State::Live;
    }
    states[0] = Some(first);

    let mut stack = vec![0];
    while let Some(next) = stack.pop() {
        let old_state = states[next].clone().unwrap();
        let new_state = process(&body[next], old_state, &func.ty_cache);
        for successor in body[next].successors() {
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

    for block in &states {
        let mut i = 0;

        for flag in &block.as_ref().unwrap().flags {
            print_state(flag, i, 1);
            i += 1;
        }
    }
    for (block, state) in body.iter().zip(states.iter()) {
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
    }
}

pub fn resolve_drops<'i>(
    ctx: &'i Ctx<'i>,
    func: &mut Function<'i>,
    impl_forest: &ImplForest<'i>,
    drop_tr: TraitRef<'i>,
) {
    let body = match &mut func.body {
        fun::Body::Sealed(_) => panic!("resolve drops on sealed body"),
        fun::Body::Unsealed(unsealed) => unsealed,
        _ => return,
    };

    let mut states = vec![None; body.len()];

    let mut first = VarState {
        flags: vec![State::Dead; func.ty_cache.len()],
    };

    for i in 0..func.sig.params.len() {
        first.flags[i] = State::Live;
    }
    states[0] = Some(first);

    let mut stack = vec![0];
    while let Some(next) = stack.pop() {
        let old_state = states[next].clone().unwrap();
        let new_state = process(&body[next], old_state, &func.ty_cache);
        for successor in body[next].successors() {
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

    for (i, state) in states.iter().enumerate() {
        let mut cursor = builder::Cursor::new(&ctx.type_pool, func);
        cursor.select(BlockRef { idx: i });
        cursor.goto_block_start();
        DropResolver {
            state: state.as_ref().unwrap().clone(),
            cursor,
            impl_forest,
            drop_tr,
        }
        .resolve();
    }
}

fn print_state(state: &State, i: usize, depth: usize) {
    for _ in 0..depth {
        eprint!("    ");
    }
    match state {
        State::Dead => {
            eprintln!("%{i} = dead")
        }
        State::Live => {
            eprintln!("%{i} = live")
        }
        State::Kindof(fields) => {
            eprintln!("%{i} = kindof {}", fields.len());
            for (i, field) in fields.iter().enumerate() {
                print_state(field, i, depth + 1)
            }
        }
    }
}

fn process(block: &UnsealedBlock<'_>, mut state: VarState, ty_idx: &[TypeRef<'_>]) -> VarState {
    for (_, idx) in &block.params {
        state.set(*idx as usize);
    }
    for (inst, idx) in &block.instructions {
        state.process_inst(inst, *idx as usize, ty_idx);
    }
    state
}

fn verify(
    block: &UnsealedBlock<'_>,
    mut state: VarState,
    states: &[Option<VarState>],
    ty_idx: &[TypeRef<'_>],
) -> VarState {
    for (_, idx) in &block.params {
        state.set(*idx as usize);
    }

    for (inst, idx) in &block.instructions {
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
                        .filter_map(|elem| match elem {
                            Elem::Prop(prop) => Some(prop.0),
                            Elem::Discriminant => None,
                            _ => panic!("OOF"),
                        })
                        .collect::<Vec<_>>(),
                ),

            Instruction::MoveElem(val, path) => state.is_partially_valid(*val, path),

            // don't
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
            | Instruction::Call(_, vals, _)
            | Instruction::TraitCall(_, _, vals, _)
            | Instruction::Tuple(vals, _) => {
                let mut valid = true;
                for val in vals {
                    valid = valid && state.is_valid(*val);
                }
                valid
            }
            Instruction::Branch(lhs, _, _, args) => {
                let mut valid = state.is_valid(*lhs);
                for val in args {
                    valid = valid && state.is_valid(*val);
                }
                valid
            }
            Instruction::Switch(idx, _, args) => {
                let mut valid = state.is_valid(*idx);
                for arg in args {
                    valid = valid && state.is_valid(*arg);
                }
                valid
            }
            Instruction::Invalidate(_, _) | Instruction::CallDrop(_, _) => {
                panic!("invalid instruction")
            }
        };
        if !valid {
            if inst.creates_value() {
                eprintln!("use after move -> %{} = {:?}", idx, inst);
            } else {
                eprintln!("use after move -> {:?}", inst);
            }
            panic!("use after move.");
        }
        if inst.creates_value() && !state.is_dead(*idx as usize) {
            eprintln!("reinit of undropped value -> %{} = {:?}", idx, inst);
            panic!("invalid reinit");
        }
        state.process_inst(inst, *idx as usize, ty_idx);
    }

    for successor in block.successors() {
        if &states[successor].as_ref().unwrap().flags != &state.flags {
            let mut i = 0;
            for state in &state.flags {
                print_state(&state, i, 0);
                i += 1;
            }
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

struct DropResolver<'f, 'i: 'f, 'forest> {
    state: VarState,
    cursor: builder::Cursor<'f, 'i>,
    impl_forest: &'forest ImplForest<'i>,
    drop_tr: TraitRef<'i>,
}

impl<'f, 'i: 'f, 'forest> DropResolver<'f, 'i, 'forest> {
    fn invalidate(&mut self, val: RawValue, path: Option<Vec<u8>>, base_ty: TypeRef<'i>) {
        let state = self.state.flags[val.0 as usize].clone();
        let path = if let Some(path) = path {
            path
        } else {
            self.cursor.invalidate(val, None);
            vec![]
        };

        self.invalidate_if_needed(val, path, base_ty, &state)
    }

    fn drop(&mut self, val: RawValue, path: Vec<u8>, base_ty: TypeRef<'i>) {
        let state = self.state.flags[val.0 as usize].clone();
        self.drop_if_needed(val, path, base_ty, &state);
    }

    fn call_drop(&mut self, val: RawValue, path: Vec<u8>, ty: TypeRef<'i>) {
        let Some((drop_impl, ty_args)) = self.impl_forest.find(ty, self.drop_tr) else {
            return;
        };
        let drop_impl = &*drop_impl.borrow();
        let drop_fn = &drop_impl.functions[0];

        let val_ref = self
            .cursor
            .get_prop_ref(crate::Value { ty, raw: val }, path, ty);

        self.cursor.call(drop_fn.fun, &[val_ref], ty_args);
    }

    fn drop_if_needed(&mut self, val: RawValue, path: Vec<u8>, ty: TypeRef<'i>, state: &State) {
        self.call_drop(val, path.clone(), ty);

        match (&*ty, state) {
            (_, State::Dead) => {
                return;
            }
            (Type::Struct(fields), State::Live) => {
                for (i, field) in fields.iter().enumerate() {
                    let i = i as u8;
                    let mut path = path.clone();
                    path.push(i);
                    self.drop_if_needed(val, path, field.1, &State::Live);
                }
            }
            (Type::Struct(fields), State::Kindof(field_states)) => {
                for (i, field) in fields.iter().enumerate() {
                    let mut path = path.clone();
                    path.push(i as u8);
                    self.drop_if_needed(val, path, field.1, &field_states[i]);
                }
            }
            (Type::Variant(elems), state) => {
                let state = if let State::Kindof(elem_states) = state {
                    &elem_states[0]
                } else {
                    &State::Live
                };

                let merge_block = self.cursor.split();
                let discriminant = self.cursor.discriminant(crate::Value { ty, raw: val });
                let blocks: Vec<_> = elems.iter().map(|_| self.cursor.create_block()).collect();
                self.cursor.switch(discriminant, &blocks);
                for (i, (&block, elem)) in blocks.iter().zip(elems.iter()).enumerate() {
                    self.cursor.select(block);
                    let i = i as u8;
                    let mut path = path.clone();
                    path.push(i);
                    self.drop_if_needed(val, path, elem.1, state);
                    self.cursor.jump(merge_block, &[]);
                }
                self.cursor.select(merge_block);
                self.cursor.goto_block_start();
            }
            _ => {}
        }
    }

    fn invalidate_if_needed(
        &mut self,
        val: RawValue,
        path: Vec<u8>,
        ty: TypeRef<'i>,
        state: &State,
    ) {
        match (&*ty, state) {
            (_, State::Dead) => {
                return;
            }
            (Type::Struct(fields), State::Live) => {
                for (i, field) in fields.iter().enumerate() {
                    let i = i as u8;
                    let mut path = path.clone();
                    path.push(i);
                    self.invalidate_if_needed(val, path, field.1, &State::Live);
                }
            }
            (Type::Struct(fields), State::Kindof(field_states)) => {
                for (i, field) in fields.iter().enumerate() {
                    let mut path = path.clone();
                    path.push(i as u8);
                    self.invalidate_if_needed(val, path, field.1, &field_states[i]);
                }
            }
            (Type::Variant(elems), state) => {
                let state = if let State::Kindof(elem_states) = state {
                    &elem_states[0]
                } else {
                    &State::Live
                };
                self.cursor.invalidate(val, Some(path.clone()));

                let merge_block = self.cursor.split();
                let discriminant = self.cursor.discriminant(crate::Value { ty, raw: val });
                let blocks: Vec<_> = elems.iter().map(|_| self.cursor.create_block()).collect();
                self.cursor.switch(discriminant, &blocks);
                for (i, (&block, elem)) in blocks.iter().zip(elems.iter()).enumerate() {
                    self.cursor.select(block);
                    let i = i as u8;
                    let mut path = path.clone();
                    path.push(i);
                    self.invalidate_if_needed(val, path, elem.1, state);
                    self.cursor.jump(merge_block, &[]);
                }
                self.cursor.select(merge_block);
                self.cursor.goto_block_start();
            }
            _ => {}
        }
    }

    fn resolve(&mut self) {
        while let Some(inst) = self.cursor.get_current_inst() {
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
                    let rhs = *rhs;
                    let lhs_ty = self.cursor.type_of(*lhs);
                    let rhs_ty = self.cursor.type_of(rhs);
                    self.invalidate(*lhs, None, lhs_ty);
                    self.invalidate(rhs, None, rhs_ty);
                }

                // not sure
                Instruction::Assign(lhs, path, rhs) => {
                    let rhs_ty = self.cursor.type_of(*rhs);
                    let mut lhs_ty = self.cursor.type_of(*lhs);

                    let mut u8_path = vec![];
                    for elem in path {
                        let Elem::Prop(elem) = elem else {
                            panic!("oof")
                        };
                        u8_path.push(elem.0);
                    }

                    let lhs = *lhs;
                    self.invalidate(*rhs, None, rhs_ty);
                    self.drop(lhs, u8_path, rhs_ty);
                }

                Instruction::GetElemRef(_, _) | Instruction::CopyElem(_, _) => {}

                Instruction::MoveElem(val, path) => {
                    let mut ty = self.cursor.type_of(*val);
                    let mut invalidation_path = None;
                    for (i, elem) in path.iter().enumerate() {
                        match &*ty {
                            Type::Struct(props) => {
                                ty = props[*elem as usize].1;
                            }
                            Type::Variant(elems) => {
                                ty = elems[*elem as usize].1;
                                invalidation_path = Some(&path[..i]);
                            }
                            _ => {
                                panic!("oof");
                            }
                        }
                    }

                    self.invalidate(*val, invalidation_path.map(|path| path.to_vec()), ty);
                }

                // don't
                Instruction::Ty(_) => {}
                Instruction::Null => {}

                // do
                Instruction::Name(_, val)
                | Instruction::Return(val)
                | Instruction::Neg(val)
                | Instruction::Variant(_, _, val)
                | Instruction::VariantCast(_, val)
                | Instruction::Not(val) => {
                    let ty = self.cursor.type_of(*val);
                    self.invalidate(*val, None, ty);
                }

                // do
                Instruction::Jump(_, vals)
                | Instruction::Call(_, vals, _)
                | Instruction::TraitCall(_, _, vals, _)
                | Instruction::Tuple(vals, _) => {
                    let vals = vals.clone();
                    for val in vals {
                        let ty = self.cursor.type_of(val);
                        self.invalidate(val, None, ty);
                    }
                }
                Instruction::Branch(lhs, _, _, args) => {
                    let args = args.clone();
                    let lhs_ty = self.cursor.type_of(*lhs);
                    self.invalidate(*lhs, None, lhs_ty);
                    for val in args {
                        let ty = self.cursor.type_of(val);
                        self.invalidate(val, None, ty);
                    }
                }
                Instruction::Switch(idx, _, args) => {
                    let args = args.clone();
                    let idx_ty = self.cursor.type_of(*idx);
                    self.invalidate(*idx, None, idx_ty);
                    for val in args {
                        let ty = self.cursor.type_of(val);
                        self.invalidate(val, None, ty);
                    }
                }
                Instruction::Drop(val) => {
                    let val = *val;
                    self.cursor.delete_inst();
                    let ty = self.cursor.type_of(val);
                    self.invalidate(val, None, ty);
                    continue;
                }
                Instruction::Invalidate(_, _) | Instruction::CallDrop(_, _) => {
                    panic!("invalid instruction")
                }
            };
            self.cursor.advance();
        }
    }
}

pub fn copies_to_moves<'i>(
    func: &mut Function<'i>,
    copy_tr: TraitRef<'i>,
    impl_forest: &ImplForest<'i>,
) {
    let body = match &mut func.body {
        fun::Body::Sealed(_) => panic!("copies to moves on sealed body"),
        fun::Body::Unsealed(unsealed) => unsealed,
        _ => return,
    };

    for block in body {
        for (inst, idx) in &mut block.instructions {
            if let Instruction::CopyElem(value, path) = inst {
                let ty = func.ty_cache[*idx as usize];
                if impl_forest.find(ty, copy_tr).is_none() {
                    let path = std::mem::replace(path, vec![]);
                    let value = *value;
                    *inst = Instruction::MoveElem(
                        value,
                        path.into_iter()
                            .map(|elem| match elem {
                                Elem::Prop(prop) => prop.0,
                                Elem::Index(_) | Elem::Discriminant => panic!("invalid move elem"),
                            })
                            .collect(),
                    )
                }
            }
        }
    }
}

pub fn inject_auto_copy_impl<'i>(ctx: &'i Ctx<'i>, func: &mut Function<'i>) {
    let mut cursor = Cursor::new(&ctx.type_pool, func);
    let value = cursor.params().next().unwrap();
    let Type::Ref(ty) = &*value.ty else {
        panic!("copy impl signature must accept a reference!");
    };
    let copy = match &**ty {
        Type::Named(decl, args, proto) => {
            let copied_proto = cursor.copy_prop_deep(
                value,
                vec![Elem::Prop(crate::Prop(0)), Elem::Prop(crate::Prop(0))],
                *proto,
            );
            cursor.named(*ty, copied_proto)
        }
        Type::Struct(props) => {
            let copied_props: Vec<_> = props
                .iter()
                .enumerate()
                .map(|(i, prop)| {
                    cursor.copy_prop_deep(
                        value,
                        vec![Elem::Prop(crate::Prop(0)), Elem::Prop(crate::Prop(i as u8))],
                        prop.1,
                    )
                })
                .collect();
            cursor.make_struct(&copied_props, *ty)
        }
        Type::Variant(variants) => {
            let merge_block = cursor.create_block();
            let discriminant = cursor.copy_prop_deep(
                value,
                vec![Elem::Prop(crate::Prop(0)), Elem::Discriminant],
                ctx.type_pool.get_int(),
            );
            let blocks: Vec<_> = variants.iter().map(|_| cursor.create_block()).collect();
            cursor.switch(discriminant, &blocks);
            for (i, (&block, elem)) in blocks.iter().zip(variants.iter()).enumerate() {
                cursor.select(block);
                let inner_copy = cursor.copy_prop_deep(
                    value,
                    vec![Elem::Prop(crate::Prop(0)), Elem::Prop(crate::Prop(i as u8))],
                    ctx.type_pool.get_int(),
                );
                let copy = cursor.variant(*ty, i as u64, inner_copy);
                cursor.jump(merge_block, &[copy]);
            }
            cursor.select(merge_block);
            cursor.block_param(*ty)
        }
        Type::Tuple(_) => unimplemented!(),
        Type::Union(_) => unimplemented!(),
        _ => panic!("invalid auto copy impl type"),
    };
    cursor.ret(copy);
}
