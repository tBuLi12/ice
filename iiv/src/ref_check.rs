// struct InstReader<I>(I);

// impl<I: Iterator<Item = u8>> InstReader<I> {
//     fn read_u16(&mut self) -> Option<u16> {
//         Some((self.0.next()? as u16) & 8 >> (self.0.next()? as u16))
//     }

//     fn read_u32(&mut self) -> Option<u32> {
//         Some((self.read_u16()? as u32) & 16 >> (self.read_u16()? as u32))
//     }

//     fn read_func_id(&mut self) -> Option<FuncID> {
//         Some(FuncID(self.read_u32()?))
//     }

//     fn read_type_id(&mut self) -> Option<TypeID> {
//         Some(TypeID(self.read_u32()?))
//     }

//     fn read_value(&mut self) -> Option<Value> {
//         Some(Value(self.read_u16()?))
//     }

//     fn read_label(&mut self) -> Option<Label> {
//         Some(Label(self.read_u16()?))
//     }

//     fn read_values(&mut self) -> Option<Vec<Value>> {
//         (0..(self.read_u16()?)).map(|_| self.read_value()).collect()
//     }

//     fn read_elems(&mut self) -> Option<Vec<Elem>> {
//         (0..(self.read_u16()?)).map(|_| self.read_value()).collect()
//     }

//     fn read_instruction(&mut self) -> Option<Instruction> {
//         let tag = self.read_u16()?;
//         Some(match tag {
//             0 => Instruction::Add(self.read_value()?, self.read_value()?),
//             1 => Instruction::Sub(self.read_value()?, self.read_value()?),
//             2 => Instruction::Mul(self.read_value()?, self.read_value()?),
//             3 => Instruction::Div(self.read_value()?, self.read_value()?),
//             4 => Instruction::Not(self.read_value()?),
//             5 => Instruction::Neg(self.read_value()?),
//             6 => Instruction::Eq(self.read_value()?, self.read_value()?),
//             7 => Instruction::Neq(self.read_value()?, self.read_value()?),
//             8 => Instruction::Gt(self.read_value()?, self.read_value()?),
//             9 => Instruction::Lt(self.read_value()?, self.read_value()?),
//             10 => Instruction::GtEq(self.read_value()?, self.read_value()?),
//             11 => Instruction::LtEq(self.read_value()?, self.read_value()?),
//             12 => Instruction::Push(self.read_value()?, self.read_value()?),
//             13 => Instruction::Call(self.read_func_id()?, self.read_values()?),
//             14 => Instruction::Assign(self.read_value()?, self.read_value()?),
//             15 => Instruction::RefAssign(self.read_value()?, self.read_value()?),
//             16 => Instruction::Aggregate(self.read_values()?),
//             17 => Instruction::Name(self.read_type_id()?, self.read_value()?),
//             18 => Instruction::Vec(self.read_values()?),
//             // 19 => Instruction::GetElem(Value, Vec<Elem>),
//             // 20 => Instruction::GetElemRef(Value, Vec<Elem>),
//             21 => Instruction::Branch(self.read_value()?, self.read_label()?, self.read_label()?),
//             22 => Instruction::Break(self.read_label()?),
//             23 => Instruction::Return(self.read_value()?),
//             _ => return None,
//         })
//     }
// }

// struct Block(Vec<Instruction>);

use std::{
    iter::Scan,
    ops::{Add, Sub},
};

use crate::{Instruction, Prop, RawValue};

#[derive(Clone, PartialEq, Eq)]
struct Path(RawValue, Vec<Prop>);

impl Path {
    /// 'contains' in the context of memory, not the string representation (eg. this.prop contains this.prop.nested)
    fn contains(&self, other: &Self) -> bool {
        false
    }

    fn try_remove_prefix(&self, prefix: &Self) -> Option<Vec<Prop>> {
        unimplemented!()
    }

    fn append(&mut self, tail: Vec<Prop>) {}

    fn is_var(&self, val: RawValue) -> bool {
        self.1.is_empty() && self.0 == val
    }
}

impl Sub<Path> for Path {
    type Output = Option<Vec<Prop>>;

    fn sub(self, rhs: Path) -> Self::Output {
        None
    }
}

impl Add<Vec<Prop>> for Path {
    type Output = Path;

    fn add(mut self, rhs: Vec<Prop>) -> Self::Output {
        self.1.extend(rhs);
        self
    }
}

struct RefState {
    value: Path,
    sources: Vec<Path>,
    is_valid: bool,
}

struct RefSet(Vec<RefState>);

impl RefSet {
    fn write(&mut self, srcs: &[Path], dsts: &[Path]) {
        self.invalidate_refs_in(dsts);

        for i in 0..self.0.len() {
            for dst in dsts {
                if let Some(tail) = self.0[i].value.clone() - dst.clone() {
                    if dsts.len() == 1 {
                        self.0[i].sources.clear();
                    }
                    for src in srcs {
                        let src_i = self.get_idx(&(src.clone() + tail.clone()));
                        if i == src_i {
                            continue;
                        }
                        unsafe {
                            let state = &mut *self.0.as_mut_ptr().add(i);
                            let src_state = &mut *self.0.as_mut_ptr().add(src_i);
                            state.sources.extend(src_state.sources.clone());
                            state.sources.dedup();
                        }
                    }
                }
            }
        }
    }

    fn get(&self, path: &Path) -> &RefState {
        self.0.iter().find(|state| &state.value == path).unwrap()
    }

    fn get_idx(&self, path: &Path) -> usize {
        self.0
            .iter()
            .position(|state| &state.value == path)
            .unwrap()
    }

    fn invalidate_refs_in(&mut self, set: &[Path]) {
        for state in &mut self.0 {
            if state
                .sources
                .iter()
                .find(|src| set.iter().find(|dst| dst.contains(&src)).is_some())
                .is_some()
            {
                state.is_valid = false;
            }
        }
    }

    fn get_underlying_paths_old(&self, value: Path) -> Vec<Path> {
        let mut stack = vec![value];
        let mut paths = vec![];

        while let Some(path) = stack.pop() {
            let matched_path = self.0.iter().find_map(|ref_state| {
                path.try_remove_prefix(&ref_state.value)
                    .map(|rest| (rest, ref_state))
            });
            match matched_path {
                Some((rest, ref_state)) => {
                    for source in &ref_state.sources {
                        let mut new_source = source.clone();
                        new_source.append(rest.clone());
                        stack.push(new_source);
                    }
                }
                None => paths.push(path),
            }
        }

        paths
    }

    fn get_underlying_paths(&self, value: RawValue) -> Vec<Path> {
        self.0
            .iter()
            .find_map(|state| {
                if state.value.is_var(value) {
                    Some(state.sources.clone())
                } else {
                    None
                }
            })
            .unwrap_or_else(|| vec![Path(value, vec![])])
    }
}

/*
    when a variable is written to:
        get the underlying storage of that variable
            if it's a variable it's just itself
            if it's a refernce, get the source list for it
        write to the underlying storage
            each reference whose source list includes a written to path gets invalidated
                a path must go from the written to path to the referenced path,
                though a variant, union member or vector item reference
*/

fn process(mut input: RefSet, block: &[Instruction]) -> RefSet {
    for inst in block {
        match inst {
            Instruction::Call(func_id, args) => {}
            Instruction::Assign(dst, src) => {
                let dsts = input.get_underlying_paths(*dst);
                let srcs = input.get_underlying_paths(*src);
                input.write(&srcs, &dsts);
            }
            Instruction::RefAssign(dst, src) => {
                let paths = input.get_underlying_paths(*dst);
                // input.write_to(&paths);
            }
            _ => {}
        }
    }
    input
}
