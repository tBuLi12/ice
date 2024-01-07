use iiv::{
    err,
    fun::Bound,
    impl_tree::ImplForest,
    pool::List,
    ty::{PropRef, Type, TypeOverlap, TypeRef},
    Span,
};

#[derive(Debug)]
struct Goal<'i> {
    span: Span,
    bound: Bound<'i>,
    givens: Vec<Bound<'i>>,
}

pub struct InferenceCtx<'i> {
    vars: Vec<Option<TypeRef<'i>>>,
    goals: Vec<Goal<'i>>,
    ty_pool: &'i iiv::ty::Pool<'i>,
    messages: &'i iiv::diagnostics::Diagnostics,
}

pub struct EqAttempt<'c, 'i> {
    ctx: &'c mut InferenceCtx<'i>,
    vars: Vec<Option<TypeRef<'i>>>,
    error: bool,
}

impl<'i> InferenceCtx<'i> {
    pub fn new(
        ty_pool: &'i iiv::ty::Pool<'i>,
        messages: &'i iiv::diagnostics::Diagnostics,
    ) -> Self {
        InferenceCtx {
            vars: vec![],
            goals: vec![],
            ty_pool,
            messages,
        }
    }

    pub fn new_var(&mut self) -> TypeRef<'i> {
        let var = self.ty_pool.get_ty_inference_var(self.vars.len());
        self.vars.push(None);
        var
    }

    pub fn new_bound(&mut self, span: Span, bound: Bound<'i>, givens: Vec<Bound<'i>>) {
        self.goals.push(Goal {
            bound,
            givens,
            span,
        });
    }

    pub fn check_bounds(&mut self, impls: &ImplForest<'i>) {
        for mut goal in std::mem::replace(&mut self.goals, vec![]) {
            goal.bound.ty = self.unwrap(goal.bound.ty);
            goal.bound.tr.1 = self.ty_pool.get_ty_list(self.unwrap_list(goal.bound.tr.1));
            eprintln!("trying: {:?}", goal);
            if !goal.givens.iter().any(|given| {
                let mut given = *given;
                given.ty = self.unwrap(given.ty);
                given.tr.1 = self.ty_pool.get_ty_list(self.unwrap_list(given.tr.1));
                goal.bound == given
            }) && impls.find(goal.bound.ty, goal.bound.tr).is_none()
            {
                self.messages
                    .add(err!(&goal.span, "unsatisfied trait bound"))
            }
        }
    }

    pub fn clear(&mut self, impls: &ImplForest<'i>) {
        self.check_bounds(impls);
        self.vars.clear();
    }

    pub fn try_eq<'c>(&'c mut self) -> EqAttempt<'c, 'i> {
        let vars = self.vars.clone();
        EqAttempt {
            ctx: self,
            vars,
            error: false,
        }
    }

    pub fn get_intersection(&self, ty: TypeRef<'i>, other: TypeRef<'i>) -> TypeOverlap {
        match (&*ty, &*other) {
            (Type::Builtin(types), Type::Builtin(types2)) => {
                if types == types2 {
                    TypeOverlap::Complete
                } else {
                    TypeOverlap::None
                }
            }
            (Type::Vector(ty), Type::Vector(ty2)) => self.get_intersection(*ty, *ty2),
            (Type::Tuple(t1), Type::Tuple(t2)) | (Type::Union(t1), Type::Union(t2)) => {
                if t1.len() != t2.len() {
                    return TypeOverlap::None;
                }
                t1.iter()
                    .zip(t2.iter())
                    .map(|(&p1, &p2)| self.get_intersection(p1, p2))
                    .fold(TypeOverlap::Complete, std::cmp::min)
            }
            (Type::Struct(props), Type::Struct(props2))
            | (Type::Variant(props), Type::Variant(props2)) => {
                if props.len() != props2.len() {
                    return TypeOverlap::None;
                }
                props
                    .iter()
                    .zip(props2.iter())
                    .map(|(&p1, &p2)| {
                        if p1.0 != p2.0 {
                            return TypeOverlap::None;
                        }
                        self.get_intersection(p1.1, p2.1)
                    })
                    .fold(TypeOverlap::Complete, std::cmp::min)
            }
            (Type::Ref(ty), Type::Ref(ty2)) => self.get_intersection(*ty, *ty2),
            (Type::Named(decl1, args1, _), Type::Named(decl2, args2, _)) => {
                if decl1 != decl2 || args1.len() != args2.len() {
                    TypeOverlap::None
                } else {
                    args1
                        .iter()
                        .zip(args2.iter())
                        .map(|(&t1, &t2)| self.get_intersection(t1, t2))
                        .fold(TypeOverlap::Complete, std::cmp::min)
                }
            }
            (Type::Type(s1), Type::Type(s2)) => {
                if s1 == s2 {
                    TypeOverlap::Complete
                } else {
                    TypeOverlap::None
                }
            }
            (Type::Constant(val), Type::Constant(val2)) => {
                if val == val2 {
                    TypeOverlap::Complete
                } else {
                    TypeOverlap::Partial
                }
            }
            (_, Type::Constant(_)) | (Type::Constant(_), _) => TypeOverlap::Partial,
            (_, Type::InferenceVar(_)) | (Type::InferenceVar(_), _) => TypeOverlap::Partial,
            // the rest is listed explicitly to cause errors when more kinds are added, instead of silently returning None
            (Type::Invalid, _)
            | (Type::Tuple(_), _)
            | (Type::Struct(_), _)
            | (Type::Vector(_), _)
            | (Type::Union(_), _)
            | (Type::Named(_, _, _), _)
            | (Type::Variant(_), _)
            | (Type::Ref(_), _)
            | (Type::Type(_), _)
            | (Type::Builtin(_), _) => TypeOverlap::None,
        }
    }

    pub fn resolve(&self, ty: TypeRef<'i>) -> TypeRef<'i> {
        match &*ty {
            Type::Builtin(_) | Type::Constant(_) | Type::Invalid => ty,
            Type::Vector(_) => unimplemented!(),
            Type::Tuple(types) => self
                .ty_pool
                .get_tuple(types.iter().map(|&ty| self.resolve(ty)).collect()),
            Type::Union(types) => self
                .ty_pool
                .get_union(types.iter().map(|&ty| self.resolve(ty)).collect()),
            Type::Struct(props) => self.ty_pool.get_struct(
                props
                    .iter()
                    .map(|&prop| self.ty_pool.get_prop(prop.0, prop.1))
                    .collect(),
            ),
            Type::Variant(props) => self.ty_pool.get_variant(
                props
                    .iter()
                    .map(|&prop| self.ty_pool.get_prop(prop.0, prop.1))
                    .collect(),
            ),
            Type::Named(decl, args, _) => {
                let args = args.iter().map(|&ty| self.resolve(ty)).collect();
                self.ty_pool.get_ty_named(*decl, args)
            }
            Type::Ref(ty) => self.ty_pool.get_ref(self.resolve(*ty)),
            Type::InferenceVar(idx) => {
                if let Some(ty) = self.vars[*idx] {
                    self.resolve(ty)
                } else {
                    ty
                }
            }
            Type::Type(_) => {
                unimplemented!()
            }
        }
    }

    pub fn contains_var(ty: TypeRef<'i>, idx: usize) -> bool {
        ty.contains(|ty| match &*ty {
            Type::InferenceVar(other_idx) => idx == *other_idx,
            _ => false,
        })
    }

    pub fn unwrap_list(&self, ty_list: List<'i, TypeRef<'i>>) -> Vec<TypeRef<'i>> {
        ty_list.iter().map(|&ty| self.unwrap(ty)).collect()
    }

    pub fn unwrap_prop_list(&self, prop_list: List<'i, PropRef<'i>>) -> Vec<PropRef<'i>> {
        prop_list
            .iter()
            .map(|prop| self.ty_pool.get_prop(prop.0, self.unwrap(prop.1)))
            .collect()
    }

    pub fn unwrap(&self, ty: TypeRef<'i>) -> TypeRef<'i> {
        match &*ty {
            Type::Builtin(_) | Type::Constant(_) | Type::Invalid => ty,
            Type::Vector(_) => unimplemented!(),
            Type::Union(types) => self.ty_pool.get_union(self.unwrap_list(*types)),
            Type::Tuple(types) => self.ty_pool.get_tuple(self.unwrap_list(*types)),
            Type::Named(decl, types, _) => {
                self.ty_pool.get_ty_named(*decl, self.unwrap_list(*types))
            }

            Type::Variant(props) => self.ty_pool.get_variant(self.unwrap_prop_list(*props)),
            Type::Struct(props) => self.ty_pool.get_struct(self.unwrap_prop_list(*props)),
            Type::Ref(ty) => self.ty_pool.get_ref(self.unwrap(*ty)),
            Type::InferenceVar(idx) => {
                if let Some(ty) = self.vars[*idx] {
                    self.unwrap(ty)
                } else {
                    panic!("Could not infer :c");
                }
            }
            Type::Type(_) => {
                unimplemented!()
            }
        }
    }

    pub fn eq(&mut self, at: &iiv::Span, t1: TypeRef<'i>, t2: TypeRef<'i>) -> bool {
        let mut eq = self.try_eq();
        if eq.eq(t1, t2) {
            eq.commit(at);
            true
        } else {
            false
        }
    }
}

impl<'c, 'i> EqAttempt<'c, 'i> {
    pub fn eq(&mut self, t1: TypeRef<'i>, t2: TypeRef<'i>) -> bool {
        if t1 == t2 {
            return true;
        }
        match (&*t1, &*t2) {
            (Type::Builtin(types), Type::Builtin(types2)) => types == types2,
            (Type::Union(types), Type::Union(types2))
            | (Type::Tuple(types), Type::Tuple(types2)) => {
                if types.len() != types2.len() {
                    false
                } else {
                    types
                        .iter()
                        .zip(types2.iter())
                        .all(|(&t1, &t2)| self.eq(t1, t2))
                }
            }
            (Type::Variant(props), Type::Variant(props2))
            | (Type::Struct(props), Type::Struct(props2)) => {
                if props.len() != props2.len() {
                    false
                } else {
                    props
                        .iter()
                        .zip(props2.iter())
                        .all(|(&p1, &p2)| p1.0 == p1.0 && self.eq(p1.1, p2.1))
                }
            }
            (Type::Ref(ty), Type::Ref(ty2)) | (Type::Vector(ty), Type::Vector(ty2)) => {
                self.eq(*ty, *ty2)
            }
            (Type::Named(decl1, args1, _), Type::Named(decl2, args2, _)) => {
                if decl1 != decl2 || args1.len() != args2.len() {
                    false
                } else {
                    args1
                        .iter()
                        .zip(args2.iter())
                        .all(|(&t1, &t2)| self.eq(t1, t2))
                }
            }
            (Type::Type(_), Type::Type(_)) => {
                unimplemented!()
            }
            (Type::Constant(val), Type::Constant(val2)) => val == val2,
            (Type::InferenceVar(idx), Type::InferenceVar(idx2)) => {
                idx == idx2
                    || match (&self.vars[*idx], &self.vars[*idx2]) {
                        (Some(t1), Some(t2)) => self.eq(*t1, *t2),
                        (None, Some(ty)) => {
                            self.set(*idx, *ty);
                            true
                        }
                        (Some(ty), None) => {
                            self.set(*idx2, *ty);
                            true
                        }
                        (None, None) => {
                            let common = self.ctx.new_var();
                            self.vars[*idx] = Some(common);
                            self.vars[*idx2] = Some(common);
                            true
                        }
                    }
            }
            (Type::InferenceVar(idx), _) => match self.vars[*idx] {
                Some(ty) => self.eq(ty, t2),
                None => {
                    self.set(*idx, t2);
                    true
                }
            },
            (_, Type::InferenceVar(idx)) => match self.vars[*idx] {
                Some(ty) => self.eq(ty, t1),
                None => {
                    self.set(*idx, t1);
                    true
                }
            },
            // the rest is listed explicitly to cause errors when more kinds are added, instead of silently returning None
            (Type::Invalid, _)
            | (Type::Tuple(_), _)
            | (Type::Struct(_), _)
            | (Type::Vector(_), _)
            | (Type::Union(_), _)
            | (Type::Variant(_), _)
            | (Type::Named(_, _, _), _)
            | (Type::Ref(_), _)
            | (Type::Type(_), _)
            | (Type::Constant(_), _)
            | (Type::Builtin(_), _) => false,
        }
    }

    fn set(&mut self, idx: usize, ty: TypeRef<'i>) {
        if InferenceCtx::contains_var(ty, idx) {
            self.vars[idx] = Some(self.ctx.ty_pool.get_ty_invalid());
            self.error = true;
        } else {
            self.vars[idx] = Some(ty);
        }
    }

    pub fn commit(self, at: &iiv::Span) {
        if self.error {
            self.ctx.messages.add(err!(at, "implied cyclic type"))
        }
        self.ctx.vars = self.vars;
    }
}
