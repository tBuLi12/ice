use std::cell::UnsafeCell;

use crate::{
    diagnostics::Diagnostics,
    err,
    fun::Bound,
    pool::{self, TraitImplRef},
    ty::{TraitRef, Type, TypeOverlap, TypeRef},
    ty_decl::TraitImpl,
    Ctx, Span,
};

#[derive(Clone)]
pub struct ImplTreeNode<'i> {
    impl_ref: TraitImplRef<'i>,
    children: Vec<ImplTreeNode<'i>>,
}

pub struct CellImplForest<'i>(UnsafeCell<Option<ImplForest<'i>>>);

impl<'i> CellImplForest<'i> {
    pub fn new() -> Self {
        Self(UnsafeCell::new(None))
    }

    pub fn set_ctx(&self, ctx: &'i Ctx<'i>) {
        let this = unsafe { &mut *self.0.get() };
        *this = Some(ImplForest::new(ctx));
    }

    pub fn is_satisfied(
        &self,
        ty: TypeRef<'i>,
        tr: TraitRef<'i>,
        local_bounds: &[Bound<'i>],
    ) -> bool {
        let this = unsafe { &*self.0.get() }.as_ref().unwrap();
        this.is_satisfied(ty, tr, local_bounds)
    }

    pub fn find(
        &self,
        ty: TypeRef<'i>,
        tr: TraitRef<'i>,
    ) -> Option<(TraitImplRef<'i>, pool::List<'i, TypeRef<'i>>)> {
        let this = unsafe { &*self.0.get() }.as_ref().unwrap();
        this.find(ty, tr)
    }

    pub fn add(&self, span: &Span, impl_ref: TraitImplRef<'i>) {
        let this = unsafe { &mut *self.0.get() }.as_mut().unwrap();
        this.add(span, impl_ref)
    }
}

pub struct ImplForest<'i> {
    roots: Vec<ImplTreeNode<'i>>,
    ctx: &'i Ctx<'i>,
    messages: &'i Diagnostics,
}

impl<'i> ImplForest<'i> {
    fn find_explicit(
        &self,
        ty: TypeRef<'i>,
        tr: TraitRef<'i>,
        local_bounds: &[Bound<'i>],
        checking: &mut Vec<Bound<'i>>,
    ) -> Option<(TraitImplRef<'i>, pool::List<'i, TypeRef<'i>>)> {
        self.roots
            .iter()
            .find_map(|root| root.find(ty, tr, self.ctx, local_bounds, self, checking))
            .map(|(impl_ref, args)| (impl_ref, self.ctx.type_pool.get_ty_list(args)))
    }

    fn is_satisfied_given(
        &self,
        ty: TypeRef<'i>,
        tr: TraitRef<'i>,
        local_bounds: &[Bound<'i>],
        checking: &mut Vec<Bound<'i>>,
    ) -> bool {
        if local_bounds
            .iter()
            .find(|bound| **bound == Bound { ty, tr })
            .is_some()
        {
            return true;
        }

        if checking
            .iter()
            .find(|bound| **bound == Bound { ty, tr })
            .is_some()
        {
            return false;
        }
        checking.push(Bound { ty, tr });
        let was_found = self
            .find_with_givens(ty, tr, local_bounds, checking)
            .is_some();
        checking.pop();
        was_found
    }

    pub fn is_satisfied(
        &self,
        ty: TypeRef<'i>,
        tr: TraitRef<'i>,
        local_bounds: &[Bound<'i>],
    ) -> bool {
        let mut checking = vec![];
        self.is_satisfied_given(ty, tr, local_bounds, &mut checking)
    }

    pub fn find(
        &self,
        ty: TypeRef<'i>,
        tr: TraitRef<'i>,
    ) -> Option<(TraitImplRef<'i>, pool::List<'i, TypeRef<'i>>)> {
        let mut checking = vec![];
        self.find_with_givens(ty, tr, &[], &mut checking)
    }

    fn find_with_givens(
        &self,
        ty: TypeRef<'i>,
        tr: TraitRef<'i>,
        local_bounds: &[Bound<'i>],
        checking: &mut Vec<Bound<'i>>,
    ) -> Option<(TraitImplRef<'i>, pool::List<'i, TypeRef<'i>>)> {
        if let Some(found) = self.find_explicit(ty, tr, local_bounds, checking) {
            return Some(found);
        }

        if tr != self.ctx.builtins.get_copy() {
            return None;
        }

        let bitwise_copy_impl = self.ctx.builtins.get_bitwise_copy();
        let auto_copy_impl = self.ctx.builtins.get_bitwise_copy();
        let auto_copy_impl_option = (auto_copy_impl, self.ctx.type_pool.get_ty_list(vec![ty]));
        let bitwise_copy_impl_option =
            (bitwise_copy_impl, self.ctx.type_pool.get_ty_list(vec![ty]));

        let get_copy_impl_for_list = |types: &mut dyn Iterator<Item = TypeRef<'i>>| {
            types
                .map(|prop| self.find(prop, self.ctx.builtins.get_copy()))
                .collect::<Option<Vec<_>>>()
                .map(|impls| {
                    if impls
                        .iter()
                        .all(|(impl_ref, _)| *impl_ref == bitwise_copy_impl)
                    {
                        bitwise_copy_impl_option
                    } else {
                        auto_copy_impl_option
                    }
                })
        };

        match &*ty {
            Type::Builtin(_) => Some(bitwise_copy_impl_option),
            Type::Constant(_) => None,
            Type::Ref(_) => Some(bitwise_copy_impl_option),
            Type::Ptr(_) => Some(bitwise_copy_impl_option),
            Type::Struct(props) => get_copy_impl_for_list(&mut props.iter().map(|prop| prop.1)),
            Type::Tuple(fields) => get_copy_impl_for_list(&mut fields.iter().copied()),
            Type::Vector(_) => unimplemented!(),
            Type::Union(_) => unimplemented!(),
            Type::Named(decl, _, proto) => {
                if decl.is_copy {
                    let proto_impl = self.find(*proto, self.ctx.builtins.get_copy()).unwrap();
                    if proto_impl.0 == bitwise_copy_impl {
                        Some(bitwise_copy_impl_option)
                    } else {
                        Some(auto_copy_impl_option)
                    }
                } else {
                    None
                }
            }
            Type::Variant(variants) => {
                get_copy_impl_for_list(&mut variants.iter().map(|prop| prop.1))
            }
            Type::Invalid => Some(bitwise_copy_impl_option),
            Type::Type(_) => unimplemented!(),
            Type::InferenceVar(_) => panic!("invalid bound target - inf variable"),
        }
    }

    pub fn add(&mut self, span: &Span, impl_ref: TraitImplRef<'i>) {
        match ImplTreeNode::add_to_children(
            &self.ctx.type_pool,
            &mut self.roots,
            ImplTreeNode {
                impl_ref,
                children: vec![],
            },
        ) {
            NewImplResult::Added => {}
            NewImplResult::Ambiguous => self.messages.add(err!(span, "ambiguous implementation")),
            NewImplResult::Duplicate => self.messages.add(err!(span, "duplicate implementation")),
            NewImplResult::NotAdded | NewImplResult::ReplacedSelf => unreachable!(),
        }
    }

    pub fn new(ctx: &'i Ctx<'i>) -> Self {
        ImplForest {
            roots: vec![],
            ctx,
            messages: &ctx.diagnostcs,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Overlap {
    Equal,
    LeftMoreSpecific,
    RightMoreSpecific,
    Ambiguous,
    Disjoint,
}

impl Overlap {
    fn and(self, other: Self) -> Self {
        match (self, other) {
            (Overlap::Equal, overlap) | (overlap, Overlap::Equal) => overlap,
            (Overlap::Disjoint, _) | (_, Overlap::Disjoint) => Overlap::Disjoint,
            (Overlap::Ambiguous, _) | (_, Overlap::Ambiguous) => Overlap::Ambiguous,
            (left, right) => {
                if left == right {
                    left
                } else {
                    Overlap::Ambiguous
                }
            }
        }
    }
}

enum NewImplResult {
    Ambiguous,
    Duplicate,
    Added,
    NotAdded,
    ReplacedSelf,
}

impl<'i> ImplTreeNode<'i> {
    pub fn find(
        &self,
        ty: TypeRef<'i>,
        tr: TraitRef<'i>,
        ctx: &'i Ctx<'i>,
        local_bounds: &[Bound<'i>],
        forest: &ImplForest<'i>,
        checking: &mut Vec<Bound<'i>>,
    ) -> Option<(TraitImplRef<'i>, Vec<TypeRef<'i>>)> {
        if let Some(args) =
            self.impl_ref
                .borrow()
                .try_match(ty, tr, ctx, local_bounds, forest, checking)
        {
            for child in &self.children {
                if let Some(args) = child.find(ty, tr, ctx, local_bounds, forest, checking) {
                    return Some(args);
                }
            }

            return Some((self.impl_ref, args));
        } else {
            None
        }
    }

    fn try_add(
        &mut self,
        ty_pool: &'i crate::ty::Pool<'i>,
        impl_node: ImplTreeNode<'i>,
    ) -> NewImplResult {
        let overlap = Self::compare_impls(ty_pool, impl_node.impl_ref, self.impl_ref);

        match overlap {
            Overlap::RightMoreSpecific => {
                let new_child = std::mem::replace(self, impl_node);
                self.children.push(new_child);
                NewImplResult::ReplacedSelf
            }
            Overlap::LeftMoreSpecific => {
                Self::add_to_children(ty_pool, &mut self.children, impl_node)
            }
            Overlap::Ambiguous => NewImplResult::Ambiguous,
            Overlap::Equal => NewImplResult::Duplicate,
            Overlap::Disjoint => NewImplResult::NotAdded,
        }
    }

    fn add_to_children(
        ty_pool: &'i crate::ty::Pool<'i>,
        children: &mut Vec<ImplTreeNode<'i>>,
        impl_node: ImplTreeNode<'i>,
    ) -> NewImplResult {
        for (mut i, child) in children.iter_mut().enumerate() {
            match child.try_add(ty_pool, impl_node.clone()) {
                NewImplResult::NotAdded => {}
                NewImplResult::Ambiguous => {
                    children.push(impl_node);
                    return NewImplResult::Ambiguous;
                }
                NewImplResult::ReplacedSelf => {
                    let mut replaced = children.swap_remove(i);
                    let mut result = NewImplResult::Added;
                    while i < children.len() {
                        match replaced.try_add(ty_pool, children[i].clone()) {
                            NewImplResult::NotAdded => {}
                            NewImplResult::Added => {
                                children.swap_remove(i);
                                continue;
                            }
                            NewImplResult::Ambiguous => {
                                result = NewImplResult::Ambiguous;
                            }
                            NewImplResult::Duplicate | NewImplResult::ReplacedSelf => {
                                panic!("invalid tree state")
                            }
                        }
                        i += 1;
                    }
                    children.push(replaced);
                    return result;
                }
                other => return other,
            }
        }
        children.push(impl_node);
        NewImplResult::Added
    }

    fn compare_impls(
        ty_pool: &'i crate::ty::Pool<'i>,
        left: TraitImplRef<'i>,
        right: TraitImplRef<'i>,
    ) -> Overlap {
        let left = &*left.borrow();
        let right = &*right.borrow();

        if left.tr.decl != right.tr.decl {
            return Overlap::Disjoint;
        }

        {
            let mut left_ty_param_map = vec![None; left.ty_params.len()];
            let mut right_ty_param_map = vec![None; right.ty_params.len()];
            let mut overlap = Self::compare_types(
                &mut left_ty_param_map,
                &mut right_ty_param_map,
                left.ty,
                right.ty,
            );
            for (&left_ty, &right_ty) in left.tr.ty_args.iter().zip(right.tr.ty_args.iter()) {
                overlap = Self::compare_types(
                    &mut left_ty_param_map,
                    &mut right_ty_param_map,
                    left_ty,
                    right_ty,
                )
                .and(overlap);
            }

            match overlap {
                Overlap::Ambiguous | Overlap::Disjoint => {
                    return overlap;
                }
                Overlap::Equal | Overlap::RightMoreSpecific => {
                    let map = left_ty_param_map
                        .into_iter()
                        .map(Option::unwrap)
                        .collect::<Vec<_>>();
                    let left_mapped: Vec<_> = left
                        .trait_bounds
                        .iter()
                        .map(|&bound| ty_pool.resolve_bound(bound, &map))
                        .collect();
                    return Self::compare_bounds(&left_mapped, &right.trait_bounds).and(overlap);
                }
                Overlap::LeftMoreSpecific => {
                    eprintln!("left");
                    let map = right_ty_param_map
                        .into_iter()
                        .map(Option::unwrap)
                        .collect::<Vec<_>>();
                    let right_mapped: Vec<_> = right
                        .trait_bounds
                        .iter()
                        .map(|&bound| ty_pool.resolve_bound(bound, &map))
                        .collect();
                    eprintln!(
                        "{:?}",
                        Self::compare_bounds(&left.trait_bounds, &right_mapped)
                    );
                    eprintln!("{:?} {:?}", &left.trait_bounds, &right_mapped);
                    return Self::compare_bounds(&left.trait_bounds, &right_mapped).and(overlap);
                }
            }
        };
    }

    fn compare_bounds(left: &[Bound<'i>], right: &[Bound<'i>]) -> Overlap {
        let left_must = left.iter().all(|&bound| right.iter().any(|&b| b == bound));
        let right_must = right.iter().all(|&bound| left.iter().any(|&b| b == bound));
        match (left_must, right_must) {
            (true, true) => Overlap::Equal,
            (false, true) => Overlap::RightMoreSpecific,
            (true, false) => Overlap::LeftMoreSpecific,
            (false, false) => Overlap::Ambiguous,
        }
    }

    fn compare_types(
        left_ty_param_map: &mut [Option<TypeRef<'i>>],
        right_ty_param_map: &mut [Option<TypeRef<'i>>],
        left: TypeRef<'i>,
        right: TypeRef<'i>,
    ) -> Overlap {
        match (&*left, &*right) {
            (Type::Builtin(types), Type::Builtin(types2)) => {
                if types == types2 {
                    Overlap::Equal
                } else {
                    Overlap::Disjoint
                }
            }
            (Type::Vector(ty), Type::Vector(ty2)) => unimplemented!(),
            (Type::Tuple(t1), Type::Tuple(t2)) | (Type::Union(t1), Type::Union(t2)) => {
                if t1.len() != t2.len() {
                    return Overlap::Disjoint;
                }
                t1.iter()
                    .zip(t2.iter())
                    .fold(Overlap::Equal, |overlap, (&p1, &p2)| {
                        let other =
                            Self::compare_types(left_ty_param_map, right_ty_param_map, p1, p2);
                        overlap.and(other)
                    })
            }
            (Type::Struct(props), Type::Struct(props2))
            | (Type::Variant(props), Type::Variant(props2)) => {
                if props.len() != props2.len() {
                    return Overlap::Disjoint;
                }
                props
                    .iter()
                    .zip(props2.iter())
                    .fold(Overlap::Equal, |overlap, (&p1, &p2)| {
                        if p1.0 != p2.0 {
                            return Overlap::Disjoint;
                        }
                        let other =
                            Self::compare_types(left_ty_param_map, right_ty_param_map, p1.1, p2.1);
                        overlap.and(other)
                    })
            }
            (Type::Ref(ty), Type::Ref(ty2)) => {
                Self::compare_types(left_ty_param_map, right_ty_param_map, *ty, *ty2)
            }
            (Type::Ptr(ty), Type::Ptr(ty2)) => {
                Self::compare_types(left_ty_param_map, right_ty_param_map, *ty, *ty2)
            }
            (Type::Named(decl1, args1, _), Type::Named(decl2, args2, _)) => {
                if decl1 != decl2 || args1.len() != args2.len() {
                    return Overlap::Disjoint;
                }
                args1
                    .iter()
                    .zip(args2.iter())
                    .fold(Overlap::Equal, |overlap, (&p1, &p2)| {
                        let other =
                            Self::compare_types(left_ty_param_map, right_ty_param_map, p1, p2);
                        overlap.and(other)
                    })
            }
            (Type::Type(s1), Type::Type(s2)) => {
                unimplemented!()
            }
            (Type::Constant(val), Type::Constant(other)) => {
                match (left_ty_param_map[*val], right_ty_param_map[*other]) {
                    (Some(left), Some(right)) => {
                        Self::compare_types(left_ty_param_map, right_ty_param_map, right, left)
                    }
                    (Some(left_mapped), None) => {
                        right_ty_param_map[*other] = Some(left);
                        match left_mapped.get_intersection(right) {
                            TypeOverlap::Complete => Overlap::Equal,
                            TypeOverlap::Partial => Overlap::Ambiguous,
                            TypeOverlap::None => Overlap::Disjoint,
                        }
                    }
                    (None, Some(righ_mappedt)) => {
                        left_ty_param_map[*val] = Some(right);
                        match left.get_intersection(righ_mappedt) {
                            TypeOverlap::Complete => Overlap::Equal,
                            TypeOverlap::Partial => Overlap::Ambiguous,
                            TypeOverlap::None => Overlap::Disjoint,
                        }
                    }
                    (None, None) => {
                        left_ty_param_map[*val] = Some(right);
                        right_ty_param_map[*other] = Some(left);
                        Overlap::Equal
                    }
                }
            }
            (Type::Constant(val), _) => {
                if let Some(ty) = left_ty_param_map[*val] {
                    match ty.get_intersection(right) {
                        TypeOverlap::Complete => Overlap::Equal,
                        TypeOverlap::Partial => Overlap::Ambiguous,
                        TypeOverlap::None => Overlap::Disjoint,
                    }
                } else {
                    left_ty_param_map[*val] = Some(right);
                    Overlap::RightMoreSpecific
                }
            }
            (_, Type::Constant(val)) => {
                if let Some(ty) = right_ty_param_map[*val] {
                    match ty.get_intersection(left) {
                        TypeOverlap::Complete => Overlap::Equal,
                        TypeOverlap::Partial => Overlap::Ambiguous,
                        TypeOverlap::None => Overlap::Disjoint,
                    }
                } else {
                    right_ty_param_map[*val] = Some(left);
                    Overlap::LeftMoreSpecific
                }
            }
            (_, Type::InferenceVar(_)) | (Type::InferenceVar(_), _) => {
                panic!("unexpected inf_var")
            }
            (Type::Invalid, _)
            | (Type::Tuple(_), _)
            | (Type::Struct(_), _)
            | (Type::Vector(_), _)
            | (Type::Union(_), _)
            | (Type::Named(_, _, _), _)
            | (Type::Variant(_), _)
            | (Type::Ref(_), _)
            | (Type::Ptr(_), _)
            | (Type::Type(_), _)
            | (Type::Builtin(_), _) => Overlap::Disjoint,
        }
    }
}

impl<'i> TraitImpl<'i> {
    pub fn try_match(
        &self,
        ty: TypeRef<'i>,
        tr: TraitRef<'i>,
        ctx: &'i Ctx<'i>,
        local_bounds: &[Bound<'i>],
        forest: &ImplForest<'i>,
        checking: &mut Vec<Bound<'i>>,
    ) -> Option<Vec<TypeRef<'i>>> {
        if tr.decl != self.tr.decl {
            eprintln!("no trait match");
            return None;
        }

        let mut args = vec![None; self.ty_params.len()];

        if !Self::match_ty(&mut args, self.ty, ty)
            && self
                .tr
                .ty_args
                .iter()
                .zip(tr.ty_args.iter())
                .all(|(&matched, &ty)| Self::match_ty(&mut args, matched, ty))
        {
            eprintln!("no arg match");
            return None;
        }

        let args: Vec<_> = args.into_iter().map(Option::unwrap).collect();
        if self
            .trait_bounds
            .iter()
            .map(|bound| ctx.type_pool.resolve_bound(*bound, &args))
            .any(|bound| !forest.is_satisfied_given(bound.ty, bound.tr, local_bounds, checking))
        {
            eprintln!("bounds not satisfied");
            return None;
        }

        Some(args)
    }

    fn match_ty(args: &mut [Option<TypeRef<'i>>], matched: TypeRef<'i>, ty: TypeRef<'i>) -> bool {
        match (&*matched, &*ty) {
            (Type::Builtin(types), Type::Builtin(types2)) => types == types2,
            (Type::Vector(ty), Type::Vector(ty2)) => unimplemented!(),
            (Type::Tuple(t1), Type::Tuple(t2)) | (Type::Union(t1), Type::Union(t2)) => {
                t1.len() == t2.len()
                    && t1
                        .iter()
                        .zip(t2.iter())
                        .all(|(&p1, &p2)| Self::match_ty(args, p1, p2))
            }
            (Type::Struct(props), Type::Struct(props2))
            | (Type::Variant(props), Type::Variant(props2)) => {
                props.len() != props2.len()
                    && props
                        .iter()
                        .zip(props2.iter())
                        .all(|(&p1, &p2)| p1.0 == p2.0 && Self::match_ty(args, p1.1, p2.1))
            }
            (Type::Ref(ty), Type::Ref(ty2)) | (Type::Ptr(ty), Type::Ptr(ty2)) => {
                Self::match_ty(args, *ty, *ty2)
            }
            (Type::Named(decl1, args1, _), Type::Named(decl2, args2, _)) => {
                decl1 == decl2
                    && args1.len() == args2.len()
                    && args1
                        .iter()
                        .zip(args2.iter())
                        .all(|(&t1, &t2)| Self::match_ty(args, t1, t2))
            }
            (Type::Type(s1), Type::Type(s2)) => {
                unimplemented!()
            }
            (Type::Constant(val), _) => {
                if let Some(matched) = args[*val] {
                    matched == ty
                } else {
                    args[*val] = Some(ty);
                    true
                }
            }
            (_, Type::InferenceVar(_)) | (Type::InferenceVar(_), _) => {
                panic!("unexpected inf_var")
            }
            (_, Type::Constant(_))
            | (Type::Invalid, _)
            | (Type::Tuple(_), _)
            | (Type::Struct(_), _)
            | (Type::Vector(_), _)
            | (Type::Union(_), _)
            | (Type::Named(_, _, _), _)
            | (Type::Variant(_), _)
            | (Type::Ref(_), _)
            | (Type::Ptr(_), _)
            | (Type::Type(_), _)
            | (Type::Builtin(_), _) => false,
        }
    }
}
