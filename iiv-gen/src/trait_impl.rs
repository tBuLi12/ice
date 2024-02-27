use ast::Spanned;
use iiv::{
    err,
    errors::AnyError,
    errors::IceError,
    fun::{Bound, Method, Receiver},
    pool::{FuncRef, TraitImplRef},
    str::Str,
    ty::{TraitRef, TypeRef},
    ty_decl::TraitImpl,
    Ctx, Span,
};

use std::iter;

pub struct TraitImplBuilder<'i> {
    ctx: &'i Ctx<'i>,
    ty: TypeRef<'i>,
    tr: Option<TraitRef<'i>>,
    methods: Vec<Option<Method<'i>>>,
    ty_param_count: usize,
    trait_bounds: Vec<Bound<'i>>,
}

impl<'i> TraitImplBuilder<'i> {
    pub fn new(
        ctx: &'i Ctx<'i>,
        ty: TypeRef<'i>,
        tr: Option<TraitRef<'i>>,
        ty_param_count: usize,
        trait_bounds: Vec<Bound<'i>>,
    ) -> Self {
        Self {
            ctx,
            ty,
            tr,
            ty_param_count,
            methods: tr
                .map(|tr| tr.0.borrow().signatures.iter().map(|_| None).collect())
                .unwrap_or(vec![]),
            trait_bounds,
        }
    }

    pub fn add_method(&mut self, method: Method<'i>, ast_node: &ast::Function<'i>) {
        let Some(tr) = self.tr else {
            return;
        };
        let tr_decl = &*tr.0.borrow();
        let func = &*method.fun.borrow();
        let idx = tr_decl
            .signatures
            .iter()
            .position(|&sig| sig.fun.borrow().sig.name == func.sig.name);

        let Some(idx) = idx else {
            NoMatchingTraitMethod {
                span: ast_node.span(),
            }
            .emit(&self.ctx.diagnostcs);
            return;
        };
        self.methods[idx] = Some(method);

        let tr_ty_args = tr.1;
        let tr_fun = &*tr_decl.signatures[idx].fun.borrow();
        let trait_method_ty_args = self.ctx.type_pool.get_ty_list(
            iter::once(self.ty)
                .chain(tr_ty_args.iter().copied())
                .chain(
                    (self.ty_param_count..)
                        .map(|i| self.ctx.type_pool.get_ty_constant(i))
                        .take(tr_fun.sig.ty_params.len() - 1 - tr_ty_args.len()),
                )
                .collect(),
        );

        if (func.sig.ty_params.len() - self.ty_param_count)
            != (tr_fun.sig.ty_params.len() - 1 - tr_ty_args.len())
        {
            self.ctx.diagnostcs.add(err!(
                &ast_node.signature.name.span,
                "trait method declares {} type parameters",
                tr_fun.sig.ty_params.len() - 1 - tr_ty_args.len()
            ));
        }

        if tr_fun.sig.params.len() != tr_fun.sig.params.len() {
            self.ctx.diagnostcs.add(err!(
                &ast_node.signature.name.span,
                "trait method declares {} parameters",
                tr_fun.sig.params.len() - 1
            ));
        }

        for ((&tr_ty, &ty), param) in tr_fun
            .sig
            .params
            .iter()
            .zip(func.sig.params.iter())
            .skip(1)
            .zip(ast_node.signature.params.iter())
        {
            let tr_ty = self
                .ctx
                .type_pool
                .resolve_ty_args(tr_ty, &trait_method_ty_args);
            if tr_ty != ty {
                self.ctx.diagnostcs.add(err!(
                    &param.span(),
                    "parameter type does not match the one declared by the trait"
                ));
            }
        }

        let tr_ret_ty = self
            .ctx
            .type_pool
            .resolve_ty_args(tr_fun.sig.ret_ty, &trait_method_ty_args);
        if tr_ret_ty != func.sig.ret_ty {
            self.ctx.diagnostcs.add(err!(
                &ast_node.signature.span,
                "return type does not match the one declared by the trait"
            ));
        }

        let mut bounds = func.sig.trait_bounds[self.trait_bounds.len()..].to_vec();

        for &tr_bound in &tr_fun.sig.trait_bounds[(tr_decl.trait_bounds.len() + 1)..] {
            let tr_bound = self
                .ctx
                .type_pool
                .resolve_bound(tr_bound, &trait_method_ty_args);

            if let Some(bound_idx) = bounds.iter().position(|&bound| bound == tr_bound) {
                bounds.swap_remove(bound_idx);
            } else {
                self.ctx.diagnostcs.add(err!(
                    &ast_node.signature.span,
                    "trait declares stricter bounds on this function"
                ))
            }
        }
        if !bounds.is_empty() {
            self.ctx.diagnostcs.add(err!(
                &ast_node.signature.span,
                "cannot add extra trait bounds"
            ))
        }
    }

    pub fn get_receiver(&self, is_mut: bool) -> Receiver {
        self.ctx
            .resolve_reciever(self.ty, &self.trait_bounds, is_mut)
    }

    pub fn build(self, span: Span) -> Option<TraitImplRef<'i>> {
        let tr = self.tr?;

        for (i, method) in self.methods.iter().enumerate() {
            if method.is_none() {
                MissingTraitMethod {
                    span,
                    name: tr.0.borrow().signatures[i].fun.borrow().sig.name,
                }
                .emit(&self.ctx.diagnostcs);
            }
        }

        let trait_impl = TraitImpl {
            ty: self.ty,
            tr,
            ty_params: vec![(); self.ty_param_count],
            trait_bounds: self.trait_bounds,
            functions: self
                .methods
                .into_iter()
                .collect::<Option<Vec<Method<'i>>>>()?,
        };

        Some(self.ctx.trait_impl_pool.insert(trait_impl))
    }
}

struct NoMatchingTraitMethod {
    span: Span,
}

struct MissingTraitMethod<'i> {
    span: Span,
    name: Str<'i>,
}

impl IceError for NoMatchingTraitMethod {
    fn emit(&self, diagnostics: &iiv::diagnostics::Diagnostics) {
        diagnostics.add(err!(
            &self.span,
            "no such method found in trait declaration"
        ))
    }
}

impl<'i> IceError for MissingTraitMethod<'i> {
    fn emit(&self, diagnostics: &iiv::diagnostics::Diagnostics) {
        diagnostics.add(err!(
            &self.span,
            "missing implementation of method {}",
            self.name
        ))
    }
}
