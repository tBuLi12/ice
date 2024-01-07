use std::collections::HashMap;

use ast::{
    BindPattern, BindingType, BlockItem, Expr, Ident, Module, NarrowTypePattern, Pattern,
    PatternBody, PropsTy, Spanned,
};
use iiv::{
    builder::BlockRef,
    diagnostics::{self},
    err,
    fun::{Bound, Function, Method, Receiver},
    impl_tree::ImplForest,
    pool::{self, FuncRef, TraitDeclRef, TyDeclRef},
    str::Str,
    ty::{PropRef, TraitRef, Type, TypeOverlap, TypeRef},
    ty_decl, Ctx, Package, Prop, Span, Value,
};
use ty::InferenceCtx;

mod ty;

#[derive(Debug)]
pub enum TokenType {
    Type,
    Variable,
    Keyword,
    Variant,
    Property,
    Function,
}

pub struct LspIndex(pub Option<Vec<(Span, TokenType)>>);

impl LspIndex {
    fn register_token_obj<'i>(&mut self, span: Span, obj: &Object<'i>) {
        if let Some(idx) = &mut self.0 {
            let tt = match obj {
                Object::TypeDecl(_) => Some(TokenType::Type),
                Object::Type(ty) => Some(match &**ty {
                    Type::Builtin(_) => TokenType::Keyword,
                    _ => TokenType::Type,
                }),
                Object::Place(_, _) => Some(TokenType::Variable),
                Object::Fun(_, _) | Object::FunDecl(_) => Some(TokenType::Function),
                _ => None,
            };
            if let Some(tt) = tt {
                idx.push((span, tt));
            }
        }
    }

    fn register_token<'i>(&mut self, span: Span, tt: TokenType) {
        if let Some(idx) = &mut self.0 {
            idx.push((span, tt));
        }
    }
}

pub struct Generator<'i> {
    pub index: LspIndex,
    scopes: Scopes<'i>,
    trait_method_scope: HashMap<Str<'i>, Vec<(TraitDeclRef<'i>, usize)>>,
    ty: &'i iiv::ty::Pool<'i>,
    fun_pool: &'i iiv::pool::FunPool<'i>,
    ty_decl_pool: &'i iiv::pool::TyDeclPool<'i>,
    trait_decl_pool: &'i iiv::pool::TraitDeclPool<'i>,
    trait_impl_pool: &'i iiv::pool::TraitImplPool<'i>,
    messages: &'i iiv::diagnostics::Diagnostics,
    impl_forest: ImplForest<'i>,
    copy_trait: TraitRef<'i>,
}

struct Scope<'i> {
    index: HashMap<Str<'i>, Object<'i>>,
    values: Vec<Value<'i>>,
    known_bounds: Vec<Bound<'i>>,
    next_free_ty_param_idx: usize,
}

impl<'i> Scopes<'i> {
    pub fn define(&mut self, name: &Ident<'i>, object: Object<'i>) {
        let scope = self.inner.last_mut().unwrap();
        if let Object::Place(val, _) = &object {
            scope.values.push(*val);
        }
        if scope.index.insert(name.value, object).is_some() {
            self.messages
                .add(err!(&name.span, "{} is defined multiple times", name.value))
        }
    }

    pub fn resolve(&mut self, name: Str<'i>) -> Option<Object<'i>> {
        for scope in &self.inner {
            if let Some(obj) = scope.index.get(&name) {
                return Some(obj.clone());
            }
        }
        None
    }

    pub fn begin(&mut self) {
        let next_free_ty_param_idx = self
            .inner
            .last()
            .map(|scope| scope.next_free_ty_param_idx)
            .unwrap_or(0);

        self.inner.push(Scope {
            index: HashMap::new(),
            known_bounds: vec![],
            values: vec![],
            next_free_ty_param_idx,
        });
    }

    pub fn end(&mut self) -> Vec<Value<'i>> {
        let Scope { values, .. } = self.inner.pop().unwrap();
        values
    }

    pub fn generate_ty_param(&mut self) -> TypeRef<'i> {
        let scope = &mut self.inner.last_mut().unwrap();
        let ty_param = self.ty_pool.get_ty_constant(scope.next_free_ty_param_idx);
        scope.next_free_ty_param_idx += 1;

        ty_param
    }

    pub fn new_ty_param(&mut self, name: &Ident<'i>) -> TypeRef<'i> {
        let ty_param = self.generate_ty_param();
        self.define(name, Object::Type(ty_param));
        ty_param
    }

    pub fn new_bound(&mut self, bound: Bound<'i>) {
        let scope = &mut self.inner.last_mut().unwrap();
        scope.known_bounds.push(bound);
    }
}

struct Scopes<'i> {
    inner: Vec<Scope<'i>>,
    messages: &'i iiv::diagnostics::Diagnostics,
    ty_pool: &'i iiv::ty::Pool<'i>,
    this_ty: Option<TypeRef<'i>>,
    this: Option<Value<'i>>,
}

enum Callable<'i> {
    Fun {
        fun: FuncRef<'i>,
        ty_args: pool::List<'i, TypeRef<'i>>,
    },
    Type {
        ty: TyDeclRef<'i>,
        ty_args: pool::List<'i, TypeRef<'i>>,
    },
    TraitMethod {
        this: Option<Value<'i>>,
        tr: TraitDeclRef<'i>,
        idx: usize,
        ty_args: pool::List<'i, TypeRef<'i>>,
    },
    None,
    Invalid,
}

pub struct FunctionGenerator<'f, 'i, 'g> {
    trait_method_scope: &'g HashMap<Str<'i>, Vec<(TraitDeclRef<'i>, usize)>>,
    index: &'g mut LspIndex,
    scopes: &'g mut Scopes<'i>,
    impl_forest: &'g mut ImplForest<'i>,
    ty: &'i iiv::ty::Pool<'i>,
    messages: &'i iiv::diagnostics::Diagnostics,
    constant_depth: usize,
    ret_type: Option<TypeRef<'i>>,
    iiv: iiv::builder::Cursor<'f, 'i>,
    inf_ctx: InferenceCtx<'i>,
    copy_trait: TraitRef<'i>,
}

impl<'i> Generator<'i> {
    pub fn new(ctx: &'i Ctx<'i>, index_tokens: bool) -> Self {
        Generator {
            scopes: Scopes {
                inner: vec![Scope {
                    index: HashMap::new(),
                    known_bounds: vec![],
                    next_free_ty_param_idx: 0,
                    values: vec![],
                }],
                messages: &ctx.diagnostcs,
                ty_pool: &ctx.type_pool,
                this_ty: None,
                this: None,
            },
            trait_method_scope: HashMap::new(),
            ty: &ctx.type_pool,
            fun_pool: &ctx.fun_pool,
            ty_decl_pool: &ctx.ty_decl_pool,
            trait_decl_pool: &ctx.trait_decl_pool,
            trait_impl_pool: &ctx.trait_impl_pool,
            messages: &ctx.diagnostcs,
            impl_forest: ImplForest::new(&ctx.type_pool, &ctx.diagnostcs),
            index: if index_tokens {
                LspIndex(Some(vec![]))
            } else {
                LspIndex(None)
            },
            copy_trait: ctx.builtins.get_copy(),
        }
    }

    fn define_global(&mut self, name: Str<'i>, obj: Object<'i>) {
        self.scopes.inner[0].index.insert(name, obj);
    }

    pub fn resolve_ty_param_list(
        &mut self,
        ty_params: &[ast::TypeParam<'i>],
    ) -> (Vec<Bound<'i>>, pool::List<'i, TypeRef<'i>>) {
        let mut bounds = vec![];
        let trait_ty_params = self.ty.get_ty_list(
            ty_params
                .iter()
                .map(|param| {
                    let ty = self.scopes.new_ty_param(&param.name);
                    let mut fun = Function::empty(self.ty, self.ty.str_pool.get(""));
                    let mut fun_gen = FunctionGenerator::new(self, &mut fun);
                    for tr_expr in &param.trait_bounds {
                        if let Some(mut tr) = fun_gen.check_trait(tr_expr) {
                            tr.1 = fun_gen.ty.get_ty_list(fun_gen.inf_ctx.unwrap_list(tr.1));
                            bounds.push(Bound { ty, tr });
                        }
                    }
                    ty
                })
                .collect(),
        );
        (bounds, trait_ty_params)
    }

    pub fn emit_iiv(&mut self, modules: &[Module<'i>]) -> Package<'i> {
        self.define_global(self.ty.str_pool.get("int"), self.ty.get_int().obj());
        self.define_global(self.ty.str_pool.get("bool"), self.ty.get_ty_bool().obj());

        // types
        let types = &modules[0].types;
        let mut package_types = vec![];

        for ty_node in types {
            let mut fun = Function::empty(self.ty, self.ty.str_pool.get(""));
            let mut fun_gen = FunctionGenerator::new(self, &mut fun);

            fun_gen.begin_scope();
            for param in &ty_node.type_params {
                fun_gen.scopes.new_ty_param(&param.name);
            }

            let proto = fun_gen.check_type(&ty_node.proto);
            let ty = ty_decl::TypeDecl {
                proto: fun_gen.inf_ctx.unwrap(proto),
                ty_params: ty_node.type_params.iter().map(|_| ()).collect(),
                name: ty_node.name.value,
            };
            fun_gen.end_scope();

            let ty = self.ty_decl_pool.insert(ty);

            self.define_global(ty_node.name.value, Object::TypeDecl(ty));
            package_types.push(ty);
        }

        // traits
        let traits = &modules[0].traits;
        let mut package_traits = vec![];

        for trait_node in traits {
            self.scopes.begin();
            let this_ty = self.scopes.generate_ty_param();
            self.scopes.this_ty = Some(this_ty);
            let (mut bounds, trait_ty_params) = self.resolve_ty_param_list(&trait_node.type_params);

            let tr = ty_decl::TraitDecl {
                name: trait_node.name.value,
                ty_params: trait_node.type_params.iter().map(|_| ()).collect(),
                signatures: Vec::with_capacity(trait_node.signatures.len()),
                trait_bounds: bounds.clone(),
            };
            let tr_ref = self.trait_decl_pool.insert(tr);
            let tr = &mut *tr_ref.borrow_mut();

            bounds.insert(
                0,
                Bound {
                    ty: this_ty,
                    tr: TraitRef(tr_ref, trait_ty_params),
                },
            );

            for sig in &trait_node.signatures {
                let mut fun = Function::empty(self.ty, sig.name.value);
                let mut fun_gen = FunctionGenerator::new(self, &mut fun);
                fun_gen.begin_scope();
                fun_gen.set_signature(&sig, bounds.clone(), tr.ty_params.len() + 1, Some(this_ty));
                fun_gen.end_scope();
                let fun = self.fun_pool.insert(fun);
                tr.signatures.push(Method {
                    fun,
                    receiver: Receiver::Immutable,
                });
            }

            tr.signatures.sort_by_key(|sig| sig.fun.borrow().sig.name);

            self.scopes.end();

            for (i, sig) in tr.signatures.iter().enumerate() {
                let methods = self
                    .trait_method_scope
                    .entry(sig.fun.borrow().sig.name)
                    .or_insert_with(|| vec![]);
                methods.push((tr_ref, i));
            }
            self.scopes.this_ty = None;
            self.define_global(trait_node.name.value, Object::TraitDecl(tr_ref));
            package_traits.push(tr_ref);
        }

        let trait_impls = &modules[0].trait_impls;
        let mut package_trait_impls = vec![];

        for impl_node in trait_impls {
            self.scopes.begin();
            let (own_bounds, impl_ty_params) = self.resolve_ty_param_list(&impl_node.type_params);

            let (ty, tr) = {
                let mut fun = Function::empty(self.ty, self.ty.str_pool.get(""));
                let mut fun_gen = FunctionGenerator::new(self, &mut fun);
                let ty = fun_gen.check_type(&impl_node.ty);
                fun_gen.scopes.this_ty = Some(ty);
                let tr = fun_gen.check_trait(&impl_node.tr);
                let ty = fun_gen.inf_ctx.unwrap(ty);
                let tr = tr.map(|mut tr| {
                    tr.1 = fun_gen.ty.get_ty_list(fun_gen.inf_ctx.unwrap_list(tr.1));
                    tr
                });
                (ty, tr)
            };

            self.scopes.this_ty = Some(ty);

            let bounds = tr
                .map(|tr| {
                    std::iter::once(Bound { ty, tr })
                        .chain(
                            tr.0.borrow()
                                .trait_bounds
                                .iter()
                                .map(|&bound| self.ty.resolve_bound(bound, &tr.1)),
                        )
                        .chain(own_bounds.clone())
                        .collect::<Vec<_>>()
                })
                .unwrap_or(vec![]);

            let funcs = impl_node
                .functions
                .iter()
                .map(|fun_node| {
                    let mut fun = Function::empty(self.ty, fun_node.signature.name.value);
                    let mut fun_gen = FunctionGenerator::new(self, &mut fun);

                    fun_gen.begin_scope();
                    fun_gen.set_signature(
                        &fun_node.signature,
                        bounds.clone(),
                        impl_ty_params.len(),
                        Some(ty),
                    );
                    fun_gen.end_scope();

                    if let Some(tr) = tr {
                        let tr_ty_args = tr.1;
                        let tr = &*tr.0.borrow();
                        let method = tr.signatures.iter().find(|method| {
                            method.fun.borrow().sig.name == fun_node.signature.name.value
                        });
                        if let Some(method) = method {
                            self.scopes.begin();
                            let mut method_ty_args = self.ty.get_ty_list(
                                impl_ty_params.iter().copied().chain(
                                    fun_node
                                    .signature
                                    .type_params
                                    .iter()
                                    .map(|_| self.scopes.generate_ty_param())
                                )

                                    .collect()
                            );
                            let tr_fun = &*method.fun.borrow();
                            if (method_ty_args.len() - impl_ty_params.len()) != (tr_fun.sig.ty_params.len() - 1 - tr_ty_args.len()) {
                                self.messages.add(err!(
                                    &fun_node.signature.name.span,
                                    "trait method declares {} type parameters",
                                    tr_fun.sig.ty_params.len() - 1 - tr_ty_args.len()
                                ));
                                method_ty_args = self.ty.get_ty_list(
                                    (0..(tr_fun.sig.ty_params.len() - 1 - tr_ty_args.len() + impl_ty_params.len()))
                                        .into_iter()
                                        .map(|i| {
                                            method_ty_args
                                                .get(i)
                                                .copied()
                                                .unwrap_or_else(|| self.ty.get_ty_invalid())
                                        })
                                        .collect(),
                                )
                            }
                            if tr_fun.sig.params.len() != fun.sig.params.len() {
                                self.messages.add(err!(
                                    &fun_node.signature.name.span,
                                    "trait method declares {} parameters",
                                    tr_fun.sig.params.len()
                                ));
                            }
                            for ((&tr_ty, &ty), param) in tr_fun
                                .sig
                                .params
                                .iter()
                                .skip(1)
                                .zip(fun.sig.params.iter().skip(1))
                                .zip(fun_node.signature.params.iter())
                            {
                                let tr_ty = self.ty.resolve_ty_args(tr_ty, &method_ty_args);
                                if tr_ty != ty {
                                    self.messages.add(err!(
                                        &param.span(),
                                        "parameter type does not match the one declared by the trait"
                                    ));
                                }
                            }
                            let tr_ret_ty =
                                self.ty.resolve_ty_args(tr_fun.sig.ret_ty, &method_ty_args);
                            if tr_ret_ty != fun.sig.ret_ty {
                                self.messages.add(err!(
                                    &fun_node.signature.span,
                                    "return type does not match the one declared by the trait"
                                ));
                            }

                            let mut bounds = fun.sig.trait_bounds[bounds.len()..].to_vec();

                            for tr_bound in tr_fun.sig.trait_bounds[(tr.trait_bounds.len() + 1)..]
                                .iter()
                                .copied()
                            {
                                let tr_bound = self.ty.resolve_bound(tr_bound, &method_ty_args);

                                if let Some(bound_idx) =
                                    bounds.iter().position(|&bound| bound == tr_bound)
                                {
                                    bounds.swap_remove(bound_idx);
                                } else {
                                    self.messages.add(err!(
                                        &fun_node.signature.span,
                                        "trait declares stricter bounds on this function"
                                    ))
                                }
                            }
                            if !bounds.is_empty() {
                                self.messages.add(err!(
                                    &fun_node.signature.span,
                                    "cannot add extra trait bounds"
                                ))
                            }
                            self.scopes.end();
                        } else {
                            self.messages.add(err!(
                                &fun_node.signature.name.span,
                                "trait does not declare a function named {}",
                                fun_node.signature.name.value
                            ));
                        }
                    }
                    let fun = self.fun_pool.insert(fun);
                    self.index
                        .register_token_obj(fun_node.signature.name.span, &Object::FunDecl(fun));
                    self.scopes.end();
                    Method {
                        fun,
                        receiver: Receiver::Immutable,
                    }
                })
                .collect();

            if let Some(tr) = tr {
                for tr_sig in &tr.0.borrow().signatures {
                    if impl_node
                        .functions
                        .iter()
                        .find(|fun| fun.signature.name.value == tr_sig.fun.borrow().sig.name)
                        .is_none()
                    {
                        self.messages.add(err!(
                            &impl_node.span,
                            "missing associated function {}",
                            tr_sig.fun.borrow().sig.name
                        ));
                    }
                }
                let trait_impl = ty_decl::TraitImpl {
                    ty_params: impl_node.type_params.iter().map(|_| ()).collect(),
                    functions: funcs,
                    tr,
                    ty,
                    trait_bounds: own_bounds,
                };
                let impl_ref = self.trait_impl_pool.insert(trait_impl);
                package_trait_impls.push(impl_ref);
                self.impl_forest.add(&impl_node.span, impl_ref);
            }
            self.scopes.this_ty = None;
        }

        let funcs = &modules[0].functions;
        let mut package_funs = vec![];
        for fun_node in funcs {
            let mut fun = Function::empty(self.ty, fun_node.signature.name.value);

            let mut fun_gen = FunctionGenerator::new(self, &mut fun);

            fun_gen.begin_scope();
            fun_gen.set_signature(&fun_node.signature, vec![], 0, None);
            fun_gen.end_scope();

            let name = fun_node.signature.name.value;

            let fun = self.fun_pool.insert(fun);
            self.index
                .register_token_obj(fun_node.signature.name.span, &Object::FunDecl(fun));

            self.define_global(name, Object::FunDecl(fun));
            package_funs.push(fun);
        }

        for (&fun, fun_node) in package_funs.iter().zip(funcs) {
            let mut fun = fun.borrow_mut();
            eprintln!("emitting {}", fun.sig.name);
            let gen = FunctionGenerator::new(self, &mut *fun);
            gen.emit_function_body(fun_node, Receiver::None);
        }

        for (&trait_impl, impl_node) in package_trait_impls.iter().zip(trait_impls) {
            let trait_impl = trait_impl.borrow();

            for (method, fun_node) in trait_impl.functions.iter().zip(&impl_node.functions) {
                let mut fun = method.fun.borrow_mut();
                let gen = FunctionGenerator::new(self, &mut *fun);
                gen.emit_function_body(fun_node, Receiver::Immutable);
            }
        }

        let main = if let Some(Object::FunDecl(main)) =
            self.scopes.resolve(self.ty.str_pool.get("main"))
        {
            Some(main)
        } else {
            None
        };

        Package {
            impl_forest: std::mem::replace(
                &mut self.impl_forest,
                ImplForest::new(self.ty, self.messages),
            ),
            funcs: package_funs,
            main,
        }
    }
}

impl<'f, 'i: 'f, 'g> FunctionGenerator<'f, 'i, 'g> {
    pub fn new(parent: &'g mut Generator<'i>, func: &'f mut Function<'i>) -> Self {
        Self {
            trait_method_scope: &parent.trait_method_scope,
            scopes: &mut parent.scopes,
            index: &mut parent.index,
            ty: &parent.ty,
            messages: &parent.messages,
            impl_forest: &mut parent.impl_forest,
            constant_depth: 0,
            ret_type: None,
            iiv: iiv::builder::Cursor::new(parent.ty, func),
            inf_ctx: InferenceCtx::new(parent.ty, parent.messages),
            copy_trait: parent.copy_trait,
        }
    }

    fn set_signature(
        &mut self,
        sig_node: &ast::Signature<'i>,
        mut bounds: Vec<Bound<'i>>,
        block_ty_param_count: usize,
        this: Option<TypeRef<'i>>,
    ) {
        for param in &sig_node.type_params {
            let ty = self.scopes.new_ty_param(&param.name);
            for tr_expr in &param.trait_bounds {
                if let Some(tr) = self.check_trait(tr_expr) {
                    bounds.push(Bound { ty, tr });
                }
            }
        }
        self.iiv.set_bounds(bounds);

        let params = self.ty.get_ty_list(
            this.into_iter()
                .chain(sig_node.params.iter().map(|param| {
                    let ty = self.check_type(&param.ty);
                    self.inf_ctx.unwrap(ty)
                }))
                .collect(),
        );
        self.iiv.set_params(params);

        self.iiv
            .set_ty_params(block_ty_param_count + sig_node.type_params.len());
        let ret_ty = sig_node
            .return_ty
            .as_ref()
            .map(|expr| {
                let ty = self.check_type(&expr);
                self.inf_ctx.unwrap(ty)
            })
            .unwrap_or_else(|| self.ty.get_null());
        self.iiv.set_ret_ty(ret_ty);
    }

    fn emit_function_body(mut self, fun_node: &ast::Function<'i>, receiver: Receiver) {
        self.begin_scope();
        let sig = self.iiv.current_signature();
        let ret_ty = sig.ret_ty;

        for param in &fun_node.signature.type_params {
            self.scopes.new_ty_param(&param.name);
        }
        for bound in &sig.trait_bounds {
            self.scopes.new_bound(*bound);
        }

        let mut params = self.iiv.params();

        match receiver {
            Receiver::Immutable => {
                self.scopes.this = params.next();
            }
            Receiver::None => {}
            _ => panic!("unsupported receiver"),
        }

        for (param, ast_param) in params.zip(&fun_node.signature.params) {
            self.scopes
                .define(&ast_param.name, Object::Place(param, vec![]));
        }

        self.ret_type = Some(ret_ty);

        self.iiv.create_block();
        let body = self.check_val(&fun_node.body);
        let body = self.ensure_ty(&fun_node.body.span(), ret_ty, body);
        let body = self.move_val(body);
        self.end_scope();
        self.iiv.ret(body);

        for ty in self.iiv.ty_cache_mut().iter_mut() {
            *ty = self.inf_ctx.unwrap(*ty);
        }
        for block in self.iiv.body_mut().iter_mut() {
            for (param, _) in &mut block.params {
                *param = self.inf_ctx.unwrap(*param);
            }
            for (inst, _) in &mut block.instructions {
                inst.visit_type(self.ty, |ty| {
                    *ty = self.inf_ctx.unwrap(*ty);
                })
            }
        }
        self.inf_ctx.clear(&mut self.impl_forest);
        self.scopes.this = None;
    }

    fn check_val(&mut self, expr: &Expr<'i>) -> Value<'i> {
        let obj = self.check(expr, None);
        self.get_val(&expr.span(), obj)
    }

    fn in_const_ctx<T>(&mut self, fun: impl FnOnce(&mut Self) -> T) -> T {
        self.constant_depth += 1;
        let res = fun(self);
        self.constant_depth -= 1;
        res
    }

    fn check_type(&mut self, expr: &Expr<'i>) -> TypeRef<'i> {
        self.in_const_ctx(|g| {
            let obj = g.check(expr, None);
            g.get_type(&expr.span(), obj)
        })
    }

    fn check_trait(&mut self, expr: &Expr<'i>) -> Option<TraitRef<'i>> {
        self.in_const_ctx(|g| {
            let obj = g.check(expr, None);
            g.get_trait(&expr.span(), obj)
        })
    }

    fn msg(&mut self, message: diagnostics::Diagnostic) {
        self.messages.add(message);
    }

    fn ensure_ty(&mut self, span: &Span, ty: TypeRef<'i>, value: Value<'i>) -> Value<'i> {
        if !self.inf_ctx.eq(span, ty, value.ty) {
            match (&*value.ty, &*ty) {
                (Type::Variant(props1), Type::Variant(props2)) => {
                    let mut eq = self.inf_ctx.try_eq();
                    if props1.iter().all(|prop| {
                        props2
                            .iter()
                            .find(|prop2| prop.0 == prop2.0 && eq.eq(prop.1, prop2.1))
                            .is_some()
                    }) {
                        eq.commit(span);
                        return self.iiv.variant_cast(ty, value);
                    };
                }
                _ => {
                    self.msg(err!(span, "expected type {}, got {} instead", ty, value.ty));
                }
            }
        }
        Value { raw: value.raw, ty }
    }

    fn check_bound(&mut self, span: Span, bound: Bound<'i>) {
        let mut relevant_bounds = vec![];
        for scope in self.scopes.inner.iter().rev() {
            for known_bound in &scope.known_bounds {
                // let relevant = |ty| ty == bound.ty || bound.tr.1.iter().any(|&arg| arg == ty);
                // if known_bound.ty.contains(relevant)
                //     || known_bound.tr.1.iter().any(|&ty| ty.contains(relevant))
                // {
                relevant_bounds.push(*known_bound);
                // }
            }
        }

        self.inf_ctx.new_bound(span, bound, relevant_bounds);
    }

    fn make_type_union(&mut self, span: &Span, types: &[TypeRef<'i>]) -> TypeRef<'i> {
        let mut unqiue = vec![];

        'outer: for &ty in types {
            for &member in unqiue.iter() {
                match self.inf_ctx.get_intersection(ty, member) {
                    TypeOverlap::Complete => continue 'outer,
                    TypeOverlap::Partial => {
                        self.msg(err!(
                            span,
                            "union members {} and {} may refer to the same type",
                            member,
                            ty
                        ));
                    }
                    TypeOverlap::None => {}
                }
            }
            unqiue.push(ty);
        }

        if let [only] = unqiue[..] {
            return only;
        }
        self.ty.get_union(unqiue)
    }

    // fn bind(&mut self, pattern: &Pattern, value: Object) {
    //     match (pattern.body, value) {
    //         (PatternBody::Bind(BindingType::Let, ident), value) => {
    //             self.read_rt_value(value);
    //         }
    //         (PatternBody::Bind(BindingType::Var, ident), value) => {
    //             self.read_rt_value(value);
    //         }
    //         (PatternBody::Bind(BindingType::Const, ident), value) => {
    //             self.read_ct_value(value);
    //         }
    //         _ => panic!(),
    //     }
    // }

    fn null(&mut self) -> Value<'i> {
        self.iiv.null()
    }

    fn _never(&mut self) -> Value<'i> {
        Value {
            raw: self.iiv.null().raw,
            ty: self.ty.get_ty_never(),
        }
    }

    fn invalid(&mut self) -> Value<'i> {
        Value {
            raw: self.iiv.null().raw,
            ty: self.ty.get_ty_invalid(),
        }
    }

    fn bind(
        &mut self,
        pattern: &Pattern<'i>,
        value: Value<'i>,
        no_match_block: Option<BlockRef>,
        match_block: Option<BlockRef>,
    ) {
        match &pattern.body {
            PatternBody::Bind(BindPattern { binding_type, name }) => {
                self.register_token_obj(name.span, &Object::Place(value, vec![]));
                match binding_type {
                    BindingType::Var => self.scopes.define(name, Object::Place(value, vec![])),
                    BindingType::Let => self.scopes.define(name, Object::Place(value, vec![])),
                    _ => unimplemented!(),
                }
            }
            PatternBody::NarrowType(NarrowTypePattern { inner, ty }) => {
                let target_ty = self.check_type(ty);
                let narrowed = self.ensure_ty(&ty.span(), target_ty, value);
                self.bind(&inner, narrowed, no_match_block, match_block);
            }
            PatternBody::Variant(vairant) => {
                if let Type::Variant(elems) = *value.ty {
                    let i = elems.iter().enumerate().find_map(|(i, elem)| {
                        if elem.0 == vairant.name.value {
                            Some(i)
                        } else {
                            None
                        }
                    });
                    if let Some(i) = i {
                        let discriminant = self.iiv.discriminant(value);
                        let expected = self.iiv.int_lit(i as u32);
                        let cond = self.iiv.equals(discriminant, expected);
                        let next_block = self.iiv.create_block();
                        let no_match = no_match_block.unwrap();
                        self.iiv.branch(cond, next_block, no_match);
                        self.iiv.select(next_block);
                        let inner = self.iiv.move_prop(value, i as u8, elems[i].1);
                        if let Some(inner_patter) = &vairant.inner {
                            self.bind(&inner_patter, inner, no_match_block, match_block);
                        } else {
                            self.iiv.select(match_block.unwrap());
                            self.drop(inner);
                            self.iiv.select(next_block);
                        }
                    } else {
                        self.msg(err!(
                            &pattern.span(),
                            "variant {} not found on type {}",
                            vairant.name.value,
                            value.ty
                        ));
                    }
                } else {
                    self.msg(err!(
                        &pattern.span(),
                        "expected a variant type, found {}",
                        value.ty
                    ));
                }
            }
            PatternBody::Literal(_) => unimplemented!(),
            PatternBody::Struct(struct_pattern) => {
                if let Type::Struct(_) = *value.ty {
                    for (name, prop_pattern) in &struct_pattern.inner {
                        if let Some((props, ty)) = self.lookup_field_on_type(value.ty, name) {
                            let prop = self.iiv.move_prop_deep(value, props, ty);
                            self.bind(prop_pattern, prop, no_match_block, match_block);
                        } else {
                            self.msg(err!(
                                &name.span,
                                "field {} does not exist on type {}",
                                name.value,
                                value.ty
                            ));
                        }
                    }
                } else {
                    self.msg(err!(
                        &pattern.span(),
                        "expected a struct type, found {}",
                        value.ty
                    ));
                }
            }
            _ => unimplemented!(),
        }
    }

    fn check_statement(&mut self, item: &BlockItem<'i>) -> Value<'i> {
        match item {
            BlockItem::Expr(expr) => match self.check(expr, None) {
                Object::Place(val, offsets) => {
                    if offsets.is_empty() {
                        val
                    } else {
                        self.copy_prop(val, offsets, val.ty)
                    }
                }
                Object::Value(val) => val,
                Object::Condition(_) => panic!("unexpected condtion!"),
                _ => self.null(),
            },
            BlockItem::Bind(binding) => {
                let initializer = self.check_val(&binding.value);
                self.bind(
                    &binding.binding,
                    initializer,
                    None,
                    Some(self.iiv.get_current_block()),
                );

                self.null()
            }
            _ => unimplemented!(),
            // BlockItem::Break(_break) => self.never(),
            // BlockItem::Return(ret) => {
            //     let Some(return_type) = self.ret_type else {
            //         self.msg(err!(&ret.span, "return is invalid within this context"));
            //         return self.invalid();
            //     };

            //     let val = if let Some(e) = &ret.value {
            //         self.check_val(&e)
            //     } else {
            //         self.null()
            //     };

            //     self.ensure_ty(&ret.span, return_type, val);
            //     self.never()
            // }
            // BlockItem::Continue(_continue) => self.never(),
        }
    }

    fn get_val(&mut self, span: &Span, obj: Object<'i>) -> Value<'i> {
        match obj {
            // Object::Trait => {
            //     err!(span, "expected a value, found trait name");
            //     self.invalid()
            // }
            // Object::Module => {
            //     err!(span, "expected a value, found module");
            //     self.invalid()
            // }
            Object::Value(val) => val,
            Object::Place(val, offsets) => {
                let result = if offsets.is_empty() {
                    val
                } else {
                    self.copy_prop(val, offsets, val.ty)
                };
                self.copy(result)
            }
            Object::TypeDecl(ty_decl) => {
                self.msg(err!(span, "expected a value, found type {}", ty_decl.name));
                self.invalid()
            }
            Object::TraitDecl(tr_decl) => {
                self.msg(err!(
                    span,
                    "expected a value, found trait {}",
                    tr_decl.borrow().name
                ));
                self.invalid()
            }
            Object::Type(ty) => self.iiv.ty_expr(ty),
            Object::Condition(_raw) => panic!("unexpected condition returned"),
            Object::Fun(_, _)
            | Object::FunDecl(_)
            | Object::TraitMethod(_, _, _, _)
            | Object::TraitMethodDecl(_, _, _, _) => {
                self.msg(err!(span, "expected a value, found function"));
                self.invalid()
            }
            Object::Invalid => self.invalid(),
        }
    }

    fn get_type(&mut self, span: &Span, obj: Object<'i>) -> TypeRef<'i> {
        match obj {
            // Object::Trait => {
            //     err!(span, "expected a type, found trait name");
            //     self.ty.get_ty_invalid()
            // }
            // Object::Module => {
            //     err!(span, "expected a type, found module");
            //     self.ty.get_ty_invalid()
            // }
            Object::Value(val) | Object::Place(val, _) => {
                // if val.ty == self.ty.get_ty_type() {
                //     self.ty.get_ty_constant(val.raw)
                // } else {
                // }
                self.msg(err!(
                    span,
                    "expected a type, found value of type {}",
                    val.ty
                ));
                self.ty.get_ty_invalid()
            }
            Object::TypeDecl(ty_decl) => self.ty.get_ty_named(
                ty_decl,
                ty_decl
                    .ty_params
                    .iter()
                    .map(|_| self.inf_ctx.new_var())
                    .collect(),
            ),
            Object::Type(ty) => ty,
            Object::Condition(_) => panic!("unexpedted condition"),
            Object::Fun(_, _)
            | Object::FunDecl(_)
            | Object::TraitMethodDecl(_, _, _, _)
            | Object::TraitMethod(_, _, _, _) => {
                self.msg(err!(span, "expected a type, found function"));
                self.ty.get_ty_invalid()
            }
            Object::TraitDecl(tr_decl) => {
                self.msg(err!(
                    span,
                    "expected a type, found trait {}",
                    tr_decl.borrow().name
                ));
                self.ty.get_ty_invalid()
            }
            Object::Invalid => self.ty.get_ty_invalid(),
        }
    }

    fn get_trait(&mut self, span: &Span, obj: Object<'i>) -> Option<TraitRef<'i>> {
        match obj {
            // Object::Trait => {
            //     err!(span, "expected a type, found trait name");
            //     self.ty.get_ty_invalid()
            // }
            // Object::Module => {
            //     err!(span, "expected a type, found module");
            //     self.ty.get_ty_invalid()
            // }
            Object::Value(val) | Object::Place(val, _) => {
                self.msg(err!(
                    span,
                    "expected a trait, found value of type {}",
                    val.ty
                ));
                None
            }
            Object::TypeDecl(ty_decl) => {
                self.msg(err!(span, "expected a trait, found type {}", ty_decl.name));
                None
            }
            Object::Type(ty) => {
                self.msg(err!(span, "expected a trait, found type {}", ty));
                None
            }
            Object::Condition(_) => panic!("unexpedted condition"),
            Object::Fun(_, _)
            | Object::FunDecl(_)
            | Object::TraitMethodDecl(_, _, _, _)
            | Object::TraitMethod(_, _, _, _) => {
                self.msg(err!(span, "expected a trait, found function"));
                None
            }
            Object::TraitDecl(tr_decl) => Some(TraitRef(
                tr_decl,
                self.ty.get_ty_list(
                    tr_decl
                        .borrow()
                        .ty_params
                        .iter()
                        .map(|_| self.inf_ctx.new_var())
                        .collect(),
                ),
            )),
            Object::Invalid => None,
        }
    }

    fn create_ty_arg_list(&mut self, len: usize) -> pool::List<'i, TypeRef<'i>> {
        self.ty.get_ty_list(vec![self.inf_ctx.new_var(); len])
    }

    fn get_callable(&mut self, obj: Object<'i>) -> Callable<'i> {
        match obj {
            Object::Condition(_) => panic!("unexpedted condition"),
            Object::Fun(func, ty_args) => Callable::Fun { fun: func, ty_args },
            Object::FunDecl(func) => Callable::Fun {
                fun: func,
                ty_args: self.create_ty_arg_list(func.borrow().sig.ty_params.len()),
            },
            Object::Type(ty) => match &*ty {
                Type::Named(decl, args, _) => Callable::Type {
                    ty: *decl,
                    ty_args: *args,
                },
                _ => Callable::None,
            },
            Object::TypeDecl(ty_decl) => Callable::Type {
                ty: ty_decl,
                ty_args: self.create_ty_arg_list(ty_decl.ty_params.len()),
            },
            Object::TraitMethod(this, tr, idx, ty_args) => Callable::TraitMethod {
                this,
                tr,
                idx,
                ty_args,
            },
            Object::TraitMethodDecl(this, ty, tr, idx) => {
                let ty_args = self.ty.get_ty_list(
                    std::iter::once(ty)
                        .chain(tr.1.iter().copied())
                        .chain(
                            tr.0.borrow().signatures[idx]
                                .fun
                                .borrow()
                                .sig
                                .ty_params
                                .iter()
                                .skip(tr.1.len() + 1)
                                .map(|_| self.inf_ctx.new_var()),
                        )
                        .collect(),
                );
                Callable::TraitMethod {
                    this,
                    tr: tr.0,
                    idx,
                    ty_args: ty_args,
                }
            }
            Object::Invalid => Callable::Invalid,
            _ => Callable::None,
        }
    }

    fn check_condition(&mut self, expr: &Expr<'i>, false_block: BlockRef) -> usize {
        match self.check(expr, Some(false_block)) {
            Object::Condition(scopes) => scopes,
            obj => {
                let val = self.get_val(&expr.span(), obj);
                let val = self.ensure_ty(&expr.span(), self.ty.get_ty_bool(), val);
                let next_block = self.iiv.create_block();
                self.iiv.branch(val, next_block, false_block);
                self.iiv.select(next_block);
                0
            }
        }
    }

    fn lookup_field_on_type(
        &mut self,
        ty: TypeRef<'i>,
        field_name: &Ident<'i>,
    ) -> Option<(Vec<u8>, TypeRef<'i>)> {
        let prop_res = match &*ty {
            Type::Struct(props) | Type::Variant(props) => {
                Some(props.into_iter().enumerate().find_map(|(i, &prop)| {
                    if prop.0 == field_name.value {
                        Some((vec![i as u8], prop.1))
                    } else {
                        None
                    }
                }))
            }
            Type::Named(_, _, proto) => {
                let (mut props, ty) = self.lookup_field_on_type(*proto, field_name)?;
                props.insert(0, 0);
                Some(Some((props, ty)))
            }
            _ => None,
        };
        if let Some(Some(prop_res)) = prop_res {
            return Some(prop_res);
        }
        None
    }

    fn lookup_item_on_type(&mut self, ty: TypeRef<'i>, item_name: &Ident<'i>) -> Item<'i> {
        if let Some((offset, ty)) = self.lookup_field_on_type(ty, item_name) {
            return Item::Field(offset, ty);
        }

        if let Some(methods) = self.trait_method_scope.get(&item_name.value) {
            if methods.len() == 1 {
                let (decl, idx) = methods[0];
                return Item::TraitMethod(decl, idx);
            }

            self.msg(err!(
                &item_name.span(),
                "multiple trait method canditates found"
            ));
            return Item::Invalid;
        }

        self.msg(err!(
            &item_name.span(),
            "item {} does not exist on type {}",
            item_name.value,
            ty
        ));
        Item::Invalid
    }

    fn register_token_obj(&mut self, span: Span, obj: &Object<'i>) {
        self.index.register_token_obj(span, obj);
    }

    fn get_prop(&mut self, value: Object<'i>, prop: &Ident<'i>) -> Object<'i> {
        let obj = self._get_prop(value, prop);
        self.register_token_obj(prop.span, &obj);
        obj
    }

    fn _get_prop(&mut self, value: Object<'i>, prop: &Ident<'i>) -> Object<'i> {
        match value {
            Object::Place(mut place, mut offsets) => {
                match self.lookup_item_on_type(place.ty, prop) {
                    Item::Field(extra_offsets, ty) => {
                        offsets.extend(extra_offsets);
                        place.ty = ty;
                        Object::Place(place, offsets)
                    }
                    Item::TraitMethod(tr_decl, idx) => Object::TraitMethodDecl(
                        match tr_decl.borrow().signatures[idx].receiver {
                            Receiver::Immutable => Some(self.copy_prop(place, offsets, place.ty)),
                            Receiver::Mut => Some(self.iiv.get_prop_ref(place, offsets, place.ty)),
                            Receiver::None => {
                                self.msg(err!(&prop.span(), "static method called on a value"));
                                None
                            }
                            _ => unimplemented!(),
                        },
                        place.ty,
                        TraitRef(
                            tr_decl,
                            self.create_ty_arg_list(tr_decl.borrow().ty_params.len()),
                        ),
                        idx,
                    ),
                    Item::Invalid => Object::Invalid,
                }
            }
            Object::Value(value) => match self.lookup_item_on_type(value.ty, prop) {
                Item::Field(offsets, ty) => {
                    let prop = self.copy_prop(value, offsets, ty).obj();
                    self.drop(value);
                    prop
                }
                Item::TraitMethod(tr_decl, idx) => Object::TraitMethodDecl(
                    match tr_decl.borrow().signatures[idx].receiver {
                        Receiver::Immutable => Some(value),
                        Receiver::Mut => {
                            self.msg(err!(&prop.span(), "mut method called on immutable value"));
                            Some(self.invalid())
                        }
                        Receiver::None => {
                            self.msg(err!(&prop.span(), "static method called on a value"));
                            None
                        }
                        _ => unimplemented!(),
                    },
                    value.ty,
                    TraitRef(
                        tr_decl,
                        self.create_ty_arg_list(tr_decl.borrow().ty_params.len()),
                    ),
                    idx,
                ),
                Item::Invalid => Object::Invalid,
            },
            _ => {
                self.msg(err!(&prop.span(), "invalid property access target"));
                Object::Invalid
            }
        }
    }

    fn resolve_props(&mut self, props: &PropsTy<'i>) -> Vec<PropRef<'i>> {
        props
            .props
            .iter()
            .map(|prop| {
                let ty = if let Some(ty_name) = &prop.ty {
                    self.check_type(&ty_name)
                } else {
                    self.ty.get_null()
                };
                self.ty.get_prop(prop.name.value, ty)
            })
            .collect()
    }

    fn check(&mut self, expr: &Expr<'i>, false_block: Option<BlockRef>) -> Object<'i> {
        let object = match expr {
            Expr::Variable(ident) => self.resolve(ident),
            Expr::Block(block) => {
                self.begin_scope();
                let result = if let Some((last, rest)) = block.items.split_last() {
                    for item in rest {
                        let value = self.check_statement(item);
                        self.drop(value);
                    }
                    let last_result = self.check_statement(last);
                    let result = if block.has_trailing_expression {
                        last_result
                    } else {
                        self.null()
                    };
                    let result = self.move_val(result);
                    result.obj()
                } else {
                    self.null().obj()
                };
                self.end_scope();
                result
            }
            Expr::Int(int) => self.iiv.int_lit(int.value).obj(),
            Expr::Bool(boolean) => self.iiv.bool_lit(boolean.value).obj(),
            Expr::Float(_float) => unimplemented!(),
            Expr::String(_string_lit) => unimplemented!(),
            Expr::Char(_char) => unimplemented!(),
            Expr::Tuple(tuple) => {
                let items: Vec<_> = tuple.fields.iter().map(|e| self.check_val(e)).collect();
                self.iiv.make_tuple(&items).obj()
            }
            Expr::Struct(structure) => {
                for prop in &structure.props {
                    self.index
                        .register_token(prop.name.span, TokenType::Property);
                }
                let (props, vals): (Vec<_>, Vec<_>) = structure
                    .props
                    .iter()
                    .map(|prop| {
                        let val = if let Some(expr) = &prop.value {
                            self.check_val(&expr)
                        } else {
                            self.resolve_val(&ast::Ident {
                                span: prop.span(),
                                value: prop.name.value,
                            })
                        };
                        (
                            self.ty.get_prop(prop.name.value, val.ty),
                            (prop.name.value, val),
                        )
                    })
                    .unzip();
                let ty = self.ty.get_struct(props);
                let iiv::ty::Type::Struct(prop_list) = *ty else {
                    unreachable!()
                };
                let vals: Vec<_> = prop_list
                    .iter()
                    .map(|prop| {
                        *vals
                            .iter()
                            .find_map(|(name, val)| if *name == prop.0 { Some(val) } else { None })
                            .unwrap()
                    })
                    .collect();
                self.iiv.make_struct(&vals, ty).obj()
            }
            Expr::If(if_expr) => {
                let no_block = self.iiv.create_block();
                let after_block = self.iiv.create_block();

                let scopes = self.check_condition(&*if_expr.condition, no_block);

                let yes_block = self.iiv.get_current_block();
                let yes = self.check_val(&if_expr.yes);
                let yes = self.move_val(yes);

                for _ in 0..scopes {
                    self.end_scope();
                }

                self.iiv.select(no_block);
                let no = if let Some(ref expr) = if_expr.no {
                    self.check_val(expr)
                } else {
                    self.null()
                };

                let result_ty = self.make_type_union(&if_expr.span, &[no.ty, yes.ty]);

                let no = self.ensure_ty(&if_expr.span(), result_ty, no);
                self.iiv.jump(after_block, &[no]);

                self.iiv.select(yes_block);
                let yes = self.ensure_ty(&if_expr.yes.span(), result_ty, yes);
                self.iiv.jump(after_block, &[yes]);

                self.iiv.select(after_block);
                self.iiv.block_param(result_ty).obj()
            }
            Expr::While(while_expr) => {
                let cond_block = self.iiv.create_block();
                self.iiv.select(cond_block);

                let body = self.iiv.create_block();
                let after = self.iiv.create_block();

                let _scopes = self.check_condition(&while_expr.condition, after);

                self.iiv.jump(body, &[]);
                self.iiv.select(body);
                self.check_val(&*while_expr.body);

                self.iiv.select(after);
                self.null().obj();
                unimplemented!();
            }
            Expr::Match(_match) => unimplemented!(),
            Expr::TyArgApply(apply) => {
                let obj = self.check(&apply.lhs, None);
                let args: Vec<_> = apply.args.iter().map(|arg| self.check_type(arg)).collect();
                match obj {
                    Object::FunDecl(decl) => {
                        if args.len() == decl.borrow().sig.ty_params.len() {
                            Object::Fun(decl, self.ty.get_ty_list(args))
                        } else {
                            self.msg(err!(&apply.span, "invalid number of type arguments"));
                            Object::Invalid
                        }
                    }
                    Object::TypeDecl(decl) => {
                        if args.len() == decl.ty_params.len() {
                            Object::Type(self.ty.get_ty_named(decl, args))
                        } else {
                            self.msg(err!(&apply.span, "invalid number of type arguments"));
                            Object::Invalid
                        }
                    }
                    Object::TraitMethodDecl(recv, ty, tr, idx) => {
                        if args.len()
                            == tr.0.borrow().signatures[idx]
                                .fun
                                .borrow()
                                .sig
                                .ty_params
                                .len()
                                - tr.1.len()
                                - 1
                        {
                            Object::TraitMethod(
                                recv,
                                tr.0,
                                idx,
                                self.ty.get_trait_method_ty_args(ty, tr.1.to_vec(), args),
                            )
                        } else {
                            self.msg(err!(&apply.span, "invalid number of type arguments"));
                            Object::Invalid
                        }
                    }
                    obj => {
                        dbg!(obj);
                        self.msg(err!(&apply.lhs.span(), "expected a generic declaration"));
                        Object::Invalid
                    }
                }
            }
            Expr::Call(call) => {
                let obj = self.check(&call.lhs, None);
                match self.get_callable(obj) {
                    Callable::Fun {
                        fun: fun_ref,
                        ty_args,
                    } => {
                        let fun = fun_ref.borrow();

                        if fun.sig.params.len() != call.args.len() {
                            self.msg(err!(
                                &call.span(),
                                "invalid number of arguments in this call"
                            ));
                        }

                        let mut args: Vec<_> =
                            call.args.iter().map(|arg| self.check_val(arg)).collect();

                        for (i, (arg, &param)) in
                            args.iter_mut().zip(fun.sig.params.iter()).enumerate()
                        {
                            *arg = self.ensure_ty(
                                &call.args[i].span(),
                                self.ty.resolve_ty_args(param, &ty_args),
                                *arg,
                            );
                        }

                        for bound in fun.sig.trait_bounds.iter() {
                            let mut bound = *bound;
                            bound.ty = self.ty.resolve_ty_args(bound.ty, &ty_args);
                            bound.tr.1 = self.ty.resolve_ty_list_args(bound.tr.1, &ty_args);
                            self.check_bound(call.span, bound);
                        }

                        self.iiv.call(fun_ref, &args, ty_args).obj()
                    }
                    Callable::Type {
                        ty: ty_decl,
                        ty_args,
                    } => {
                        if call.args.len() != 1 {
                            self.msg(err!(
                                &call.span(),
                                "invalid number of arguments in named construction"
                            ));
                        }

                        let ty = self.ty.get_ty_named(ty_decl, ty_args.to_vec());
                        let iiv::ty::Type::Named(_, _, proto_ty) = &*ty else {
                            unreachable!()
                        };

                        let proto = self.check_val(&call.args[0]);
                        let proto = self.ensure_ty(&call.args[0].span(), *proto_ty, proto);

                        self.iiv.named(ty, proto).obj()
                    }
                    Callable::TraitMethod {
                        this,
                        tr,
                        idx,
                        ty_args,
                    } => {
                        let method = &tr.borrow().signatures[idx];
                        let fun = method.fun.borrow();

                        let mut args: Vec<_> = this
                            .into_iter()
                            .chain(call.args.iter().map(|arg| self.check_val(arg)))
                            .collect();

                        if fun.sig.params.len() != args.len() {
                            self.msg(err!(
                                &call.span(),
                                "invalid number of arguments in this call"
                            ));
                        }

                        for (i, (arg, &param)) in args
                            .iter_mut()
                            .zip(fun.sig.params.iter())
                            .skip(this.is_some() as usize)
                            .enumerate()
                        {
                            *arg = self.ensure_ty(
                                &call.args[i].span(),
                                self.ty.resolve_ty_args(param, &ty_args),
                                *arg,
                            );
                        }

                        for bound in fun.sig.trait_bounds.iter() {
                            let mut bound = *bound;
                            bound.ty = self.ty.resolve_ty_args(bound.ty, &ty_args);
                            bound.tr.1 = self.ty.resolve_ty_list_args(bound.tr.1, &ty_args);
                            self.check_bound(call.span, bound);
                        }

                        self.iiv.trait_call(tr, idx, &args, ty_args).obj()
                    }
                    Callable::Invalid => self.invalid().obj(),
                    Callable::None => {
                        self.msg(err!(&call.lhs.span(), "expected a callable"));
                        self.invalid().obj()
                    }
                }
            }
            Expr::Prop(prop) => {
                let lhs = self.check(&prop.lhs, false_block);
                self.get_prop(lhs, &prop.prop)
            }
            Expr::Field(_field) => unimplemented!(),
            Expr::Cast(_cast) => unimplemented!(),
            Expr::RefTo(ref_to) => {
                let checked = self.check(&ref_to.rhs, false_block);
                match checked {
                    Object::Place(place, offsets) => {
                        let r = self.iiv.get_prop_ref(place, offsets, place.ty);
                        r.obj()
                    }
                    _ => {
                        self.msg(err!(&ref_to.rhs.span(), "expected an assignable l-value"));
                        self.invalid().obj()
                    }
                }
            }
            Expr::Deref(deref) => {
                let checked = self.check(&deref.lhs, None);
                if let Object::Place(val, mut offsets) = checked {
                    if let Type::Ref(inner) = &*val.ty {
                        offsets.push(0);
                        Object::Place(
                            Value {
                                ty: *inner,
                                raw: val.raw,
                            },
                            offsets,
                        )
                    } else {
                        self.msg(err!(
                            &deref.lhs.span(),
                            "only references can be derefernced"
                        ));
                        self.invalid().obj()
                    }
                } else {
                    self.msg(err!(&deref.lhs.span(), "only l-values can be derefernced"));
                    self.invalid().obj()
                }
            }
            Expr::Add(add) => {
                let lhs = self.check_val(&add.lhs);
                let rhs = self.check_val(&add.rhs);
                self.iiv.add(lhs, rhs).obj()
            }
            Expr::Mul(_mul) => unimplemented!(),
            Expr::Eq(equals) => {
                let lhs = self.check_val(&equals.lhs);
                let rhs = self.check_val(&equals.rhs);
                self.iiv.equals(lhs, rhs).obj()
            }
            Expr::Neq(_not_equals) => unimplemented!(),
            Expr::And(_and) => unimplemented!(),
            Expr::Or(_or) => unimplemented!(),
            Expr::Geq(_greater_eq) => unimplemented!(),
            Expr::Leq(_less_eq) => unimplemented!(),
            Expr::Lt(_less) => unimplemented!(),
            Expr::Gt(_greater) => unimplemented!(),
            Expr::Assign(assign) => {
                let rhs = self.check_val(&assign.rhs);
                let lhs = self.check(&assign.lhs, None);
                match lhs {
                    Object::Place(place, offsets) => {
                        let rhs = self.ensure_ty(&assign.rhs.span(), place.ty, rhs);
                        let elems = offsets
                            .into_iter()
                            .map(|offset| iiv::Elem::Prop(Prop(offset)))
                            .collect();
                        self.iiv.assign(place, elems, rhs).obj()
                    }
                    _other => {
                        self.msg(err!(&assign.lhs.span(), "expected an assignable l-value"));
                        self.invalid().obj()
                        // let lhs = self.get_val(&assign.lhs.span(), other);
                        // if let Type::Ref(inner) = *lhs.ty {
                        //     let rhs = self.ensure_ty(&assign.rhs.span(), inner, rhs);
                        //     self.iiv.assign(lhs, rhs).obj()
                        // } else {
                        //     self.msg(err!(
                        //         &assign.lhs.span(),
                        //         "expected an assignable l-value, got {}",
                        //         lhs.ty
                        //     ));
                        //
                        // }
                    }
                }
            }
            Expr::Div(_div) => unimplemented!(),
            Expr::Sub(_sub) => unimplemented!(),
            Expr::Neg(_neg) => unimplemented!(),
            Expr::Not(_not) => unimplemented!(),
            Expr::Vec(_vector) => unimplemented!(),
            Expr::Variant(variant) => {
                self.index
                    .register_token(variant.variant.span, TokenType::Variant);
                let value = if let Some(val) = &variant.value {
                    self.check_val(&val)
                } else {
                    self.null()
                };
                let variant_ty = self
                    .ty
                    .get_variant(vec![self.ty.get_prop(variant.variant.value, value.ty)]);
                self.iiv.variant(variant_ty, 0, value).obj()
            }
            Expr::AddAssign(_add_assign) => unimplemented!(),
            Expr::Is(is_expr) => {
                let lhs = self.check_val(&is_expr.lhs);
                self.begin_scope();
                if let Some(false_block) = false_block {
                    let match_block = self.iiv.create_block();
                    self.bind(&is_expr.rhs, lhs, Some(false_block), Some(match_block));
                    self.iiv.jump(match_block, &[]);
                    self.iiv.select(false_block);
                    self.drop(lhs);
                    self.iiv.select(match_block);
                    Object::Condition(1)
                } else {
                    let false_block = self.iiv.create_block();
                    let true_block = self.iiv.create_block();
                    let after_block = self.iiv.create_block();
                    self.bind(&is_expr.rhs, lhs, Some(false_block), Some(true_block));
                    self.iiv.jump(true_block, &[]);
                    self.iiv.select(true_block);
                    let true_val = self.iiv.bool_lit(true);
                    self.iiv.jump(after_block, &[true_val]);
                    self.iiv.select(false_block);
                    self.drop(lhs);
                    let false_val = self.iiv.bool_lit(false);
                    self.iiv.jump(after_block, &[false_val]);
                    self.iiv.select(after_block);
                    let value = self.iiv.block_param(self.ty.get_ty_bool());
                    self.end_scope();
                    Object::Value(value)
                }
            }
            Expr::VariantTy(props) => {
                for prop in &props.props {
                    self.index
                        .register_token(prop.name.span, TokenType::Variant);
                }
                let props = self.resolve_props(props);
                self.ty.get_variant(props).obj()
            }
            Expr::StructTy(props) => {
                for prop in &props.props {
                    self.index
                        .register_token(prop.name.span, TokenType::Property);
                }
                let props = self.resolve_props(props);
                self.ty.get_struct(props).obj()
            }
        };
        object
    }

    fn drop(&mut self, val: Value<'i>) {
        self.iiv.drop(val);
    }

    fn copy(&mut self, val: Value<'i>) -> Value<'i> {
        self.copy_prop(val, vec![], val.ty)
    }

    fn copy_prop(&mut self, value: Value<'i>, props: Vec<u8>, ty: TypeRef<'i>) -> Value<'i> {
        if let Some(_) = self.impl_forest.find(ty, self.copy_trait) {
            let prop_ref = self.iiv.get_prop_ref(value, props, ty);
            self.iiv.trait_call(
                self.copy_trait.0,
                0,
                &[prop_ref],
                self.ty.get_ty_list(vec![ty]),
            )
        } else {
            self.iiv.move_prop_deep(value, props, ty)
        }
    }

    fn move_val(&mut self, val: Value<'i>) -> Value<'i> {
        self.iiv.move_prop_deep(val, vec![], val.ty)
    }
}

impl<'f, 'i: 'f, 'g> FunctionGenerator<'f, 'i, 'g> {
    fn begin_scope(&mut self) {
        self.scopes.begin();
    }

    fn end_scope(&mut self) {
        let values = self.scopes.end();
        for val in values {
            self.drop(val);
        }
    }

    fn resolve(&mut self, name: &Ident<'i>) -> Object<'i> {
        let obj = self._resolve(name);
        self.register_token_obj(name.span, &obj);
        obj
    }

    fn _resolve(&mut self, name: &Ident<'i>) -> Object<'i> {
        match &*name.value {
            "this" => {
                if let Some(this) = self.scopes.this {
                    return Object::Place(this, vec![]);
                }
                self.msg(err!(&name.span, "this is not avaliable in this context"));
                return self.invalid().obj();
            }
            "This" => {
                if let Some(this) = self.scopes.this_ty {
                    return Object::Type(this);
                }
                self.msg(err!(&name.span, "This is not avaliable in this context"));
                return self.ty.get_ty_invalid().obj();
            }
            _ => {}
        }

        if let Some(obj) = self.scopes.resolve(name.value) {
            return obj.clone();
        }

        self.msg(err!(
            &name.span,
            "\"{}\" is not defined in this context",
            name.value
        ));

        Object::Invalid
    }

    fn resolve_val(&mut self, name: &Ident<'i>) -> Value<'i> {
        let obj = self.resolve(name);
        self.get_val(&name.span, obj)
    }
}

trait ObjContent<'i> {
    fn obj(self) -> Object<'i>;
}

impl<'i> ObjContent<'i> for Value<'i> {
    fn obj(self) -> Object<'i> {
        Object::Value(self)
    }
}

impl<'i> ObjContent<'i> for TypeRef<'i> {
    fn obj(self) -> Object<'i> {
        Object::Type(self)
    }
}

#[derive(Clone, Debug)]
enum Item<'i> {
    TraitMethod(TraitDeclRef<'i>, usize),
    Field(Vec<u8>, TypeRef<'i>),
    Invalid,
}

#[derive(Clone, Debug)]
enum Object<'i> {
    // Trait,
    // Module,
    Invalid,
    Value(Value<'i>),
    Place(Value<'i>, Vec<u8>),
    Type(TypeRef<'i>),
    TypeDecl(TyDeclRef<'i>),
    TraitDecl(TraitDeclRef<'i>),
    Fun(FuncRef<'i>, pool::List<'i, TypeRef<'i>>),
    TraitMethodDecl(Option<Value<'i>>, TypeRef<'i>, TraitRef<'i>, usize),
    TraitMethod(
        Option<Value<'i>>,
        TraitDeclRef<'i>,
        usize,
        pool::List<'i, TypeRef<'i>>,
    ),
    FunDecl(FuncRef<'i>),
    Condition(usize),
}
