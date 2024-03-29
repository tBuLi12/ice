use std::{collections::HashMap, iter};

use ast::{
    BindPattern, BindingType, BlockItem, Expr, Ident, Module, NarrowTypePattern, Pattern,
    PropPattern, PropsTy, Spanned, VariantPatternBody,
};
use iiv::{
    builder::BlockRef,
    diagnostics, err,
    fun::{Body, Bound, Function, Method, Receiver, SourceMap},
    impl_tree::CellImplForest,
    pool::{self, FuncRef, TraitDeclRef, TyDeclRef},
    str::Str,
    ty::{PropRef, TraitMethodRef, TraitRef, Type, TypeOverlap, TypeRef},
    ty_decl, Ctx, Elem, Package, Position, Prop, Span, Value,
};
use trait_impl::TraitImplBuilder;
use ty::InferenceCtx;

mod trait_impl;
mod ty;

// #[derive(Debug)]
// pub enum TokenType<'i> {
//     Type { decl: TyDeclRef<'i> },
//     Variable { ty: TypeRef<'i>, def_span: Span },
//     Keyword,
//     Variant,
//     Property { name: Str<'i>, ty: TypeRef<'i> },
//     Function { decl: FuncRef<'i> },
// }

// #[derive(Debug)]
// pub enum CompletionType {
//     Function,
//     Property,
//     Type,
//     Variable,
// }

// #[derive(Debug)]
// pub struct CompletionItem<'i> {
//     pub text: Str<'i>,
//     pub ty: CompletionType,
// }

// pub struct LspIndex<'i>(pub Option<Vec<(Span, TokenType<'i>)>>);

// impl<'i> LspIndex<'i> {
//     fn register_token_obj(&mut self, span: Span, obj: &Object<'i>) {
//         if let Some(idx) = &mut self.0 {
//             let tt = match obj {
//                 Object::TypeDecl(decl) => Some(TokenType::Type { decl: *decl }),
//                 Object::Type(ty) => match &**ty {
//                     Type::Builtin(_) => Some(TokenType::Keyword),
//                     Type::Named(decl, _, _) => Some(TokenType::Type { decl: *decl }),
//                     _ => None,
//                 },
//                 Object::Place(val, _, span) => Some(TokenType::Variable {
//                     ty: val.ty,
//                     def_span: *span,
//                 }),
//                 Object::Fun(decl, _) | Object::FunDecl(decl) => {
//                     Some(TokenType::Function { decl: *decl })
//                 }
//                 _ => None,
//             };
//             if let Some(tt) = tt {
//                 idx.push((span, tt));
//             }
//         }
//     }

//     fn register_token(&mut self, span: Span, tt: TokenType<'i>) {
//         if let Some(idx) = &mut self.0 {
//             idx.push((span, tt));
//         }
//     }
// }

pub struct Generator<'i> {
    // pub index: LspIndex<'i>,
    // pub completion_token: Option<Position>,
    // pub completion_items: Vec<CompletionItem<'i>>,
    scopes: Scopes<'i>,
    trait_method_scope: HashMap<Str<'i>, Vec<TraitMethodRef<'i>>>,
    ctx: &'i iiv::Ctx<'i>,
}

struct Scope<'i> {
    index: HashMap<Str<'i>, Definition<'i>>,
    values: Vec<Value<'i>>,
    known_bounds: Vec<Bound<'i>>,
    next_free_ty_param_idx: usize,
}

impl<'i> Scopes<'i> {
    pub fn define(&mut self, name: &Ident<'i>, definition: Definition<'i>) {
        let scope = self.inner.last_mut().unwrap();
        if let Definition::MutValue(val) | Definition::Value(val) = &definition {
            scope.values.push(*val);
        }

        let Some(name_str) = name.value else {
            return;
        };

        if scope.index.insert(name_str, definition).is_some() {
            self.messages
                .add(err!(&name.span, "{} is defined multiple times", name_str))
        }
    }

    pub fn resolve(&mut self, name: Str<'i>) -> Option<Definition<'i>> {
        for scope in self.inner.iter().rev() {
            if let Some(def) = scope.index.get(&name) {
                return Some(*def);
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
        let ty_param = self
            .ctx
            .type_pool
            .get_ty_constant(scope.next_free_ty_param_idx);

        scope.next_free_ty_param_idx += 1;

        ty_param
    }

    pub fn new_ty_param(&mut self, name: &Ident<'i>) -> TypeRef<'i> {
        let ty_param = self.generate_ty_param();
        self.define(name, Definition::Type(ty_param));

        ty_param
    }

    pub fn new_bound(&mut self, bound: Bound<'i>) {
        let scope = &mut self.inner.last_mut().unwrap();
        scope.known_bounds.push(bound);
    }
}

struct Scopes<'i> {
    inner: Vec<Scope<'i>>,
    ctx: &'i Ctx<'i>,
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
    mod_gen: &'g mut Generator<'i>,
    ctx: &'i Ctx<'i>,
    constant_depth: usize,
    ret_type: Option<TypeRef<'i>>,
    iiv: iiv::builder::Cursor<'f, 'i>,
    inf_ctx: InferenceCtx<'i>,
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
                ctx: &ctx,
                this_ty: None,
                this: None,
            },
            trait_method_scope: HashMap::new(),
            ctx,
        }
    }

    fn define_global(&mut self, name: Str<'i>, def: Definition<'i>) -> bool {
        self.scopes.inner[0].index.insert(name, def).is_none()
    }

    fn define_spanned_global(&mut self, name: Ident<'i>, def: Definition<'i>) {
        let Some(name_str) = name.value else {
            return;
        };

        if !self.define_global(name_str, def) {
            self.ctx
                .diagnostcs
                .add(err!(&name.span, "duplicate definition"));
        }
    }

    pub fn resolve_ty_param_list(
        &mut self,
        ty_params: &[ast::TypeParam<'i>],
    ) -> (Vec<Bound<'i>>, pool::List<'i, TypeRef<'i>>) {
        let mut bounds = vec![];
        let trait_ty_params = self.ctx.type_pool.get_ty_list(
            ty_params
                .iter()
                .map(|param| {
                    let ty = self.scopes.new_ty_param(&param.name);
                    let mut fun = Function::empty(&self.ctx.type_pool, &[]);
                    let mut fun_gen = FunctionGenerator::new(self, &mut fun, None);
                    for tr_expr in param.bounds() {
                        if let Some(mut tr) = fun_gen.check_trait(tr_expr) {
                            tr.ty_args = fun_gen
                                .ctx
                                .type_pool
                                .get_ty_list(fun_gen.inf_ctx.unwrap_list(tr.ty_args));
                            bounds.push(Bound { ty, tr });
                        }
                    }
                    ty
                })
                .collect(),
        );
        (bounds, trait_ty_params)
    }

    fn get_type_and_infer(
        &mut self,
        span: &Span,
        obj: Object<'i>,
        inf_ctx: Option<&mut InferenceCtx<'i>>,
    ) -> TypeRef<'i> {
        match obj {
            // Object::Trait => {
            //     err!(span, "expected a type, found trait name");
            //     self.ty.get_ty_invalid()
            // }
            // Object::Module => {
            //     err!(span, "expected a type, found module");
            //     self.ty.get_ty_invalid()
            // }
            Object::Value(val) | Object::Place(val, _, _) | Object::UnsafePlace(val, _) => {
                // if val.ty == self.ty.get_ty_type() {
                //     self.ty.get_ty_constant(val.raw)
                // } else {
                // }
                self.ctx.diagnostcs.add(err!(
                    span,
                    "expected a type, found value of type {}",
                    val.ty
                ));
                self.ctx.type_pool.get_ty_invalid()
            }
            Object::TypeDecl(ty_decl) => match inf_ctx {
                Some(inf_ctx) => self.ctx.type_pool.get_ty_named(
                    ty_decl,
                    ty_decl
                        .ty_params
                        .iter()
                        .map(|_| inf_ctx.new_var(*span))
                        .collect(),
                ),
                None => {
                    if ty_decl.ty_params.is_empty() {
                        self.ctx.type_pool.get_ty_named(ty_decl, vec![])
                    } else {
                        self.ctx
                            .diagnostcs
                            .add(err!(span, "missing type arguments"));
                        self.ctx.type_pool.get_ty_invalid()
                    }
                }
            },
            Object::Type(ty) => ty,
            Object::Condition(_) => panic!("unexpedted condition"),
            Object::Fun(_, _)
            | Object::FunDecl(_)
            | Object::TraitMethodDecl(_, _, _, _)
            | Object::TraitMethod(_, _, _, _) => {
                self.ctx
                    .diagnostcs
                    .add(err!(span, "expected a type, found function"));
                self.ctx.type_pool.get_ty_invalid()
            }
            Object::TraitDecl(tr_decl) | Object::Trait(tr_decl, _) => {
                self.ctx.diagnostcs.add(err!(
                    span,
                    "expected a type, found trait {}",
                    tr_decl.borrow().name
                ));
                self.ctx.type_pool.get_ty_invalid()
            }
            Object::Invalid => self.ctx.type_pool.get_ty_invalid(),
        }
    }

    pub fn get_type(&mut self, span: &Span, obj: Object<'i>) -> TypeRef<'i> {
        self.get_type_and_infer(span, obj, None)
    }

    fn get_str(&self, value: &str) -> Str<'i> {
        self.ctx.type_pool.str_pool.get(value)
    }

    fn scoped<T>(&mut self, fun: impl FnOnce(&mut Self) -> T) -> T {
        self.scopes.begin();
        let result = fun(self);
        self.scopes.end();
        result
    }

    pub fn emit_iiv(&mut self, modules: &[Module<'i>]) -> Package<'i> {
        self.define_global(
            self.get_str("int"),
            Definition::Type(self.ctx.type_pool.get_int()),
        );
        self.define_global(
            self.get_str("bool"),
            Definition::Type(self.ctx.type_pool.get_ty_bool()),
        );
        self.define_global(
            self.ctx.type_pool.str_pool.get("Drop"),
            Definition::Trait(self.ctx.builtins.get_drop().decl),
        );
        self.define_global(
            self.ctx.type_pool.str_pool.get("Copy"),
            Definition::Trait(self.ctx.builtins.get_copy().decl),
        );
        self.define_global(
            self.get_str("memAlloc"),
            Definition::Fun(self.ctx.builtins.get_mem_alloc()),
        );
        self.define_global(
            self.get_str("memFree"),
            Definition::Fun(self.ctx.builtins.get_mem_free()),
        );
        self.define_global(
            self.get_str("ptrAdd"),
            Definition::Fun(self.ctx.builtins.get_ptr_add()),
        );
        self.define_global(
            self.get_str("ptrWrite"),
            Definition::Fun(self.ctx.builtins.get_ptr_write()),
        );
        self.define_global(
            self.get_str("List"),
            Definition::NamedType(self.ctx.builtins.get_list()),
        );
        self.define_global(
            self.get_str("String"),
            Definition::NamedType(self.ctx.builtins.get_string()),
        );
        // types
        let types = &modules[0].types;
        let mut package_types = vec![];

        for ty_node in types {
            self.scoped(|this| {
                for param in ty_node.ty_params() {
                    this.scopes.new_ty_param(&param.name);
                }

                let proto = this.check_type(&ty_node.proto);
                let ty = ty_decl::TypeDecl {
                    is_copy: ty_node.is_data(),
                    proto,
                    ty_params: ty_node.type_params.iter().map(|_| ()).collect(),
                    name: ty_node.name.value.unwrap_or_else(|| this.get_str("")),
                };
            });

            if ty.is_copy
                && self
                    .ctx
                    .impl_forest
                    .find(ty.proto, self.copy_trait)
                    .is_none()
            {
                self.messages.add(err!(
                    &ty_node.span(),
                    "declaration is marked as data, but the prototype does not implement Copy"
                ));
            }

            let ty = self.ty_decl_pool.insert(ty);

            self.define_spanned_global(ty_node.name, Object::TypeDecl(ty));
            package_types.push(ty);
        }

        // traits
        let traits = &modules[0].traits;
        let mut package_traits = vec![];

        for trait_node in traits {
            self.scopes.begin();
            let this_ty = self.scopes.generate_ty_param();
            self.scopes.this_ty = Some(this_ty);
            let (mut bounds, trait_ty_params) = self.resolve_ty_param_list(trait_node.ty_params());

            let tr = ty_decl::TraitDecl {
                name: trait_node
                    .name
                    .value
                    .unwrap_or_else(|| self.ty.str_pool.get("")),
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
                    tr: TraitRef {
                        decl: tr_ref,
                        ty_args: trait_ty_params,
                    },
                },
            );

            for sig in &trait_node.signatures {
                let mut fun = Function::empty(
                    self.ty,
                    &[
                        tr.name,
                        sig.name
                            .value
                            .unwrap_or_else(|| self.ty.str_pool.get("unknown")),
                    ],
                );
                let mut fun_gen = FunctionGenerator::new(self, &mut fun, None);
                fun_gen.begin_scope();
                fun_gen.set_signature(
                    &sig,
                    bounds.clone(),
                    tr.ty_params.len() + 1,
                    Some(fun_gen.ty.get_ref(this_ty)),
                );
                fun_gen.end_scope();
                let fun = self.fun_pool.insert(fun);
                tr.signatures.push(Method {
                    fun,
                    receiver: if sig.is_mut() {
                        Receiver::ByValue
                    } else {
                        Receiver::ByReference
                    },
                });
            }

            tr.signatures.sort_by_key(|sig| sig.fun.borrow().sig.name);

            self.scopes.end();

            for (idx, sig) in tr.signatures.iter().enumerate() {
                if let Some(name) = sig.fun.borrow().sig.name.last() {
                    let methods = self
                        .trait_method_scope
                        .entry(*name)
                        .or_insert_with(|| vec![]);
                    methods.push(TraitMethodRef { tr: tr_ref, idx });
                }
            }

            self.scopes.this_ty = None;
            self.define_spanned_global(trait_node.name, Object::TraitDecl(tr_ref));
            package_traits.push(tr_ref);
        }

        let trait_impls = &modules[0].trait_impls;
        let mut package_trait_impls = vec![];

        for impl_node in trait_impls {
            self.scopes.begin();
            let (own_bounds, impl_ty_params) = self.resolve_ty_param_list(impl_node.ty_params());

            let (ty, tr) = {
                let mut fun = Function::empty(self.ty, &[]);
                let mut fun_gen = FunctionGenerator::new(self, &mut fun, None);
                let ty = fun_gen.check_type(&impl_node.ty);
                fun_gen.scopes.this_ty = Some(ty);
                let tr = fun_gen.check_trait(&impl_node.tr);
                let ty = fun_gen.inf_ctx.unwrap(ty);
                let tr = tr.map(|mut tr| {
                    tr.ty_args = fun_gen
                        .ty
                        .get_ty_list(fun_gen.inf_ctx.unwrap_list(tr.ty_args));
                    tr
                });
                (ty, tr)
            };

            if let Some(tr) = tr {
                let mut present: Vec<_> = impl_ty_params.iter().map(|_| false).collect();
                iter::once(ty)
                    .chain(tr.ty_args.iter().copied())
                    .for_each(|ty| {
                        ty.visit(&mut |ty| {
                            if let Type::Constant(i) = &*ty {
                                present[*i] = true;
                            }
                        })
                    });
                if present.iter().any(|&used| !used) {
                    self.messages
                        .add(err!(&impl_node.span(), "unused type parameters {}", ty));
                }
            }

            self.scopes.this_ty = Some(ty);

            let bounds = tr
                .map(|tr| {
                    iter::once(Bound { ty, tr })
                        .chain(
                            tr.decl
                                .borrow()
                                .trait_bounds
                                .iter()
                                .map(|&bound| self.ty.resolve_bound(bound, &tr.ty_args)),
                        )
                        .chain(own_bounds.clone())
                        .collect::<Vec<_>>()
                })
                .unwrap_or(vec![]);

            let mut builder =
                TraitImplBuilder::new(self.ctx, ty, tr, impl_ty_params.len(), bounds.clone());

            for fun_node in &impl_node.functions {
                let mut fun = Function::empty(
                    self.ty,
                    &[
                        tr.map(|tr| tr.decl.borrow().name)
                            .unwrap_or(self.ty.str_pool.get("{unknown}")),
                        &format!("{}", ty),
                        fun_node
                            .name
                            .value
                            .unwrap_or(self.ty.str_pool.get("{unknown}")),
                    ],
                );
                let receiver = builder.get_receiver(fun_node.is_mut());
                let receiver_type = self.ctx.get_received_type(ty, receiver);
                let mut fun_gen = FunctionGenerator::new(self, &mut fun, None);

                fun_gen.begin_scope();
                fun_gen.set_signature(
                    &fun_node,
                    bounds.clone(),
                    impl_ty_params.len(),
                    receiver_type,
                );
                fun_gen.end_scope();

                let fun = self.fun_pool.insert(fun);
                self.index
                    .register_token_obj(fun_node.name.span, &Object::FunDecl(fun));
                builder.add_method(Method { fun, receiver }, fun_node);
            }

            if let Some(trait_impl) = builder.build(impl_node.span()) {
                package_trait_impls.push(trait_impl);
                self.ctx.impl_forest.add(&impl_node.span(), trait_impl);
            }
            self.scopes.this_ty = None;
            self.scopes.end();
        }

        let funcs = &modules[0].functions;
        let mut package_funs = vec![];
        for fun_node in funcs {
            let name: Vec<_> = fun_node.name.value.into_iter().collect();
            let mut fun = Function::empty(self.ty, &name);

            let mut fun_gen = FunctionGenerator::new(self, &mut fun, None);

            fun_gen.begin_scope();
            fun_gen.set_signature(&fun_node, vec![], 0, None);
            fun_gen.end_scope();

            let name = fun_node.name.value;

            let fun = self.fun_pool.insert(fun);
            self.index
                .register_token_obj(fun_node.name.span, &Object::FunDecl(fun));

            self.define_spanned_global(fun_node.name, Object::FunDecl(fun));
            package_funs.push(fun);
        }

        for (&fun_ref, fun_node) in package_funs.iter().zip(funcs) {
            let mut fun = fun_ref.borrow_mut();
            let mut src = SourceMap(vec![]);
            if fun_node.body.is_not_empty() {
                let gen = FunctionGenerator::new(self, &mut *fun, Some(&mut src));
                gen.emit_function_body(fun_node, Receiver::None);
                self.ctx.fun_pool.set_source_map(fun_ref, src);
            } else {
                fun.body = Body::None;
            }
        }

        for (&trait_impl, impl_node) in package_trait_impls.iter().zip(trait_impls) {
            let trait_impl = trait_impl.borrow();
            self.scopes.begin();
            for param in &impl_node.ty_params() {
                self.scopes.new_ty_param(&param.name);
            }
            for (method, fun_node) in trait_impl.functions.iter().zip(&impl_node.functions) {
                let mut src = SourceMap(vec![]);
                let mut fun = method.fun.borrow_mut();
                eprintln!("{:?}", fun.sig);
                if fun_node.body.is_not_empty() {
                    let gen = FunctionGenerator::new(self, &mut *fun, Some(&mut src));
                    gen.emit_function_body(fun_node, method.receiver);
                    self.ctx.fun_pool.set_source_map(method.fun, src);
                } else {
                    self.messages.add(err!(
                        &fun_node.span(),
                        "implementation functions must have a body"
                    ));
                }
            }
            self.scopes.end();
        }

        let main = if let Some(Object::FunDecl(main)) =
            self.scopes.resolve(self.ty.str_pool.get("main"))
        {
            Some(main)
        } else {
            None
        };

        Package {
            funcs: package_funs,
            main,
        }
    }

    fn check_type(&mut self, expr: &Expr<'i>) -> TypeRef {
        match expr {
            Expr::StructTy(props) => {}
            Expr::VariantTy(props) => self.ctx.type_pool.get_variant(),
            Expr::PtrTy(inner) => self.ctx.type_pool.get_ptr(self.check_type(&inner.rhs)),
            Expr::RefTy(inner) => self.ctx.type_pool.get_ref(self.check_type(&inner.rhs)),
        }
    }
}

impl<'f, 'i: 'f, 'g> FunctionGenerator<'f, 'i, 'g> {
    pub fn new(
        parent: &'g mut Generator<'i>,
        func: &'f mut Function<'i>,
        src: Option<&'f mut SourceMap>,
    ) -> Self {
        Self {
            trait_method_scope: &parent.trait_method_scope,
            scopes: &mut parent.scopes,
            index: &mut parent.index,
            ty: &parent.ty,
            messages: &parent.messages,
            impl_forest: &parent.ctx.impl_forest,
            constant_depth: 0,
            ret_type: None,
            iiv: iiv::builder::Cursor::new(parent.ty, func, src),
            inf_ctx: InferenceCtx::new(parent.ctx),
            copy_trait: parent.copy_trait,
            list_ty: parent.ctx.builtins.get_list(),
            string_ty: parent.ctx.builtins.get_string(),
            mem_alloc: parent.ctx.builtins.get_mem_alloc(),
            ptr_add: parent.ctx.builtins.get_ptr_add(),
            ptr_write: parent.ctx.builtins.get_ptr_write(),
            completion_token: &mut parent.completion_token,
            completion_items: &mut parent.completion_items,
        }
    }

    fn set_signature(
        &mut self,
        fun_node: &ast::Function<'i>,
        mut bounds: Vec<Bound<'i>>,
        block_ty_param_count: usize,
        this: Option<TypeRef<'i>>,
    ) {
        for param in fun_node.ty_params() {
            let ty = self.scopes.new_ty_param(&param.name);
            for tr_expr in param.bounds() {
                if let Some(tr) = self.check_trait(tr_expr) {
                    bounds.push(Bound { ty, tr });
                }
            }
        }
        self.iiv.set_bounds(bounds);

        let params = self.ty.get_ty_list(
            this.into_iter()
                .chain(fun_node.params.items().iter().map(|param| {
                    let ty = self.check_type(&param.ty);
                    self.inf_ctx.unwrap(ty)
                }))
                .collect(),
        );
        self.iiv.set_params(params);

        self.iiv
            .set_ty_params(block_ty_param_count + fun_node.ty_params().len());
        let ret_ty = fun_node
            .return_ty
            .as_ref()
            .map(|ret_ty| {
                let ty = self.check_type(&ret_ty.ty);
                self.inf_ctx.unwrap(ty)
            })
            .unwrap_or_else(|| self.ty.get_null());
        self.iiv.set_ret_ty(ret_ty);
    }

    fn emit_function_body(mut self, fun_node: &ast::Function<'i>, receiver: Receiver) {
        self.begin_scope();
        let sig = self.iiv.current_signature();
        let ret_ty = sig.ret_ty;

        for param in fun_node.ty_params() {
            self.scopes.new_ty_param(&param.name);
        }
        for bound in &sig.trait_bounds {
            self.scopes.new_bound(*bound);
        }

        let mut params = self.iiv.params();

        match receiver {
            Receiver::None => {}
            _ => self.scopes.this = params.next(),
        }

        for (param, ast_param) in params.zip(fun_node.params.items()) {
            self.scopes.define(
                &ast_param.name,
                Object::Place(param, vec![], ast_param.name.span),
            );
        }

        self.ret_type = Some(ret_ty);

        self.iiv.create_block();
        let body_node = fun_node.body.as_ref().unwrap();
        let body = self.check_val(body_node);
        let body = self.ensure_ty(&body_node.span(), ret_ty, body);
        let body = self.move_val(body);
        self.end_scope();
        self.iiv.at(body_node.span()).ret(body);

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
        self.inf_ctx.clear();
        self.scopes.this = None;
    }

    fn do_completion_for(&self, node: &impl Spanned) -> bool {
        let span = node.span();
        if let Some(Position { line, column }) = *self.completion_token {
            span.left.line == line && span.left.column == column
        } else {
            false
        }
    }

    fn complete_expression(&mut self, expr: &ast::Expr<'i>) {
        if !self.do_completion_for(expr) {
            return;
        }

        *self.completion_items = self
            .scopes
            .inner
            .iter()
            .flat_map(|scope| {
                scope.index.iter().map(|(name, obj)| {
                    let ty = match obj {
                        Object::Value(_) | Object::Place(_, _, _) | Object::UnsafePlace(_, _) => {
                            CompletionType::Variable
                        }
                        Object::Type(_) | Object::TypeDecl(_) | Object::TraitDecl(_) => {
                            CompletionType::Type
                        }
                        Object::Fun(_, _)
                        | Object::TraitMethodDecl(_, _, _, _)
                        | Object::TraitMethod(_, _, _, _)
                        | Object::FunDecl(_) => CompletionType::Function,
                        _ => CompletionType::Variable,
                    };
                    CompletionItem { text: *name, ty }
                })
            })
            .collect();

        *self.completion_token = None;
    }

    fn complete_property(&mut self, prop: &Ident<'i>, ty: TypeRef<'i>) {
        if !self.do_completion_for(prop) {
            return;
        }
        match &*ty {
            Type::Struct(props) => {
                self.completion_items
                    .extend(props.iter().map(|prop| CompletionItem {
                        text: prop.0,
                        ty: CompletionType::Property,
                    }));
            }
            Type::Variant(props) => {
                self.completion_items
                    .extend(props.iter().map(|prop| CompletionItem {
                        text: prop.0,
                        ty: CompletionType::Property,
                    }));
            }
            Type::Named(_, _, proto) => self.complete_property(prop, *proto),
            _ => {}
        }
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
                        return self.iiv.at(*span).variant_cast(ty, value);
                    };
                }
                (Type::Ref(inner), Type::Ptr(inner2)) if inner == inner2 => {
                    return self.iiv.at(*span).ref_to_ptr(value, *inner)
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
    //         (Pattern::Bind(BindingType::Let, ident), value) => {
    //             self.read_rt_value(value);
    //         }
    //         (Pattern::Bind(BindingType::Var, ident), value) => {
    //             self.read_rt_value(value);
    //         }
    //         (Pattern::Bind(BindingType::Const, ident), value) => {
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
        match &pattern {
            Pattern::Bind(BindPattern { binding_type, name }) => {
                self.register_token_obj(name.span, &Object::Place(value, vec![], name.span));
                match binding_type {
                    BindingType::Var(_) => self
                        .scopes
                        .define(name, Object::Place(value, vec![], name.span)),
                    BindingType::Let(_) => self
                        .scopes
                        .define(name, Object::Place(value, vec![], name.span)),
                    _ => unimplemented!(),
                }
            }
            Pattern::NarrowType(NarrowTypePattern { inner, ty, .. }) => {
                let target_ty = self.check_type(ty);
                let narrowed = self.ensure_ty(&ty.span(), target_ty, value);
                self.bind(&inner, narrowed, no_match_block, match_block);
            }
            Pattern::Variant(variant) => {
                let Type::Variant(elems) = *value.ty else {
                    self.msg(err!(
                        &pattern.span(),
                        "expected a variant type, found {}",
                        value.ty
                    ));
                    return;
                };

                let Some(name) = variant.name.value else {
                    return;
                };

                let Some(i) =
                    elems.iter().enumerate().find_map(
                        |(i, elem)| {
                            if elem.0 == name {
                                Some(i)
                            } else {
                                None
                            }
                        },
                    )
                else {
                    self.msg(err!(
                        &pattern.span(),
                        "variant {} not found on type {}",
                        name,
                        value.ty
                    ));
                    return;
                };

                let discriminant = self.iiv.at(pattern.span()).discriminant(value);
                let expected = self.iiv.int_lit(i as u32);
                let cond = self.iiv.equals(discriminant, expected);
                let next_block = self.iiv.create_block();
                let no_match = no_match_block.unwrap();
                self.iiv.branch(cond, next_block, no_match);
                self.iiv.select(next_block);
                let inner = self.iiv.at(pattern.span()).copy_prop_deep(
                    value,
                    vec![Elem::Prop(iiv::Prop(i as u8))],
                    elems[i].1,
                );
                if let Some(VariantPatternBody {
                    inner: inner_pattern,
                    ..
                }) = &variant.inner
                {
                    self.bind(&inner_pattern, inner, no_match_block, match_block);
                } else {
                    self.iiv.select(match_block.unwrap());
                    self.drop(inner);
                    self.iiv.select(next_block);
                }
            }
            Pattern::Literal(_) => unimplemented!(),
            Pattern::Struct(struct_pattern) => {
                if let Type::Struct(_) = *value.ty {
                    for PropPattern { name, pattern, .. } in struct_pattern.inner.items() {
                        if let Some((props, ty)) = self.lookup_field_on_type(value.ty, name) {
                            let prop = self.iiv.at(pattern.span()).move_prop_deep(value, props, ty);
                            self.bind(pattern, prop, no_match_block, match_block);
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
                Object::Place(val, offsets, _) | Object::UnsafePlace(val, offsets) => {
                    self.copy_prop(&expr.span(), val, offsets, val.ty)
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
            Object::Place(val, offsets, _) | Object::UnsafePlace(val, offsets) => {
                self.copy_prop(span, val, offsets, val.ty)
            }
            Object::TypeDecl(ty_decl) => {
                self.msg(err!(span, "expected a value, found type {}", ty_decl.name));
                self.invalid()
            }
            Object::TraitDecl(tr_decl) | Object::Trait(tr_decl, _) => {
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
            Object::Value(val) | Object::Place(val, _, _) | Object::UnsafePlace(val, _) => {
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
            Object::TraitDecl(tr_decl) => Some(TraitRef {
                decl: tr_decl,
                ty_args: self.ty.get_ty_list(
                    tr_decl
                        .borrow()
                        .ty_params
                        .iter()
                        .map(|_| self.inf_ctx.new_var(*span))
                        .collect(),
                ),
            }),
            Object::Trait(decl, ty_args) => Some(TraitRef { decl, ty_args }),
            Object::Invalid => None,
        }
    }

    fn create_ty_arg_list(&mut self, len: usize, span: Span) -> pool::List<'i, TypeRef<'i>> {
        self.ty.get_ty_list(
            (0..len)
                .into_iter()
                .map(|_| self.inf_ctx.new_var(span))
                .collect(),
        )
    }

    fn get_callable(&mut self, obj: Object<'i>, span: Span) -> Callable<'i> {
        match obj {
            Object::Condition(_) => panic!("unexpedted condition"),
            Object::Fun(func, ty_args) => Callable::Fun { fun: func, ty_args },
            Object::FunDecl(func) => Callable::Fun {
                fun: func,
                ty_args: self.create_ty_arg_list(func.borrow().sig.ty_params.len(), span),
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
                ty_args: self.create_ty_arg_list(ty_decl.ty_params.len(), span),
            },
            Object::TraitMethod(this, tr, idx, ty_args) => Callable::TraitMethod {
                this,
                tr,
                idx,
                ty_args,
            },
            Object::TraitMethodDecl(this, ty, tr, idx) => {
                let ty_args = self.ty.get_ty_list(
                    iter::once(ty)
                        .chain(tr.ty_args.iter().copied())
                        .chain(
                            tr.decl.borrow().signatures[idx]
                                .fun
                                .borrow()
                                .sig
                                .ty_params
                                .iter()
                                .skip(tr.ty_args.len() + 1)
                                .map(|_| self.inf_ctx.new_var(span)),
                        )
                        .collect(),
                );
                Callable::TraitMethod {
                    this,
                    tr: tr.decl,
                    idx,
                    ty_args,
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
            Object::UnsafePlace(mut place, mut offsets) => {
                match self.lookup_item_on_type(place.ty, prop) {
                    Item::Field(extra_offsets, ty) => {
                        offsets.extend(extra_offsets);
                        place.ty = ty;
                        Object::UnsafePlace(place, offsets)
                    }
                    Item::TraitMethod(_, _) => {
                        self.msg(err!(
                            &prop.span(),
                            "trait methods may not be invoked on raw pointers"
                        ));
                        Object::Invalid
                    }
                    Item::Invalid => Object::Invalid,
                }
            }
            Object::Place(mut place, mut offsets, span) => {
                match self.lookup_item_on_type(place.ty, prop) {
                    Item::Field(extra_offsets, ty) => {
                        offsets.extend(extra_offsets);
                        place.ty = ty;
                        Object::Place(place, offsets, span)
                    }
                    Item::TraitMethod(tr_decl, idx) => Object::TraitMethodDecl(
                        match tr_decl.borrow().signatures[idx].receiver {
                            Receiver::ByValue => Some(
                                self.iiv.copy_prop_deep(
                                    place,
                                    offsets
                                        .iter()
                                        .map(|i| iiv::Elem::Prop(iiv::Prop(*i)))
                                        .collect(),
                                    place.ty,
                                ),
                            ),
                            Receiver::ByReference => {
                                Some(self.iiv.get_prop_ref(place, offsets, place.ty))
                            }
                            Receiver::None => {
                                self.msg(err!(&prop.span(), "static method called on a value"));
                                None
                            }
                        },
                        place.ty,
                        TraitRef(
                            tr_decl,
                            self.create_ty_arg_list(tr_decl.borrow().ty_params.len(), prop.span),
                        ),
                        idx,
                    ),
                    Item::Invalid => Object::Invalid,
                }
            }
            Object::Value(value) => match self.lookup_item_on_type(value.ty, prop) {
                Item::Field(offsets, ty) => {
                    let prop = self.iiv.move_prop_deep(value, offsets, ty).obj();
                    self.drop(value);
                    prop
                }
                Item::TraitMethod(tr_decl, idx) => Object::TraitMethodDecl(
                    match tr_decl.borrow().signatures[idx].receiver {
                        Receiver::ByValue => Some(value),
                        Receiver::ByReference => {
                            self.msg(err!(&prop.span(), "mut method called on immutable value"));
                            Some(self.invalid())
                        }
                        Receiver::None => {
                            self.msg(err!(&prop.span(), "static method called on a value"));
                            None
                        }
                    },
                    value.ty,
                    TraitRef(
                        tr_decl,
                        self.create_ty_arg_list(tr_decl.borrow().ty_params.len(), prop.span),
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
        self.complete_expression(expr);

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
                        self.drop(last_result);
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
            Expr::String(string_lit) => {
                let int_list = self.ty.get_ty_named(self.list_ty, vec![self.ty.get_int()]);
                let mut idx = self.iiv.int_lit(0);
                let one = self.iiv.int_lit(1);
                let len = self.iiv.int_lit(string_lit.value.len() as u32);
                let only_int_list = self.ty.get_ty_list(vec![self.ty.get_int()]);
                let len_cp2 = self.copy(len);
                let buf = self.iiv.call(self.mem_alloc, &[len_cp2], only_int_list);
                for character in string_lit.value.chars() {
                    let buf = self.copy(buf);
                    let idx_cp = self.copy(idx);
                    let ptr = self.iiv.call(self.ptr_add, &[buf, idx_cp], only_int_list);
                    let val = self.iiv.int_lit(character as u32);
                    let null = self.iiv.call(self.ptr_write, &[ptr, val], only_int_list);
                    self.drop(null);
                    let one = self.copy(one);
                    idx = self.iiv.add(idx, one);
                }
                self.drop(one);
                self.drop(idx);
                let len_cp = self.copy(len);

                let Type::Named(_, _, proto) = &*int_list else {
                    unreachable!()
                };
                let Type::Struct(ty) = &**proto else {
                    unreachable!()
                };

                let mut props = vec![len_cp; 3];
                props[ty.iter().position(|prop| &*prop.0 == "buf").unwrap()] = buf;
                props[ty.iter().position(|prop| &*prop.0 == "cap").unwrap()] = len_cp;
                props[ty.iter().position(|prop| &*prop.0 == "len").unwrap()] = len;
                let inner = self.iiv.make_struct(&props, *proto);
                let inner = self.iiv.named(int_list, inner);
                self.iiv
                    .named(self.ty.get_ty_named(self.string_ty, vec![]), inner)
                    .obj()
            }
            Expr::Char(_char) => unimplemented!(),
            Expr::Tuple(tuple) => {
                let items: Vec<_> = tuple.fields.iter().map(|e| self.check_val(e)).collect();
                self.iiv.make_tuple(&items).obj()
            }
            Expr::Struct(structure) => {
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

                for (ast_prop, prop) in structure.props.iter().zip(&props) {
                    self.index.register_token(
                        ast_prop.name.span,
                        TokenType::Property {
                            name: ast_prop.name.value,
                            ty: prop.1,
                        },
                    );
                }
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

                let yes = self.check_val(&if_expr.yes);
                let yes = self.move_val(yes);
                let yes_block = self.iiv.get_current_block();

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
                self.iiv.jump(cond_block, &[]);
                self.iiv.select(cond_block);

                let after = self.iiv.create_block();

                let scopes = self.check_condition(&while_expr.condition, after);

                let body = self.check_val(&*while_expr.body);
                self.drop(body);
                for _ in 0..scopes {
                    self.end_scope();
                }
                self.iiv.jump(cond_block, &[]);

                self.iiv.select(after);
                self.null().obj()
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
                    Object::TraitDecl(decl) => {
                        if args.len() == decl.borrow().ty_params.len() {
                            Object::Trait(decl, self.ty.get_ty_list(args))
                        } else {
                            self.msg(err!(&apply.span, "invalid number of type arguments"));
                            Object::Invalid
                        }
                    }
                    _ => {
                        self.msg(err!(&apply.lhs.span(), "expected a generic declaration"));
                        Object::Invalid
                    }
                }
            }
            Expr::Call(call) => {
                let obj = self.check(&call.lhs, None);
                match self.get_callable(obj, call.span) {
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
                if let Object::Value(val) | Object::Place(val, _, _) | Object::UnsafePlace(val, _) =
                    lhs
                {
                    self.complete_property(&prop.prop, val.ty);
                }
                self.get_prop(lhs, &prop.prop)
            }
            Expr::Field(_field) => unimplemented!(),
            Expr::Cast(_cast) => unimplemented!(),
            Expr::RefTo(ref_to) => {
                let checked = self.check(&ref_to.rhs, false_block);
                match checked {
                    Object::Place(place, offsets, _) => {
                        let r = self
                            .iiv
                            .at(ref_to.rhs.span())
                            .get_prop_ref(place, offsets, place.ty);
                        r.obj()
                    }
                    Object::UnsafePlace(place, offsets) => {
                        let r = self
                            .iiv
                            .at(ref_to.rhs.span())
                            .get_prop_ptr(place, offsets, place.ty);
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
                if let Object::Place(val, mut offsets, _) | Object::UnsafePlace(val, mut offsets) =
                    checked
                {
                    match &*val.ty {
                        Type::Ref(inner) => {
                            offsets.push(0);
                            Object::Place(
                                Value {
                                    ty: *inner,
                                    raw: val.raw,
                                },
                                offsets,
                                Span::null(),
                            )
                        }
                        Type::Ptr(inner) => {
                            offsets.push(0);
                            Object::UnsafePlace(
                                Value {
                                    ty: *inner,
                                    raw: val.raw,
                                },
                                offsets,
                            )
                        }
                        _ => {
                            self.msg(err!(
                                &deref.lhs.span(),
                                "only references can be derefernced"
                            ));
                            self.invalid().obj()
                        }
                    }
                } else {
                    self.msg(err!(&deref.lhs.span(), "only l-values can be derefernced"));
                    self.invalid().obj()
                }
            }
            Expr::Add(add) => {
                let lhs = self.check_val(&add.lhs);
                let rhs = self.check_val(&add.rhs);
                let lhs = self.ensure_ty(&add.lhs.span(), self.ty.get_int(), lhs);
                let rhs = self.ensure_ty(&add.rhs.span(), self.ty.get_int(), rhs);
                self.iiv.add(lhs, rhs).obj()
            }
            Expr::Mul(mul) => {
                let lhs = self.check_val(&mul.lhs);
                let rhs = self.check_val(&mul.rhs);
                let lhs = self.ensure_ty(&mul.lhs.span(), self.ty.get_int(), lhs);
                let rhs = self.ensure_ty(&mul.rhs.span(), self.ty.get_int(), rhs);
                self.iiv.mul(lhs, rhs).obj()
            }
            Expr::Eq(equals) => {
                let lhs = self.check_val(&equals.lhs);
                let rhs = self.check_val(&equals.rhs);
                let lhs = self.ensure_ty(&equals.lhs.span(), self.ty.get_int(), lhs);
                let rhs = self.ensure_ty(&equals.rhs.span(), self.ty.get_int(), rhs);
                self.iiv.equals(lhs, rhs).obj()
            }
            Expr::Neq(not_equals) => {
                let lhs = self.check_val(&not_equals.lhs);
                let rhs = self.check_val(&not_equals.rhs);
                let lhs = self.ensure_ty(&not_equals.lhs.span(), self.ty.get_int(), lhs);
                let rhs = self.ensure_ty(&not_equals.rhs.span(), self.ty.get_int(), rhs);
                self.iiv.not_equals(lhs, rhs).obj()
            }
            Expr::And(_and) => unimplemented!(),
            Expr::Or(_or) => unimplemented!(),
            Expr::Geq(greater_eq) => {
                let lhs = self.check_val(&greater_eq.lhs);
                let rhs = self.check_val(&greater_eq.rhs);
                let lhs = self.ensure_ty(&greater_eq.lhs.span(), self.ty.get_int(), lhs);
                let rhs = self.ensure_ty(&greater_eq.rhs.span(), self.ty.get_int(), rhs);
                self.iiv.greater_eq(lhs, rhs).obj()
            }
            Expr::Leq(less_eq) => {
                let lhs = self.check_val(&less_eq.lhs);
                let rhs = self.check_val(&less_eq.rhs);
                let lhs = self.ensure_ty(&less_eq.lhs.span(), self.ty.get_int(), lhs);
                let rhs = self.ensure_ty(&less_eq.rhs.span(), self.ty.get_int(), rhs);
                self.iiv.less_eq(lhs, rhs).obj()
            }
            Expr::Lt(less) => {
                let lhs = self.check_val(&less.lhs);
                let rhs = self.check_val(&less.rhs);
                let lhs = self.ensure_ty(&less.lhs.span(), self.ty.get_int(), lhs);
                let rhs = self.ensure_ty(&less.rhs.span(), self.ty.get_int(), rhs);
                self.iiv.less(lhs, rhs).obj()
            }
            Expr::Gt(greater) => {
                let lhs = self.check_val(&greater.lhs);
                let rhs = self.check_val(&greater.rhs);
                let lhs = self.ensure_ty(&greater.lhs.span(), self.ty.get_int(), lhs);
                let rhs = self.ensure_ty(&greater.rhs.span(), self.ty.get_int(), rhs);
                self.iiv.greater(lhs, rhs).obj()
            }
            Expr::Assign(assign) => {
                let rhs = self.check_val(&assign.rhs);
                let lhs = self.check(&assign.lhs, None);
                match lhs {
                    Object::Place(place, offsets, _) | Object::UnsafePlace(place, offsets) => {
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
            Expr::StructTy(ast_props) => {
                let props = self.resolve_props(ast_props);
                for (ast_prop, prop) in ast_props.props.iter().zip(&props) {
                    self.index.register_token(
                        ast_prop.name.span,
                        TokenType::Property {
                            name: ast_prop.name.value,
                            ty: prop.1,
                        },
                    );
                }
                self.ty.get_struct(props).obj()
            }
            Expr::RefTy(inner) => {
                let inner = self.check_type(&inner.rhs);
                self.ty.get_ref(inner).obj()
            }
            Expr::PtrTy(inner) => {
                let inner = self.check_type(&inner.rhs);
                self.ty.get_ptr(inner).obj()
            }
            Expr::Invalid(_) => Object::Invalid,
        };
        object
    }

    fn drop(&mut self, val: Value<'i>) {
        self.iiv.drop(val);
    }

    fn copy(&mut self, value: Value<'i>) -> Value<'i> {
        self.iiv.copy_prop_deep(value, vec![], value.ty)
    }

    fn copy_prop(
        &mut self,
        span: &Span,
        value: Value<'i>,
        props: Vec<u8>,
        ty: TypeRef<'i>,
    ) -> Value<'i> {
        self.iiv.at(*span).copy_prop_deep(
            value,
            props.iter().map(|prop| Elem::Prop(Prop(*prop))).collect(),
            ty,
        )
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
        let Some(name_str) = name.value else {
            return Object::Invalid;
        };

        match &*name_str {
            "this" => {
                if let Some(this) = self.scopes.this {
                    return Object::Place(this, vec![], Span::null());
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

        if let Some(obj) = self.scopes.resolve(name_str) {
            return obj.clone();
        }

        self.msg(err!(
            &name.span,
            "\"{}\" is not defined in this context",
            name_str
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

#[derive(Clone, Copy, Debug)]
enum Definition<'i> {
    // Trait,
    // Module,
    Invalid,
    Value(Value<'i>),
    MutValue(Value<'i>),
    NamedType(TyDeclRef<'i>),
    Type(TypeRef<'i>),
    Trait(TraitDeclRef<'i>),
    Fun(FuncRef<'i>),
}

#[derive(Clone, Debug)]
enum Object<'i> {
    // Trait,
    // Module,
    Invalid,
    Value(Value<'i>),
    Place(Value<'i>, Vec<u8>, Span),
    UnsafePlace(Value<'i>, Vec<u8>),
    Type(TypeRef<'i>),
    TypeDecl(TyDeclRef<'i>),
    TraitDecl(TraitDeclRef<'i>),
    Trait(TraitDeclRef<'i>, pool::List<'i, TypeRef<'i>>),
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
