use ast::{
    BindingType, BlockItem, Expr, Ident, Parameter, Pattern, PatternBody, PropsTy, Signature,
    Spanned, StructProp, TyProp,
};
use iiv::{diagnostics::fmt::n_of_digits, LeftSpan, RightSpan, Span};
use lsp_types::{Position, Range, TextEdit};

enum Declaration<'i> {
    Function(ast::Function<'i>),
    Type(ast::TypeDecl<'i>),
    Trait(ast::TraitDecl<'i>),
}

trait TextRange: Sized + Copy {
    type Left: TextPosition;
    type Right: TextPosition;

    fn range(self) -> Range {
        self.left().to_pos(self.right())
    }

    fn left(self) -> Self::Left;
    fn right(self) -> Self::Right;
}

impl TextRange for Span {
    type Left = LeftSpan;
    type Right = RightSpan;
    fn left(self) -> Self::Left {
        self.left_span()
    }
    fn right(self) -> Self::Right {
        self.right_span()
    }
}

impl TextRange for Range {
    type Left = Position;
    type Right = Position;
    fn left(self) -> Self::Left {
        self.start
    }
    fn right(self) -> Self::Right {
        self.end
    }
}

impl<L: TextPosition, R: TextPosition> TextRange for (L, R) {
    type Left = L;
    type Right = R;
    fn left(self) -> Self::Left {
        self.0
    }
    fn right(self) -> Self::Right {
        self.1
    }
}

trait TextPosition: Sized + Copy {
    fn pos(self) -> Position;

    fn to_pos(self, other: impl TextPosition) -> Range {
        Range {
            start: self.pos(),
            end: other.pos(),
        }
    }

    fn move_char(self, distance: u32) -> Position {
        let mut pos = self.pos();
        pos.character += distance;
        pos
    }
}

impl TextPosition for LeftSpan {
    fn pos(self) -> Position {
        Position {
            line: self.first_line,
            character: self.begin_highlight_offset,
        }
    }
}

impl TextPosition for RightSpan {
    fn pos(self) -> Position {
        Position {
            line: self.last_line,
            character: self.end_highlight_offset,
        }
    }
}

impl TextPosition for Position {
    fn pos(self) -> Position {
        self
    }
}

fn span_range(span: Span) -> Range {
    Range {
        start: span.left().pos(),
        end: span.right().pos(),
    }
}

// fn between(left: Span, right: Span) -> Range {
//     Range {
//         start: Position {
//             line: left.last_line,
//             character: left.end_highlight_offset,
//         },
//         end: Position {
//             line: right.first_line,
//             character: right.begin_highlight_offset,
//         },
//     }
// }

enum FmtResult {
    InlineFit(u32),
    MultilineFit(u32),
    NoFit(u32),
}

pub fn format_module(module: ast::Module) -> Vec<TextEdit> {
    let mut decls: Vec<_> = module
        .functions
        .into_iter()
        .map(Declaration::Function)
        .chain(module.types.into_iter().map(Declaration::Type))
        .chain(module.traits.into_iter().map(Declaration::Trait))
        .collect();

    decls.sort_by_key(|decl| match decl {
        Declaration::Function(fun) => fun.span().begin_offset + fun.span().begin_highlight_offset,
        Declaration::Trait(tr) => tr.span.begin_offset + tr.span.begin_highlight_offset,
        Declaration::Type(ty) => ty.span().begin_offset + ty.span().begin_highlight_offset,
    });

    let mut edits = Edits(vec![]);

    for decl in &decls {
        match decl {
            Declaration::Function(fun) => edits.format_function(fun),
            _ => {}
        }
    }

    edits.0
}

struct Edits(Vec<TextEdit>);

struct Ctx<'e> {
    edits: &'e mut Edits,
    indent: u32,
    offset: u32,
    ty: CtxType,
}

enum CtxType {
    Inline,
    BlockItem,
}

impl<'e> Ctx<'e> {
    fn move_right(mut self, chars: usize) -> Self {
        self.offset += chars as u32;
        // if self.offset > MAX_LINE_LEN {
        //     self.ok = false;
        // }
        self
    }

    fn replace(self, range: impl TextRange, text: &str) -> Self {
        let mut new_line = String::with_capacity(self.indent as usize + 1);
        new_line.push('\n');
        for _ in 0..(self.indent) {
            new_line.push(' ');
        }
        self.edits.add(range.range(), text.replace("\n", &new_line));
        self.move_right(text.len())
    }

    fn add_indent(mut self) -> Self {
        self.indent += 4;
        self
    }

    fn remove_indent(mut self) -> Self {
        self.indent -= 4;
        self
    }

    fn save(self) -> (Self, usize) {
        let len = self.edits.0.len();
        (self, len)
    }

    fn format_list(mut self, things: &[impl Fmt], separator: &str) -> Self {
        for thing in things {
            self = thing.format_in(self);
        }
        for pair in things.windows(2) {
            self = self.replace((pair[0].right_span(), pair[1].left_span()), separator);
        }
        self
    }

    fn struct_like(self, span: Span, items: &[impl Fmt]) -> Self {
        match items.len() {
            0 => self.replace(span, "{}"),
            _ => {
                let left = span.left().to_pos(items[0].left_span());
                let right = items.last().unwrap().right_span().to_pos(span.right());
                match self.ty {
                    CtxType::Inline => self
                        .replace(left, "{ ")
                        .format_list(items, ", ")
                        .replace(right, " }"),
                    CtxType::BlockItem => self
                        .add_indent()
                        .replace(left, "{\n")
                        .format_list(items, ",\n")
                        .remove_indent()
                        .replace(right, ",\n}"),
                }
            }
        }
    }

    fn format_inline_list(mut self, things: &[impl Fmt], separator: &str) -> Self {
        for thing in things {
            self = thing.format_in(self);
        }
        for pair in things.windows(2) {
            self = self.replace((pair[0].right_span(), pair[1].left_span()), separator);
        }
        self
    }

    fn then(self, item: &impl Fmt) -> Self {
        item.format_in(self)
    }

    fn set_ty(mut self, ty: CtxType) -> Self {
        self.ty = ty;
        self
    }

    fn call_like(
        self,
        span: impl TextRange,
        lhs: &impl Fmt,
        args: &[impl Fmt],
        delimiters: &str,
    ) -> Self {
        match args.len() {
            0 => {
                let right = lhs.right_span().to_pos(span.right());
                self.replace(right, delimiters)
            }
            _ => {
                let lhs_to_args = lhs.right_span().to_pos(args[0].left_span());
                let right = args.last().unwrap().right_span().to_pos(span.right());
                self.replace(lhs_to_args, &delimiters[..1])
                    .format_list(&args, ", ")
                    .replace(right, &delimiters[1..])
            }
        }
    }

    fn bin_op(self, lhs: &impl Fmt, rhs: &impl Fmt, op: &str) -> Self {
        self.then(lhs)
            .replace((lhs.right_span(), rhs.left_span()), op)
            .then(rhs)
    }

    fn variant_like(mut self, span: Span, name: &Ident, inner: &Option<impl Fmt>) -> Self {
        let dot_to_name = span.left().to_pos(name.span.left());
        self = self.replace(dot_to_name, ".");

        if let Some(inner) = inner {
            let left = name.span.right().to_pos(inner.left_span());
            let right = inner.right_span().to_pos(span.right());
            self.replace(left, "(").then(inner).replace(right, ")")
        } else {
            let parens = name.span.right().to_pos(span.right_span());
            self.replace(parens, "()")
        }
    }
}

trait Fmt: Spanned {
    fn format_in<'e>(&self, ctx: Ctx<'e>) -> Ctx<'e>;
}

impl<'i> Fmt for Ident<'i> {
    fn format_in<'e>(&self, ctx: Ctx<'e>) -> Ctx<'e> {
        ctx.move_right(self.value.len())
    }
}

impl<'i> Fmt for Parameter<'i> {
    fn format_in<'e>(&self, ctx: Ctx<'e>) -> Ctx<'e> {
        let range = self.name.right_span().to_pos(self.ty.left_span());
        ctx.then(&self.name).replace(range, ": ").then(&self.ty)
    }
}

impl<'i> Fmt for (Ident<'i>, Pattern<'i>) {
    fn format_in<'e>(&self, ctx: Ctx<'e>) -> Ctx<'e> {
        let range = self.0.right_span().to_pos(self.1.left_span());
        ctx.then(&self.0).replace(range, ": ").then(&self.1)
    }
}

impl<'i> Fmt for StructProp<'i> {
    fn format_in<'e>(&self, ctx: Ctx<'e>) -> Ctx<'e> {
        if let Some(value) = &self.value {
            let range = self.name.right_span().to_pos(value.left_span());
            ctx.then(&self.name).replace(range, ": ").then(value)
        } else {
            ctx.then(&self.name)
        }
    }
}

impl<'i> Fmt for TyProp<'i> {
    fn format_in<'e>(&self, ctx: Ctx<'e>) -> Ctx<'e> {
        if let Some(ty) = &self.ty {
            let range = self.name.right_span().to_pos(ty.left_span());
            ctx.then(&self.name).replace(range, ": ").then(ty)
        } else {
            ctx.then(&self.name)
        }
    }
}

impl<T: Fmt> Fmt for Box<T>
where
    Box<T>: Spanned,
{
    fn format_in<'e>(&self, ctx: Ctx<'e>) -> Ctx<'e> {
        <T as Fmt>::format_in(&*self, ctx)
    }
}

impl<'i> Fmt for Expr<'i> {
    fn format_in<'e>(&self, ctx: Ctx<'e>) -> Ctx<'e> {
        match self {
            Expr::Variable(var) => ctx.then(var),
            Expr::Block(items) => match items.items.len() {
                0 => ctx.replace(items.span, "{}"),
                _ => {
                    let left = items.span.left().to_pos(items.items[0].left_span());
                    let right = items
                        .items
                        .last()
                        .unwrap()
                        .right_span()
                        .to_pos(items.span.right());
                    match ctx.ty {
                        CtxType::Inline => {
                            let ctx = ctx.format_list(&items.items, "; ").replace(left, "{ ");
                            if items.has_trailing_expression {
                                ctx.replace(right, " }")
                            } else {
                                ctx.replace(right, "; }")
                            }
                        }
                        CtxType::BlockItem => {
                            let ctx = ctx
                                .add_indent()
                                .replace(left, "{\n")
                                .format_list(&items.items, ";\n")
                                .remove_indent();
                            if items.has_trailing_expression {
                                ctx.replace(right, "\n}")
                            } else {
                                ctx.replace(right, ";\n}")
                            }
                        }
                    }
                }
            },
            Expr::Int(int) => ctx.move_right(n_of_digits(int.value) as usize),
            Expr::Bool(boolean) => ctx.move_right({
                if boolean.value {
                    4
                } else {
                    5
                }
            }),
            Expr::Float(_) => unimplemented!(),
            Expr::String(_) => unimplemented!(),
            Expr::Char(_) => unimplemented!(),
            Expr::Tuple(_) => unimplemented!(),
            Expr::Struct(structure) => ctx.struct_like(structure.span, &structure.props),
            Expr::If(if_expr) => {
                let cond_left = if_expr.span.left().to_pos(if_expr.condition.left_span());
                let cond_to_yes = if_expr
                    .condition
                    .right_span()
                    .to_pos(if_expr.yes.left_span());
                match ctx.ty {
                    CtxType::Inline => {
                        let ctx = ctx
                            .replace(cond_left, "if (")
                            .then(&if_expr.condition)
                            .replace(cond_to_yes, ") ")
                            .then(&if_expr.yes);
                        if let Some(no) = &if_expr.no {
                            let yes_to_no = if_expr.yes.right_span().to_pos(no.left_span());
                            ctx.replace(yes_to_no, " else ").then(no)
                        } else {
                            ctx
                        }
                    }
                    CtxType::BlockItem => {
                        let ctx = if let Expr::Block(_) = &*if_expr.yes {
                            ctx.replace(cond_left, "if (")
                                .then(&if_expr.condition)
                                .replace(cond_to_yes, ") ")
                                .then(&if_expr.yes)
                        } else {
                            ctx.replace(cond_left, "if (")
                                .then(&if_expr.condition)
                                .add_indent()
                                .replace(cond_to_yes, ")\n")
                                .then(&if_expr.yes)
                                .remove_indent()
                        };

                        if let Some(no) = &if_expr.no {
                            let yes_to_no = if_expr.yes.right_span().to_pos(no.left_span());
                            let before_no = no.left_span().to_pos(no.left_span());
                            if let Expr::Block(_) = &**no {
                                ctx.replace(yes_to_no, " else ").then(no)
                            } else {
                                ctx.replace(yes_to_no, "\nelse")
                                    .add_indent()
                                    .replace(before_no, "\n")
                                    .then(no)
                                    .remove_indent()
                            }
                        } else {
                            ctx
                        }
                    }
                }
            }
            Expr::While(_) => unimplemented!(),
            Expr::Match(_) => unimplemented!(),
            Expr::Call(call) => ctx.call_like(call.span, &*call.lhs, &call.args, "()"),
            Expr::Prop(prop) => {
                let dot = prop.lhs.right_span().to_pos(prop.prop.left_span());
                ctx.then(&prop.lhs).replace(dot, ".").then(&prop.prop)
            }
            Expr::Field(_) => unimplemented!(),
            Expr::TyArgApply(apply) => ctx.call_like(apply.span, &*apply.lhs, &apply.args, "[]"),
            Expr::Cast(_) => unimplemented!(),
            Expr::Add(add) => ctx.bin_op(&*add.lhs, &*add.rhs, " + "),
            Expr::Mul(_) => unimplemented!(),
            Expr::Eq(eq) => ctx.bin_op(&*eq.lhs, &*eq.rhs, " == "),
            Expr::Neq(_) => unimplemented!(),
            Expr::And(_) => unimplemented!(),
            Expr::Or(_) => unimplemented!(),
            Expr::Geq(_) => unimplemented!(),
            Expr::Leq(_) => unimplemented!(),
            Expr::Lt(_) => unimplemented!(),
            Expr::Gt(_) => unimplemented!(),
            Expr::Assign(assign) => ctx.bin_op(&*assign.lhs, &*assign.rhs, " = "),
            Expr::Div(_) => unimplemented!(),
            Expr::Sub(_) => unimplemented!(),
            Expr::Neg(_) => unimplemented!(),
            Expr::Not(_) => unimplemented!(),
            Expr::RefTo(ref_to) | Expr::RefTy(ref_to) | Expr::PtrTy(ref_to) => {
                let left = ref_to.span.left().to_pos(ref_to.rhs.left_span());
                ctx.replace(left, "&").then(&ref_to.rhs)
            }
            Expr::Deref(deref) => {
                let right = deref.lhs.right_span().to_pos(deref.span.right());
                ctx.then(&deref.lhs).replace(right, ".*")
            }
            Expr::Vec(_) => unimplemented!(),
            Expr::Variant(variant) => {
                ctx.variant_like(variant.span, &variant.variant, &variant.value)
            }
            Expr::AddAssign(_) => unimplemented!(),
            Expr::Is(is) => ctx.bin_op(&is.lhs, &is.rhs, " is "),
            Expr::VariantTy(variant) => match variant.props.len() {
                0 => ctx.replace(variant.span, "{|}"),
                _ => {
                    let left = variant.span.left().to_pos(variant.props[0].left_span());
                    let right = variant
                        .props
                        .last()
                        .unwrap()
                        .right_span()
                        .to_pos(variant.span.right());
                    match ctx.ty {
                        CtxType::Inline => ctx
                            .replace(left, "{ ")
                            .format_list(&variant.props, " | ")
                            .replace(right, " }"),
                        CtxType::BlockItem => ctx
                            .add_indent()
                            .replace(left, "{\n|")
                            .format_list(&variant.props, "\n|")
                            .remove_indent()
                            .replace(right, "\n}"),
                    }
                }
            },
            Expr::StructTy(structure) => ctx.struct_like(structure.span, &structure.props),
        }
    }
}

impl<'i> Fmt for Pattern<'i> {
    fn format_in<'e>(&self, ctx: Ctx<'e>) -> Ctx<'e> {
        match &self.body {
            PatternBody::Literal(lit) => ctx.then(lit),
            PatternBody::Tuple(tuple) => {
                unimplemented!()
                // match tuple.patterns.len() {
                // 0 => {
                //     edits.add(span_range(tuple.span), "()");
                //     2
                // }
                // 1 => {
                //     let left = tuple.span.left().to_pos(tuple.patterns[0].left_span());
                //     let right = tuple.patterns[0].right_span().to_pos(tuple.span.right());
                //     let pattern_len = tuple.patterns[0].format_into(edits);
                //     edits.add(left, "(");
                //     edits.add(right, ",)");
                //     pattern_len + 3
                // }
                // _ => {
                //     let left = tuple.span.left().to_pos(tuple.patterns[0].left_span());
                //     let right = tuple
                //         .patterns
                //         .last()
                //         .unwrap()
                //         .right_span()
                //         .to_pos(tuple.span.right());

                //     let patten_lens = edits.format_comma_list(&tuple.patterns);

                //     edits.add(left, "(");
                //     edits.add(right, ")");
                //     patten_lens + 2
                // }}
            }
            PatternBody::Struct(structure) => ctx.struct_like(structure.span, &structure.inner),
            PatternBody::Variant(variant) => {
                ctx.variant_like(variant.span, &variant.name, &variant.inner)
            }
            PatternBody::NarrowType(_) => unimplemented!(),
            PatternBody::Named(_) => unimplemented!(),
            PatternBody::Vector(_) => unimplemented!(),
            PatternBody::UnionTy(_) => unimplemented!(),
            PatternBody::VariantTy(_) => unimplemented!(),
            PatternBody::NamedTy(_) => unimplemented!(),
            PatternBody::Type(_) => unimplemented!(),
            PatternBody::Bind(bind) => ctx.then(&bind.name),
        }
    }
}

impl<'i> Fmt for BlockItem<'i> {
    fn format_in<'e>(&self, ctx: Ctx<'e>) -> Ctx<'e> {
        match self {
            BlockItem::Expr(expr) => ctx.then(expr),
            BlockItem::Bind(binding) => {
                let kw = match binding.binding_type {
                    BindingType::Const => 5,
                    BindingType::Let => 3,
                    BindingType::Var => 3,
                };
                let kw_to_pattern = binding
                    .span
                    .left()
                    .move_char(kw)
                    .to_pos(binding.binding.left_span());
                let pattern_to_epxr = binding
                    .binding
                    .span()
                    .right()
                    .to_pos(binding.value.left_span());
                ctx.move_right(kw as usize)
                    .replace(kw_to_pattern, " ")
                    .then(&binding.binding)
                    .replace(pattern_to_epxr, " = ")
                    .then(&binding.value)
            }
            BlockItem::Continue(_) => unimplemented!(),
            BlockItem::Return(_) => unimplemented!(),
            BlockItem::Break(_) => unimplemented!(),
        }
    }
}

impl<'i> Fmt for Signature<'i> {
    fn format_in<'e>(&self, ctx: Ctx<'e>) -> Ctx<'e> {
        let name = self.name.span;
        let fn_to_name = self.span.left().move_char(3).to_pos(name.left());
        let ctx = ctx.move_right(3).replace(fn_to_name, " ");
        if let Some(ret_ty) = &self.return_ty {
            ctx.call_like(
                (self.name.right_span(), ret_ty.left_span()),
                &self.name,
                &self.params,
                "(): ",
            )
            .then(ret_ty)
        } else {
            ctx.call_like(
                (self.name.right_span(), (self.right_span())),
                &self.name,
                &self.params,
                "()",
            )
        }
    }
}

const MAX_LINE_LEN: u32 = 80;

impl Edits {
    fn add(&mut self, range: Range, new_text: impl Into<String>) {
        self.0.push(TextEdit {
            range,
            new_text: new_text.into(),
        });
    }

    fn save_state(&self) -> usize {
        self.0.len()
    }

    fn restore_state(&mut self, state: usize) {
        self.0.truncate(state);
    }

    // fn format_comma_list(&mut self, things: &[impl Fmt]) -> u32 {
    //     self.format_inline_list(things, ", ")
    // }

    // fn format_inline_list(&mut self, things: &[impl Fmt], separator: &str) -> u32 {
    //     let lens: u32 = things.iter().map(|thing| thing.format_in(self)).sum();
    //     for pair in things.windows(2) {
    //         self.add(pair[0].right_span().to_pos(pair[1].left_span()), separator);
    //     }
    //     lens + ((things.len() as u32) - 1) * (separator.len() as u32)
    // }

    // fn format_list(&mut self, things: &[impl Fmt], separator: &str, indent: u32) {
    //     for thing in things {
    //         thing.try_format_inline_into(self, indent);
    //     }
    //     for pair in things.windows(2) {
    //         self.add(pair[0].right_span().to_pos(pair[1].left_span()), separator);
    //     }
    // }

    // fn format_struct_like_inline(&mut self, span: Span, items: &[impl Fmt]) -> u32 {
    //     self.format_braced_list_inline(span, items, ", ")
    // }

    // fn format_struct_like(&mut self, span: Span, items: &[impl Fmt], indent: u32) {
    //     self.format_braced_list(span, items, ", ", indent);
    // }

    // fn format_braced_list_inline(
    //     &mut self,
    //     span: Span,
    //     items: &[impl Fmt],
    //     separator: &str,
    // ) -> u32 {
    //     match items.len() {
    //         0 => {
    //             self.add(span_range(span), "{}");
    //             2
    //         }
    //         _ => {
    //             let left = span.left().to_pos(items[0].left_span());
    //             let right = items.last().unwrap().right_span().to_pos(span.right());
    //             let pattern_lens = self.format_inline_list(&items, separator);
    //             self.add(left, "{ ");
    //             self.add(right, " }");
    //             pattern_lens + 4
    //         }
    //     }
    // }

    // fn format_braced_list(&mut self, span: Span, items: &[impl Fmt], separator: &str, indent: u32) {
    //     match items.len() {
    //         0 => {
    //             self.add(span_range(span), "{}");
    //         }
    //         _ => {
    //             let left = span.left().to_pos(items[0].left_span());
    //             let right = items.last().unwrap().right_span().to_pos(span.right());
    //             self.format_list(&items, separator, indent);
    //             self.add(left, "{\n");
    //             self.add(right, "\n}");
    //         }
    //     }
    // }

    // fn format_bin_op_inline(&mut self, lhs: &impl Fmt, rhs: &impl Fmt, op: &str) -> u32 {
    //     let lhs_len = lhs.format_in(self);
    //     let rhs_len = rhs.format_in(self);
    //     let op_range = lhs.right_span().to_pos(rhs.left_span());
    //     self.add(op_range, op);
    //     lhs_len + rhs_len + (op.len() as u32)
    // }

    fn format_function(&mut self, fun: &ast::Function) {
        let ctx = Ctx {
            edits: self,
            indent: 0,
            offset: 0,
            ty: CtxType::Inline,
        };

        let sig_to_body = fun.signature.right_span().to_pos(fun.body.left_span());
        let state = ctx.edits.save_state();
        let ctx = ctx
            .then(&fun.signature)
            .replace(sig_to_body, " -> ")
            .then(&fun.body);
        if ctx.offset > MAX_LINE_LEN {
            ctx.edits.restore_state(state);
            ctx.then(&fun.signature)
                .replace(sig_to_body, " ")
                .set_ty(CtxType::BlockItem)
                .then(&fun.body);
        }
    }
}
