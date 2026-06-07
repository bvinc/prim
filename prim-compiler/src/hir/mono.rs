//! Monomorphization: replace each call to a generic function with a call
//! to a freshly-specialized concrete function, and each generic-struct
//! instantiation with a freshly-specialized concrete struct. After this
//! pass runs, `Type::Param` and `ExprKind::TraitBoundCall` are gone,
//! generic-struct carriers (`Type::Struct(_, args)` with `args` non-
//! empty) are gone, and only concrete IDs remain — codegen needs no
//! awareness of generics.
//!
//! Driven by the call graph from every non-generic function. Each unique
//! `(generic FuncId, concrete type-arg tuple)` or
//! `(generic StructId, concrete type-arg tuple)` becomes one
//! specialization, dedup'd via `*_mono_map`. Self-recursion and mutual
//! recursion work because each mono_map entry is inserted *before* the
//! cloned body / fields are processed.

use super::{Block, EnumId, Expr, ExprKind, FuncId, Program, Stmt, StructId, Type};
use std::collections::HashMap;

pub fn monomorphize(program: &mut Program) {
    let mut pass = Mono {
        program,
        func_mono_map: HashMap::new(),
        struct_mono_map: HashMap::new(),
        enum_mono_map: HashMap::new(),
    };
    // Process every non-generic function. Calls to generic functions in
    // their bodies kick off instantiation; calls inside generic bodies
    // are left alone here and rewritten when the body is cloned and
    // specialized.
    //
    // Even non-generic functions can carry `Type::Struct(_, args)`
    // carriers (e.g. `let x = Pair[i32] { ... }`) that need struct
    // instantiation, so the substitution step runs with an empty `subst`
    // — it does nothing for `Type::Param` but still triggers the
    // generic-struct mono pass through `substitute_type`.
    let n = pass.program.functions.len();
    for fid in 0..n {
        if pass.program.functions[fid].type_params.is_empty() {
            pass.substitute_and_rewrite(FuncId(fid as u32), &[]);
        }
    }
}

struct Mono<'a> {
    program: &'a mut Program,
    /// `(original FuncId, type-arg tuple)` → specialized `FuncId`.
    /// Inserted before each clone is built so self/mutual recursion
    /// finds the already-allocated specialization.
    func_mono_map: HashMap<(FuncId, Vec<Type>), FuncId>,
    /// Same dedup for generic structs.
    struct_mono_map: HashMap<(StructId, Vec<Type>), StructId>,
    /// Same dedup for generic enums.
    enum_mono_map: HashMap<(EnumId, Vec<Type>), EnumId>,
}

impl Mono<'_> {
    /// Full processing of `fid`: substitute `Type::Param` (and any
    /// embedded generic-struct carriers) throughout signature + body,
    /// then rewrite `Call`/`StructLit`/`TraitBoundCall` to specialized
    /// ids. `subst` is the type-arg substitution for this function —
    /// empty for non-generic functions, populated for specialized
    /// clones.
    fn substitute_and_rewrite(&mut self, fid: FuncId, subst: &[Type]) {
        // Substitute the signature.
        let mut params = std::mem::take(&mut self.program.functions[fid.0 as usize].params);
        for p in &mut params {
            p.ty = self.substitute_type(&p.ty, subst);
        }
        self.program.functions[fid.0 as usize].params = params;
        let mut ret = self.program.functions[fid.0 as usize].ret.take();
        if let Some(r) = &mut ret {
            *r = self.substitute_type(r, subst);
        }
        self.program.functions[fid.0 as usize].ret = ret;
        // Substitute + rewrite the body in one walk over a moved-out
        // body so the substitution methods can take `&mut self`.
        let mut body = std::mem::replace(
            &mut self.program.functions[fid.0 as usize].body,
            Block {
                stmts: Vec::new(),
                expr: None,
            },
        );
        self.substitute_block(&mut body, subst);
        self.rewrite_block(&mut body, subst);
        self.program.functions[fid.0 as usize].body = body;
    }

    fn rewrite_block(&mut self, block: &mut Block, subst: &[Type]) {
        for stmt in &mut block.stmts {
            self.rewrite_stmt(stmt, subst);
        }
        if let Some(expr) = &mut block.expr {
            self.rewrite_expr(expr, subst);
        }
    }

    fn rewrite_stmt(&mut self, stmt: &mut Stmt, subst: &[Type]) {
        match stmt {
            Stmt::Let { value, .. } => self.rewrite_expr(value, subst),
            Stmt::Assign { value, .. } => self.rewrite_expr(value, subst),
            Stmt::DerefAssign { ptr, value, .. } => {
                self.rewrite_expr(ptr, subst);
                self.rewrite_expr(value, subst);
            }
            Stmt::Expr(e) => self.rewrite_expr(e, subst),
            Stmt::Loop { body, .. } => self.rewrite_block(body, subst),
            Stmt::While {
                condition, body, ..
            } => {
                self.rewrite_expr(condition, subst);
                self.rewrite_block(body, subst);
            }
            Stmt::Break { .. } => {}
            Stmt::Return { value, .. } => {
                if let Some(v) = value {
                    self.rewrite_expr(v, subst);
                }
            }
        }
    }

    fn rewrite_expr(&mut self, expr: &mut Expr, subst: &[Type]) {
        match &mut expr.kind {
            ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Bool(_)
            | ExprKind::Str(_)
            | ExprKind::Ident(_)
            | ExprKind::Error => {}
            ExprKind::Binary { left, right, .. } => {
                self.rewrite_expr(left, subst);
                self.rewrite_expr(right, subst);
            }
            ExprKind::Field { base, .. } => self.rewrite_expr(base, subst),
            ExprKind::Deref(operand) => self.rewrite_expr(operand, subst),
            ExprKind::BitNot(operand) => self.rewrite_expr(operand, subst),
            ExprKind::StructLit {
                struct_id,
                type_args,
                fields,
            } => {
                for (_, v) in fields.iter_mut() {
                    self.rewrite_expr(v, subst);
                }
                if !type_args.is_empty() {
                    let concrete: Vec<Type> = type_args
                        .iter()
                        .map(|t| self.substitute_type(t, subst))
                        .collect();
                    let new_sid = self.instantiate_struct(*struct_id, concrete);
                    *struct_id = new_sid;
                    type_args.clear();
                }
            }
            ExprKind::VariantLit {
                enum_id,
                type_args,
                fields,
                ..
            } => {
                for (_, v) in fields.iter_mut() {
                    self.rewrite_expr(v, subst);
                }
                if !type_args.is_empty() {
                    let concrete: Vec<Type> = type_args
                        .iter()
                        .map(|t| self.substitute_type(t, subst))
                        .collect();
                    let new_eid = self.instantiate_enum(*enum_id, concrete);
                    *enum_id = new_eid;
                    type_args.clear();
                }
            }
            ExprKind::Match { scrutinee, arms } => {
                self.rewrite_expr(scrutinee, subst);
                for arm in arms {
                    self.rewrite_expr(&mut arm.body, subst);
                }
            }
            ExprKind::ArrayLit(elems) => {
                for e in elems {
                    self.rewrite_expr(e, subst);
                }
            }
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.rewrite_expr(condition, subst);
                self.rewrite_block(then_branch, subst);
                if let Some(eb) = else_branch {
                    self.rewrite_block(eb, subst);
                }
            }
            ExprKind::Block(block) => self.rewrite_block(block, subst),
            ExprKind::Dbg { inner, .. } => self.rewrite_expr(inner, subst),
            ExprKind::Coerce { value, .. } => self.rewrite_expr(value, subst),
            ExprKind::DynCall { receiver, args, .. } => {
                self.rewrite_expr(receiver, subst);
                for a in args {
                    self.rewrite_expr(a, subst);
                }
            }
            ExprKind::MethodCall { receiver, args, .. } => {
                self.rewrite_expr(receiver, subst);
                for a in args {
                    self.rewrite_expr(a, subst);
                }
            }
            ExprKind::Call {
                func,
                type_args,
                args,
            } => {
                for a in args.iter_mut() {
                    self.rewrite_expr(a, subst);
                }
                if !type_args.is_empty() {
                    let concrete: Vec<Type> = type_args
                        .iter()
                        .map(|t| self.substitute_type(t, subst))
                        .collect();
                    let new_fid = self.instantiate_function(*func, concrete);
                    *func = new_fid;
                    type_args.clear();
                }
            }
            ExprKind::TraitBoundCall {
                receiver,
                type_param,
                method,
                args,
                ..
            } => {
                self.rewrite_expr(receiver, subst);
                for a in args.iter_mut() {
                    self.rewrite_expr(a, subst);
                }
                let concrete = subst
                    .get(type_param.0 as usize)
                    .cloned()
                    .unwrap_or_else(|| panic!("unsubstituted type parameter in TraitBoundCall"));
                let sid = match concrete {
                    Type::Struct(sid, _) => sid,
                    other => panic!("TraitBoundCall substituted to non-struct: {:?}", other),
                };
                let impl_fid = *self
                    .program
                    .impl_methods
                    .get(&(super::MethodOwner::Struct(sid), *method))
                    .expect("missing impl method after substitution");
                let receiver_owned =
                    std::mem::replace(receiver.as_mut(), placeholder_expr(expr.span));
                let mut new_args = Vec::with_capacity(args.len() + 1);
                new_args.push(receiver_owned);
                new_args.append(args);
                expr.kind = ExprKind::Call {
                    func: impl_fid,
                    type_args: Vec::new(),
                    args: new_args,
                };
            }
        }
    }

    fn instantiate_function(&mut self, orig: FuncId, type_args: Vec<Type>) -> FuncId {
        let key = (orig, type_args);
        if let Some(&existing) = self.func_mono_map.get(&key) {
            return existing;
        }
        let new_fid = FuncId(self.program.functions.len() as u32);
        self.func_mono_map.insert(key.clone(), new_fid);
        let (_, type_args) = key;
        let mut clone = self.program.functions[orig.0 as usize].clone();
        clone.id = new_fid;
        clone.type_params = Vec::new();
        self.program.functions.push(clone);
        self.substitute_and_rewrite(new_fid, &type_args);
        new_fid
    }

    fn instantiate_struct(&mut self, orig: StructId, type_args: Vec<Type>) -> StructId {
        let key = (orig, type_args);
        if let Some(&existing) = self.struct_mono_map.get(&key) {
            return existing;
        }
        let new_sid = StructId(self.program.structs.len() as u32);
        self.struct_mono_map.insert(key.clone(), new_sid);
        let (_, type_args) = key;
        let mut clone = self.program.structs[orig.0 as usize].clone();
        clone.id = new_sid;
        clone.type_params = Vec::new();
        // Substitute field types — fields may reference Type::Param of
        // this struct's own type params (resolved by type_args) or carry
        // nested generic struct types (which the substitution chain
        // instantiates recursively).
        let mut new_fields = Vec::with_capacity(clone.fields.len());
        for f in &clone.fields {
            new_fields.push(super::Field {
                name: f.name,
                ty: self.substitute_type(&f.ty, &type_args),
                span: f.span,
            });
        }
        clone.fields = new_fields;
        self.program.structs.push(clone);
        new_sid
    }

    fn instantiate_enum(&mut self, orig: EnumId, type_args: Vec<Type>) -> EnumId {
        let key = (orig, type_args);
        if let Some(&existing) = self.enum_mono_map.get(&key) {
            return existing;
        }
        let new_eid = EnumId(self.program.enums.len() as u32);
        self.enum_mono_map.insert(key.clone(), new_eid);
        let (_, type_args) = key;
        let mut clone = self.program.enums[orig.0 as usize].clone();
        clone.id = new_eid;
        clone.type_params = Vec::new();
        for variant in &mut clone.variants {
            for field in &mut variant.fields {
                field.ty = self.substitute_type(&field.ty, &type_args);
            }
        }
        self.program.enums.push(clone);
        new_eid
    }

    /// Replace `Type::Param(i)` with `subst[i]` and recursively
    /// instantiate any generic struct types encountered.
    fn substitute_type(&mut self, ty: &Type, subst: &[Type]) -> Type {
        match ty {
            Type::Param(i) => subst
                .get(i.0 as usize)
                .cloned()
                .unwrap_or_else(|| ty.clone()),
            Type::Pointer { mutable, pointee } => Type::Pointer {
                mutable: *mutable,
                pointee: Box::new(self.substitute_type(pointee, subst)),
            },
            Type::Array(elem) => Type::Array(Box::new(self.substitute_type(elem, subst))),
            Type::Struct(sid, args) if args.is_empty() => Type::Struct(*sid, Vec::new()),
            Type::Struct(sid, args) => {
                let concrete_args: Vec<Type> = args
                    .iter()
                    .map(|t| self.substitute_type(t, subst))
                    .collect();
                let new_sid = self.instantiate_struct(*sid, concrete_args);
                Type::Struct(new_sid, Vec::new())
            }
            Type::Enum(eid, args) if args.is_empty() => Type::Enum(*eid, Vec::new()),
            Type::Enum(eid, args) => {
                let concrete_args: Vec<Type> = args
                    .iter()
                    .map(|t| self.substitute_type(t, subst))
                    .collect();
                let new_eid = self.instantiate_enum(*eid, concrete_args);
                Type::Enum(new_eid, Vec::new())
            }
            _ => ty.clone(),
        }
    }

    fn substitute_block(&mut self, block: &mut Block, subst: &[Type]) {
        for stmt in &mut block.stmts {
            self.substitute_stmt(stmt, subst);
        }
        if let Some(expr) = &mut block.expr {
            self.substitute_expr(expr, subst);
        }
    }

    fn substitute_stmt(&mut self, stmt: &mut Stmt, subst: &[Type]) {
        match stmt {
            Stmt::Let { ty, value, .. } => {
                *ty = self.substitute_type(ty, subst);
                self.substitute_expr(value, subst);
            }
            Stmt::Assign { value, .. } => self.substitute_expr(value, subst),
            Stmt::DerefAssign { ptr, value, .. } => {
                self.substitute_expr(ptr, subst);
                self.substitute_expr(value, subst);
            }
            Stmt::Expr(e) => self.substitute_expr(e, subst),
            Stmt::Loop { body, .. } => self.substitute_block(body, subst),
            Stmt::While {
                condition, body, ..
            } => {
                self.substitute_expr(condition, subst);
                self.substitute_block(body, subst);
            }
            Stmt::Break { .. } => {}
            Stmt::Return { value, .. } => {
                if let Some(v) = value {
                    self.substitute_expr(v, subst);
                }
            }
        }
    }

    fn substitute_expr(&mut self, expr: &mut Expr, subst: &[Type]) {
        expr.ty = self.substitute_type(&expr.ty, subst);
        match &mut expr.kind {
            ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Bool(_)
            | ExprKind::Str(_)
            | ExprKind::Ident(_)
            | ExprKind::Error => {}
            ExprKind::Binary { left, right, .. } => {
                self.substitute_expr(left, subst);
                self.substitute_expr(right, subst);
            }
            ExprKind::Field { base, .. } => self.substitute_expr(base, subst),
            ExprKind::Deref(operand) => self.substitute_expr(operand, subst),
            ExprKind::BitNot(operand) => self.substitute_expr(operand, subst),
            ExprKind::StructLit {
                type_args, fields, ..
            } => {
                for t in type_args.iter_mut() {
                    *t = self.substitute_type(t, subst);
                }
                for (_, v) in fields {
                    self.substitute_expr(v, subst);
                }
            }
            ExprKind::VariantLit {
                type_args, fields, ..
            } => {
                for t in type_args.iter_mut() {
                    *t = self.substitute_type(t, subst);
                }
                for (_, v) in fields {
                    self.substitute_expr(v, subst);
                }
            }
            ExprKind::Match { scrutinee, arms } => {
                self.substitute_expr(scrutinee, subst);
                for arm in arms {
                    match &mut arm.pattern {
                        super::Pattern::Wildcard { .. } => {}
                        super::Pattern::Variant { bindings, .. } => {
                            for (_, _, ty) in bindings {
                                *ty = self.substitute_type(ty, subst);
                            }
                        }
                    }
                    self.substitute_expr(&mut arm.body, subst);
                }
            }
            ExprKind::ArrayLit(elems) => {
                for e in elems {
                    self.substitute_expr(e, subst);
                }
            }
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.substitute_expr(condition, subst);
                self.substitute_block(then_branch, subst);
                if let Some(eb) = else_branch {
                    self.substitute_block(eb, subst);
                }
            }
            ExprKind::Block(block) => self.substitute_block(block, subst),
            ExprKind::Dbg { inner, .. } => self.substitute_expr(inner, subst),
            ExprKind::Coerce { value, .. } => self.substitute_expr(value, subst),
            ExprKind::DynCall { receiver, args, .. } => {
                self.substitute_expr(receiver, subst);
                for a in args {
                    self.substitute_expr(a, subst);
                }
            }
            ExprKind::MethodCall { receiver, args, .. } => {
                self.substitute_expr(receiver, subst);
                for a in args {
                    self.substitute_expr(a, subst);
                }
            }
            ExprKind::Call {
                type_args, args, ..
            } => {
                for t in type_args.iter_mut() {
                    *t = self.substitute_type(t, subst);
                }
                for a in args {
                    self.substitute_expr(a, subst);
                }
            }
            ExprKind::TraitBoundCall { receiver, args, .. } => {
                self.substitute_expr(receiver, subst);
                for a in args {
                    self.substitute_expr(a, subst);
                }
            }
        }
    }
}

fn placeholder_expr(span: super::SpanId) -> Expr {
    Expr {
        kind: ExprKind::Error,
        ty: Type::Undetermined,
        span,
    }
}
