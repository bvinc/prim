//! Closure extraction: lift each inline `ExprKind::Closure` body into a
//! freshly-synthesized `Function` and replace the node with
//! `ExprKind::ClosureRef { func }`.
//!
//! Runs after typecheck (so every block parameter type is resolved and the
//! block's `Type::Fn` is concrete) and before ownership/mono/drop/codegen,
//! which all operate on `program.functions` uniformly. Keeping the body inline
//! through typecheck is what lets `apply_expected` push the expected `fn`
//! signature into the block's parameters from the call site; lifting it into a
//! `Function` afterward makes every later pass treat it like a normal
//! (indirectly-called) function.

use super::{
    Block, Expr, ExprKind, FuncId, Function, ModuleId, Program, Stmt, Symbol, SymbolId, SymbolKind,
    Type, TypeParam,
};

pub fn extract_closures(program: &mut Program) {
    // Every function in the program has its body extracted; newly created
    // closure functions are appended during the walk and processed too, since
    // a block body may itself contain a nested block.
    let mut i = 0;
    while i < program.functions.len() {
        let (type_params, module) = {
            let f = &program.functions[i];
            let module = program.symbols[f.name.0 as usize].module;
            (f.type_params.clone(), module)
        };
        let body = std::mem::take(&mut program.functions[i].body);
        program.functions[i].body = extract_block(program, body, &type_params, module);
        i += 1;
    }
}

fn extract_block(
    program: &mut Program,
    mut block: Block,
    type_params: &[TypeParam],
    module: ModuleId,
) -> Block {
    for stmt in &mut block.stmts {
        extract_stmt(program, stmt, type_params, module);
    }
    if let Some(expr) = &mut block.expr {
        extract_expr(program, expr, type_params, module);
    }
    block
}

fn extract_stmt(
    program: &mut Program,
    stmt: &mut Stmt,
    type_params: &[TypeParam],
    module: ModuleId,
) {
    match stmt {
        Stmt::Let { value, .. } => extract_expr(program, value, type_params, module),
        Stmt::Assign { value, .. } => extract_expr(program, value, type_params, module),
        Stmt::DerefAssign { ptr, value, .. } => {
            extract_expr(program, ptr, type_params, module);
            extract_expr(program, value, type_params, module);
        }
        Stmt::FieldAssign { object, value, .. } => {
            extract_expr(program, object, type_params, module);
            extract_expr(program, value, type_params, module);
        }
        Stmt::Expr(e) => extract_expr(program, e, type_params, module),
        Stmt::Loop { body, .. } => {
            *body = extract_block(program, std::mem::take(body), type_params, module);
        }
        Stmt::While {
            condition, body, ..
        } => {
            extract_expr(program, condition, type_params, module);
            *body = extract_block(program, std::mem::take(body), type_params, module);
        }
        Stmt::Break { .. } => {}
        Stmt::Return { value, .. } => {
            if let Some(v) = value {
                extract_expr(program, v, type_params, module);
            }
        }
        Stmt::Drop { .. } => {}
    }
}

fn extract_expr(
    program: &mut Program,
    expr: &mut Expr,
    type_params: &[TypeParam],
    module: ModuleId,
) {
    match &mut expr.kind {
        ExprKind::Closure { params, ret, body } => {
            // Lift nested closures first, so this block's function holds an
            // already-extracted body.
            *body = extract_block(program, std::mem::take(body), type_params, module);

            let fid = FuncId(program.functions.len() as u32);
            let name = program
                .interner
                .get_or_intern(format!("__closure_{}", fid.0));
            let sym = SymbolId(program.symbols.len() as u32);
            program.symbols.push(Symbol {
                id: sym,
                module,
                name,
                kind: SymbolKind::Function(fid),
            });

            let params = std::mem::take(params);
            let _declared_ret = std::mem::take(ret);
            // The wasm-level return type comes from the block's resolved
            // `Type::Fn`. A unit-returning block stores `None` (no result),
            // matching how every other unit function is represented.
            let fn_ret = match &expr.ty {
                Type::Fn { ret, .. } => match ret.as_ref() {
                    Type::Unit => None,
                    t => Some(t.clone()),
                },
                _ => None,
            };

            program.functions.push(Function {
                id: fid,
                name: sym,
                type_params: type_params.to_vec(),
                params,
                ret: fn_ret,
                body: std::mem::take(body),
                span: expr.span,
                runtime: None,
                unsafe_fn: false,
                is_closure: true,
            });
            expr.kind = ExprKind::ClosureRef { func: fid };
        }
        ExprKind::Call { args, .. } => {
            for a in args {
                extract_expr(program, a, type_params, module);
            }
        }
        ExprKind::MethodCall { receiver, args, .. }
        | ExprKind::DynCall { receiver, args, .. }
        | ExprKind::TraitBoundCall { receiver, args, .. } => {
            extract_expr(program, receiver, type_params, module);
            for a in args {
                extract_expr(program, a, type_params, module);
            }
        }
        ExprKind::IndirectCall { callee, args } => {
            extract_expr(program, callee, type_params, module);
            for a in args {
                extract_expr(program, a, type_params, module);
            }
        }
        ExprKind::Binary { left, right, .. } => {
            extract_expr(program, left, type_params, module);
            extract_expr(program, right, type_params, module);
        }
        ExprKind::StructLit { fields, .. } => {
            for (_, e) in fields {
                extract_expr(program, e, type_params, module);
            }
        }
        ExprKind::VariantLit { fields, .. } => {
            for (_, e) in fields {
                extract_expr(program, e, type_params, module);
            }
        }
        ExprKind::Match {
            scrutinee, arms, ..
        } => {
            extract_expr(program, scrutinee, type_params, module);
            for arm in arms {
                extract_expr(program, &mut arm.body, type_params, module);
            }
        }
        ExprKind::Field { base, .. }
        | ExprKind::TupleIndex { base, .. }
        | ExprKind::Deref(base)
        | ExprKind::BitNot(base)
        | ExprKind::Neg(base)
        | ExprKind::Coerce { value: base, .. } => {
            extract_expr(program, base, type_params, module);
        }
        ExprKind::TupleLit(elems) | ExprKind::ArrayLit(elems) => {
            for e in elems {
                extract_expr(program, e, type_params, module);
            }
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            extract_expr(program, condition, type_params, module);
            *then_branch = extract_block(program, std::mem::take(then_branch), type_params, module);
            if let Some(else_branch) = else_branch {
                *else_branch =
                    extract_block(program, std::mem::take(else_branch), type_params, module);
            }
        }
        ExprKind::Block(b) | ExprKind::UnsafeBlock(b) => {
            *b = extract_block(program, std::mem::take(b), type_params, module);
        }
        ExprKind::ClosureRef { .. } => {}
        ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Bool(_)
        | ExprKind::Str(_)
        | ExprKind::Ident(_)
        | ExprKind::Spawn { .. }
        | ExprKind::ConstParam(_)
        | ExprKind::Error => {}
    }
}
