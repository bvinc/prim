//! Pattern-matrix usefulness analysis (Maranget, *Warnings for pattern
//! matching*, JFP 2007).
//!
//! Both exhaustiveness and reachability reduce to one question: is a pattern
//! vector `q` *useful* with respect to a matrix `P` of earlier rows — i.e. does
//! some value match `q` but none of the rows of `P`?
//!
//! - **Reachability**: arm `i` is reachable iff its row is useful w.r.t. the
//!   matrix of arms `0..i`.
//! - **Exhaustiveness**: the match is exhaustive iff a fully-wild row is *not*
//!   useful w.r.t. the matrix of all arms. When it is useful, the returned
//!   witness is a value the match fails to cover.

use super::{Enum, Pattern, Program, Type};

/// A constructor: the "head" of a value's shape.
#[derive(Clone, PartialEq)]
enum Ctor {
    Variant(u32),
    Bool(bool),
    Int(i64),
    Tuple,
}

/// A pattern normalized for the algorithm: either a wildcard (covers
/// everything, binds nothing relevant here) or a constructor applied to
/// sub-patterns.
#[derive(Clone)]
enum Pat {
    Wild,
    Ctor(Ctor, Vec<Pat>),
}

/// Normalize an HIR pattern. Variant fields are reordered into the enum's
/// declared field order, with omitted fields filled by wildcards, so the
/// sub-pattern vector is positional.
fn normalize(pat: &Pattern, program: &Program) -> Pat {
    match pat {
        Pattern::Wildcard { .. } | Pattern::Binding { .. } => Pat::Wild,
        Pattern::Int { value, .. } => Pat::Ctor(Ctor::Int(*value), Vec::new()),
        Pattern::Bool { value, .. } => Pat::Ctor(Ctor::Bool(*value), Vec::new()),
        Pattern::Tuple { elems, .. } => Pat::Ctor(
            Ctor::Tuple,
            elems.iter().map(|e| normalize(e, program)).collect(),
        ),
        Pattern::Variant {
            enum_id,
            variant_idx,
            fields,
            ..
        } => {
            let declared =
                &program.enums[enum_id.0 as usize].variants[*variant_idx as usize].fields;
            let args = declared
                .iter()
                .map(|f| {
                    fields
                        .iter()
                        .find(|fp| fp.field == f.name)
                        .map(|fp| normalize(&fp.pattern, program))
                        .unwrap_or(Pat::Wild)
                })
                .collect();
            Pat::Ctor(Ctor::Variant(*variant_idx), args)
        }
    }
}

fn enum_def<'a>(ty: &'a Type, program: &'a Program) -> Option<(&'a Enum, &'a [Type])> {
    match ty {
        Type::Enum(eid, args) => Some((&program.enums[eid.0 as usize], args)),
        _ => None,
    }
}

/// The sub-column types introduced by matching `ctor` against a column of type
/// `ty` (the constructor's "argument" types, in order).
fn ctor_sub_types(ty: &Type, ctor: &Ctor, program: &Program) -> Vec<Type> {
    match (ctor, ty) {
        (Ctor::Tuple, Type::Tuple(elems)) => elems.clone(),
        (Ctor::Variant(idx), _) => {
            let Some((e, args)) = enum_def(ty, program) else {
                return Vec::new();
            };
            e.variants[*idx as usize]
                .fields
                .iter()
                .map(|f| subst_params(&f.ty, args))
                .collect()
        }
        _ => Vec::new(),
    }
}

/// Substitute generic type parameters by position (mirrors the typechecker's
/// own `substitute_params_with_slice`).
fn subst_params(ty: &Type, args: &[Type]) -> Type {
    match ty {
        Type::Param(i) => args
            .get(i.0 as usize)
            .cloned()
            .unwrap_or_else(|| ty.clone()),
        Type::Pointer { mutable, pointee } => Type::Pointer {
            mutable: *mutable,
            pointee: Box::new(subst_params(pointee, args)),
        },
        Type::Array(elem) => Type::Array(Box::new(subst_params(elem, args))),
        Type::Struct(sid, ta) => {
            Type::Struct(*sid, ta.iter().map(|t| subst_params(t, args)).collect())
        }
        Type::Enum(eid, ta) => Type::Enum(*eid, ta.iter().map(|t| subst_params(t, args)).collect()),
        Type::Tuple(elems) => Type::Tuple(elems.iter().map(|t| subst_params(t, args)).collect()),
        _ => ty.clone(),
    }
}

/// The complete set of constructors for a column type, or `None` if the type's
/// value space is unbounded (integers) — which can never be fully covered by
/// constructors, so a wildcard is always required.
fn full_signature(ty: &Type, program: &Program) -> Option<Vec<Ctor>> {
    match ty {
        Type::Bool => Some(vec![Ctor::Bool(false), Ctor::Bool(true)]),
        Type::Tuple(_) => Some(vec![Ctor::Tuple]),
        Type::Enum(eid, _) => {
            let n = program.enums[eid.0 as usize].variants.len() as u32;
            Some((0..n).map(Ctor::Variant).collect())
        }
        _ => None, // integers: infinite domain
    }
}

/// Constructors that appear as the head of column 0 of `matrix`.
fn head_ctors(matrix: &[Vec<Pat>]) -> Vec<Ctor> {
    let mut out: Vec<Ctor> = Vec::new();
    for row in matrix {
        if let Pat::Ctor(c, _) = &row[0] {
            if !out.contains(c) {
                out.push(c.clone());
            }
        }
    }
    out
}

/// Specialize `matrix` by constructor `c` of arity `arity`: keep rows whose
/// head is `c` (expanding into its sub-patterns) or a wildcard (expanding into
/// `arity` wildcards), dropping rows headed by a different constructor.
fn specialize(matrix: &[Vec<Pat>], c: &Ctor, arity: usize) -> Vec<Vec<Pat>> {
    let mut out = Vec::new();
    for row in matrix {
        match &row[0] {
            Pat::Ctor(rc, args) if rc == c => {
                let mut new_row = args.clone();
                new_row.extend_from_slice(&row[1..]);
                out.push(new_row);
            }
            Pat::Ctor(_, _) => {}
            Pat::Wild => {
                let mut new_row = vec![Pat::Wild; arity];
                new_row.extend_from_slice(&row[1..]);
                out.push(new_row);
            }
        }
    }
    out
}

/// The default matrix: rows headed by a wildcard, with column 0 dropped.
fn default_matrix(matrix: &[Vec<Pat>]) -> Vec<Vec<Pat>> {
    let mut out = Vec::new();
    for row in matrix {
        if matches!(row[0], Pat::Wild) {
            out.push(row[1..].to_vec());
        }
    }
    out
}

/// Is `q` useful w.r.t. `matrix` (columns typed by `types`)? Returns a witness
/// vector (a value matched by `q` but no row of `matrix`) when useful.
fn useful(matrix: &[Vec<Pat>], q: &[Pat], types: &[Type], program: &Program) -> Option<Vec<Pat>> {
    if q.is_empty() {
        // Base case: useful iff no row remains (no earlier row subsumes it).
        return if matrix.is_empty() {
            Some(Vec::new())
        } else {
            None
        };
    }

    let head = &q[0];
    let rest = &q[1..];
    let col_ty = &types[0];

    match head {
        Pat::Ctor(c, args) => {
            let sub_types = ctor_sub_types(col_ty, c, program);
            let arity = sub_types.len();
            let spec = specialize(matrix, c, arity);
            let mut new_q = args.clone();
            new_q.extend_from_slice(rest);
            let mut new_types = sub_types;
            new_types.extend_from_slice(&types[1..]);
            useful(&spec, &new_q, &new_types, program).map(|w| regroup(c.clone(), arity, w))
        }
        Pat::Wild => {
            let used = head_ctors(matrix);
            match full_signature(col_ty, program) {
                // Complete, finite signature present: the column is exhaustive
                // only if every constructor's specialized sub-problem is.
                Some(sig) if sig.iter().all(|c| used.iter().any(|u| u == c)) => {
                    for c in &sig {
                        let sub_types = ctor_sub_types(col_ty, c, program);
                        let arity = sub_types.len();
                        let spec = specialize(matrix, c, arity);
                        let mut new_q = vec![Pat::Wild; arity];
                        new_q.extend_from_slice(rest);
                        let mut new_types = sub_types;
                        new_types.extend_from_slice(&types[1..]);
                        if let Some(w) = useful(&spec, &new_q, &new_types, program) {
                            return Some(regroup(c.clone(), arity, w));
                        }
                    }
                    None
                }
                // Incomplete (or infinite) signature: a wildcard here can match
                // a value with a constructor not present, so recurse on the
                // default matrix and prepend a witness head.
                sig => {
                    let def = default_matrix(matrix);
                    useful(&def, rest, &types[1..], program).map(|mut w| {
                        let mut full = Vec::with_capacity(w.len() + 1);
                        full.push(witness_head(col_ty, &used, sig));
                        full.append(&mut w);
                        full
                    })
                }
            }
        }
    }
}

/// Rebuild a witness row by grouping the first `arity` patterns under `c`.
fn regroup(c: Ctor, arity: usize, mut w: Vec<Pat>) -> Vec<Pat> {
    let rest = w.split_off(arity);
    let mut out = Vec::with_capacity(rest.len() + 1);
    out.push(Pat::Ctor(c, w));
    out.extend(rest);
    out
}

/// Choose a witness head for an incomplete column: a missing constructor when
/// one is nameable (enum/bool), otherwise a wildcard (e.g. integers). Missing
/// constructors are rendered without their payload, so empty args suffice.
fn witness_head(_col_ty: &Type, used: &[Ctor], sig: Option<Vec<Ctor>>) -> Pat {
    if let Some(sig) = sig {
        for c in sig {
            if !used.contains(&c) {
                return Pat::Ctor(c, Vec::new());
            }
        }
    }
    Pat::Wild
}

/// Render a witness pattern to a human-readable string for error messages.
fn render(pat: &Pat, ty: &Type, program: &Program) -> String {
    match pat {
        Pat::Wild => "_".to_string(),
        Pat::Ctor(Ctor::Bool(b), _) => b.to_string(),
        Pat::Ctor(Ctor::Int(v), _) => v.to_string(),
        Pat::Ctor(Ctor::Tuple, args) => {
            let elem_types = match ty {
                Type::Tuple(ts) => ts.clone(),
                _ => vec![Type::Undetermined; args.len()],
            };
            let parts: Vec<String> = args
                .iter()
                .zip(elem_types.iter())
                .map(|(a, t)| render(a, t, program))
                .collect();
            format!("({})", parts.join(", "))
        }
        Pat::Ctor(Ctor::Variant(idx), args) => {
            let Some((e, _)) = enum_def(ty, program) else {
                return "_".to_string();
            };
            let variant = &e.variants[*idx as usize];
            let name = program.interner.resolve(&variant.name).to_string();
            if args.is_empty() {
                name
            } else {
                format!("{} {{ .. }}", name)
            }
        }
    }
}

/// Outcome of analyzing a list of match arms.
pub struct MatchAnalysis {
    /// Indices of arms that can never match (unreachable / redundant).
    pub unreachable: Vec<usize>,
    /// A rendered witness pattern the match fails to cover, if non-exhaustive.
    pub missing: Option<String>,
}

/// Analyze the arms of a `match` on a scrutinee of type `scrut_ty` for
/// reachability and exhaustiveness.
pub fn analyze(arms: &[Pattern], scrut_ty: &Type, program: &Program) -> MatchAnalysis {
    let types = [scrut_ty.clone()];
    let mut matrix: Vec<Vec<Pat>> = Vec::with_capacity(arms.len());
    let mut unreachable = Vec::new();

    for (i, arm) in arms.iter().enumerate() {
        let row = vec![normalize(arm, program)];
        if useful(&matrix, &row, &types, program).is_none() {
            unreachable.push(i);
        }
        matrix.push(row);
    }

    let missing =
        useful(&matrix, &[Pat::Wild], &types, program).map(|w| render(&w[0], scrut_ty, program));

    MatchAnalysis {
        unreachable,
        missing,
    }
}
