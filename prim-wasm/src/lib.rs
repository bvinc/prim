//! HIR → wasm32 + WASI code generation.
//!
//! Single public entry point: [`generate_wasm`]. The work is split across
//! sibling modules:
//!
//! - [`types`] — HIR type ↔ wasm `ValType` mapping; function-type registry.
//! - [`layout`] — static memory layout constants; struct layout computation;
//!   field load/store helpers.
//! - [`walks`] — pre-walks over HIR (locals, scratch types, string literals).
//! - [`builtins`] — hand-written wasm bodies for `__alloc`, `__write_bytes`,
//!   `__rt_resume`.
//! - [`emit`] — per-function emission of user code.

mod builtins;
mod emit;
mod layout;
mod types;
mod walks;

use crate::builtins::{Builtins, emit_rt_resume, emit_write_bytes};
use crate::emit::{
    ScalarField, StrSite, StringLayout, build_emit_ctx, collect_drop_types, emit_drop_fn,
    emit_drop_glue_fn, emit_drop_trait_fn, emit_user_function, flat_scalar_fields,
    scalar_abi_params,
};
use crate::layout::{
    EnumLayout, STATIC_DATA_START, StructLayout, compute_enum_layout, compute_struct_layout,
};
use crate::types::{TypeRegistry, hir_type_to_valtype};
use crate::walks::collect_str_literals_block;
use prim_compiler::hir;
use prim_compiler::hir::inline::InlinePolicy;
use std::collections::{HashMap, HashSet};
use std::fmt;
use wasm_encoder::{
    CodeSection, ConstExpr, DataSection, ElementSection, Elements, ExportKind, ExportSection,
    FunctionSection, GlobalSection, GlobalType, HeapType, ImportSection, MemorySection, MemoryType,
    Module, RefType, TableSection, TableType, TagKind, TagSection, TagType, ValType,
};

#[derive(Debug)]
pub enum WasmError {
    MissingMain,
    /// Codegen reached an HIR shape it cannot lower — a struct field on a
    /// non-struct, an unresolved name, a missing layout, a type that should
    /// have been ruled out by an earlier pass. Rather than emit a wasm `trap`
    /// (which the scheduler swallows, so the program would exit 0), fail the
    /// build loudly. The string names the specific situation.
    Internal(String),
}

impl fmt::Display for WasmError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WasmError::MissingMain => write!(f, "main function not found"),
            WasmError::Internal(msg) => write!(f, "internal codegen error: {msg}"),
        }
    }
}

impl std::error::Error for WasmError {}

/// Compile a typechecked HIR program into a wasm32+WASI module.
pub fn generate_wasm(program: &hir::Program) -> Result<Vec<u8>, WasmError> {
    if program.main.is_none() {
        return Err(WasmError::MissingMain);
    }

    // Which aggregates are stored inline vs. boxed. Needs drop info (a
    // needs-drop type must own its box, so it is never inlined), so compute that
    // first; the same `DropInfo` backs the synthesized drop functions below.
    let drop_info = hir::DropInfo::new(program);
    let inline_policy = InlinePolicy::new(program);

    // Compute memory layout for every struct.
    let mut struct_layouts: HashMap<hir::StructId, StructLayout> = HashMap::new();
    for s in &program.structs {
        struct_layouts.insert(s.id, compute_struct_layout(s, &inline_policy));
    }
    let mut enum_layouts: HashMap<hir::EnumId, EnumLayout> = HashMap::new();
    for e in &program.enums {
        enum_layouts.insert(e.id, compute_enum_layout(e, &inline_policy));
    }
    let mut string_layout = None;
    for s in &program.structs {
        let Some(layout) = struct_layouts.get(&s.id) else {
            continue;
        };
        let Some(symbol) = program.symbols.get(s.name.0 as usize) else {
            continue;
        };
        let module_name = &program.modules[symbol.module.0 as usize].name;
        if module_name.len() != 2 || module_name[0] != "std" || module_name[1] != "string" {
            continue;
        }
        if program.interner.resolve(&symbol.name) != "String" {
            continue;
        }
        let mut data_offset = None;
        let mut len_offset = None;
        let mut cap_offset = None;
        for field in &s.fields {
            let Some((offset, _)) = layout.fields.get(&field.name) else {
                continue;
            };
            match program.interner.resolve(&field.name) {
                "data" => data_offset = Some(*offset),
                "len" => len_offset = Some(*offset),
                "cap" => cap_offset = Some(*offset),
                _ => {}
            }
        }
        if let (Some(data_offset), Some(len_offset), Some(cap_offset)) =
            (data_offset, len_offset, cap_offset)
        {
            string_layout = Some(StringLayout {
                struct_id: s.id,
                size: layout.size,
                data_offset,
                len_offset,
                cap_offset,
            });
        }
    }

    let mut types = TypeRegistry::new();

    // Register function types for the builtin runtime helpers.
    let fd_write_type = types.register(
        vec![ValType::I32, ValType::I32, ValType::I32, ValType::I32],
        vec![ValType::I32],
    );
    let write_bytes_type = types.register(
        vec![ValType::I32, ValType::I32, ValType::I32],
        vec![ValType::I32],
    );
    // clock_time_get(clock_id: i32, precision: i64, out: i32) -> errno: i32
    let clock_time_get_type = types.register(
        vec![ValType::I32, ValType::I64, ValType::I32],
        vec![ValType::I32],
    );
    // poll_oneoff(in: i32, out: i32, nsubscriptions: i32, nevents: i32) -> errno: i32
    let poll_oneoff_type = types.register(
        vec![ValType::I32, ValType::I32, ValType::I32, ValType::I32],
        vec![ValType::I32],
    );
    // fd_read(fd: i32, iovs: i32, iovs_len: i32, nread: i32) -> errno: i32
    let fd_read_type = types.register(
        vec![ValType::I32, ValType::I32, ValType::I32, ValType::I32],
        vec![ValType::I32],
    );
    // path_open(dir_fd, dirflags, path, path_len, oflags, rights_base: i64,
    //   rights_inheriting: i64, fdflags, opened_fd_out) -> errno: i32
    let path_open_type = types.register(
        vec![
            ValType::I32,
            ValType::I32,
            ValType::I32,
            ValType::I32,
            ValType::I32,
            ValType::I64,
            ValType::I64,
            ValType::I32,
            ValType::I32,
        ],
        vec![ValType::I32],
    );
    // fd_close(fd: i32) -> errno: i32
    let fd_close_type = types.register(vec![ValType::I32], vec![ValType::I32]);

    // Function index layout:
    //   0: fd_write (import)
    //   1: clock_time_get (import)
    //   2: poll_oneoff (import)
    //   3: fd_read (import)
    //   4: path_open (import)
    //   5: fd_close (import)
    //   6: __write_bytes
    //   7+: user functions (the `@entry` function is exported as `_start`)
    //   last: __rt_resume
    let fd_write_idx: u32 = 0;
    let clock_idx: u32 = 1;
    let poll_oneoff_idx: u32 = 2;
    let fd_read_idx: u32 = 3;
    let path_open_idx: u32 = 4;
    let fd_close_idx: u32 = 5;
    let mut builtins = Builtins {
        write_bytes: 6,
        clock: clock_idx,
        poll_oneoff: poll_oneoff_idx,
        fd_read: fd_read_idx,
        path_open: path_open_idx,
        fd_close: fd_close_idx,
        // Object allocation routes through std.mem.alloc, resolved below. It is
        // always linked (the prelude force-loads std.io, which imports std.mem),
        // so there is no fallback allocator.
        alloc: u32::MAX,
        // Resolved below alongside `alloc`; only needed when something is
        // dropped, so a missing `free` is not fatal.
        free: u32::MAX,
        yield_tag: 0,
        cont_table: 1, // table 1 is the scheduler's task table
        rt_resume: 0,  // resolved once the function layout is known
        cont_type: 0,  // resolved once main's continuation type is registered
        main_func: 0,  // resolved once main's wasm index is known
    };

    // Build func_map (user functions) and runtime_map (runtime-bound functions).
    let mut func_map: HashMap<hir::FuncId, u32> = HashMap::new();
    let mut runtime_map: HashMap<hir::FuncId, hir::RuntimeAbi> = HashMap::new();
    let mut user_func_types: Vec<u32> = Vec::new();
    let mut next_idx: u32 = 7;
    let mut main_wasm_idx = None;
    let mut main_func_type: Option<u32> = None;

    // Per-function: which parameters use the by-value scalar ABI (phase 3).
    // Shared by signature registration here and call-site argument emission.
    // Methods are excluded: they may be dispatched virtually through a vtable
    // whose entries use a fixed pointer ABI, so their signatures can't expand.
    let method_fns: HashSet<hir::FuncId> = program.impls.values().flatten().copied().collect();
    let mut scalar_abi: HashMap<hir::FuncId, Vec<bool>> = HashMap::new();
    // Per-function: the leaf fields of a by-value scalar-ABI return (phase 3).
    // A flat-POD return is returned as one wasm result per field. Methods are
    // excluded for the same vtable reason as parameters.
    let mut scalar_ret: HashMap<hir::FuncId, Vec<ScalarField>> = HashMap::new();
    for func in &program.functions {
        if func.type_params.is_empty() && func.runtime.is_none() && !method_fns.contains(&func.id) {
            let flags = scalar_abi_params(func, program, &inline_policy, &drop_info);
            if flags.iter().any(|&s| s) {
                scalar_abi.insert(func.id, flags);
            }
            if let Some(ret) = &func.ret
                && let Some(fields) = flat_scalar_fields(ret, program, &inline_policy, &drop_info)
            {
                scalar_ret.insert(func.id, fields);
            }
        }
    }

    for func in &program.functions {
        // Uninstantiated generic templates are never called (only their
        // monomorphized clones are); they get no wasm function.
        if !func.type_params.is_empty() {
            continue;
        }
        if let Some(runtime) = func.runtime {
            runtime_map.insert(func.id, runtime);
        } else {
            func_map.insert(func.id, next_idx);
            // A scalar-ABI parameter expands to one wasm value per leaf field.
            let abi = scalar_abi.get(&func.id);
            let mut params: Vec<ValType> = Vec::with_capacity(func.params.len());
            for (i, p) in func.params.iter().enumerate() {
                if abi.is_some_and(|v| v[i]) {
                    let fields = flat_scalar_fields(&p.ty, program, &inline_policy, &drop_info)
                        .expect("scalar-ABI param must be a flat scalar aggregate");
                    params.extend(fields.iter().map(|sf| sf.valtype));
                } else {
                    params.push(hir_type_to_valtype(&p.ty));
                }
            }
            // A scalar-ABI return expands to one wasm result per leaf field.
            let results: Vec<ValType> = if let Some(fields) = scalar_ret.get(&func.id) {
                fields.iter().map(|sf| sf.valtype).collect()
            } else {
                func.ret
                    .as_ref()
                    .map(|t| vec![hir_type_to_valtype(t)])
                    .unwrap_or_default()
            };
            let type_idx = types.register(params, results);
            user_func_types.push(type_idx);
            if program.main == Some(func.name) {
                main_wasm_idx = Some(next_idx);
                main_func_type = Some(type_idx);
            }
            next_idx += 1;
        }
    }

    // Route codegen-internal object allocations (struct / enum / string / dyn
    // boxes) through the Prim allocator `std.mem.alloc` (signature
    // (usize) -> *mut u8) so the whole program shares one heap.
    for func in &program.functions {
        if func.runtime.is_some() || !func.type_params.is_empty() {
            continue;
        }
        let Some(sym) = program.symbols.get(func.name.0 as usize) else {
            continue;
        };
        let name = program.interner.resolve(&sym.name);
        if name != "alloc" && name != "free" {
            continue;
        }
        let m = &program.modules[sym.module.0 as usize].name;
        if m.len() == 2
            && m[0] == "std"
            && m[1] == "mem"
            && let Some(idx) = func_map.get(&func.id)
        {
            match name {
                "alloc" => builtins.alloc = *idx,
                "free" => builtins.free = *idx,
                _ => {}
            }
        }
    }
    assert!(
        builtins.alloc != u32::MAX,
        "std.mem.alloc must be linked (the prelude force-loads std.io -> std.mem)"
    );

    // `__rt_resume` (backs std.rt.resume) is the last emitted function, after
    // the user functions; reserve its index and (i32)->(i32) type here.
    let rt_resume_idx = next_idx;
    let rt_resume_type = types.register(vec![ValType::I32], vec![ValType::I32]);
    // The empty function type backs the `yield` tag.
    let start_type = types.register(vec![], vec![]);
    builtins.rt_resume = rt_resume_idx;
    let main_wasm_idx = main_wasm_idx.ok_or(WasmError::MissingMain)?;
    let main_func_type = main_func_type.ok_or(WasmError::MissingMain)?;
    builtins.main_func = main_wasm_idx;

    // The program's entry point is the `@entry` function (`std.rt.boot`),
    // exported as wasm `_start`. There is no generated fallback.
    let entry_wasm_idx = program
        .entry
        .and_then(|fid| func_map.get(&fid).copied())
        .expect("an @entry function must be linked (prelude force-loads std.rt)");

    // WasmFX: continuation type wrapping main's function signature; the
    // scheduler in `_start` uses this for `cont.new` and `resume`. The
    // `yield` tag has the empty function-type signature so suspend/resume
    // carry no values — yield is "I want to reschedule," nothing else.
    let main_cont_type = types.register_cont(main_func_type);
    builtins.cont_type = main_cont_type;
    let yield_tag_idx: u32 = 0;

    // Synthesized per-type drop functions, emitted after `__rt_resume`. Every
    // concrete needs-drop type T gets a `drop_T(ptr)`; a `Stmt::Drop` of type T
    // lowers to a call of it. Indices are assigned up front (before any body is
    // emitted) so mutually-referencing types resolve.
    let drop_types = collect_drop_types(program, &drop_info, &inline_policy);
    let drop_fn_type = types.register(vec![ValType::I32], vec![]);
    let mut drop_fns: HashMap<hir::Type, u32> = HashMap::new();
    for (i, ty) in drop_types.iter().enumerate() {
        drop_fns.insert(ty.clone(), rt_resume_idx + 1 + i as u32);
    }

    // First pass: walk every user function in the same order they'll be
    // emitted, collect dbg prefix strings AND string literal bytes, lay them
    // out in static memory starting at STATIC_DATA_START. Record per-function
    // slice ranges so each function's EmitCtx can index into the global
    // tables by per-function counter.
    let mut str_sites: Vec<StrSite> = Vec::new();
    let mut static_data: Vec<u8> = Vec::new();
    let mut per_func_str_range: HashMap<hir::FuncId, std::ops::Range<usize>> = HashMap::new();
    let mut cursor: u32 = STATIC_DATA_START;
    for func in &program.functions {
        if func.runtime.is_some() || !func.type_params.is_empty() {
            continue;
        }
        let str_start = str_sites.len();
        let mut literals: Vec<&str> = Vec::new();
        collect_str_literals_block(&func.body, &mut literals);
        for s in literals {
            let bytes = s.as_bytes();
            let len = bytes.len() as u32;
            static_data.extend_from_slice(bytes);
            str_sites.push(StrSite { ptr: cursor, len });
            cursor += len;
        }
        per_func_str_range.insert(func.id, str_start..str_sites.len());
    }

    // The allocator's out-of-memory abort (`prim_rt_oom`) needs a static,
    // allocation-free message: lay its bytes out here (after the user string
    // literals) so the emission can reference them by offset. No String box is
    // ever built, so the abort path cannot re-enter the allocator. The
    // sentinel must stay in sync with ABORT_SENTINEL in prim-cli/src/lib.rs
    // and the `panic` in prim-std/src/std/sys/sys.prim.
    let oom_msg = {
        let bytes = b"out of memory\n";
        let site = StrSite {
            ptr: cursor,
            len: bytes.len() as u32,
        };
        static_data.extend_from_slice(bytes);
        cursor += bytes.len() as u32;
        site
    };
    let oom_sentinel = {
        let bytes = b"prim-runtime-abort: nonzero exit\n";
        let site = StrSite {
            ptr: cursor,
            len: bytes.len() as u32,
        };
        static_data.extend_from_slice(bytes);
        cursor += bytes.len() as u32;
        site
    };

    // Trait dispatch: every impl method that may be invoked through a
    // trait fat pointer gets a stable slot in wasm table 0. The vtables
    // (one per (TraitId, StructId) impl) live in static memory and store
    // the table slot index for each method in trait declaration order.
    //
    // method_table_idx: FuncId -> wasm table slot
    // table_entries:    wasm function indices in slot order (for the
    //                   active element segment)
    // vtable_addr:      (TraitId, StructId) -> static-memory address
    // Vtables back dynamic dispatch, which is struct-only; enum and primitive
    // impls are recorded in `program.impls` for bound checking but dispatch
    // statically, so they get no vtable. Map each struct owner back to its
    // `(TraitId, StructId)` key and keep the full impl entry for lookup.
    let mut impl_keys: Vec<(
        (hir::TraitId, hir::StructId),
        (hir::TraitId, hir::MethodOwner),
    )> = program
        .impls
        .keys()
        .filter_map(|&(t, owner)| match owner {
            hir::MethodOwner::Struct(s) => Some(((t, s), (t, owner))),
            _ => None,
        })
        .collect();
    impl_keys.sort_by_key(|((t, s), _)| (t.0, s.0));

    let mut method_table_idx: HashMap<hir::FuncId, u32> = HashMap::new();
    let mut table_entries: Vec<u32> = Vec::new();
    for (_, key) in &impl_keys {
        for fid in &program.impls[key] {
            if fid.0 == u32::MAX || method_table_idx.contains_key(fid) {
                continue;
            }
            let wasm_fn = *func_map.get(fid).expect("impl method missing in func_map");
            let slot = table_entries.len() as u32;
            method_table_idx.insert(*fid, slot);
            table_entries.push(wasm_fn);
        }
    }

    // --- Trait-object destructors ---
    //
    // Each coercible struct S gets a `drop_glue_S(data_ptr)` box destructor
    // (`(i32) -> ()`) reachable through a per-vtable drop slot, so a trait
    // object can free its erased value. Its wasm index is fixed up front; its
    // table-0 slot is what the vtable stores. The glue functions follow the
    // per-type drop functions in the function index layout.
    let drop_glue_base = rt_resume_idx + 1 + drop_types.len() as u32;
    let mut drop_glue_slot: HashMap<hir::StructId, u32> = HashMap::new();
    let mut coercible_structs: Vec<hir::StructId> = impl_keys.iter().map(|(sk, _)| sk.1).collect();
    coercible_structs.sort_by_key(|s| s.0);
    coercible_structs.dedup();
    for (i, sid) in coercible_structs.iter().enumerate() {
        let wasm_fn = drop_glue_base + i as u32;
        let slot = table_entries.len() as u32;
        table_entries.push(wasm_fn);
        drop_glue_slot.insert(*sid, slot);
    }

    // Each trait with a vtable gets a `drop_trait_Trait(fat_ptr)` destructor
    // (after the drop glues). It reads the vtable's drop slot and dispatches.
    let drop_trait_base = drop_glue_base + coercible_structs.len() as u32;
    let mut drop_trait_fns: HashMap<hir::TraitId, u32> = HashMap::new();
    let mut coercible_traits: Vec<hir::TraitId> = impl_keys.iter().map(|(sk, _)| sk.0).collect();
    coercible_traits.sort_by_key(|t| t.0);
    coercible_traits.dedup();
    for (i, tid) in coercible_traits.iter().enumerate() {
        drop_trait_fns.insert(*tid, drop_trait_base + i as u32);
    }

    // Lay out vtables in static memory, 4 bytes per slot. Each slot holds
    // a wasm table index (i32). Pad static_data once up to the aligned
    // cursor, then append vtable bytes contiguously.
    cursor = (cursor + 3) & !3;
    let pad_to = (cursor - STATIC_DATA_START) as usize;
    if static_data.len() < pad_to {
        static_data.resize(pad_to, 0);
    }
    let mut vtable_addr: HashMap<(hir::TraitId, hir::StructId), u32> = HashMap::new();
    for (struct_key, key) in &impl_keys {
        vtable_addr.insert(*struct_key, cursor);
        for fid in &program.impls[key] {
            let slot = if fid.0 == u32::MAX {
                // Missing impl method — sentinel slot 0 traps via the
                // null-funcref check (or wrong-signature trap) on dispatch.
                0u32
            } else {
                *method_table_idx.get(fid).expect("missing table slot")
            };
            static_data.extend_from_slice(&slot.to_le_bytes());
            cursor += 4;
        }
        // Drop slot: the struct's drop-glue table slot, at a fixed offset
        // after the trait's method slots. `drop_trait_Trait` reads it to free
        // the erased value.
        let glue_slot = *drop_glue_slot
            .get(&struct_key.1)
            .expect("missing drop glue slot for coercible struct");
        static_data.extend_from_slice(&glue_slot.to_le_bytes());
        cursor += 4;
    }

    // Register a wasm type-index for each trait method's signature so
    // call_indirect at dispatch sites can reference it. The dispatched
    // function's wasm signature is (i32 receiver-data-ptr, ...remaining
    // params) → return type — uniform across all impls of a given trait
    // method since pointer types all lower to i32.
    let mut dyn_call_types: HashMap<(hir::TraitId, u32), u32> = HashMap::new();
    for t in &program.traits {
        for (mi, sig) in t.methods.iter().enumerate() {
            let mut params: Vec<ValType> = Vec::with_capacity(sig.params.len().max(1));
            if sig.params.is_empty() {
                params.push(ValType::I32);
            } else {
                params.push(ValType::I32);
                for p in &sig.params[1..] {
                    params.push(hir_type_to_valtype(p));
                }
            }
            let results: Vec<ValType> = sig
                .ret
                .as_ref()
                .map(|r| vec![hir_type_to_valtype(r)])
                .unwrap_or_default();
            let type_idx = types.register(params, results);
            dyn_call_types.insert((t.id, mi as u32), type_idx);
        }
    }

    let mut module = Module::new();

    // Type section
    module.section(&types.build_section());

    // Import section
    let mut imports = ImportSection::new();
    imports.import(
        "wasi_snapshot_preview1",
        "fd_write",
        wasm_encoder::EntityType::Function(fd_write_type),
    );
    imports.import(
        "wasi_snapshot_preview1",
        "clock_time_get",
        wasm_encoder::EntityType::Function(clock_time_get_type),
    );
    imports.import(
        "wasi_snapshot_preview1",
        "poll_oneoff",
        wasm_encoder::EntityType::Function(poll_oneoff_type),
    );
    imports.import(
        "wasi_snapshot_preview1",
        "fd_read",
        wasm_encoder::EntityType::Function(fd_read_type),
    );
    imports.import(
        "wasi_snapshot_preview1",
        "path_open",
        wasm_encoder::EntityType::Function(path_open_type),
    );
    imports.import(
        "wasi_snapshot_preview1",
        "fd_close",
        wasm_encoder::EntityType::Function(fd_close_type),
    );
    module.section(&imports);

    // Function section
    let mut functions = FunctionSection::new();
    functions.function(write_bytes_type); // __write_bytes
    for &type_idx in &user_func_types {
        functions.function(type_idx);
    }
    functions.function(rt_resume_type); // __rt_resume
    for _ in &drop_types {
        functions.function(drop_fn_type); // drop_T(ptr) for each needs-drop type
    }
    for _ in &coercible_structs {
        functions.function(drop_fn_type); // drop_glue_S(ptr) for each coercible struct
    }
    for _ in &coercible_traits {
        functions.function(drop_fn_type); // drop_trait_Trait(fat_ptr) for each trait
    }
    module.section(&functions);

    // Table section: a funcref table holding every impl method that may
    // be invoked through a trait fat pointer. Table 0 is the dispatch
    // table; if there are no impls, we still emit an empty table so a
    // call_indirect against table 0 is always well-formed.
    let mut tables = TableSection::new();
    let table_size = table_entries.len() as u64;
    tables.table(TableType {
        element_type: RefType::FUNCREF,
        minimum: table_size,
        maximum: Some(table_size),
        table64: false,
        shared: false,
    });
    // Table 1: the scheduler's task table, holding each task's continuation.
    // Slot 0 is the initial task (`main`). The element type is nullable so a
    // finished task's slot can be cleared, and the table is growable so tasks
    // can be added at runtime.
    let cont_table_idx: u32 = 1;
    // Starts empty; the entry point's `spawn_main` grows it to add `main`, and
    // `spawn` grows it for each further task.
    tables.table(TableType {
        element_type: RefType {
            nullable: true,
            heap_type: HeapType::Concrete(main_cont_type),
        },
        minimum: 0,
        maximum: None,
        table64: false,
        shared: false,
    });
    module.section(&tables);

    // Memory section
    let mut memories = MemorySection::new();
    memories.memory(MemoryType {
        minimum: 1,
        maximum: None,
        memory64: false,
        shared: false,
        page_size_log2: None,
    });
    module.section(&memories);

    // Tag section: declare the `yield` tag used by the scheduler's resume
    // handler. Stack-switching tags use the same binary encoding as
    // exception tags (attribute byte 0); `TagKind::Exception` is just the
    // name wasm-encoder gives that encoding.
    let mut tags = TagSection::new();
    tags.tag(TagType {
        kind: TagKind::Exception,
        func_type_idx: start_type,
    });
    module.section(&tags);

    // Global section: the user's globals (the allocator's `GM`, user `global`
    // declarations, ...). The heap is managed entirely by `std.mem` via
    // `memory.grow`, so the module has no runtime heap-pointer global.
    let mut globals = GlobalSection::new();
    let mut global_wasm_idx: HashMap<hir::GlobalId, u32> = HashMap::new();
    for (i, g) in program.globals.iter().enumerate() {
        let val_type = hir_type_to_valtype(&g.ty);
        let init = match g.init {
            hir::GlobalInit::I32(v) => ConstExpr::i32_const(v),
            hir::GlobalInit::I64(v) => ConstExpr::i64_const(v),
            hir::GlobalInit::F32(v) => ConstExpr::f32_const(v.into()),
            hir::GlobalInit::F64(v) => ConstExpr::f64_const(v.into()),
        };
        globals.global(
            GlobalType {
                val_type,
                mutable: g.mutable,
                shared: false,
            },
            &init,
        );
        global_wasm_idx.insert(g.id, i as u32);
    }
    module.section(&globals);

    // Export section
    let mut exports = ExportSection::new();
    exports.export("_start", ExportKind::Func, entry_wasm_idx);
    exports.export("memory", ExportKind::Memory, 0);
    module.section(&exports);

    // Element section: an active segment populates the dispatch table at
    // offset 0 with each impl method's wasm function index, plus a
    // declared segment for `ref.func $main` (the scheduler in `_start`
    // uses ref.func, which requires the target to be declared ref-able).
    let mut elements = ElementSection::new();
    if !table_entries.is_empty() {
        elements.active(
            Some(0),
            &ConstExpr::i32_const(0),
            Elements::Functions(table_entries.clone().into()),
        );
    }
    // Declare every user function ref-able: `_start` takes `ref.func main`, and
    // `spawn` takes `ref.func` of its target. Declaring all of them keeps any
    // function spawnable without tracking which are spawn targets.
    let mut declared: Vec<u32> = func_map.values().copied().collect();
    declared.sort_unstable();
    declared.dedup();
    elements.declared(Elements::Functions(declared.into()));
    module.section(&elements);

    // Code section
    let mut codes = CodeSection::new();
    codes.function(&emit_write_bytes(fd_write_idx));
    for func in &program.functions {
        if func.runtime.is_none() && func.type_params.is_empty() {
            let str_range = per_func_str_range.get(&func.id).cloned().unwrap_or(0..0);
            let str_slice = &str_sites[str_range];
            let ctx = build_emit_ctx(
                program,
                &inline_policy,
                &drop_info,
                &scalar_abi,
                &scalar_ret,
                func,
                &func_map,
                &drop_fns,
                &drop_trait_fns,
                &runtime_map,
                &builtins,
                &struct_layouts,
                &enum_layouts,
                string_layout,
                &global_wasm_idx,
                &dyn_call_types,
                &vtable_addr,
                str_slice,
                oom_msg,
                oom_sentinel,
            );
            codes.function(&emit_user_function(func, &ctx)?);
        }
    }
    let main_func = program
        .functions
        .iter()
        .find(|f| program.main == Some(f.name))
        .unwrap();
    codes.function(&emit_rt_resume(
        main_cont_type,
        cont_table_idx,
        yield_tag_idx,
        main_func.ret.is_some(),
    ));
    // Bodies of the synthesized drop functions, in the same order their indices
    // were assigned (so they line up with the function-section entries above).
    for ty in &drop_types {
        codes.function(&emit_drop_fn(
            ty,
            &drop_fns,
            &drop_trait_fns,
            &func_map,
            program,
            &drop_info,
            &inline_policy,
            builtins.free,
        ));
    }
    // Drop glues (one per coercible struct) and trait-object destructors (one
    // per trait), in the same order their indices were assigned.
    for sid in &coercible_structs {
        codes.function(&emit_drop_glue_fn(
            *sid,
            &drop_fns,
            &drop_info,
            builtins.free,
        ));
    }
    for tid in &coercible_traits {
        codes.function(&emit_drop_trait_fn(
            *tid,
            program,
            drop_fn_type,
            builtins.free,
        ));
    }
    module.section(&codes);

    // Data section
    let mut data = DataSection::new();
    if !static_data.is_empty() {
        data.active(
            0,
            &ConstExpr::i32_const(STATIC_DATA_START as i32),
            static_data.iter().copied(),
        );
    }
    module.section(&data);

    Ok(module.finish())
}
