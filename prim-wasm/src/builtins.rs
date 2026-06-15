//! Hand-written wasm builtin function bodies.
//!
//! Each emit_*` here produces a complete `wasm_encoder::Function` for one
//! builtin slot in the wasm module. These are independent of user code — the
//! main `generate_wasm` orchestration calls them once each, then references
//! them by index from emitted user code.

use crate::layout::{
    DIGIT_BUF_END, DOT_OFFSET, FALSE_OFFSET, FLOAT_SCRATCH, HEAP_PTR_GLOBAL, MEM8, MEM32,
    NEWLINE_OFFSET, TRUE_OFFSET,
};
use wasm_encoder::{BlockType, Function, Handle, HeapType, Instruction, RefType, ValType};

/// Wasm function indices for the builtin runtime helpers, computed by the
/// orchestration in `lib.rs` and passed into the emitters that need them.
pub(crate) struct Builtins {
    pub println_i64: u32,
    pub println_u64: u32,
    pub println_bool: u32,
    pub println_f64: u32,
    /// Allocator for codegen-internal object boxes (struct / enum / string /
    /// dyn). Defaults to the fallback bump allocator (`emit_alloc`); `lib.rs`
    /// upgrades it to the real Prim allocator (`std.mem.alloc`) whenever that
    /// module is linked, so all heap use shares a single allocator.
    pub alloc: u32,
    pub print_bytes: u32,
    /// Tag index for cooperative yield. Used by `std.rt.yield` which
    /// lowers to `suspend $yield_tag`.
    pub yield_tag: u32,
    /// Table index of the scheduler's task table (continuations). Used by
    /// `std.rt.task_count` / `task_live`.
    pub cont_table: u32,
    /// Function index of the `__rt_resume` helper backing `std.rt.resume`.
    pub rt_resume: u32,
    /// Type index of the task continuation type `(cont fn() -> ())`. Used by
    /// `spawn` for `cont.new`.
    pub cont_type: u32,
    /// Function index of the user's `main`. Used by `spawn_main`.
    pub main_func: u32,
}

// ---- Shared snippets used by multiple builtin emitters ----

/// `fd_write(stdout=1, iovs=0, iovs_len=1, nwritten=8)` followed by `drop`.
/// Assumes the iovec at offset 0 is already populated.
fn emit_fd_write_buf(f: &mut Function, fd_write_idx: u32) {
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::Call(fd_write_idx));
    f.instruction(&Instruction::Drop);
}

/// Write the canned `'\n'` byte to stdout. Used by every println builtin.
fn emit_newline(f: &mut Function, fd_write_idx: u32) {
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(NEWLINE_OFFSET));
    f.instruction(&Instruction::I32Store(MEM32));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Store(MEM32));
    emit_fd_write_buf(f, fd_write_idx);
}

// ---- Builtin function bodies ----

/// `__print_bytes(ptr, len)` — write `len` bytes from `ptr` to stdout via
/// WASI fd_write. No trailing newline. Used by `@dbg` for its prefix string
/// and by `write(fd, s: String)` for printing string contents.
pub(crate) fn emit_print_bytes(fd_write_idx: u32) -> Function {
    let mut f = Function::new(vec![]);
    let ptr: u32 = 0;
    let len: u32 = 1;

    // iovec at [0..8) = { buf: ptr, buf_len: len }
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Store(MEM32));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::LocalGet(len));
    f.instruction(&Instruction::I32Store(MEM32));

    emit_fd_write_buf(&mut f, fd_write_idx);

    f.instruction(&Instruction::End);
    f
}

/// `__alloc(size: i32) -> i32` — bump-allocate `size` bytes (aligned up to
/// 8), advance the heap pointer global, return the allocated pointer.
pub(crate) fn emit_alloc() -> Function {
    let mut f = Function::new(vec![(1, ValType::I32)]);
    let size: u32 = 0;
    let ptr: u32 = 1;
    const PAGE: i32 = 65536;

    // ptr = heap_ptr
    f.instruction(&Instruction::GlobalGet(HEAP_PTR_GLOBAL));
    f.instruction(&Instruction::LocalSet(ptr));

    // heap_ptr = (ptr + size + 7) & ~7
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::LocalGet(size));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(7));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(-8));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::GlobalSet(HEAP_PTR_GLOBAL));

    // Grow linear memory if the new heap pointer exceeds the current size:
    //   if heap_ptr > memory.size*PAGE {
    //       memory.grow((heap_ptr - memory.size*PAGE + PAGE-1) / PAGE)
    //   }
    f.instruction(&Instruction::GlobalGet(HEAP_PTR_GLOBAL));
    f.instruction(&Instruction::MemorySize(0));
    f.instruction(&Instruction::I32Const(PAGE));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32GtU);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::GlobalGet(HEAP_PTR_GLOBAL));
    f.instruction(&Instruction::MemorySize(0));
    f.instruction(&Instruction::I32Const(PAGE));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Sub); // deficit bytes
    f.instruction(&Instruction::I32Const(PAGE - 1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(PAGE));
    f.instruction(&Instruction::I32DivU); // pages needed
    f.instruction(&Instruction::MemoryGrow(0));
    f.instruction(&Instruction::Drop); // ignore previous size / -1 on OOM
    f.instruction(&Instruction::End);

    // return ptr
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::End);
    f
}

/// `__println_i64(val: i64)` — print the value as signed decimal + newline.
pub(crate) fn emit_println_i64(fd_write_idx: u32) -> Function {
    let mut f = Function::new(vec![
        (1, ValType::I32), // ptr
        (1, ValType::I32), // is_neg
        (1, ValType::I64), // abs_val
    ]);

    let val: u32 = 0;
    let ptr: u32 = 1;
    let is_neg: u32 = 2;
    let abs_val: u32 = 3;

    f.instruction(&Instruction::I32Const(DIGIT_BUF_END));
    f.instruction(&Instruction::LocalSet(ptr));

    f.instruction(&Instruction::LocalGet(val));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64LtS);
    f.instruction(&Instruction::LocalSet(is_neg));

    f.instruction(&Instruction::LocalGet(is_neg));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::LocalGet(val));
    f.instruction(&Instruction::I64Sub);
    f.instruction(&Instruction::LocalSet(abs_val));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(val));
    f.instruction(&Instruction::LocalSet(abs_val));
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(ptr));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::LocalGet(abs_val));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64RemU);
    f.instruction(&Instruction::I64Const(0x30));
    f.instruction(&Instruction::I64Add);
    f.instruction(&Instruction::I64Store8(MEM8));
    f.instruction(&Instruction::LocalGet(abs_val));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64DivU);
    f.instruction(&Instruction::LocalSet(abs_val));
    f.instruction(&Instruction::LocalGet(abs_val));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64Ne);
    f.instruction(&Instruction::BrIf(0));
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(is_neg));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(ptr));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Const(0x2D));
    f.instruction(&Instruction::I32Store8(MEM8));
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Store(MEM32));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(DIGIT_BUF_END));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Store(MEM32));
    emit_fd_write_buf(&mut f, fd_write_idx);

    emit_newline(&mut f, fd_write_idx);

    f.instruction(&Instruction::End);
    f
}

/// `__println_u64(val: i64)` — print the value as unsigned decimal + newline.
pub(crate) fn emit_println_u64(fd_write_idx: u32) -> Function {
    let mut f = Function::new(vec![(1, ValType::I32)]);

    let val: u32 = 0;
    let ptr: u32 = 1;

    f.instruction(&Instruction::I32Const(DIGIT_BUF_END));
    f.instruction(&Instruction::LocalSet(ptr));

    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(ptr));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::LocalGet(val));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64RemU);
    f.instruction(&Instruction::I64Const(0x30));
    f.instruction(&Instruction::I64Add);
    f.instruction(&Instruction::I64Store8(MEM8));
    f.instruction(&Instruction::LocalGet(val));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64DivU);
    f.instruction(&Instruction::LocalSet(val));
    f.instruction(&Instruction::LocalGet(val));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64Ne);
    f.instruction(&Instruction::BrIf(0));
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Store(MEM32));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(DIGIT_BUF_END));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Store(MEM32));
    emit_fd_write_buf(&mut f, fd_write_idx);

    emit_newline(&mut f, fd_write_idx);

    f.instruction(&Instruction::End);
    f
}

/// `__println_bool(val: i32)` — print "true" / "false" + newline.
pub(crate) fn emit_println_bool(fd_write_idx: u32) -> Function {
    let mut f = Function::new(vec![]);
    let val: u32 = 0;

    f.instruction(&Instruction::LocalGet(val));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(TRUE_OFFSET));
    f.instruction(&Instruction::I32Store(MEM32));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Store(MEM32));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(FALSE_OFFSET));
    f.instruction(&Instruction::I32Store(MEM32));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(5));
    f.instruction(&Instruction::I32Store(MEM32));
    f.instruction(&Instruction::End);

    emit_fd_write_buf(&mut f, fd_write_idx);
    emit_newline(&mut f, fd_write_idx);

    f.instruction(&Instruction::End);
    f
}

/// `__println_f64(val: f64)` — print integer part, '.', fractional part
/// (trimmed of trailing zeros), and newline.
pub(crate) fn emit_println_f64(fd_write_idx: u32) -> Function {
    let mut f = Function::new(vec![
        (1, ValType::I32),
        (1, ValType::I32),
        (1, ValType::F64),
        (1, ValType::I64),
        (1, ValType::F64),
        (1, ValType::I32),
        (1, ValType::I32),
    ]);

    let val: u32 = 0;
    let ptr: u32 = 1;
    let is_neg: u32 = 2;
    let abs_val: u32 = 3;
    let int_part: u32 = 4;
    let frac: u32 = 5;
    let frac_end: u32 = 6;
    let count: u32 = 7;

    f.instruction(&Instruction::LocalGet(val));
    f.instruction(&Instruction::F64Const(0.0_f64.into()));
    f.instruction(&Instruction::F64Lt);
    f.instruction(&Instruction::LocalSet(is_neg));

    f.instruction(&Instruction::LocalGet(is_neg));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(val));
    f.instruction(&Instruction::F64Neg);
    f.instruction(&Instruction::LocalSet(abs_val));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(val));
    f.instruction(&Instruction::LocalSet(abs_val));
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(abs_val));
    f.instruction(&Instruction::I64TruncSatF64U);
    f.instruction(&Instruction::LocalSet(int_part));

    f.instruction(&Instruction::LocalGet(abs_val));
    f.instruction(&Instruction::LocalGet(int_part));
    f.instruction(&Instruction::F64ConvertI64U);
    f.instruction(&Instruction::F64Sub);
    f.instruction(&Instruction::LocalSet(frac));

    f.instruction(&Instruction::I32Const(DIGIT_BUF_END));
    f.instruction(&Instruction::LocalSet(ptr));

    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(ptr));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::LocalGet(int_part));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64RemU);
    f.instruction(&Instruction::I64Const(0x30));
    f.instruction(&Instruction::I64Add);
    f.instruction(&Instruction::I64Store8(MEM8));
    f.instruction(&Instruction::LocalGet(int_part));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64DivU);
    f.instruction(&Instruction::LocalSet(int_part));
    f.instruction(&Instruction::LocalGet(int_part));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64Ne);
    f.instruction(&Instruction::BrIf(0));
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(is_neg));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(ptr));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Const(0x2D));
    f.instruction(&Instruction::I32Store8(MEM8));
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Store(MEM32));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(DIGIT_BUF_END));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Store(MEM32));
    emit_fd_write_buf(&mut f, fd_write_idx);

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(DOT_OFFSET));
    f.instruction(&Instruction::I32Store(MEM32));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Store(MEM32));
    emit_fd_write_buf(&mut f, fd_write_idx);

    f.instruction(&Instruction::I32Const(FLOAT_SCRATCH));
    f.instruction(&Instruction::LocalSet(frac_end));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(count));

    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));

    f.instruction(&Instruction::LocalGet(frac));
    f.instruction(&Instruction::F64Const(10.0_f64.into()));
    f.instruction(&Instruction::F64Mul);
    f.instruction(&Instruction::LocalSet(frac));

    f.instruction(&Instruction::LocalGet(frac));
    f.instruction(&Instruction::I32TruncSatF64S);
    f.instruction(&Instruction::LocalSet(ptr));

    f.instruction(&Instruction::LocalGet(frac_end));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::I32Const(0x30));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Store8(MEM8));

    f.instruction(&Instruction::LocalGet(frac));
    f.instruction(&Instruction::LocalGet(ptr));
    f.instruction(&Instruction::F64ConvertI32S);
    f.instruction(&Instruction::F64Sub);
    f.instruction(&Instruction::LocalSet(frac));

    f.instruction(&Instruction::LocalGet(frac_end));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(frac_end));

    f.instruction(&Instruction::LocalGet(count));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(count));

    f.instruction(&Instruction::LocalGet(frac));
    f.instruction(&Instruction::F64Const(1e-10_f64.into()));
    f.instruction(&Instruction::F64Lt);
    f.instruction(&Instruction::BrIf(1));

    f.instruction(&Instruction::LocalGet(count));
    f.instruction(&Instruction::I32Const(15));
    f.instruction(&Instruction::I32GeS);
    f.instruction(&Instruction::BrIf(1));

    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(frac_end));
    f.instruction(&Instruction::I32Const(FLOAT_SCRATCH + 1));
    f.instruction(&Instruction::I32LeS);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(frac_end));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Load8U(MEM8));
    f.instruction(&Instruction::I32Const(0x30));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(frac_end));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(frac_end));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(FLOAT_SCRATCH));
    f.instruction(&Instruction::I32Store(MEM32));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::LocalGet(frac_end));
    f.instruction(&Instruction::I32Const(FLOAT_SCRATCH));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Store(MEM32));
    emit_fd_write_buf(&mut f, fd_write_idx);

    emit_newline(&mut f, fd_write_idx);

    f.instruction(&Instruction::End);
    f
}

/// `__rt_resume(handle: i32) -> i32` — backs `std.rt.resume`. Resume the task
/// in `cont_table[handle]` until it yields or finishes. On yield, store the
/// resumed continuation back into the slot and return 1. On finish, null the
/// slot and return 0. The scheduler loop (`std.rt.schedule`, in Prim) drives
/// this; the per-task continuation save/restore lives here because it needs the
/// `resume` / `on $yield` wasm instructions.
pub(crate) fn emit_rt_resume(
    main_cont_type: u32,
    cont_table: u32,
    yield_tag: u32,
    main_returns_value: bool,
) -> Function {
    let cont_ref = ValType::Ref(RefType {
        nullable: false,
        heap_type: HeapType::Concrete(main_cont_type),
    });
    let cont_null = HeapType::Concrete(main_cont_type);

    // param `handle` (local 0); scratch continuation for the store-back.
    let mut f = Function::new(vec![(1, cont_ref)]);
    let handle: u32 = 0;
    let tmp_cont: u32 = 1;

    f.instruction(&Instruction::Block(BlockType::Result(ValType::I32))); // $after
    f.instruction(&Instruction::Block(BlockType::Result(cont_ref))); // $resumed

    f.instruction(&Instruction::LocalGet(handle));
    f.instruction(&Instruction::TableGet(cont_table));
    f.instruction(&Instruction::RefAsNonNull);
    f.instruction(&Instruction::Resume {
        cont_type_index: main_cont_type,
        resume_table: vec![Handle::OnLabel {
            tag: yield_tag,
            label: 0, // -> $resumed
        }]
        .into(),
    });
    // Returned normally → task finished: null the slot, result 0.
    if main_returns_value {
        f.instruction(&Instruction::Drop);
    }
    f.instruction(&Instruction::LocalGet(handle));
    f.instruction(&Instruction::RefNull(cont_null));
    f.instruction(&Instruction::TableSet(cont_table));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Br(1)); // -> $after with result 0
    f.instruction(&Instruction::End); // $resumed: stack = [new cont]
    // Yielded: store the resumed continuation back, result 1.
    f.instruction(&Instruction::LocalSet(tmp_cont));
    f.instruction(&Instruction::LocalGet(handle));
    f.instruction(&Instruction::LocalGet(tmp_cont));
    f.instruction(&Instruction::TableSet(cont_table));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::End); // $after: stack = [i32 result]
    f.instruction(&Instruction::End); // end function
    f
}
