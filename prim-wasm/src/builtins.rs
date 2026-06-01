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
use wasm_encoder::{BlockType, Function, Instruction, ValType};

/// Wasm function indices for the builtin runtime helpers, computed by the
/// orchestration in `lib.rs` and passed into the emitters that need them.
pub(crate) struct Builtins {
    pub println_i64: u32,
    pub println_u64: u32,
    pub println_bool: u32,
    pub println_f64: u32,
    pub alloc: u32,
    pub print_bytes: u32,
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

/// `_start()` — WASI entry point. Calls the user's `main`; drops its return
/// value if it has one.
pub(crate) fn emit_start(main_idx: u32, main_returns_value: bool) -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::Call(main_idx));
    if main_returns_value {
        f.instruction(&Instruction::Drop);
    }
    f.instruction(&Instruction::End);
    f
}
