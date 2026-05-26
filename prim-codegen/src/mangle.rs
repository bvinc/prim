use prim_compiler::hir;

fn push_len_segment(out: &mut String, tag: &str, segment: &str) {
    use std::fmt::Write;
    let _ = write!(out, "${}{}_{segment}", tag, segment.len());
}

fn push_tag(out: &mut String, tag: &str, value: &str) {
    use std::fmt::Write;
    let _ = write!(out, "${}{value}", tag);
}

fn kind_tag(kind: &hir::SymbolKind) -> &'static str {
    match kind {
        hir::SymbolKind::Module => "mod",
        hir::SymbolKind::Function(_) => "fn",
        hir::SymbolKind::Struct(_) => "struct",
        hir::SymbolKind::Param => "param",
        hir::SymbolKind::Local => "local",
        hir::SymbolKind::Field => "field",
        hir::SymbolKind::Trait => "trait",
        hir::SymbolKind::Unknown => "unknown",
    }
}

pub(crate) fn symbol_name(sym: hir::SymbolId, program: &hir::Program) -> String {
    let entry = &program.symbols.entries[sym.0 as usize];
    let mut name = String::from("prim");
    let module = &program.modules[entry.module.0 as usize];
    for seg in &module.name {
        push_len_segment(&mut name, "M", seg);
    }
    push_tag(&mut name, "K", kind_tag(&entry.kind));
    let entry_name = program
        .interner
        .resolve(entry.name)
        .expect("missing symbol name");
    push_len_segment(&mut name, "N", entry_name);
    name
}

pub(crate) fn string_literal_symbol(
    program: &hir::Program,
    module_id: hir::ModuleId,
    span_id: hir::SpanId,
    bytes_len: usize,
) -> String {
    let mut name = String::from("prim");
    if let Some(module) = program.modules.iter().find(|m| m.id == module_id) {
        for seg in &module.name {
            push_len_segment(&mut name, "M", seg);
        }
    }
    push_tag(&mut name, "K", "str");
    if let Some((file_id, span)) = program.spans.get(span_id.0 as usize) {
        push_tag(&mut name, "F", &file_id.0.to_string());
        use std::fmt::Write;
        let _ = write!(name, "$S{}_{}", span.start(), span.end());
    }
    push_tag(&mut name, "B", &bytes_len.to_string());
    name
}

#[cfg(test)]
mod tests {
    use super::*;
    use hir::{
        FileId, FileInfo, Interner, Items, Module, ModuleId, Program, SpanId, Symbol, SymbolId,
        SymbolKind, SymbolTable,
    };
    use prim_tok::Span;
    use std::path::PathBuf;

    fn program_with_module_and_symbol(
        module_name: Vec<&str>,
        symbol_name: &str,
        kind: SymbolKind,
    ) -> Program {
        let module_id = ModuleId(0);
        let symbol_id = SymbolId(0);
        let mut interner = Interner::new();
        let name_sym = interner.get_or_intern(symbol_name);
        let module = Module {
            id: module_id,
            name: module_name.into_iter().map(|s| s.to_string()).collect(),
        };
        let symbol = Symbol {
            id: symbol_id,
            module: module_id,
            name: name_sym,
            kind,
        };
        Program {
            modules: vec![module],
            items: Items::default(),
            symbols: SymbolTable {
                entries: vec![symbol],
            },
            interner,
            main: None,
            files: vec![FileInfo {
                id: FileId(0),
                path: PathBuf::from("dummy.prim"),
            }],
            spans: vec![(FileId(0), Span::new(0, 0))],
        }
    }

    #[test]
    fn mangle_symbol_name_with_module_path() {
        let program = program_with_module_and_symbol(
            vec!["std", "io"],
            "foo",
            SymbolKind::Function(hir::FuncId(0)),
        );
        let got = symbol_name(SymbolId(0), &program);
        assert_eq!(got, "prim$M3_std$M2_io$Kfn$N3_foo");
    }

    #[test]
    fn mangle_string_literal_with_span_and_file() {
        let mut program = program_with_module_and_symbol(
            vec!["app"],
            "main",
            SymbolKind::Function(hir::FuncId(0)),
        );
        program.spans = vec![(FileId(7), Span::new(5, 9))];
        let got = string_literal_symbol(&program, ModuleId(0), SpanId(0), 4);
        assert_eq!(got, "prim$M3_app$Kstr$F7$S5_9$B4");
    }
}
