use prim_hir::HirProgram;

fn push_len_segment(out: &mut String, tag: &str, segment: &str) {
    use std::fmt::Write;
    let _ = write!(out, "${}{}_{segment}", tag, segment.len());
}

fn push_tag(out: &mut String, tag: &str, value: &str) {
    use std::fmt::Write;
    let _ = write!(out, "${}{value}", tag);
}

fn kind_tag(kind: &prim_hir::SymbolKind) -> &'static str {
    match kind {
        prim_hir::SymbolKind::Module => "mod",
        prim_hir::SymbolKind::Function(_) => "fn",
        prim_hir::SymbolKind::Struct(_) => "struct",
        prim_hir::SymbolKind::Global(_) => "global",
        prim_hir::SymbolKind::Param => "param",
        prim_hir::SymbolKind::Local => "local",
        prim_hir::SymbolKind::Field => "field",
        prim_hir::SymbolKind::Trait => "trait",
        prim_hir::SymbolKind::Impl => "impl",
        prim_hir::SymbolKind::Unknown => "unknown",
    }
}

pub(crate) fn symbol_name(sym: prim_hir::SymbolId, program: &HirProgram) -> String {
    if let Some(entry) = program.symbols.entries.iter().find(|e| e.id == sym) {
        let mut name = String::from("prim");
        if let Some(module) = program.modules.iter().find(|m| m.id == entry.module) {
            for seg in &module.name {
                push_len_segment(&mut name, "M", seg);
            }
        }
        push_tag(&mut name, "K", kind_tag(&entry.kind));
        push_len_segment(&mut name, "N", &entry.name);
        return name;
    }
    format!("sym_{}", sym.0)
}

pub(crate) fn string_literal_symbol(
    program: &HirProgram,
    module_id: prim_hir::ModuleId,
    span_id: prim_hir::SpanId,
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
    use prim_hir::{
        FileId, FileInfo, HirProgram, Items, Module, ModuleId, SpanId, Symbol, SymbolId,
        SymbolKind, SymbolTable,
    };
    use prim_tok::Span;
    use std::collections::HashMap;
    use std::path::PathBuf;

    fn program_with_module_and_symbol(
        module_name: Vec<&str>,
        symbol_name: &str,
        kind: SymbolKind,
    ) -> HirProgram {
        let module_id = ModuleId(0);
        let symbol_id = SymbolId(0);
        let module = Module {
            id: module_id,
            name: module_name.into_iter().map(|s| s.to_string()).collect(),
            files: Vec::new(),
            exports: Vec::new(),
            imports: Vec::new(),
        };
        let symbol = Symbol {
            id: symbol_id,
            module: module_id,
            name: symbol_name.to_string(),
            kind,
        };
        HirProgram {
            modules: vec![module],
            items: Items::default(),
            symbols: SymbolTable {
                entries: vec![symbol],
                by_name: HashMap::new(),
            },
            main: None,
            files: vec![FileInfo {
                id: FileId(0),
                path: PathBuf::from("dummy.prim"),
                source: String::new(),
            }],
            spans: vec![(FileId(0), Span::new(0, 0))],
        }
    }

    #[test]
    fn mangle_symbol_name_with_module_path() {
        let program = program_with_module_and_symbol(
            vec!["std", "io"],
            "foo",
            SymbolKind::Function(prim_hir::FuncId(0)),
        );
        let got = symbol_name(SymbolId(0), &program);
        assert_eq!(got, "prim$M3_std$M2_io$Kfn$N3_foo");
    }

    #[test]
    fn mangle_string_literal_with_span_and_file() {
        let mut program = program_with_module_and_symbol(
            vec!["app"],
            "main",
            SymbolKind::Function(prim_hir::FuncId(0)),
        );
        program.spans = vec![(FileId(7), Span::new(5, 9))];
        let got = string_literal_symbol(&program, ModuleId(0), SpanId(0), 4);
        assert_eq!(got, "prim$M3_app$Kstr$F7$S5_9$B4");
    }
}
