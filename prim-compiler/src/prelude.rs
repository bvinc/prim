//! The Prim prelude: a small, hardcoded set of stdlib modules that every
//! source file gets implicit access to.
//!
//! Currently:
//!   - `std.string` is auto-imported into every module (it provides the
//!     `String` type that string literals lower to).
//!   - `std.io` is auto-imported into every USER module (it provides `print`,
//!     `println_*`, etc.). Stdlib modules don't get `std.io` auto-imported
//!     to keep the stdlib's internal dependencies explicit.
//!
//! This module owns the policy. The loader doesn't know the word "prelude";
//! `compile()` asks this module for the list of extra modules to load and
//! then asks it to inject auto-imports after loading is done.

use crate::program::{ImportCoverage, ImportRequest, Module, ModuleOrigin, Program};

/// Modules that must be loaded even if nothing in the source tree imports
/// them explicitly. Passed to the loader as "extra modules to ensure-load."
pub(crate) fn extra_modules() -> Vec<Vec<String>> {
    vec![
        vec!["std".into(), "string".into()],
        vec!["std".into(), "io".into()],
    ]
}

/// Inject prelude imports into every loaded module's `imports` list. Called
/// after the loader has finished so the resolver sees the augmented imports.
pub(crate) fn inject(program: &mut Program) {
    for module in &mut program.modules {
        inject_into_module(module);
    }
}

fn inject_into_module(module: &mut Module) {
    let segs: &[String] = &module.name;
    let origin = module_origin(segs);

    if segs != ["std", "string"] {
        ensure_import(&mut module.imports, &["std", "string"]);
    }
    if matches!(origin, ModuleOrigin::User) && segs != ["std", "io"] {
        ensure_import(&mut module.imports, &["std", "io"]);
    }
}

/// Classify a module as stdlib (top-level segment is "std") or user code.
pub(crate) fn module_origin(module_segments: &[String]) -> ModuleOrigin {
    if module_segments.first().is_some_and(|s| s == "std") {
        ModuleOrigin::Stdlib
    } else {
        ModuleOrigin::User
    }
}

/// Add `path` to the imports list with `ImportCoverage::All` if it isn't
/// already present. If it's already there with `Symbols(_)` coverage, upgrade
/// to `All` (the prelude grants visibility to everything in the module).
fn ensure_import(imports: &mut Vec<ImportRequest>, path: &[&str]) {
    let path_owned: Vec<String> = path.iter().map(|s| (*s).into()).collect();
    if let Some(existing) = imports.iter_mut().find(|i| i.module == path_owned) {
        existing.coverage = ImportCoverage::All;
        return;
    }
    imports.push(ImportRequest {
        module: path_owned,
        coverage: ImportCoverage::All,
    });
}
