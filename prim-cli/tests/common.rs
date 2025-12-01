use std::fs;
use std::path::{Path, PathBuf};
use std::sync::OnceLock;
use tempfile::tempdir;

pub fn staged_prim_root() -> PathBuf {
    static ROOT: OnceLock<PathBuf> = OnceLock::new();
    ROOT.get_or_init(|| {
        let dir = tempdir().expect("create staging dir");
        #[allow(deprecated)]
        let root = dir.into_path();

        let workspace = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("workspace root available");

        // Copy std library
        let std_src = workspace.join("prim-std").join("src").join("std");
        let std_dst = root.join("src").join("std");
        copy_dir(&std_src, &std_dst).expect("copy std");

        // Copy runtime library
        let profile = std::env::var("PROFILE").unwrap_or_else(|_| "debug".to_string());
        let lib_name = if cfg!(target_os = "windows") {
            "prim_rt.lib"
        } else {
            "libprim_rt.a"
        };
        let rt_src = workspace.join("target").join(&profile).join(lib_name);
        let rt_dst_dir = root.join("lib");
        fs::create_dir_all(&rt_dst_dir).expect("create lib dir");
        fs::copy(&rt_src, rt_dst_dir.join(lib_name)).expect("copy runtime lib");

        root
    })
    .clone()
}

fn copy_dir(src: &Path, dst: &Path) -> std::io::Result<()> {
    fs::create_dir_all(dst)?;
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let ty = entry.file_type()?;
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if ty.is_dir() {
            copy_dir(&from, &to)?;
        } else {
            fs::copy(&from, &to)?;
        }
    }
    Ok(())
}
