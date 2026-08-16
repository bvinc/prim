# rex task files for prim

These task files build and test the prim workspace on a rex tool VM — a full
Linux VM where the toolchain and build cache persist between runs. They are
the reproducible, "initialize once, reuse forever" path for anyone (human or
agent) building this repo on a machine without the Rust toolchain.

## Why this speeds things up

- **One persistent VM** (`--name prim`): installed tools, `~/.cargo` registry
  cache, and a persistent cargo target cache (`$HOME/.cache/prim-target`,
  symlinked to `target/` by the build task) all survive across runs. The
  one-time toolchain install and crate downloads happen exactly once; after
  that the ~25 dependency crates are compile-cache hits and each build is a
  few seconds.
- **Pinned toolchain** ([rust-toolchain.toml](../rust-toolchain.toml)): the
  exact Rust 1.88 + rustfmt + clippy is auto-installed by rustup on first
  `cargo` — no version drift, no manual installs.
- **Warm registry**: `init` runs `cargo fetch`, so builds never wait on the
  network to download crates.
- **wasmtime**: `init` installs the latest wasmtime (via the official
  installer) and symlinks it into `/usr/local/bin` — the CLI integration
  tests run generated wasm through it.
- **Warm baseline** (`--warm` on init): fold the initialized state into the
  VM's work-baseline so a later `--clean`/`--refresh` start is already set up.
- **Baked base image**: after init, save the VM's disk as a reusable image
  so brand-new VMs boot with the whole toolchain pre-installed — **no init
  run at all**. See "Skip init: baked base image" below.

## Quick start

```sh
# 1. Initialize (once per VM — installs toolchain + warms the registry)
rex run .rex/init.yaml --name prim

# 2. Build (debug by default; --set profile=release for optimized)
rex run .rex/build.yaml --name prim
rex run .rex/build.yaml --name prim --set profile=release

# 3. Full pre-merge validation (fmt + clippy + tests)
rex run .rex/test.yaml --name prim

# Check a VM is ready without disturbing a running job:
rex run .rex/check.yaml --name prim
```

`--name prim` finds-or-creates the session VM `prim` and reuses it; drop it
for a one-shot throwaway run. Use `--warm` on the first init run to bake the
setup into the VM's work-baseline:

```sh
rex run .rex/init.yaml --name prim --warm
```

## Skip init: baked base image

`build.yaml` and `test.yaml` declare `vm.image: prim-toolchain-rust188` — a
project-scoped image baked from an initialized VM. Because of that, a
**brand-new session/agent in this workspace runs the normal commands and gets
the warm VM automatically**:

```sh
# Fresh session, no VM, no init:
rex run .rex/build.yaml --name prim    # auto-boots prim from the image
rex run .rex/test.yaml --name prim     # reuses it
```

The toolchain, wasmtime, warm cargo registry, and even the warm cargo target
cache (`$HOME/.cache/prim-target`) are all baked into the image, so builds
are fast from the first run. `init.yaml` is only needed if the image doesn't
exist (e.g. a brand-new project) or you want to refresh the image.

### How the image was made (and how to make a new one)

```sh
# One-time, after init: quiesce the disk, then save it as an image
rex resource powerdown prim                       # ACPI shutdown, freeze disk
rex resource save prim prim-toolchain-rust188 \
  --scope project --description "…" --label prim --label rust   # project = any session in this workspace
```

Then boot from it explicitly (only needed if a task file doesn't already name
it via `vm.image`):

```sh
rex resource create prim-toolchain-rust188 --name prim --wait   # boot canonical VM from the image
# or a one-shot throwaway:
rex run --clone prim-toolchain-rust188 .rex/build.yaml
```

Notes:

- **Scope is what makes it survive sessions**: `--scope session` (default)
  is reaped when the saving session ends; `--scope project` is visible to
  and bootable by any session in this workspace (shown in their `rex
  images`) for the project's lifetime; `--scope user` is private to you
  across projects. Use `project`.
- `rex resource save` needs the VM **stopped** (run `powerdown` first). If
  you want the auto-detected tool manifest on the image, pass `--detect`
  while the VM is still running (rex probes, then powers down).
- Image names allow `[A-Za-z0-9_-]` only — no dots (e.g. `rust188`, not
  `1.88`).
- The image is a point-in-time disk copy: to pick up newer toolchain/dep
  changes, re-init a VM and re-save under a new version (e.g.
  `prim-toolchain-rust188-v2`), then update `vm.image` in `build.yaml` /
  `test.yaml`.

## Task summary

| File             | What it does                                                          |
| ---------------- | -------------------------------------------------------------------- |
| `init.yaml`      | apt build deps + rustup + pinned 1.88 toolchain + wasmtime + `cargo fetch` (idempotent) |
| `build.yaml`     | `./build.sh` (cargo build + stage dist) against a persistent target cache; pulls `target/{debug,release}/dist` back |
| `test.yaml`      | `cargo fmt --check` + `clippy -D warnings` + `cargo test --workspace` |
| `check.yaml`     | Fast readiness probe (rustup/toolchain/cargo present) |

Each file carries a `description:`; run `rex validate .rex/<file>.yaml` to
print it or check syntax.
