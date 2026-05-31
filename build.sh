#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: build.sh [--release]

Build the prim toolchain and stage a runnable tree under target/{debug,release}/dist:
  bin/prim       - compiled binary
  src/std/...    - standard library sources

The staged tree is self-contained: run with PRIM_ROOT pointing at the dist directory.
EOF
}

PROFILE=debug
BUILD_ARGS=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    --release)
      PROFILE=release
      BUILD_ARGS+=(--release)
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage
      exit 1
      ;;
  esac
done

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TARGET="$ROOT/target/$PROFILE"
DIST="$TARGET/dist"

echo ">> Building prim ($PROFILE)..."
cargo build "${BUILD_ARGS[@]}"

echo ">> Preparing dist at $DIST"
rm -rf "$DIST"
mkdir -p "$DIST/bin" "$DIST/src"

# Copy binary
cp "$TARGET/prim" "$DIST/bin/" 2>/dev/null || cp "$TARGET/prim.exe" "$DIST/bin/" 2>/dev/null || {
  echo "prim binary not found under $TARGET" >&2
  exit 1
}

# Copy standard library sources
if [[ -d "$ROOT/prim-std/src" ]]; then
  mkdir -p "$DIST/src/std"
  cp -r "$ROOT/prim-std/src/std" "$DIST/src/"
elif [[ -d "$ROOT/src/std" ]]; then
  mkdir -p "$DIST/src"
  cp -r "$ROOT/src/std" "$DIST/src/"
else
  echo "Standard library not found (expected prim-std/src or src/std)" >&2
  exit 1
fi

cat > "$DIST/README.md" <<EOF
Staged Prim distribution for profile: $PROFILE

Layout:
  bin/prim       - CLI binary
  src/std/*      - standard library sources

Run with:
  export PRIM_ROOT="\$(pwd)"
  ./bin/prim run path/to/file.prim
EOF

echo ">> Done. Staged artifacts in $DIST"
