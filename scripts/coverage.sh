#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$root_dir"

profile_dir="${COVERAGE_PROFILE_DIR:-$root_dir/target/coverage}"
report_dir="${COVERAGE_REPORT_DIR:-$root_dir/target/coverage-report}"

rm -rf "$profile_dir" "$report_dir"
mkdir -p "$profile_dir" "$report_dir"

export LLVM_PROFILE_FILE="$profile_dir/prim-%p-%m.profraw"
export RUSTFLAGS="${RUSTFLAGS:-} -Cinstrument-coverage -Ccodegen-units=1 -Cinline-threshold=0 -Clink-dead-code"
export RUSTDOCFLAGS="${RUSTDOCFLAGS:-} -Cinstrument-coverage"

cargo test

grcov "$profile_dir" \
  --binary-path "$root_dir/target/debug" \
  --source-dir "$root_dir" \
  --output-type html \
  --output-path "$report_dir/index.html"

echo "Coverage report: $report_dir/index.html"
