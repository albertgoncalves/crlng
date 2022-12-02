#!/usr/bin/env bash

set -eu

"$WD/bin/com" "$1" "$WD/build/main.asm"
fasm "$WD/build/main.asm" "$WD/build/main_asm.o" > /dev/null
mold -run clang -no-pie -o "$WD/bin/run" "$WD/build/runtime_c.o" \
    "$WD/build/main_asm.o" "$WD/build/runtime_asm.o"
"$WD/bin/run" || echo "$?"
