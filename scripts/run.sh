#!/usr/bin/env bash

set -eu

"$WD/bin/com" "$1" "$WD/build/main.asm"
fasm "$WD/build/main.asm" "$WD/build/main_asm.o" > /dev/null
mold -o "$WD/bin/run" -L "/usr/lib/" -lc \
    -dynamic-linker "/usr/lib64/ld-linux-x86-64.so.2" \
    "$WD/build/runtime_c.o" "$WD/build/main_asm.o" "$WD/build/runtime_asm.o"
"$WD/bin/run"
