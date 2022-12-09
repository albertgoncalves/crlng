#!/usr/bin/env bash

set -eu

for x in bin build; do
    if [ ! -d "$x" ]; then
        mkdir "$x"
    fi
done

flags_hs=(
    "-fdiagnostics-color=always"
    "-i$WD/src"
    "-outputdir $WD/build"
    -Wall
    -Wcompat
    -Werror
    -Widentities
    -Wincomplete-record-updates
    -Wincomplete-uni-patterns
    -Wmonomorphism-restriction
    -Wpartial-fields
    -Wredundant-constraints
    -Wunused-packages
    -Wunused-type-patterns
)
flags_c=(
    -c
    "-ferror-limit=1"
    -fshort-enums
    "-march=native"
    -O3
    "-std=c99"
    -Werror
    -Weverything
    -Wno-declaration-after-statement
    -Wno-padded
    -Wno-pointer-arith
)

clang-format -i -verbose "$WD/src/"runtime.c &
for x in "$WD/src"/*.hs; do
    (
        hlint "$x"
        ormolu -m inplace "$x"
    ) &
done

for _ in $(jobs -p); do
    wait -n
done

ghc "${flags_hs[@]}" -o "$WD/bin/com" "$WD/src/Main.hs" &
clang "${flags_c[@]}" -o "$WD/build/runtime_c.o" "$WD/src/runtime.c" &
fasm "$WD/src/runtime.asm" "$WD/build/runtime_asm.o" &

for _ in $(jobs -p); do
    wait -n
done
