MAKEFLAGS += --silent
FLAGS_HS = \
    -fdiagnostics-color=always \
    -isrc \
    -outputdir build \
    -Wall \
    -Wcompat \
    -Werror \
    -Widentities \
    -Wincomplete-record-updates \
    -Wincomplete-uni-patterns \
    -Wmonomorphism-restriction \
    -Wpartial-fields \
    -Wredundant-constraints \
    -Wunused-packages \
    -Wunused-type-patterns
FLAGS_C = \
    -c \
    -D_GNU_SOURCE \
    -ferror-limit=1 \
    -fshort-enums \
    -march=native \
    -O3 \
    -std=c99 \
    -Werror \
    -Weverything \
    -Wno-declaration-after-statement \
    -Wno-disabled-macro-expansion \
    -Wno-padded \
    -Wno-pointer-arith \
	-Wno-unsafe-buffer-usage
MODULES = \
	Ast \
	Compile \
	Main \
	Parse
LINTS = $(foreach x,$(MODULES),build/$(x).lint)

.PHONY: all
all: bin/com build/runtime_c.o build/runtime_asm.o

.PHONY: clean
clean:
	rm -rf bin/
	rm -rf build/

.PHONY: test
test: all
	./scripts/test.py

$(LINTS): build/%.lint: src/%.hs
	mkdir -p build/
	hlint $^
	ormolu -i --no-cabal $^
	touch $@

bin/com: $(LINTS)
	mkdir -p bin/
	ghc $(FLAGS_HS) -o bin/com src/Main.hs

build/runtime_c.o: src/runtime.c
	mkdir -p build/
	clang-format -i src/runtime.c
	clang $(FLAGS_C) -o build/runtime_c.o src/runtime.c

build/runtime_asm.o: src/runtime.asm
	mkdir -p build/
	fasm src/runtime.asm build/runtime_asm.o
