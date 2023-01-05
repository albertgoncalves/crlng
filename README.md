# crlng

Dependencies
---
 - [Clang](https://clang.llvm.org/)
```console
$ clang --version
clang version 14.0.6
Target: x86_64-pc-linux-gnu
Thread model: posix
InstalledDir: /usr/bin
```
 - [flat assembler](https://flatassembler.net/)
```console
$ fasm
flat assembler  version 1.73.30
```
 - [Nix](https://nixos.org/download.html)
```console
$ nix --version
nix (Nix) 2.11.1
```

Quick start
---
```console
$ nix-shell
[nix-shell:path/to/crlng]$ ./scripts/build.sh
[nix-shell:path/to/crlng]$ ./scripts/test.py
[nix-shell:path/to/crlng]$ cat > program.crl << EOF
> fib n a b {
>     if (= n 0) {
>         a
>     } else {
>         (fib (- n 1) b (+ a b))
>     }
> }
>
> main {
>     (printf "%lu\n" (fib 50 0 1))
> }
> EOF
[nix-shell:path/to/crlng]$ ./scripts/run.sh program.crl
fib n a b {
    if (= n 0) {
        a
    } else {
        (/* tail-call */ fib (- n 1) b (+ a b))
    }
}

main_thread {
    (/* tail-call */ printf "%lu\n" (fib 50 0 1))
}

12586269025
```
