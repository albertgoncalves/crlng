with import <nixpkgs> {};
mkShell {
    buildInputs = [
        ghc
        hlint
        ormolu
        shellcheck
    ];
    shellHook = ''
        . .shellhook
    '';
}
