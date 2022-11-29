with import <nixpkgs> {};
mkShell {
    buildInputs = [
        ghc
        hlint
        mold
        ormolu
        shellcheck
    ];
    shellHook = ''
        . .shellhook
    '';
}
