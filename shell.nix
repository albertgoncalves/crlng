with import <nixpkgs> {};
mkShell {
    buildInputs = [
        ghc
        hlint
        mold
        ormolu
        python3Packages.flake8
        shellcheck
    ];
    shellHook = ''
        . .shellhook
    '';
}
