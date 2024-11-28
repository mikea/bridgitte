{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
    hardeningDisable = [ "fortify" ];
    nativeBuildInputs = with pkgs.buildPackages; [ 
        # tools
        watchexec just valgrind
        # rust
        rustup
        # to visualize traces
        graphviz
    ];
}