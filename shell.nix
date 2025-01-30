{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
    buildInputs = [
        pkgs.python3
        pkgs.nodejs
        pkgs.zlib
        pkgs.gmp
        pkgs.pkg-config
        pkgs.openssl
                pkgs.libffi
    ];
}