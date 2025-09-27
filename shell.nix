let revision = "50a7139fbd1acd4a3d4cfa695e694c529dd26f3a";
    url = "https://github.com/NixOS/nixpkgs/archive/${revision}.tar.gz";
    nixpkgs = builtins.fetchTarball { inherit url; };
in with import nixpkgs {};
mkShell {
  buildInputs = [ ghc haskell-language-server ];
}
