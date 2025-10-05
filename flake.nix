{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/50a7139fbd1acd4a3d4cfa695e694c529dd26f3a";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: with nixpkgs.legacyPackages.${system}; {
      devShells.default = mkShell {
        buildInputs = [ ghc cabal-install haskell-language-server ];
      };
    });
}