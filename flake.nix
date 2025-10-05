{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: {
      devShells.default = with nixpkgs.legacyPackages.${system}; mkShell {
        buildInputs = [ ghc cabal-install haskell-language-server ];
      };
    });
}