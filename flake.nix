{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # Build the Haskell core (library + foreign-library) from the Cabal file.
        piDslHs = pkgs.haskellPackages.callCabal2nix "pi-dsl" self {};

        # Build the Python package and bundle the produced shared library into it.
        piDsl = pkgs.python3Packages.buildPythonPackage {
          pname = "pi_dsl";
          version = "0.1.0";
          src = self;

          pyproject = true;

          nativeBuildInputs = with pkgs.python3Packages; [ pkgs.pyright setuptools wheel ];
          buildInputs = [ piDslHs ];

          # Bundle the shared library into the wheel by copying it into the Python package source tree before the wheel is built.
          postPatch = ''
            lib="$(find ${piDslHs} -name 'libpi-dsl-shared-lib.so' -print -quit)"
            mkdir -p pi_dsl/_native && cp -v "$lib" pi_dsl/_native
          '';

          pythonImportsCheck = [ "pi_dsl" ];
          pythonImportsCheckPhase = ''
            export PYTHONDONTWRITEBYTECODE=1
            export PYTHONPATH="$out/${pkgs.python3.sitePackages}:$PYTHONPATH"
            python -c "import pi_dsl"

            ${piDslHs}/bin/bindgen | diff -u pi_dsl/bindings.py -

            pyright pi_dsl/*
            python test/*
          '';
        };
      in {
        packages.default = piDsl;

        devShells.default = pkgs.mkShell {
          packages = with pkgs;
            [ ghc cabal-install haskell-language-server pyright ];

          PYTHONDONTWRITEBYTECODE = 1;

          shellHook = ''
            toplevel="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
            export PYTHONPATH="$toplevel:$PYTHONPATH"
          '';
        };
      });
}
