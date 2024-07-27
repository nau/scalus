{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    plutus.url = "github:input-output-hk/plutus/1.30.0.0";
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    , plutus
    , ...
    } @ inputs:
    (flake-utils.lib.eachSystem [ "x86_64-darwin" "x86_64-linux" "aarch64-darwin" ]
      (system:
      let
        pkgs = import nixpkgs { inherit system; };
        uplc = plutus.cabalProject.${system}.hsPkgs.plutus-core.components.exes.uplc;
      in
      rec {
        devShell = pkgs.mkShell {
          JAVA_OPTS="-Xmx4g -XX:+UseG1GC";
          # This fixes bash prompt/autocomplete issues with subshells (i.e. in VSCode) under `nix develop`/direnv
          buildInputs = [ pkgs.bashInteractive ];
          packages = with pkgs; [
            git
            openjdk21
            sbt
            mill
            scalafmt
            niv
            nixpkgs-fmt
            nodejs
            uplc
          ];
          shellHook = ''
             ln -s ${plutus}/plutus-conformance plutus-conformance
          '';
        };
      })
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
}
