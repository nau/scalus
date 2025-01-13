{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    plutus.url = "github:input-output-hk/plutus/1.30.0.0";
    # cardano-node-flake.url = "github:input-output-hk/cardano-node/9.1.1";
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    , plutus
    # , cardano-node-flake
    , ...
    } @ inputs:
    (flake-utils.lib.eachSystem [ "x86_64-darwin" "x86_64-linux" "aarch64-darwin" ]
      (system:
      let
        pkgs = import nixpkgs { inherit system; };
        uplc = plutus.cabalProject.${system}.hsPkgs.plutus-core.components.exes.uplc;
        # cardano-cli = cardano-node-flake.packages.${system}.cardano-cli;
      in
      {
        devShells = {
          default =
            let
              jdk = pkgs.openjdk21;
              sbt = pkgs.sbt.override { jre = jdk; };
            in
            pkgs.mkShell {
              JAVA_OPTS = "-Xmx4g -Xss512m -XX:+UseG1GC";
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
                yarn
                uplc
                async-profiler
                # cardano-cli
              ];
              shellHook = ''
                ln -s ${plutus}/plutus-conformance plutus-conformance
              '';
            };
          ci =
            let
              jdk = pkgs.openjdk11;
              sbt = pkgs.sbt.override { jre = jdk; };
            in
            pkgs.mkShell {
              JAVA_HOME = "${jdk}";
              JAVA_OPTS = "-Xmx4g -XX:+UseG1GC";
              packages = with pkgs; [
                jdk
                sbt
                scalafmt
                nodejs
                uplc
              ];
              shellHook = ''
                ln -s ${plutus}/plutus-conformance plutus-conformance
                export SBT_OPTS="-Xss64m $SBT_OPTS"
              '';
            };
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
