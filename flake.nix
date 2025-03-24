{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    plutus.url = "github:input-output-hk/plutus/1.40.0.0";
    rust-overlay.url = "github:oxalica/rust-overlay";
    # cardano-node-flake.url = "github:input-output-hk/cardano-node/9.1.1";
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    , plutus
    , rust-overlay
    # , cardano-node-flake
    , ...
    } @ inputs:
    (flake-utils.lib.eachSystem [ "x86_64-darwin" "x86_64-linux" "aarch64-darwin" ]
      (system:
      let
        pkgs = import nixpkgs {
                inherit system;
                overlays = [ rust-overlay.overlays.default ];
        };
        uplc = plutus.cabalProject.${system}.hsPkgs.plutus-executables.components.exes.uplc;
        tiny_keccak_wrapper = pkgs.stdenv.mkDerivation {
            name = "tiny_keccak_wrapper";
            src = ./native/lib/tiny_keccak_wrapper;  # directory with Rust code
            nativeBuildInputs = [
                          pkgs.rustc
                          pkgs.cargo
                          pkgs.git               # Added git
                          pkgs.cacert           # Added SSL certificates
                          pkgs.pkg-config       # Often needed for Rust builds
                          pkgs.openssl          # Added OpenSSL
                        ];
            buildPhase = ''
             export CARGO_HOME=$PWD/.cargo
             cargo build --release
            '';
            installPhase = ''
             mkdir -p $out/lib
             cp target/release/libtiny_keccak_wrapper.a $out/lib/
            '';
        };
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
                llvm
                libsodium
                secp256k1
                pkgs.rustc
                pkgs.cargo
                tiny_keccak_wrapper
                # cardano-cli
              ];
              shellHook = ''
                ln -s ${plutus}/plutus-conformance plutus-conformance
                echo "${pkgs.secp256k1}"
                echo "${pkgs.libsodium}"
                echo "${tiny_keccak_wrapper}"
              '';
            };
          ci =
            let
              jdk = pkgs.openjdk11;
              sbt = pkgs.sbt.override { jre = jdk; };
            in
            pkgs.mkShell {
              JAVA_HOME = "${jdk}";
              JAVA_OPTS = "-Xmx4g -Xss512m -XX:+UseG1GC";
              CARGO_NET_GIT_FETCH_WITH_CLI = "true";
              CARGO_REGISTRIES_CRATES_IO_PROTOCOL = "sparse";
              packages = with pkgs; [
                jdk
                sbt
                scalafmt
                nodejs
                uplc
                llvm
                libsodium
                secp256k1
                pkgs.rustc
                pkgs.cargo
                tiny_keccak_wrapper
              ];
              shellHook = ''
                ln -s ${plutus}/plutus-conformance plutus-conformance
                export LIBRARY_PATH="${tiny_keccak_wrapper}/lib:${pkgs.secp256k1}/lib:${pkgs.libsodium}/lib:$LIBRARY_PATH"
                export LD_LIBRARY_PATH="${tiny_keccak_wrapper}/lib:${pkgs.secp256k1}/lib:${pkgs.libsodium}/lib:$LD_LIBRARY_PATH"
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
