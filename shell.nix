{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, plutus ? import sources.plutus { }
}:
pkgs.mkShell {
  packages = with pkgs; [
    git
    openjdk11
    sbt
    scalafmt
    niv
    nixpkgs-fmt
    nodejs
    plutus.plutus.haskell.projectPackages.plutus-core.components.exes.uplc
  ];
  shellHook = ''
    ln -s ${sources.plutus}/plutus-conformance plutus-conformance
  '';
}
