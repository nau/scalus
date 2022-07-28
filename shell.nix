{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
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
  ];
  shellHook = "";
}
