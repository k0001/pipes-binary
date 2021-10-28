{ nixpkgs ? import ./nixpkgs.nix
}:

let
pkgs = import nixpkgs {};
ghc901 = pkgs.haskell.packages.ghc901.override {
  packageSetConfig = import ./hs-overlay.nix { inherit pkgs; };
};

in { inherit (ghc901) pipes-binary _shell; }
