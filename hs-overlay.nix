{ pkgs }:

# To be used as `packageSetConfig` for a Haskell pacakge set:
self: super:
{
  pipes-binary = super.callPackage ./pkg.nix {};
  _shell = super.shellFor {
    withHoogle = false;
    buildInputs = [ pkgs.cabal-install ];
    packages = p: [ p.pipes-binary ];
  };
}
