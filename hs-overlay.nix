{ pkgs }:

# To be used as `packageSetConfig` for a Haskell pacakge set:
self: super:
{
  pipes-group = pkgs.haskell.lib.doJailbreak super.pipes-group;
  pipes-binary = super.callPackage ./pkg.nix {};
}
