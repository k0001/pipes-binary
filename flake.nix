{
  description = "Haskell 'pipes-binary' library";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/389cc28963163614765721eda940fd5299f18458";
    flake-parts.url = "github:hercules-ci/flake-parts";

    hs_bsb-http-chunked = {
      url =
        "github:sjakobi/bsb-http-chunked/c0ecd72fe2beb1cf7de9340cc8b4a31045460532";
      flake = false;
    };
  };

  outputs = inputs@{ ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      flake.overlays.default = final: prev:

        let
          hsLib = prev.haskell.lib;
          hsClean = drv:
            hsLib.overrideCabal drv
            (old: { src = prev.lib.sources.cleanSource old.src; });
        in {
          haskell = prev.haskell // {
            packageOverrides = prev.lib.composeExtensions
              (prev.haskell.packageOverrides or (_: _: { })) (hself: hsuper: {
                pipes-binary = hsClean (hself.callPackage ./. { });

                # hoogle stuff
                bsb-http-chunked = hself.callCabal2nix "bsb-http-chunked"
                  inputs.hs_bsb-http-chunked { };
                warp = hsLib.dontCheck (hself.callHackage "warp" "3.3.25" { });
                warp-tls =
                  hsLib.dontCheck (hself.callHackage "warp-tls" "3.3.6" { });
                recv = hself.callHackage "recv" "0.1.0" { };
              });
          };
        };
      systems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      perSystem = { config, pkgs, system, ... }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ inputs.self.overlays.default ];
        };
        packages = {
          pipes-binary__ghc962 = pkgs.haskell.packages.ghc962.pipes-binary;
          default = pkgs.releaseTools.aggregate {
            name = "every output from this flake";
            constituents = [
              config.packages.pipes-binary__ghc962
              config.packages.pipes-binary__ghc962.doc
              config.devShells.ghc962
            ];
          };
        };
        devShells = let
          mkShellFor = ghc:
            ghc.shellFor {
              packages = p: [ p.pipes-binary ];
              withHoogle = true;
              nativeBuildInputs =
                [ pkgs.cabal-install pkgs.cabal2nix pkgs.ghcid ];
            };
        in {
          default = config.devShells.ghc962;
          ghc962 = mkShellFor pkgs.haskell.packages.ghc962;
        };
      };
    };
}
