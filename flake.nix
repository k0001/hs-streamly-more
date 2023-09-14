{
  description = "Haskell 'streamly-more' library";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/389cc28963163614765721eda940fd5299f18458";
    flake-parts.url = "github:hercules-ci/flake-parts";
    hs_streamly = {
      url =
        "github:composewell/streamly/13f91aad8c3d7cb3b81dd74599ecbfe38cfc6a6e";
      flake = false;
    };
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
                streamly-more = hsClean (hself.callPackage ./streamly-more { });

                streamly =
                  hself.callCabal2nix "streamly" inputs.hs_streamly { };
                streamly-core = hself.callCabal2nix "streamly-core"
                  "${inputs.hs_streamly}/core" { };
                unicode-data = hself.callHackage "unicode-data" "0.4.0.1" { };

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
          streamly-more__ghc962 = pkgs.haskell.packages.ghc962.streamly-more;
          default = pkgs.releaseTools.aggregate {
            name = "every output from this flake";
            constituents = [
              config.packages.streamly-more__ghc962
              config.packages.streamly-more__ghc962.doc
              config.devShells.ghc962
            ];
          };
        };
        devShells = let
          mkShellFor = ghc:
            ghc.shellFor {
              packages = p: [ p.streamly-more ];
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
