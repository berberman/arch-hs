{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs, ... }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ self.overlays.default ];
      };
    in with pkgs; {
      overlays.default = final: prev:
        let
          hpkgs = prev.haskellPackages.override {
            overrides = hself: hsuper: {
              arch-web = prev.haskell.lib.dontCheck
                (hself.callCabal2nix "arch-web" (builtins.fetchTarball {
                  url =
                    "https://hackage.haskell.org/package/arch-web-0.2/arch-web-0.2.tar.gz";
                  sha256 =
                    "0306ky9a4rfc4lz838kykl91br6s7sm9dkp446425g3yjh6x26zl";
                }) { });
            };
          };
          arch-hs = hpkgs.callCabal2nix "arch-hs" ./. { };
        in with final;
        with haskell.lib; {
          inherit arch-hs;
          arch-hs-dev =
            addBuildTools arch-hs [ cabal-install haskell-language-server ];
        };
      packages.x86_64-linux.default = arch-hs;
      devShells.x86_64-linux.default = arch-hs-dev.envFunc { };
    };
}
