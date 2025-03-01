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
            overrides = hself: hsuper: {};
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
