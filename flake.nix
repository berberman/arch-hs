{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs, ... }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ self.overlay ];
      };
    in with pkgs; {
      overlay = self: super:
        let
          hpkgs = super.haskellPackages;
          arch-hs = hpkgs.callCabal2nix "arch-hs" ./. { };
        in with super;
        with haskell.lib; {
          inherit arch-hs;
          arch-hs-dev =
            addBuildTools arch-hs [ haskell-language-server cabal-install ];
        };
      defaultPackage.x86_64-linux = arch-hs;
      devShell.x86_64-linux = arch-hs-dev.envFunc { };
    };
}
