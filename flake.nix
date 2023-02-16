{
  description = "conduit-parse flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/0591d6b57bfeb55dfeec99a671843337bc2c3323";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        # `nix develop`
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs.haskellPackages;
            [ cabal-install cabal-plan ghcid haskell-language-server hlint ormolu pkgs.zlib ];
        };
      });
}
