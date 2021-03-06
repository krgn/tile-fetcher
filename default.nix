# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

with haskellPackages; cabal.mkDerivation (self: {
  pname = "tile-fetcher";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ HTTP ];
  buildTools = [ cabalInstall ];
  meta = {
    description = "fetches osm tiles";
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
