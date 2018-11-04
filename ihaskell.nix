{
  nixpkgs ? import <nixpkgs> {}
}: 

# let conduit-dbf = with nixpkgs; with stdenv; with haskellPackages;
let
  inherit (nixpkgs) haskellPackages;
  conduit-dbf = import /home/pi/.config/nixpkgs/pkgs/conduit-dbf.nix {
    inherit (nixpkgs) stdenv zlib;
    inherit (haskellPackages) base binary bytestring conduit text
        conduit-combinators conduit-extra optparse-applicative mkDerivation; };
  dsfs = haskellPackages.callCabal2nix "data-science-from-scratch" ./. {};
    
in import ../IHaskell/release-8.4.nix {
  inherit nixpkgs;
  packages = haskellPackages: with haskellPackages; [
	  ihaskell-diagrams ihaskell-blaze ihaskell-charts csv Frames conduit-dbf
		  (builtins.trace "${dsfs}" dsfs)
  ];
}
