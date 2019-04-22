let
   pkgs = import <nixpkgs> { inherit config; };

   config = {
		packageOverrides = pkgs: rec {
         ghc = pkgs.callPackage "llvm";
			haskellPackages = pkgs.haskellPackages.override {
				overrides = haskellPackagesNew: haskellPackagesOld: rec {
					haskell-winrt-examples = haskellPackagesOld.dontHaddock haskellPackagesNew.callCabal2nix "haskell-winrt-examples" ./examples {};
					haskell-winrt-foundation = haskellPackagesNew.callCabal2nix "haskell-winrt-foundation" ./foundation {};
					haskell-winrt = haskellPackagesNew.callCabal2nix "haskell-winrt" ./library {};
					haskell-winrt-lowlevel = haskellPackagesOld.dontHaddock haskellPackagesNew.callCabal2nix "haskell-winrt-lowlevel" ./lowlevel {};
				};
			};
		};
   };


  wpkgs_base = import /home/tristan/src/webviewhs/nixpkgs;




   w64Pkgs = wpkgs_base { 
		inherit config; 
		crossSystem = (wpkgs_base {}).lib.systems.examples.mingwW64;
	};
in
	{ 
		haskell-winrt-examples = w64Pkgs.haskellPackages.haskell-winrt-examples;
	}
