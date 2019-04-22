{ mkDerivation, base, haskell-winrt, haskell-winrt-foundation
, stdenv, Win32
}:
mkDerivation {
  pname = "haskell-winrt-examples";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base haskell-winrt haskell-winrt-foundation Win32
  ];
  homepage = "http://github.com/tmcl/haskell-winrt";
  description = "Examples for Haskell WinRT";
  license = stdenv.lib.licenses.bsd3;
}
