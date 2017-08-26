{ mkDerivation, base, dependent-sum, mtl, ref-tf, reflex, sdl2
, stdenv
}:
mkDerivation {
  pname = "reflex-sdl2";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base dependent-sum mtl ref-tf reflex sdl2
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/schell/reflex-sdl2#readme";
  description = "SDL2 and reflex FRP";
  license = stdenv.lib.licenses.mit;
}
