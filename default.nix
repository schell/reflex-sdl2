{ reflex-platform ? import ./reflex-platform.nix }:
reflex-platform.ghc.callPackage ./reflex-sdl2.nix {}

