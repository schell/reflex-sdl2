# reflex-sdl2

This is a [reflex][2] host for [sdl2][0].

Writing and running [sdl2][0] apps requires version 2 of the awesome [SDL][1]
library. If you have [nix][4] then the build process will install SDL2 for you.


## building

### with stack & nix

Using [stack][3] and [nix][4] together is the path of enlightenment. [nix][4]
handles installing SDL2 while [stack][3] builds the haskell deps.

    stack --nix build

### without nix

Without [nix][4] you'll have to install SDL2 yourself.

### macOS

Using [homebrew](https://brew.sh/)...

    brew install sdl2

### Ubuntu / Debian

The [sdl2 bindings][0] require SDL >= 2.0.4, or for special instructions
to be followed. Assuming you're on `Ubuntu >= 16.04`, you can run

    apt-get install libsdl2-dev

otherwise please visit the org page above and install via their README.


## examples

After building you can view an example using the built-in 2d renderer in
[app/Main.hs](https://github.com/schell/reflex-sdl2/blob/master/app/Main.hs).

To run the example do:

    stack exec reflex-sdl2-exe


[0]: http://hackage.haskell.org/package/sdl2 "sdl2 haskell bindings"
[1]: http://libsdl.org "Simple Direct Media Layer"
[2]: https://github.com/reflex-frp/reflex "reflex frp"
[3]: https://docs.haskellstack.org/en/stable/README/ "the haskell tool stack"
[4]: https://nixos.org "Nix"
[5]: https://github.com/reflex-frp/reflex-platform "the reflex platform"
