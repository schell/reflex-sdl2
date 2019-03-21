# reflex-sdl2

![status](https://gitlab.com/schell/source-of-truth/badges/master/build.svg)

This is a [reflex][2] host for [sdl2][0].

Writing and running [sdl2][0] apps requires version 2 of the awesome [SDL][1]
library.


## building

The first step is to install SDL.

### macOS

Using [homebrew](https://brew.sh/)...

    brew install sdl2

### Ubuntu / Debian

The [sdl2 bindings][0] require SDL >= 2.0.4, or for special instructions
to be followed. Assuming you're on `Ubuntu >= 16.04`, you can run

    apt-get install libsdl2-dev

otherwise please visit the org page above and install via their README.

### building with stack

After installing SDL, we can build with stack.

Using [stack][3] is the path of least resistance.

    stack build

## examples

After building you can view an example using the built-in 2d renderer in
[app/Main.hs](https://github.com/schell/reflex-sdl2/blob/master/app/Main.hs).

To run the example do:

    stack exec reflex-sdl2-exe


[0]: http://hackage.haskell.org/package/sdl2 "sdl2 haskell bindings"
[1]: http://libsdl.org "Simple Direct Media Layer"
[2]: https://github.com/reflex-frp/reflex "reflex frp"
[3]: https://docs.haskellstack.org/en/stable/README/ "the haskell tool stack"
