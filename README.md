# reflex-sdl2
This is a very minimal host for running [reflex][2] apps using [sdl2][0].

## installation
Writing and running [sdl2][0] apps requires version 2 of the awesome [SDL][1]
library.

### macOS

Using [homebrew](https://brew.sh/)...

    brew install sdl2

### Ubuntu

The [sdl2 bindings][0] require SDL >= 2.0.4, or for special instructions
to be followed. Assuming you're on `Ubuntu >= 16.04`, you can run

    apt-get install libsdl2-dev

otherwise please visit the org page above and install via their README.

## examples

You can view an example using the built-in 2d renderer in [app/Main.hs]().


[0]: http://hackage.haskell.org/package/sdl2 "sdl2 Haskell bindings"
[1]: http://libsdl.org "Simple Direct Media Layer"
[2]: https://github.com/reflex-frp/reflex "reflex frp"
