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

## building

### with Stack

This project uses [stack][3] for building. From the project directory:

    stack build

### with Nix

You can also build this using project using Nix[4].

There are two ways to proceed.

If you're not a Nix user, the best way to get started would be to clone the [reflex-platform][5] somewhere:

    git clone https://github.com/reflex-frp/reflex-platform

After that you can ues the work-on script from the project directory to open a shell with an environment set up for developing this project:

    PATH_TO_REFLEX_PLATFORM/work-on ghc ./reflex-sdl2.nix

If you're already using Nix, then you can open the same shell with:

    nix-shell

In either case, once you are in that shell you can build the project with:

    cabal build

## examples

You can view an example using the built-in 2d renderer in
[app/Main.hs](https://github.com/schell/reflex-sdl2/blob/master/app/Main.hs).
To run the example do:

    stack build
    stack exec reflex-sdl2-exe

or from a Nix shell:

    cabal run reflex-sdl2-exe


[0]: http://hackage.haskell.org/package/sdl2 "sdl2 haskell bindings"
[1]: http://libsdl.org "Simple Direct Media Layer"
[2]: https://github.com/reflex-frp/reflex "reflex frp"
[3]: https://docs.haskellstack.org/en/stable/README/ "the haskell tool stack"
[4]: https://nixos.org "Nix"
[5]: https://github.com/reflex-frp/reflex-platform "the reflex platform"
