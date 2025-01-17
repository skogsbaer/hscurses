# hscurses -- A Haskell Binding to ncurses

`hscurses` is a Haskell binding to the ncurses library, a library of functions
that manage an application's display on character-cell terminals. `hscurses`
also provides some basic widgets implemented on top of the `ncurses` binding,
such as a text input widget and a table widget.

The `hscurses` library has been reported to work on Linux x86 using GHC 6.12.1,
7.0.3, 7.2.1, and GHC major versions starting from GHC 9.4. See the GitHub
actions for GHC versions that we test against.

## Building and installing the `hscurses` library with GHC (old)

Requirements:

- GNU `m4`
- GHC >= 6.8
- `ncurses` with headers

Build and installation steps:

    runhaskell Setup.hs configure
    runhaskell Setup.hs build
    runhaskell Setup.hs install

In order to generate the API documentation, you need haddock
(http://www.haskell.org/haddock). The command is then:

    runhaskell Setup.hs haddock

## Building the `hscurses` library with GHC and `cabal-install`

Requirements:

- `cabal-install` + GHC installation
- `ncurses` with headers

Build:

    cabal build

The repository contains a couple of example and testing applications in the
[example](./example) and [tests](./tests) directories. To run them, you have to
enable the `examples` Cabal flag; for instance:

    cabal run -f examples widget-test-edit

or:

    cabal run -f examples contact-manager -- example/contacts

In order to generate the API documentation, you need
[haddock](http://www.haskell.org/haddock). The command is then:

    cabal haddock

## Using the `hscurses` library

Just add the package `hscurses` to your dependencies and make sure you have an
installation of `ncurses`. At the moment, user documentation is only available
through Haddock API documentation.

For basic usage check out the code in the [example](./example) and
[./tests](tests) directories.

## Windows support

Windows support relies on [pdcurses](http://pdcurses.sourceforge.net/), which is
already [packaged for MinGW](http://sourceforge.net/projects/mingw/files/MinGW/PDCurses/PDCurses-3.4-1/)
and can be installed with `mingw-get`.

Thanks to José Romildo Malaquias (malaquias@gmail.com) for porting `hscurses` to
the Windows platform!

There is also a detailed installation instruction by
[Ilan Godik](https://medium.com/@NightRa):
https://medium.com/@NightRa/installing-hscurses-on-windows-830532d3268a

## Copyright

    John Meacham <john @ repetae . net>, 2002-2004.
    Tuomo Valkonen <tuomov @ iki.fi>, 2004.
    Don Stewart <http://www.cse.unsw.edu.au/~dons>, 2004.
    Stefan Wehr <http://www.stefanwehr.de>, 2004-2011.

## History

John Meacham started the binding for his chat client
[Ginsu](http://repetae.net/john/computer/ginsu/).

Tuomo Valkonen integrated to code into [Riot](http://modeemi.fi/~tuomov/riot/),
with minor modifications.

Don Stewart improved the code for the [Yi editor](http://www.cse.unsw.edu.au/~dons/yi.html).

Stefan Wehr turned the binding into a standalone library. He also added some
basic widgets.
