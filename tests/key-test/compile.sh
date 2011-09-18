#!/bin/sh

TOP=$(cd $(dirname $0)/../..; pwd -P)
v=$(gawk '/^Version:/ { print $2 }' $TOP/hscurses.cabal)

hscurses_pkg="hscurses-$v"
echo "Using $hscurses_pkg"

ghc --make -o key-test -package "$hscurses_pkg" -outputdir build KeyTest.hs
