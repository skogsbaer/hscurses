#!/bin/sh

GHC62=/usr/local/ghc6.2/bin/ghc
GHC64=/usr/bin/ghc

TOPDIR=../..
THISDIR=tests/dist-test

cleans="Test.hi Test.o"

function run() {
    hc=$1
    res=res${hc//\//_}

    if [ ! -x "$hc" ] ; then
        echo "not a valid haskell compiler: $hc"
        exit 1
    fi

    echo
    echo "==================================================="
    echo "running test with $hc"
    echo "==================================================="
    echo

    cd $TOPDIR
    make distclean && autoreconf &&./configure --with-hc=$hc && make
    if [ $? -ne 0 ] ; then
        echo "Test failed"
        exit 1
    fi

    cd $THISDIR
    $hc --make -o $res -package-conf $TOPDIR/hscurses.conf -package hscurses Test.hs
    if [ $? -ne 0 ] ; then
        echo "Test failed"
        rm "$res" $cleans
        exit 1
    fi

    rm $cleans
}

run $GHC62
run $GHC64


echo
echo "==================================================="
echo "please run the produced binaries by hand"
echo "==================================================="
echo
