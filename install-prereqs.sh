#! /bin/sh

if [ -x /usr/bin/apt ]; then
    sudo apt install ghc cabal-install
fi
cabal update
cabal install HUnit process
