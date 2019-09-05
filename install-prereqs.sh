#! /bin/sh

if [ -x /usr/bin/apt ]; then
    sudo apt install cabal-install
fi
cabal update
cabal install HUnit
