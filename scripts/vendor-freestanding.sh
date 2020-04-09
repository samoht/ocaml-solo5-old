#!/bin/sh -eux

rm -rf freestanding
git clone https://github.com/mirage/ocaml-freestanding.git --depth 1 -b master freestanding
rm -rf freestanding/.git
