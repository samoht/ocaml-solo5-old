#!/bin/sh -eux

CC=$1
shift
CFLAGS=$@

make CC="${CC}" CFLAGS="${CFLAGS}" libopenlibm.a
