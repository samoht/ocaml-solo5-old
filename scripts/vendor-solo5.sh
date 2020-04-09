#!/bin/sh -eux

GIT_VERSION=$(git ls-remote https://github.com/solo5/solo5 master | cut -f1)
VERSION_H=solo5/include/solo5/solo5_version.h.distrib

rm -rf solo5
git clone https://github.com/solo5/solo5.git --depth 1 -b master solo5
rm -rf solo5/.git solo5/opam
find solo5 -name "dune*" -delete

cat <<EOM >${VERSION_H}
/* Automatically generated, do not edit */

#ifndef __VERSION_H__
#define __VERSION_H__

#define SOLO5_VERSION "${GIT_VERSION}"

#endif
EOM

git add -f ${VERSION_H}
