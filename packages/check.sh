#!/bin/sh

# build package tar ball
R --vanilla CMD build eolts || exit 1

R --vanilla CMD check eolts_[0-9]*.[0-9]*-[0-9].tar.gz || exit 1


R --vanilla CMD build isfs || exit 1

R --vanilla CMD check isfs_[0-9]*.[0-9]*-[0-9].tar.gz || exit 1
