#!/bin/sh

# build package tar ball
R CMD build eolts

R CMD check eolts_[0-9]*.[0-9]*-[0-9].tar.gz


