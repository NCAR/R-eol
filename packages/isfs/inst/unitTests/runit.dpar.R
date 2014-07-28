# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolts" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

test.dpar <- function()
{

    stns <- c(0,5:7)
    hts <- c(9,18)
    sites <- c("a","b")
    old.dpar <- dpar(stns=stns,hts=hts,sites=sites)

    checkEquals(dpar("stns"),stns)
    checkEquals(dpar("hts"),hts)
    checkEquals(dpar("sites"),sites)

    dpar(old.dpar)

    checkEquals(dpar("stns"),old.dpar$stns)
    checkEquals(dpar("hts"),old.dpar$hts)
    checkEquals(dpar("sites"),old.dpar$sites)

    return()
}
