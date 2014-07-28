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

    old.dpar <- dpar(start="2013 oct 13 14:00",lenhr=13)

    checkEquals(dpar("start"),utime("2013 oct 13 14:00"))
    checkEquals(dpar("end"),utime("2013 oct 14 03:00"))

    dpar(old.dpar)

    checkEquals(dpar("start"),old.dpar$start)
    checkEquals(dpar("lenhr"),old.dpar$lenhr)

    return()
}
