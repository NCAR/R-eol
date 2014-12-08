# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsonde" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.


geoid_height <- function(lat, lon, name="egm96-5", cubic=TRUE)
{
    .Call("geoid", lat, lon, name, cubic, PACKAGE="eolsonde")
}
