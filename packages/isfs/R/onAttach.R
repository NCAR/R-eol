# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

.onAttach = function(libname,pkgname)
{
    # cat(paste("hello from libname=",libname,", pkgname=",pkgname," .onAttach\n"))
    options(dcache=TRUE)
    packageStartupMessage("For help on the isfs package, do: \"package? isfs\"")
}
