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

    # Append our dpar options to the list in .eoltsEnv.
    if (exists(".eoltsEnv",envir=.getNamespace("eolts")) &&
        exists(".dparDefaults",envir=eolts:::.eoltsEnv)) {
        dpardefs <- c(get(".dparDefaults",envir=eolts:::.eoltsEnv),.dparDefaults)
        assign(".dparDefaults",dpardefs,envir=eolts:::.eoltsEnv)
    }
    packageStartupMessage("For help on the isfs package, do: \"package? isfs\"")
}
