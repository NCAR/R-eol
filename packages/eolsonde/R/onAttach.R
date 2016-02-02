# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsonde" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

.onAttach = function(libname,pkgname)
{
    # Append our dpar options to the list in .eoltsEnv.
    if (exists(".eoltsEnv",envir=.getNamespace("eolts")) &&
        exists(".dparParams",envir=eolts:::.eoltsEnv)) {

        dpardefs <- c(get(".dparParams",envir=eolts:::.eoltsEnv),.dparParams)
        assign(".dparParams",dpardefs,envir=eolts:::.eoltsEnv)
        # remove cached .dpar, so that the next call sets if from the above
        if (exists(".dpar",envir=eolts:::.eoltsEnv))
            remove(".dpar",envir=eolts:::.eoltsEnv)
    }
    packageStartupMessage("For help on the eolsonde package, do: \"? eolsonde\"")
}
