# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolts" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

# Empty environment in the eolts namespace for adding session variables.
# This is not in the global environment, and so is
# not saved on exit. Currently it is not exported, and so
# isn't in the package:eolts environment.
# .getNamespace("eolts")
# ls(all=TRUE,.getNamespace("eolts"))
# get(".eoltsEnv",envir=.getNamespace("eolts"))
# <environment: 0x54fec50>
# eolts:::.eoltsEnv
# <environment: 0x54fec50>
.eoltsEnv <- new.env(parent=emptyenv())

.onAttach = function(libname,pkgname)
{
    # cat(paste("hello from libname=",libname,", pkgname=",pkgname," .onAttach\n"))
    time.zone = Sys.timezone()
    options(time.in.format=c(
            "%Y%m%d%H:%M:%OS",      # numeric month
            "%Y%m%d%H:%M",
            "%Y%m%d%H%M%OS",

            "%Y %b%d%H:%M:%OS",    # alpha month
            "%Y %b%d%H:%M",
            "%Y %b%d%H%M%OS"),
            time.out.format="%Y %m %d %H:%M:%OS3 %Z",
            time.zone=time.zone
    )

    assign(".dparParams",.dparParams,envir=.eoltsEnv)

    packageStartupMessage("For help on the eolts package, do: \"? eolts\"")

}
