# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolts" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

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

    packageStartupMessage("For help on the eolts package, do: \"package? eolts\"")

}
