# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsonde" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

test_sdngs <- function()
{

    wd <- getwd()
    # cat("wd=",wd,"\n")
    pkg <- sub("\\.Rcheck$", '', basename(dirname(wd)))
    # cat("pkg=",pkg,"\n")

    datadir <- file.path(system.file("unitTests", package=pkg),"data","D-file")
    # cat("datadir=",datadir,"\n")
    Sys.setenv(SONDE_DATA=datadir)
    Sys.setenv(SONDE_ROOT="/x")

    options(time.zone="UTC")
    dpar(start="2008 8 15 1727",lenmin=12)

    sdngs <- readSoundings()

    checkEquals(length(sdngs),2)

    datadir <- file.path(system.file("unitTests", package=pkg),"data","qc.eol")
    # cat("datadir=",datadir,"\n")
    Sys.setenv(SONDE_DATA=datadir)

    sdngs <- readSoundings()

    checkEquals(length(sdngs),2)
}
