# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolts" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

setup_test <- function(pkg="isfs")
{
    options(time.zone="US/Mountain")
    dpar(start="2016 sep 11 19:17:30",lenmin=50)

    find_datasets(path=system.file("unitTests", package=pkg),pattern="netcdf")

    load(file.path(system.file("unitTests", package=pkg),
            "RData","Vazimuth.RData"), envir=.GlobalEnv)

}

test.plot <- function()
{
    if (length(datasets(warn=FALSE)) == 0) setup_test()

    dataset("noqc_instrument")

    stns <- 2:3
    dpar(stns=stns)

    x <- dat("rhoAir")

    tmp = tempfile("test.plot",fileext=".png")
    on.exit(unlink(tmp))
    png(file=tmp)
    plot(x)
    dev.off()

    png(file=tmp)
    plot(x,log="y")
    dev.off()

    x[1,] = 0.001
    x[2,] = 99.
    png(file=tmp)
    plot(x,log="y")
    dev.off()

    return()
}
