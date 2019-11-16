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
    tmp = tempfile("test.plot",fileext=".png")
    on.exit(unlink(tmp))

    tx <- utime("now")
    len <- 100
    x = dat(nts(seq(from=0.01,to=100,len=len),
            utime(seq(as.numeric(tx),by=1,length=len)),names="test"))

    png(file=tmp)
    plot(x)
    dev.off()

    # check for crashes when doing log plots. If the scale is
    # expanded incorrectly you will get errors about taking a log of
    # a non-positive number.
    png(file=tmp)
    plot(x,log="y")
    dev.off()

    # test dat stack plots
    png(file=tmp)
    par(mfrow=c(3,1))
    plot(x)
    plot(x)
    plot(x)
    dev.off()

    return()
}
