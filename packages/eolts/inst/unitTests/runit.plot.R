# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolts" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

test.plot <- function()
{
    checkEquals(length(eolts:::xlabel.deltas), length(eolts:::xlabel.tltype))
    checkEquals(length(eolts:::xlabel.deltas), length(eolts:::xlabel.nminor))
    checkEquals(length(eolts:::xlabel.deltas), length(eolts:::xlabel.minunits))
    checkEquals(max(eolts:::xlabel.tltype), length(eolts:::xlabel.tformats))
    checkEquals(max(eolts:::xlabel.tltype), length(eolts:::xlabel.majunits))
    checkEquals(max(eolts:::xlabel.tltype), length(eolts:::xlabel.extraformats))

    t1 <- utime("2019 feb 13 00:00")
    nrow <- 5
    ncol <- 2
    dx <- matrix(1:(nrow*ncol),nrow=nrow,dimnames=list(NULL, paste("c",1:ncol)))

    plen <- 20*365*86400

    tmp = tempfile("test.plot",fileext=".png")
    on.exit(unlink(tmp))

    while (plen > .001) {
        x <- nts(dx,utime(seq(from=t1,to=t1+plen,length=nrow)))
        png(file=tmp)
        plot(x)
        dev.off()
        plen <- plen / 2
    }

    return()
}
