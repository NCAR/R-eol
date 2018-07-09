# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolts" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

# to run this test by hand:
# library(RUnit)
# source("wherever/runit.conform.R")
# test.conform()

test.conform <- function()
{
    nr <- 4
    t0 <- utime("2018 Jun 21 00:00")
    tx <- utime(seq(as.numeric(t0), length=nr, by=1))

    # basic variable names with heights, no sites
    ncx <- 3
    cx <- paste0("P.", 1:ncx, "m")
    ux <- rep("m", ncx)
    x <- dat(nts(matrix(as.double(1:ncx), byrow=TRUE, nrow=nr, ncol=ncx), tx,
            units=ux, names=cx))

    ncy <- 5
    cy <- paste0("T.", 1:ncy, "m")
    uy <- rep("cm", ncy)
    y <- dat(nts(matrix(as.double(1:ncy), byrow=TRUE, nrow=nr, ncol=ncy), tx,
            units=uy, names=cy))

    cxc <- paste0("P.", 1:ncy, "m")

    cat("checking conform of",paste0(colnames(x),collapse=","),"to",paste0(colnames(y),collapse=","),"\n")
    # all columns of x should conform to y
    xx <- conform(x, y[,1:ncx])
    # cat("colnames(xx)=",paste(colnames(xx),collapse=","),"\n")
    checkIdentical(x, xx)

    xx <- conform(x, y)
    checkIdentical(dim(xx), dim(y))
    checkIdentical(x, xx[,1:ncx])
    checkTrue(all(sapply((ncx+1):ncy, function(i) { all(xx[,i] == x[,ncx]) } )))
    # cat("units(xx)=",paste(units(xx),collapse=","),"\n")
    checkIdentical(units(xx), rep(ux[1],ncy))

    # conforming x to one column of y should return one column of x
    for (ic in 1:ncx) {
        cat("checking conform of",paste0(colnames(x),collapse=","),"to",paste0(colnames(y[,ic]),collapse=","),"\n")
        # cat("ic=",ic,"\n")
        xx <- conform(x, y[,ic])
        checkIdentical(dim(xx), dim(y[,ic]))
        checkIdentical(x[,ic], xx)
    }

    # conforming x to y at a higher height should return x at highest
    for (ic in (ncx+1):ncy) {
        cat("checking conform of",paste0(colnames(x),collapse=","),"to",paste0(colnames(y[,ic]),collapse=","),"\n")
        # cat("ic=",ic,"\n")
        xx <- conform(x, y[,ic])
        checkIdentical(dim(xx), dim(y[,ic]))
        # cat("colnames(xx)=",paste(colnames(xx),collapse=","),"\n")
        checkTrue(all(xx[,1] == x[,ncx]))
        checkIdentical(colnames(xx), cxc[ic])
    }

    # x has site name, y doesn't. This should conform.
    # We're treating no site name like a wildcard, i.e. any site.
    # cat("sites(x)=", paste0(sites(x),","),"\n")
    colnames(x) <- paste0(cx, ".sx")
    # cat("colnames(x)=", paste0(colnames(x),","),"\n")
    cat("checking conform of",paste0(colnames(x),collapse=","),"to",paste0(colnames(y),collapse=","),"\n")
    xx <- conform(x, y)
    # cat("colnames(xx)=",paste(colnames(xx),collapse=","),"\n")
    checkIdentical(x, xx[,1:ncx])
    checkIdentical(colnames(xx),paste0(cxc, ".sx"))
    checkTrue(all(sapply(1:ncx, function(i) { all(xx[,i] == i) } )))
    checkTrue(all(sapply((ncx+1):ncy, function(i) { all(xx[,i] == x[,ncx]) } )))
    # cat("units(xx)=", paste0(units(xx),collapse=","),"\n")
    checkIdentical(units(xx), rep(ux[1],ncy))

    colnames(x) <- cx

    # y has site name, x doesn't. This should also conform.
    colnames(y) <- paste0(cy, ".sy")
    cat("checking conform of",paste0(colnames(x),collapse=","),"to",paste0(colnames(y),collapse=","),"\n")
    xx <- conform(x, y)
    # cat("colnames(xx)=",paste(colnames(xx),collapse=","),"\n")
    checkIdentical(colnames(xx),paste0(cxc, ".sy"))
    checkTrue(all(sapply(1:ncx, function(i) { all(xx[,i] == i) } )))
    checkTrue(all(sapply((ncx+1):ncy, function(i) { all(xx[,i] == x[,ncx]) } )))
    checkIdentical(units(xx), rep(ux[1],ncy))
    colnames(y) <- cy

    # Different site names. Should not conform.
    colnames(x) <- paste0(cx, ".sx")
    colnames(y) <- paste0(cy, ".sy")
    cat("checking conform of",paste0(colnames(x),collapse=","),"to",paste0(colnames(y),collapse=","),"\n")
    xx <- conform(x, y)
    # cat("colnames(xx)=",paste(colnames(xx),collapse=","),"\n")
    checkIdentical(paste0(cxc,".sy"), colnames(xx))
    checkTrue(all(is.na(xx)))
    checkIdentical(units(xx), rep(ux[1], ncy))

    # conform x at heights and sites 1,2,3 to y at heights and sites 2,4.
    # Should get two column result, consisting of x at 2 and NAs.
    cxx <- paste0(cx,paste0(".s",1:ncx))
    colnames(x) <- cxx
    cxy <- paste0(cy,paste0(".s",1:ncy))
    colnames(y) <- cxy
    cat("checking conform of",paste0(colnames(x),collapse=","),"to",paste0(colnames(y[,c(2,4)]),collapse=","),"\n")
    xx <- conform(x, y[,c(2,4)])
    # cat("colnames(xx)=",paste(colnames(xx),collapse=","),"\n")
    checkIdentical(dim(xx), dim(y[,c(2,4)]))
    checkIdentical(colnames(xx), c(colnames(x[,2]),paste0(cxc[4],".s4")))
    checkTrue(all(xx[,1] ==  2))
    checkTrue(all(is.na(xx[,2])))

    cx <- paste0("P.5m.s", 1:ncx)
    colnames(x) <- cx
    cy <- paste0("T.6m.s", 1:ncy)
    colnames(y) <- cy
    cat("checking conform of",paste0(colnames(x),collapse=","),"to",paste0(colnames(y),collapse=","),"\n")
    xx <- conform(x, y)
    # cat("colnames(xx)=",paste(colnames(xx),collapse=","),"\n")

    checkIdentical(dim(xx), dim(y))
    checkTrue(all(sapply(1:ncx, function(i) { all(xx[,i] == x[,i]) } )))
    checkIdentical(colnames(xx), paste0(words(cx[1],1,1),suffixes(cy)))
    checkTrue(all(is.na(xx[,(ncx+1):ncy])))
    # cat("units(xx)=",paste(units(xx),collapse=","),"\n")
    checkIdentical(units(xx), rep(ux[1],ncy))

    cx <- paste0("Rpile.in.s",1:ncx)
    colnames(x) <- cx
    cy <- paste0("Tcase.in.s",1:ncy)
    colnames(y) <- cy
    cat("checking conform of",paste0(colnames(x),collapse=","),"to",paste0(colnames(y),collapse=","),"\n")
    xx <- conform(x, y)
    # browser()
    # cat("colnames(xx)=",paste(colnames(xx),collapse=","),"\n")
    checkIdentical(dim(xx), dim(y))
    checkTrue(all(sapply(1:ncx, function(i) { all(xx[,i] == x[,i]) } )))
    checkIdentical(x, xx[,1:ncx])
    checkTrue(all(is.na(xx[,(ncx+1):ncy])))
    # cat("units(xx)=",paste(units(xx),collapse=","),"\n")
    checkIdentical(units(xx), rep(ux[1],ncy))

    return()
}
