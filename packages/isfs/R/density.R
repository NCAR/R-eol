# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.
#
# --------------
dat.rhoAir <- function(what,derived=TRUE,cache=F,...)
{
    # compute density of moist air (kg/m^3)

    datvar <- datVar() # requested variable name, x of dat.x

    robust <- dpar("robust")
    R <- 287		# specific gas constant for dry air: J/kg-K
    P = dat("P")
    # convert to pascals
    if (any(units(P) == "mb")) P[,units(P)=="mb"] = P[,units(P)=="mb"] * 100
    if (any(units(P) == "kPa")) P[,units(P)=="kPa"] = P[,units(P)=="kPa"] * 1000
    q <- dat(sub(datvar,"Q",what,fixed=TRUE))
    Td <- dat(sub(datvar,"T",what,fixed=TRUE))
    Td <- conform(Td,q)
    Tv <- (273.15 + Td)*(1 + 0.608*q*1e-3)
    P = conform(P,Tv)

    x <- P/R/Tv
    if (!is.null(robust) && robust) x <- median(x, na.rm=T)
    else {
        colnames(x) <- sub("^[^.]+",datvar,colnames(x))
        x@units <- rep("kg/m^3",ncol(x))
    }

    x
}
dat.rhoDry <- function(what,derived=TRUE,cache=F,...)
{
    datvar <- datVar() # requested variable name, x of dat.x

    robust <- dpar("robust")
    # density of dry air (kg/m^3)
    R <- 287		# J/kg-K
    mr <- dat(sub(datvar,"MR",what,fixed=TRUE))

    P = dat("P")
    # convert to pascals
    if (any(units(P) == "mb")) P[,units(P)=="mb"] = P[,units(P)=="mb"] * 100
    if (any(units(P) == "kPa")) P[,units(P)=="kPa"] = P[,units(P)=="kPa"] * 1000

    im <- match(stations(mr), stations(P), nomatch = 0)
    # 
    if(any(im == 0)) {
        # For those stations without a pressure
        warning(paste("No variable P at stations",
                paste(unique(stations(mr)[im==0]),collapse=","),
                ". dat.rhoDry is creating a P from average of other stations"))
        Pa <- P[, 1]
        nP <- dim(P)[2]
        Pa@data <- P@data %*% rep(1/nP, nP)
        nm <- length(im)
        Pt <- P[, rep(1, nm)]
        if (any(im != 0)) Pt[,im!=0] <- P[,im]
        Pt[, im == 0] <- Pa
        P <- Pt
        stations(P) <- stations(mr)
    }
    else P <- P[, im]

    P <- P/(1 + mr/622)		# P - Pv, pascals
    Td <- dat(sub(datvar,"T",what,fixed=TRUE))
    Td <- conform(Td,mr)
    TK <- 273.15 + Td
    x <- P/R/TK		
    if (!is.null(robust) && robust) x <- median(x, na.rm=T)
    else {
        colnames(x) <- sub("^[^.]+",datvar,colnames(Td))
        x@units <- rep("kg/m^3",ncol(x))
    }
    x
}
