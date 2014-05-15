# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.
#
# Several (mostly) dimensionless turbulence parameters
# ----------------------------------------------------
dat.Cd <- function(what,derived=TRUE,cache=F,speed=dpar("bulk.speed"),...)
{
    # Drag coefficient
    if (is.null(speed))
        speed <- "spd"

    x <- (dat(expand("u*",what),avg=T,smooth=T) / 
        dat(expand(speed,what),avg=T,smooth=T))^2
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("Cd",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}
dat.Ct <- function(what,derived=TRUE,cache=F,speed=dpar("bulk.speed"),...)
{
    # Transfer coefficient for temperature
    if (is.null(speed))
        speed <- "spd"

    Td <- dat(expand("T",what),avg=T,smooth=T)
    Tsfc <- dat(expand("Tsfc",what),avg=T,smooth=T)
    if (ncol(Td) > ncol(Tsfc)) {
        # Look for a T without height, or otherwise use the minimum height
        tht <- if (any(is.na(heights(Td)))) NA_real_ else min(heights(Td))
        Td <- select(Td,hts=tht)
    }

    x <- - dat(expand("w't'",what),avg=T,smooth=T) / 
    dat(expand(speed,what),avg=T,smooth=T)/(Td - Tsfc)
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("Ct",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}
"dat.sigma_w/u*" <- function(what,derived=TRUE,cache=F,...)
{
    x <- dat(expand("w'w'",what),avg=TRUE,smooth=TRUE)^0.5 / 
    dat(expand("u*",what),avg=T,smooth=T)
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("sigma_w/u*",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}
"dat.sigma_u/u*" <- function(what,derived=TRUE,cache=F,...)
{
    # want variance of streamwise wind component
    # Do calculations in whatever coordinates the netcdf
    # data is in, since dat.u and dat.v can perform rotations
    # but dat.u'w' and dat.v'w' do not.

    check.windcoords();
    # dcoords is coordinates of dat("u",derived=F)
    dcoords = dpar("datacoords")
    # requested coordinate system
    rcoords = dpar("coords")

    if (rcoords != dcoords) {
        on.exit(dpar(coords=rcoords),add=T)
        dpar(coords=dcoords)
    }
    u2 <- dat(expand("us'us'",what),avg=T,smooth=T)

    x <- sqrt(u2)/dat(expand("u*",what),avg=T,smooth=T)
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("sigma_u/u*",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}

"dat.us'tc'" <- function(what,derived=TRUE,cache=F,...)
{
    # calculate streamwise sonic temperature flux
    u <- dat(expand("u",what), avg=T,smooth=T, derived=F)
    v <- dat(expand("v",what), avg=T,smooth=T, derived=F)
    x = (u*dat(expand("u'tc'",what), avg=T,smooth=T) +
        v*dat(expand("v'tc'",what), avg=T,smooth=T))/sqrt(u*2 + v^2)

    dimnames(x) <- list(NULL,paste("us'tc'",suffixes(x,2),sep=""))
    x@units <- rep("m/s-degK",ncol(x))
    x
}

dat.sigma_dir <- function(what,derived=TRUE, cache=F,...)
{
    # Calculate standard deviation of wind direction 
    # first want variance of cross-stream wind component 
    u <- dat(expand("u",what),avg=T,smooth=T)
    v <- dat(expand("v",what),avg=T,smooth=T)
    v2 <- dat(expand("vs'vs'",what),avg=T,smooth=T)

    x <- 180/pi*atan(sqrt(v2/(u^2 + v^2)))
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("sigma_dir",suffixes(x,2),sep=""))
    x@units <- rep("deg",ncol(x))
    x
}

"dat.sigma_v/u*" <- function(what,derived=TRUE, cache=F,...)
{
    # want variance of cross-stream wind component
    # Do calculations in whatever coordinates the netcdf
    # data is in, since dat.u and dat.v can perform rotations
    # but dat.u'w' and dat.v'w' do not.

    # do not need this coordinate matching here,
    # since streamwise rotations are done in dat("vs'vs'")

    #    check.windcoords();
    # dcoords is coordinates of dat("u",derived=F)
    #    dcoords = dpar("datacoords")
    # requested coordinate system
    #    rcoords = dpar("coords")
    #
    #    if (rcoords != dcoords) {
    #        on.exit(dpar(coords=rcoords),add=T)
    #        dpar(coords=dcoords)
    #    }
    #
    v2 <- dat(expand("vs'vs'",what),avg=T,smooth=T)

    x <- sqrt(v2)/dat(expand("u*",what),avg=T,smooth=T)
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("sigma_v/u*",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}

"dat.sigma_tc/tc*" <- function(what,derived=TRUE, cache=F,...)
{
    ustar = dat(expand("u*",what),avg=T,smooth=T)
    tc2 = dat(expand("tc'tc'",what),avg=T,smooth=T)
    wtc = dat(expand("w'tc'",what),avg=T,smooth=T)
    x = ustar*sqrt(tc2)/abs(wtc)
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("sigma_tc/tc*",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}

dat.r_uw <- function(what,derived=TRUE, cache=F,...)
{
    # correlation coefficient for u*^2
    uw = dat(expand("u*",what),avg=T,smooth=T)^2
    # uu is variance of streamwise velocity fluctuations
    uu = dat(expand("us'us'",what),avg=T,smooth=T)
    ww = dat(expand("w'w'",what),avg=T,smooth=T)
    x = uw/sqrt(uu*ww)
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("r_uw",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}

dat.r_wtc <- function(what,derived=TRUE, cache=F,...)
{
    wtc = dat(expand("w'tc'",what),avg=T,smooth=T)
    ww = dat(expand("w'w'",what),avg=T,smooth=T)
    tc2 = dat(expand("tc'tc'",what),avg=T,smooth=T)
    x = wtc/sqrt(ww*tc2)
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("r_wtc",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}

dat.r_utc <- function(what,derived=TRUE, cache=F,...)
{
    # streamwise temperature flux
    utc = dat(expand("us'tc'",what),avg=T,smooth=T)
    # streamwise velocity variance
    uu = dat(expand("us'us'",what),avg=T,smooth=T)
    tc2 = dat(expand("tc'tc'",what),avg=T,smooth=T)
    x = utc/sqrt(uu*tc2)
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("r_utc",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}

dat.r_wh2o <- function(what,derived=TRUE, cache=F,...)
{
    wh2o = dat(expand("w'h2o'",what),avg=T,smooth=T)
    ww = dat(expand("w'w'",what),avg=T,smooth=T)
    h2o2 = dat(expand("h2o'h2o'",what),avg=T,smooth=T)
    x = wh2o/sqrt(ww*h2o2)
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("r_wh2o",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}

dat.S_u <- function(what,derived=TRUE, cache=F,...)
{
    uuu = dat(expand("u'u'u'",what),avg=T,smooth=T)
    uu = dat(expand("u'u'",what),avg=T,smooth=T)
    x = uuu/uu^1.5
    x[is.infinite(x)] <- NA_real_
    dimnames(x) <- list(NULL,paste("S_u",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}

dat.S_w <- function(what,derived=TRUE, cache=F,...)
{
    www = dat(expand("w'w'w'",what),avg=T,smooth=T)
    ww = dat(expand("w'w'",what),avg=T,smooth=T)
    x = www/ww^1.5
    x[is.infinite(x)] <- NA_real_
    dimnames(x) <- list(NULL,paste("S_w",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}

dat.S_tc <- function(what,derived=TRUE, cache=F,...)
{
    ccc = dat(expand("tc'tc'tc'",what),avg=T,smooth=T)
    cc = dat(expand("tc'tc'",what),avg=T,smooth=T)
    x = ccc/cc^1.5
    x[is.infinite(x)] <- NA_real_
    dimnames(x) <- list(NULL,paste("S_tc",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}

dat.S_h2o <- function(what,derived=TRUE, cache=F,...)
{
    ccc = dat(expand("h2o'h2o'h2o'",what),avg=T,smooth=T)
    cc = dat(expand("h2o'h2o'",what),avg=T,smooth=T)
    x = ccc/cc^1.5
    x[is.infinite(x)] <- NA_real_
    dimnames(x) <- list(NULL,paste("S_h2o",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}

dat.uw_tilt_err <- function(what,derived=TRUE, cache=F,...)
{
    # tilt error in uw per degree tilt, for small tilt angles
    # Kaimal and Haugen, JAM, 1969, V8, 460-462
    # fractional error = sin(2*theta)/r_uw*
    #                                 {sigma(u)/sigma(w) - sigma(w)/sigma(u)}
    #
    # uu is variance of streamwise velocity fluctuations
    uu = dat(expand("us'us'",what),avg=T,smooth=T)
    ww = dat(expand("w'w'",what),avg=T,smooth=T)
    sig.u = sqrt(uu)
    sig.w = sqrt(ww)
    r.uw = dat(expand("r_uw",what),avg=T,smooth=T)

    x = (sig.u/sig.w - sig.w/sig.u)/r.uw*pi/180

    dimnames(x) <- list(NULL,paste("uw_tilt_err",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}
