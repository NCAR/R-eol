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
    datvar <- datVar() # requested variable name, x of dat.x

    # Drag coefficient
    if (is.null(speed))
        speed <- "spd"

    x <- (dat(sub(datvar,"u*",what,fixed=TRUE),avg=T,smooth=T) / 
        dat(sub(datvar,speed,what,fixed=TRUE),avg=T,smooth=T))^2
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("Cd",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}
dat.Ct <- function(what,derived=TRUE,cache=F,speed=dpar("bulk.speed"),...)
{
    datvar <- datVar() # requested variable name, x of dat.x

    # Transfer coefficient for temperature
    if (is.null(speed))
        speed <- "spd"

    Td <- dat(sub(datvar,"T",what,fixed=TRUE),avg=T,smooth=T)
    Tsfc <- dat(sub(datvar,"Tsfc",what,fixed=TRUE),avg=T,smooth=T)
    if (ncol(Td) > ncol(Tsfc)) {
        # Look for a T without height, or otherwise use the minimum height
        tht <- if (any(is.na(heights(Td)))) NA_real_ else min(heights(Td))
        Td <- select(Td,hts=tht)
    }

    x <- - dat(sub(datvar,"w't'",what,fixed=TRUE),avg=T,smooth=T) / 
        dat(sub(datvar,speed,what,fixed=TRUE),avg=T,smooth=T)/(Td - Tsfc)
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("Ct",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}
"dat.sigma_w/u*" <- function(what,derived=TRUE,cache=F,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    x <- dat(sub(datvar,"w'w'",what,fixed=TRUE),avg=TRUE,smooth=TRUE)^0.5 / 
        dat(sub(datvar,"u*",what,fixed=TRUE),avg=T,smooth=T)
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

    datvar <- datVar() # requested variable name, x of dat.x

    check.windcoords();
    # dcoords is coordinates of dat("u",derived=F)
    dcoords = dpar("datacoords")
    # requested coordinate system
    rcoords = dpar("coords")

    if (rcoords != dcoords) {
        on.exit(dpar(coords=rcoords),add=T)
        dpar(coords=dcoords)
    }
    u2 <- dat(sub(datvar,"us'us'",what,fixed=TRUE),avg=T,smooth=T)

    x <- sqrt(u2)/dat(sub(datvar,"u*",what,fixed=TRUE),avg=T,smooth=T)
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("sigma_u/u*",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}

"dat.us'tc'" <- function(what,derived=TRUE,cache=F,...)
{
    datvar <- datVar() # requested variable name, x of dat.x

    # calculate streamwise sonic temperature flux
    u <- dat(sub(datvar,"u",what,fixed=TRUE), avg=T,smooth=T, derived=F)
    v <- dat(sub(datvar,"v",what,fixed=TRUE), avg=T,smooth=T, derived=F)
    x = (u*dat(sub(datvar,"u'tc'",what,fixed=TRUE), avg=T,smooth=T) +
        v*dat(sub(datvar,"v'tc'",what,fixed=TRUE), avg=T,smooth=T))/sqrt(u^2 + v^2)

    dimnames(x) <- list(NULL,paste("us'tc'",suffixes(x,2),sep=""))
    x@units <- rep("m/s-degK",ncol(x))
    x
}

dat.sigma_dir <- function(what,derived=TRUE, cache=F,...)
{
    datvar <- datVar() # requested variable name, x of dat.x

    # Calculate standard deviation of wind direction 
    # first want variance of cross-stream wind component 
    u <- dat(sub(datvar,"u",what,fixed=TRUE),avg=T,smooth=T)
    v <- dat(sub(datvar,"v",what,fixed=TRUE),avg=T,smooth=T)
    v2 <- dat(sub(datvar,"vs'vs'",what,fixed=TRUE),avg=T,smooth=T)

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
    datvar <- datVar() # requested variable name, x of dat.x

    v2 <- dat(sub(datvar,"vs'vs'",what,fixed=TRUE),avg=T,smooth=T)

    x <- sqrt(v2)/dat(sub(datvar,"u*",what,fixed=TRUE),avg=T,smooth=T)
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("sigma_v/u*",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}

"dat.sigma_tc/tc*" <- function(what,derived=TRUE, cache=F,...)
{
    datvar <- datVar() # requested variable name, x of dat.x

    ustar = dat(sub(datvar,"u*",what,fixed=TRUE),avg=T,smooth=T)
    tc2 = dat(sub(datvar,"tc'tc'",what,fixed=TRUE),avg=T,smooth=T)
    wtc = dat(sub(datvar,"w'tc'",what,fixed=TRUE),avg=T,smooth=T)
    x = ustar*sqrt(tc2)/abs(wtc)
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("sigma_tc/tc*",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}

dat.r_uw <- function(what,derived=TRUE, cache=F,...)
{
    datvar <- datVar() # requested variable name, x of dat.x

    # correlation coefficient for u*^2
    uw = dat(sub(datvar,"u*",what,fixed=TRUE),avg=T,smooth=T)^2
    # uu is variance of streamwise velocity fluctuations
    uu = dat(sub(datvar,"us'us'",what,fixed=TRUE),avg=T,smooth=T)
    ww = dat(sub(datvar,"w'w'",what,fixed=TRUE),avg=T,smooth=T)
    x = uw/sqrt(uu*ww)
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("r_uw",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}

dat.r_wtc <- function(what,derived=TRUE, cache=F,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    wtc = dat(sub(datvar,"w'tc'",what,fixed=TRUE),avg=T,smooth=T)
    ww = dat(sub(datvar,"w'w'",what,fixed=TRUE),avg=T,smooth=T)
    tc2 = dat(sub(datvar,"tc'tc'",what,fixed=TRUE),avg=T,smooth=T)
    x = wtc/sqrt(ww*tc2)
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("r_wtc",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}

dat.r_utc <- function(what,derived=TRUE, cache=F,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    # streamwise temperature flux
    utc = dat(sub(datvar,"us'tc'",what,fixed=TRUE),avg=T,smooth=T)
    # streamwise velocity variance
    uu = dat(sub(datvar,"us'us'",what,fixed=TRUE),avg=T,smooth=T)
    tc2 = dat(sub(datvar,"tc'tc'",what,fixed=TRUE),avg=T,smooth=T)
    x = utc/sqrt(uu*tc2)
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("r_utc",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}

dat.r_wh2o <- function(what,derived=TRUE, cache=F,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    wh2o = dat(sub(datvar,"w'h2o'",what,fixed=TRUE),avg=T,smooth=T)
    ww = dat(sub(datvar,"w'w'",what,fixed=TRUE),avg=T,smooth=T)
    h2o2 = dat(sub(datvar,"h2o'h2o'",what,fixed=TRUE),avg=T,smooth=T)
    x = wh2o/sqrt(ww*h2o2)
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("r_wh2o",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}

dat.S_u <- function(what,derived=TRUE, cache=F,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    uuu = dat(sub(datvar,"u'u'u'",what,fixed=TRUE),avg=T,smooth=T)
    uu = dat(sub(datvar,"u'u'",what,fixed=TRUE),avg=T,smooth=T)
    x = uuu/uu^1.5
    x[is.infinite(x)] <- NA_real_
    dimnames(x) <- list(NULL,paste("S_u",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}

dat.S_w <- function(what,derived=TRUE, cache=F,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    www = dat(sub(datvar,"w'w'w'",what,fixed=TRUE),avg=T,smooth=T)
    ww = dat(sub(datvar,"w'w'",what,fixed=TRUE),avg=T,smooth=T)
    x = www/ww^1.5
    x[is.infinite(x)] <- NA_real_
    dimnames(x) <- list(NULL,paste("S_w",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}

dat.S_tc <- function(what,derived=TRUE, cache=F,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    ccc = dat(sub(datvar,"tc'tc'tc'",what,fixed=TRUE),avg=T,smooth=T)
    cc = dat(sub(datvar,"tc'tc'",what,fixed=TRUE),avg=T,smooth=T)
    x = ccc/cc^1.5
    x[is.infinite(x)] <- NA_real_
    dimnames(x) <- list(NULL,paste("S_tc",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}

dat.S_h2o <- function(what,derived=TRUE, cache=F,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    ccc = dat(sub(datvar,"h2o'h2o'h2o'",what,fixed=TRUE),avg=T,smooth=T)
    cc = dat(sub(datvar,"h2o'h2o'",what,fixed=TRUE),avg=T,smooth=T)
    x = ccc/cc^1.5
    x[is.infinite(x)] <- NA_real_
    dimnames(x) <- list(NULL,paste("S_h2o",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}

dat.uw_tilt_err <- function(what,derived=TRUE, cache=F,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    # tilt error in uw per degree tilt, for small tilt angles
    # Kaimal and Haugen, JAM, 1969, V8, 460-462
    # fractional error = sin(2*theta)/r_uw*
    #                                 {sigma(u)/sigma(w) - sigma(w)/sigma(u)}
    #
    # uu is variance of streamwise velocity fluctuations
    uu = dat(sub(datvar,"us'us'",what,fixed=TRUE),avg=T,smooth=T)
    ww = dat(sub(datvar,"w'w'",what,fixed=TRUE),avg=T,smooth=T)
    sig.u = sqrt(uu)
    sig.w = sqrt(ww)
    r.uw = dat(sub(datvar,"r_uw",what,fixed=TRUE),avg=T,smooth=T)

    x = (sig.u/sig.w - sig.w/sig.u)/r.uw*pi/180

    dimnames(x) <- list(NULL,paste("uw_tilt_err",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}
