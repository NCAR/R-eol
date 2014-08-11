# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

#
check.windcoords = function()
{
    # dpar("datacoords") is coordinates of dat("u",derived=FALSE)
    dcoords = dpar("datacoords")
    if (is.null(dcoords)) {
        warning("dpar(\"datacoords\") not set, assuming dpar(datacoords=\"instrument\")")
        dcoords = "instrument"
        dpar(datacoords=dcoords)
    }
    if (dcoords == "sonic") {
        dcoords = "instrument"
        dpar(datacoords=dcoords)
    }
    if (dcoords != "geo" && substring(dcoords,1,3) == "geo") {
        dcoords = "geo"
        dpar(datacoords=dcoords)
    }
    if (dcoords != "geo" && dcoords != "instrument")
        stop("unknown value for dpar(\"datacoords\"), should be \"instrument\" or \"geo\"")

    # requested coordinate system
    rcoords = dpar("coords")
    if (is.null(rcoords)) dpar(coords=dcoords)
    else if (rcoords == "sonic") dpar(coords="instrument")
    else if (rcoords != "geo" && rcoords != "instrument")
        stop("unknown value for dpar(\"coords\"), should be \"instrument\" or \"geo\"")

}
# Winds
# -----
dat.u <- function(what,derived=TRUE,cache=FALSE,...)
{
    # dat("u",derived=FALSE) forces reading "u" from NetCDF
    # otherwise calling dat("u") here would be an infinite loop
    u <- dat(what,derived=FALSE)

    check.windcoords();
    # dcoords is coordinates of dat("u",derived=FALSE)
    dcoords = dpar("datacoords")
    # requested coordinate system
    rcoords = dpar("coords")

    # Rotate from instrument to geographic coordinates (or back)
    if (rcoords != dcoords) {
        v <- dat(expand("v",what),derived=FALSE)
        if (is.null(u) || is.null(v)) return(NULL)

        # vazm is the geographic compass angle of the sonic +V axis
        vazm <- dat(expand("Vazimuth",what))
        if (is.null(vazm)) return(NULL)

        # Flip the sign of vazimuth if rotating from geo to instrument
        if (dcoords == "geo") vazm = -vazm
        vazm <- conform(vazm,u)

        # double check the above conform
        usfx = suffixes(u)
        vsfx = suffixes(v)

        if (!identical(usfx,vsfx)) {
            warning(paste("u suffixes (",paste(usfx,collapse=","),
                    ") are not in same order as v suffixes (",paste(vsfx,collapse=","),
                    "). Will re-order them.",
                    sep=""))
            u = u[,order(usfx)]
            v = v[,order(vsfx)]
            usfx = suffixes(u)
            vsfx = suffixes(v)
        }

        asfx = suffixes(vazm)
        if (!identical(usfx,asfx)) {
            warning(paste("u suffixes (",paste(usfx,collapse=","),
                    ") are not in same order as Vazimuth suffixes (",
                    paste(asfx,collapse=","),
                    "). Will re-order them.",
                    sep=""))
            vazm = vazm[,order(asfx)]
        }

        if (any(is.na(vazm))) vazm <- replace.nas(vazm,warn=FALSE)

        # count number of non-NAs in each column
        vazm.ok <- as.vector(t(!is.na(vazm@data)) %*% rep(1,nrow(vazm))) > 0
        if (any(!vazm.ok)) {
            warning(paste("No sonic Vazimuth data available for stations",
                    paste(stations(u)[!vazm.ok],collapse=" "),"\nusing median from other stations"))
            vazm[,!vazm.ok] <- median(vazm[,vazm.ok],na.rm=T)
        }

        vazm <- vazm * pi / 180

        # approximate azimuth over u times. Method "constant", f=0,
        # extrapolates values forward in time until the next change.

        # During sheba, Vazimuth is changing.  If you don't
        # subtract 1/2 dt from its times, then Vazimuth at t[i-1] is
        # used to rotation the winds at t[i] (minor problem)
        dt <- deltat(u)[1]
        if (!is.null(dt)) vazm@positions = vazm@positions - dt * .5

        vazm <- approx(vazm,xout=u@positions,method="constant",f=0,rule=2)

        u <-  u * cos(vazm) + v * sin(vazm)

        dimnames(u) <- list(NULL,expand("u",u))
        u@units <- rep("m/s",ncol(u))
    }
    u
}
dat.v <- function(what,derived=TRUE,cache=FALSE,...)
{
    # dat("v",derived=FALSE) forces reading "v" from NetCDF
    # otherwise calling dat("v") here would be an infinite loop
    v <- dat(what,derived=FALSE)

    check.windcoords();
    # dcoords is coordinates of dat("u",derived=FALSE)
    dcoords = dpar("datacoords")
    # requested coordinate system
    rcoords = dpar("coords")

    # Rotate from instrument to geographic coordinates (or back)
    if (rcoords != dcoords) {

        u <- dat(expand("u",what),derived=FALSE)
        if (is.null(u) || is.null(v)) return(NULL)

        vazm <- dat(expand("Vazimuth",what))
        if (is.null(vazm)) return(NULL)

        # Flip the sign of vazimuth if rotating from geo to instrument
        if (dcoords == "geo") vazm = -vazm
        vazm <- conform(vazm,v)

        # double check the above conform
        usfx = suffixes(u)
        vsfx = suffixes(v)

        if (!identical(usfx,vsfx)) {
            warning(paste("u suffixes (",paste(usfx,collapse=","),
                    ") are not in same order as v suffixes (",paste(vsfx,collapse=","),
                    "). Will re-order them.",
                    sep=""))
            u = u[,order(usfx)]
            v = v[,order(vsfx)]
            usfx = suffixes(u)
            vsfx = suffixes(v)
        }

        asfx = suffixes(vazm)
        if (!identical(usfx,asfx)) {
            warning(paste("u suffixes (",paste(usfx,collapse=","),
                    ") are not in same order as Vazimuth suffixes (",
                    paste(asfx,collapse=","),
                    "). Will re-order them.",
                    sep=""))
            vazm = vazm[,order(asfx)]
        }

        if (any(is.na(vazm))) vazm <- replace.nas(vazm,warn=FALSE)

        # count number of non-NAs in each column
        vazm.ok <- as.vector(t(!is.na(vazm@data)) %*% rep(1,nrow(vazm))) > 0
        if (any(!vazm.ok)) {
            warning(paste("No sonic Vazimuth data available for stations",
                    paste(stations(u)[!vazm.ok],collapse=" "),"\nusing median from other stations"))
            vazm[,!vazm.ok] <- median(vazm[,vazm.ok],na.rm=T)
        }

        vazm <- vazm * pi / 180.

        # approximate azimuth over u times. Method "constant", f=0,
        # extrapolates values forward in time until the next change.

        # During sheba, Vazimuth is changing.  If you don't
        # subtract 1/2 dt from its times, then Vazimuth at t[i-1] is
        # used to rotation the winds at t[i] (minor problem)
        dt <- deltat(u)[1]
        if (!is.null(dt)) vazm@positions = vazm@positions - dt * .5

        vazm <- approx(vazm,xout=u@positions,method="constant",f=0,rule=2)

        v <- -u * sin(vazm) + v * cos(vazm)

        dimnames(v) <- list(NULL,expand("v",v))
        v@units <- rep("m/s",ncol(v))
    }
    v
}
dat.spd <- function(what,derived=TRUE,cache=FALSE,...)
{
    x <- sqrt(dat(expand("u",what),avg=TRUE,smooth=TRUE)^2 +
        dat(expand("v",what),avg=TRUE,smooth=TRUE)^2)
    if (length(x) == 0) return(NULL)

    dimnames(x) <- list(NULL,expand("spd",x))
    x@units <- rep("m/s",ncol(x))
    x
}
dat.spd.uvw <- function(what,derived=TRUE,cache=FALSE,...)
{
    x <- sqrt(dat(expand("u",what),avg=TRUE,smooth=TRUE)^2 +
        dat(expand("v",what),avg=TRUE,smooth=TRUE)^2 +
        dat(expand("w",what),avg=TRUE,smooth=TRUE)^2)
    if (length(x) == 0) return(NULL)

    dimnames(x) <- list(NULL,expand("spd.uvw",x))
    x@units <- rep("m/s",ncol(x))
    x
}
dat.dir <- function(what,derived=TRUE,cache=FALSE,...)
{
    x <- dat(expand("u",what),avg=TRUE,smooth=TRUE)
    if (length(x) == 0) return(NULL)
    x@data <- atan2(-x@data,
        -dat(expand("v",what),avg=TRUE,smooth=TRUE)@data) * 180 / pi
    xneg <- !is.na(x@data) & x@data < 0
    x[xneg] <- x[xneg] + 360

    dimnames(x) <- list(NULL,expand("dir",x))
    x@units <- rep("deg",ncol(x))
    x
}
dat.Spd <- function(what,derived=TRUE,cache=FALSE,...)
{
    x <- sqrt(dat(expand("U",what),avg=TRUE,smooth=TRUE)^2 +
        dat(expand("V",what),avg=TRUE,smooth=TRUE)^2)
    if (length(x) == 0) return(NULL)
    if (length(x) == 1 && is.na(x)) return(NULL)

    dimnames(x) <- list(NULL,expand("Spd",x))
    x@units <- rep("m/s",ncol(x))
    x
}
dat.Dir <- function(what,derived=TRUE,cache=FALSE,...)
{
    x <- dat("U",avg=TRUE,smooth=TRUE)
    if (length(x) == 0) return(NULL)
    x@data <- atan2(-x@data,-dat("V",avg=TRUE,smooth=TRUE)@data) * 180 / pi
    xneg <- !is.na(x@data) & x@data < 0
    x[xneg] <- x[xneg] + 360

    dimnames(x) <- list(NULL,expand("Dir",x))
    x@units <- rep("deg",ncol(x))
    x
}
dat.elev.u <- function(what,derived=TRUE,cache=FALSE,...)
{
    x <- atan(dat("w")/dat("u"))*180/pi
    if (length(x) == 0) return(NULL)

    dimnames(x) <- list(NULL,expand("elev.u",x))
    x@units <- rep("deg",ncol(x))
    x
}
dat.elev.v <- function(what,derived=TRUE,cache=FALSE,...)
{
    x <- atan(dat("w")/dat("v"))*180/pi
    if (length(x) == 0) return(NULL)

    dimnames(x) <- list(NULL,expand("elev.v",x))
    x@units <- rep("deg",ncol(x))
    x
}

# velocity variances in streamwise coordinates

"dat.us" <- function(what,derived=TRUE,cache=FALSE,...)
{
    # mean streamwise wind required to calculate non-simple average of
    # us'us'
    u <- dat(expand("u",what), avg=TRUE,smooth=TRUE, derived=FALSE)
    v <- dat(expand("v",what), avg=TRUE,smooth=TRUE, derived=FALSE)
    x = sqrt(u^2 + v^2)

    dimnames(x) <- list(NULL,paste("us",suffixes(x,2),sep=""))
    x@units <- rep("m/s",ncol(x))
    x
}

"dat.vs" <- function(what,derived=TRUE,cache=FALSE,...)
{
    # mean cross-stream wind required to calculate non-simple average of
    # us'us'
    x = 0
    dimnames(x) <- list(NULL,paste("vs",suffixes(x,2),sep=""))
    x@units <- rep("m/s",ncol(x))
    x
}

"dat.us'us'" <- function(what,derived=TRUE,cache=FALSE,...)
{
    # calculate streamwise variance
    u <- dat(expand("u",what), avg=TRUE,smooth=TRUE, derived=FALSE)
    v <- dat(expand("v",what), avg=TRUE,smooth=TRUE, derived=FALSE)
    x <- (u^2*dat(expand("u'u'",what), avg=TRUE,smooth=T) +
        2*u*v*dat(expand("u'v'",what), avg=TRUE,smooth=T) +
        v^2*dat(expand("v'v'",what), avg=TRUE,smooth=T))/ (u^2 + v^2)

    dimnames(x) <- list(NULL,paste("us'us'",suffixes(x,2),sep=""))
    x@units <- rep("(m/s)^2",ncol(x))
    x
}

"dat.vs'vs'" <- function(what,derived=TRUE,cache=FALSE,...)
{
    # calculate cross-stream variance
    u <- dat(expand("u", what),avg=TRUE,smooth=TRUE, derived=FALSE)
    v <- dat(expand("v", what),avg=TRUE,smooth=TRUE, derived=FALSE)
    x <- (v^2*dat(expand("u'u'",what), avg=TRUE,smooth=T) -
        2*u*v*dat(expand("u'v'",what), avg=TRUE,smooth=T) +
        u^2*dat(expand("v'v'",what), avg=TRUE,smooth=T))/ (u^2 + v^2)

    dimnames(x) <- list(NULL,paste("vs'vs'",suffixes(x,2),sep=""))
    x@units <- rep("(m/s)^2",ncol(x))
    x
}

