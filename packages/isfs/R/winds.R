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
    if (rcoords != "geo" && substring(rcoords,1,3) == "geo") {
        rcoords = "geo"
        dpar(coords=rcoords)
    }
    if (is.null(rcoords)) dpar(coords=dcoords)
    else if (rcoords == "sonic") dpar(coords="instrument")
    else if (rcoords != "geo" && rcoords != "instrument")
        stop("unknown value for dpar(\"coords\"), should be \"instrument\" or \"geo\"")

}
# Winds
# -----
dat.u <- function(what,derived=TRUE,cache=FALSE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x

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
        v <- dat(sub(datvar,"v",what,fixed=TRUE),derived=FALSE)
        if (is.null(u) || is.null(v)) return(NULL)

        # vazm is the geographic compass angle of the sonic +V axis
        vazm <- dat(sub(datvar,"Vazimuth",what,fixed=TRUE))
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

        colnames(u) <- sub("[^.]+",datvar,colnames(u))
        u@units <- rep("m/s",ncol(u))
    }
    u
}
dat.v <- function(what,derived=TRUE,cache=FALSE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x

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

        u <- dat(sub(datvar,"u",what,fixed=TRUE),derived=FALSE)
        if (is.null(u) || is.null(v)) return(NULL)

        vazm <- dat(sub(datvar,"Vazimuth",what,fixed=TRUE))
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

        colnames(v) <- sub("[^.]+",datvar,colnames(v))
        v@units <- rep("m/s",ncol(v))
    }
    v
}
dat.spd <- function(what,derived=TRUE,cache=FALSE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x

    x <- sqrt(dat(sub(datvar,"u",what,fixed=TRUE),avg=TRUE,smooth=TRUE)^2 +
        dat(sub(datvar,"v",what,fixed=TRUE),avg=TRUE,smooth=TRUE)^2)
    if (length(x) == 0) return(NULL)

    colnames(x) <- sub("[^.]+",datvar,colnames(x))
    x@units <- rep("m/s",ncol(x))
    x
}
dat.spd.uvw <- function(what,derived=TRUE,cache=FALSE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x

    x <- sqrt(dat(sub(datvar,"u",what,fixed=TRUE),avg=TRUE,smooth=TRUE)^2 +
        dat(sub(datvar,"v",what,fixed=TRUE),avg=TRUE,smooth=TRUE)^2 +
        dat(sub(datvar,"w",what,fixed=TRUE),avg=TRUE,smooth=TRUE)^2)
    if (length(x) == 0) return(NULL)

    colnames(x) <- sub("[^.]+",datvar,colnames(x))
    x@units <- rep("m/s",ncol(x))
    x
}
dat.dir <- function(what,derived=TRUE,cache=FALSE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x

    x <- dat(sub(datvar,"u",what,fixed=TRUE),avg=TRUE,smooth=TRUE)
    if (length(x) == 0) return(NULL)
    x@data <- atan2(-x@data,
        -dat(sub(datvar,"v",what,fixed=TRUE),avg=TRUE,smooth=TRUE)@data) * 180 / pi
    xneg <- !is.na(x@data) & x@data < 0
    x[xneg] <- x[xneg] + 360

    colnames(x) <- sub("[^.]+",datvar,colnames(x))
    x@units <- rep("deg",ncol(x))
    x
}
dat.Spd <- function(what,derived=TRUE,cache=FALSE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x

    x <- sqrt(dat(sub(datvar,"U",what,fixed=TRUE),avg=TRUE,smooth=TRUE)^2 +
        dat(sub(datvar,"V",what,fixed=TRUE),avg=TRUE,smooth=TRUE)^2)
    if (length(x) == 0) return(NULL)
    if (length(x) == 1 && is.na(x)) return(NULL)

    colnames(x) <- sub("[^.]+",datvar,colnames(x))
    x@units <- rep("m/s",ncol(x))
    x
}
dat.Dir <- function(what,derived=TRUE,cache=FALSE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x

    x <- dat(sub(datvar,"U",what,fixed=TRUE),avg=TRUE,smooth=TRUE)
    if (length(x) == 0) return(NULL)
    x@data <- atan2(-x@data,-dat(sub(datvar,"V",what,fixed=TRUE),avg=TRUE,smooth=TRUE)@data) * 180 / pi
    xneg <- !is.na(x@data) & x@data < 0
    x[xneg] <- x[xneg] + 360

    colnames(x) <- sub("[^.]+",datvar,colnames(x))
    x@units <- rep("deg",ncol(x))
    x
}
dat.elev.u <- function(what,derived=TRUE,cache=FALSE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x

    x <- atan(dat(sub(datvar,"w",what,fixed=TRUE))/dat(sub(datvar,"u",what,fixed=TRUE)))*180/pi
    if (length(x) == 0) return(NULL)

    colnames(x) <- sub("[^.]+",datvar,colnames(x))
    x@units <- rep("deg",ncol(x))
    x
}
dat.elev.v <- function(what,derived=TRUE,cache=FALSE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x

    x <- atan(dat(sub(datvar,"w",what,fixed=TRUE))/dat(sub(datvar,"v",what,fixed=TRUE)))*180/pi
    if (length(x) == 0) return(NULL)

    colnames(x) <- sub("[^.]+",datvar,colnames(x))
    x@units <- rep("deg",ncol(x))
    x
}

# velocity variances in streamwise coordinates

"dat.us" <- function(what,derived=TRUE,cache=FALSE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x

    # mean streamwise wind required to calculate non-simple average of
    # us'us'
    u <- dat(sub(datvar,"u",what,fixed=TRUE), avg=TRUE,smooth=TRUE, derived=FALSE)
    v <- dat(sub(datvar,"v",what,fixed=TRUE), avg=TRUE,smooth=TRUE, derived=FALSE)
    x = sqrt(u^2 + v^2)

    colnames(x) <- sub("[^.]+","us",colnames(x))
    x@units <- rep("m/s",ncol(x))
    x
}

"dat.vs" <- function(what,derived=TRUE,cache=FALSE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x

    # mean cross-stream wind required to calculate non-simple average of
    # us'us'
    x = 0
    # colnames(x) <- sub("[^.]+","vs",colnames(x))
    # x@units <- rep("m/s",ncol(x))
    x
}

"dat.us'us'" <- function(what,derived=TRUE,cache=FALSE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x

    # calculate streamwise variance
    u <- dat(sub(datvar,"u",what,fixed=TRUE), avg=TRUE,smooth=TRUE, derived=FALSE)
    v <- dat(sub(datvar,"v",what,fixed=TRUE), avg=TRUE,smooth=TRUE, derived=FALSE)
    x <- (u^2*dat(sub(datvar,"u'u'",what,fixed=TRUE), avg=TRUE,smooth=T) +
        2*u*v*dat(sub(datvar,"u'v'",what,fixed=TRUE), avg=TRUE,smooth=T) +
        v^2*dat(sub(datvar,"v'v'",what,fixed=TRUE), avg=TRUE,smooth=T))/ (u^2 + v^2)

    colnames(x) <- sub("[^.]+","us'us'",colnames(x))
    x@units <- rep("(m/s)^2",ncol(x))
    x
}

"dat.vs'vs'" <- function(what,derived=TRUE,cache=FALSE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x

    # calculate cross-stream variance
    u <- dat(sub(datvar,"u", what,fixed=TRUE),avg=TRUE,smooth=TRUE, derived=FALSE)
    v <- dat(sub(datvar,"v", what,fixed=TRUE),avg=TRUE,smooth=TRUE, derived=FALSE)
    x <- (v^2*dat(sub(datvar,"u'u'",what,fixed=TRUE), avg=TRUE,smooth=T) -
        2*u*v*dat(sub(datvar,"u'v'",what,fixed=TRUE), avg=TRUE,smooth=T) +
        u^2*dat(sub(datvar,"v'v'",what,fixed=TRUE), avg=TRUE,smooth=T))/ (u^2 + v^2)

    colnames(x) <- sub("[^.]+","vs'vs'",colnames(x))
    x@units <- rep("(m/s)^2",ncol(x))
    x
}

