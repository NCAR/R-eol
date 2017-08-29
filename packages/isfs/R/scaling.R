# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

# Monin-Obukhov scaling parameters
# ----------------
#
dat.L <- function(what,derived=TRUE,k=dpar("vonKarman"),g=dpar("accelgrav"),...)
{
    # Monin_Obukhov length
    datvar <- datVar() # requested variable name, x of dat.x

    if (is.null(k)) k <- 0.4
    if (is.null(g)) g <- 9.81	# m/s^2

    x <- -(dat(sub(datvar,"tc",what,fixed=TRUE),avg=T,smooth=T)+273.15) *
        dat("u*",avg=T,smooth=T)^3/k/g /
        dat(sub(datvar,"w'tc'",what),avg=T,smooth=T)
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("L",suffixes(x,2),sep=""))
    x@units <- rep("m",ncol(x))
    x
}
"dat.u*" <- function(what,derived=TRUE,...)
{
    # calculate streamwise value of ustar

    datvar <- datVar() # requested variable name, x of dat.x

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

    u <- dat(sub(datvar,"u",what,fixed=TRUE),avg=T,smooth=T)
    v <- dat(sub(datvar,"v",what,fixed=TRUE),avg=T,smooth=T)
    uw <- dat(sub(datvar,"u'w'",what,fixed=TRUE),avg=T,smooth=T)
    vw <- dat(sub(datvar,"v'w'",what,fixed=TRUE),avg=T,smooth=T)

    # for negative values, sqrt generates a warning: NaNs produced
    x <- -(uw*u + vw*v)/sqrt(u^2+v^2)
    x[!is.na(x) & x < 0] <- NA_real_
    x <- sqrt(x)

    # When the wind data is not tilt-corrected, one can see a higher frequency 
    # of times when the dot-product within the square root is negative.
    if (dpar("robust")) {
        ib <- is.na(x)
        ustm <- (uw*uw + vw*vw)^0.25
        x[ib] <- ustm[ib]
    }
    dimnames(x) <- list(NULL,paste("u*",suffixes(x,2),sep=""))
    x@units <- rep("m/s",ncol(x))
    x
}
#
# neutral formulation of z0
#
dat.z0 <- function(what,derived=TRUE,k=dpar("vonKarman"),...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    if (is.null(k)) k <- 0.4
    x <- dat(sub(datvar,"spd",what,fixed=TRUE))

    z <- dat(sub(datvar,"heightSonic",what,fixed=TRUE))
    z <- conform(z,x)
    z <- approx(z,xout=tspar(x),method="constant",f=0,rule=2)

    d <- dat(sub(datvar,"D",what,fixed=TRUE))
    d <- conform(d,x)
    d <- approx(d,xout=tspar(x),method="constant",f=0,rule=2)

    x <- (z - d) * exp(-k*x/dat(sub(datvar,"u*",what,fixed=TRUE)))

    dimnames(x) <- list(NULL,rep("z0",ncol(x)))
    x@units <- rep("m",ncol(x))
    x
}
dat.z0raw <- function(what,derived=TRUE,k=dpar("vonKarman"),...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    # Calculates zo for neutral stability using both prop and sonic winds,
    # eliminating the need to know the displacement height
    if (is.null(k)) k <- 0.4
    U1 <- dat(sub(datvar,"spd",what,fixed=TRUE))
    U2 <- dat(sub(datvar,"Spd",what,fixed=TRUE))

    # We'll often get an error here for projects without a clear
    # pairing of sonics and props.
    U2 <- conform(U2,U1)

    z1 <- dat(sub(datvar,"heightSonic",what,fixed=TRUE))
    z1 <- conform(z1,U1)
    z1 <- approx(z1,xout=U1,method="constant",f=0,rule=2)

    z2 <- dat(sub(datvar,"heightProp",what,fixed=TRUE))
    z2 <- conform(z2,U2)
    z2 <- approx(z2,xout=U2,method="constant",f=0,rule=2)

    ustar <- dat(sub(datvar,"u*",what,fixed=TRUE))
    x <- (z2-z1)/(exp(k*U2/ustar) - exp(k*U1/ustar))

    check <- (U2 - U1) / (z2 - z1) > 0
    x@data[!check@data] <- NA_real_

    dimnames(x) <- list(NULL,rep("zoraw",ncol(x)))
    x@units <- rep("m",ncol(x))
    x
}

#
# neutral formulation of displacement height
#
dat.Draw <- function(what,derived=TRUE,k=dpar("vonKarman"),...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    if (is.null(k)) k <- 0.4
    U1 <- dat(sub(datvar,"spd",what,fixed=TRUE))
    U2 <- dat(sub(datvar,"Spd",what,fixed=TRUE))

    # We'll often get an error here for projects without a clear
    # pairing of sonics and props.
    U2 <- conform(U2,U1)

    z1 <- dat("heightSonic")
    z1 <- conform(z1,U1)
    z1 <- approx(z1,xout=U1,method="constant",f=0,rule=2)

    z2 <- dat("heightProp")
    z2 <- conform(z2,U2)
    z2 <- approx(z2,xout=U2,method="constant",f=0,rule=2)


    ustar <- dat(sub(datvar,"u*",what,fixed=TRUE))
    #  x <- z1 - (z2-z1)/(exp(k*(U2-U1)/ustar) - 1)
    x <- (z1*exp(-k*U1/ustar) - z2*exp(-k*U2/ustar)) /
        (exp(-k*U1/ustar) - exp(-k*U2/ustar))

    check <- (U2 - U1) / (z2 - z1) > 0
    x@data[!check@data] <- NA_real_
    # cat("number of !checks=",sum(!check),"\n")

    colnames(x) <- rep("Draw",ncol(x))
    x@units <- rep("m",ncol(x))
    x
}


# Default function for heightSonic
#
dat.heightSonic <- function(what,derived=TRUE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x

    warning('project version of dat("heightSonic") not found, heightSonic is being determined from names of u variable')

    x <- dat(sub(datvar,"u",what,fixed=TRUE),derived=F)
    z <- heights(x)
    if (any(is.na(z))) {
        noz <- is.na(z)
        warning(paste("sonic height not available for",
            paste(dimnames(x[,noz])[[2]],"(stn",stations(x[,noz]),")",collapse=",",sep=""),
            ". Setting to 2 meters"))
        z[noz] <- 2
    }

    sfxs <- suffixes(x,2)
    if (nrow(x) > 1) {
        tz <- c(tspar(x)[1],tspar(x)[nrow(x)])
        z <- matrix(rep(z,2),nrow=2,byrow=T)
    }
    else {
        tz <- tspar(x)[1]
    }
    dat(nts(z,tz,
            names=paste("heightSonic",sfxs,sep=""),
            units=rep("m",ncol(x)),
            stations=stations(x)))
}

dat.height.sonic <- function(what,derived=TRUE,...)
{
    stop('dat("height.sonic") is obsolete. Use dat("heightSonic")')
}

#
# Default function for heightProp
#
dat.heightProp <- function(what,derived=TRUE,...)
{
    warning("project version of dat.heightProp not found, heightProp is being determined from names of U variable")

    datvar <- datVar() # requested variable name, x of dat.x

    x <- dat(sub(datvar,"U",what,fixed=TRUE),derived=F)
    z <- heights(x)
    if (any(is.na(z))) {
        noz <- is.na(z)
        stop(paste("prop height not available for",
                paste(dimnames(x[,noz])[[2]],"(stn",stations(x[,noz]),")",collapse=",",sep="")))
    }

    sfxs <- suffixes(x,2)
    if (nrow(x) > 1) {
        tz <- c(tspar(x)[1],tspar(x)[nrow(x)])
        z <- matrix(rep(z,2),nrow=2,byrow=T)
    }
    else {
        tz <- tspar(x)[1]
    }
    dat(nts(z,tz,
            names=paste("heightProp",sfxs,sep=""),
            units=rep("m",ncol(x)),
            stations=stations(x)))
}

dat.height.prop <- function(what,derived=TRUE,...)
{
    stop('dat("height.prop") is obsolete. Use dat("heightProp")')
}

#
# Default function for displacement height
#
dat.D <- function(what,derived=TRUE,...) {

    warning("project dat.D not found for displacement height. dat.D returning 0s")
    datvar <- datVar() # requested variable name, x of dat.x
    x <- dat(sub(datvar,"u",what,fixed=TRUE),derived=F)
    sfxs <- suffixes(x)

    if (nrow(x) > 1) {
        tz <- c(tspar(x)[1],tspar(x)[nrow(x)])
        z <- matrix(rep(0,2*ncol(x)),nrow=2)
    }
    else {
        tz <- tspar(x)[1]
        z <- rep(0,ncol(x))
    }
    dat(nts(z,tz,
            names=paste("D",sfxs,sep=""),
            units=rep("m",ncol(x)),
            stations=stations(x)))
}
