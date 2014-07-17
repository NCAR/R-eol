# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

dat.azel.sun <- function(what,lat=dpar("lat"),lon=dpar("lon"),dt=dpar("avg"),
    cache=getOption("dcache"),...) {

    # if !missing(data), create dat object synchronized to data

    cacheName <- cache.name("azel.sun")
    if (is.null(dt)) dt <- 300
    found <- F
    if (check.cache(cacheName)) {
        x <- get.cache.object(cacheName)
        xlat <- x@attributes["latitude"]
        xlon <- x@attributes["longitude"]
        if (!is.null(xlat) && xlat == lat && !is.null(xlon) && xlon == lon &&
            deltat(x)[1] == dt) found <- T
    }
    if (!found) {
        if (round(dt)==300) 
            times <- as.numeric(seq(from=dpar("start")+150, to=dpar("end")-150,by=dt))
        else
            times <- as.numeric(seq(from=dpar("start"),to=dpar("end"),by=dt))

        if (is.null(lon)) {
            warning('dpar("lon") not set, using lon=NA')
            lon <- NA
        }
        # else if (lon > 0.0)
        #     warning(paste("lon=",lon, "is positive. If you want to specify a west longitude, it should be negative"))
        if (is.null(lat)) {
            warning('dpar("lat") not set, using lat=NA')
            lat <- NA
        }
        sunpos <- solar_lha_dec(times,lon)
        x <- solar_azm_elev(sunpos,lat)
        x <- nts(matrix(c(x$azm,x$elev),ncol=2,
                dimnames=list(NULL,c("az.sun","el.sun"))),
            times,units=c("deg","deg"))
        class(x) <- "dat"
        x@attributes <- c(x@attributes,latitude=lat,longitude=lon)
        if (!is.null(cache) && cache) cache.object(cacheName,x)
    }
    x
}
dat.az.sun <- function(what,lat=dpar("lat"),lon=dpar("lon"),dt=dpar("avg"),
    cache=getOption("dcache"),derived=TRUE,...) {

    x <- dat("azel.sun",lat=lat,lon=lon,dt=dt,cache=cache,derived=derived,...)
    x[,"az.sun"]

}
dat.el.sun <- function(what,lat=dpar("lat"),lon=dpar("lon"),dt=dpar("avg"),
    cache=getOption("dcache"),derived=TRUE,...) {

    x <- dat("azel.sun",lat=lat,lon=lon,dt=dt,cache=cache,derived=derived,...)
    x[,"el.sun"]

}
solar_lha_dec <- function(times,lon) {
    #
    # Computes the position of the sun to within .1 min of arc.   Provides
    # local hour angle (lha) and the earth's declanation (decl) given the
    # time in days from 1900 jan 0.5 (decimal)
    #
    #	lon: longitude in decimal degrees
    #
    # From A.T.Sinclair and B.D.Yallop  77-08-01
    #
    # From the C program sun_angle_accurate, converted 11 Jul 95 by Steve Oncley
    #
    #      time = no. of days sin 1900.0
    #      t       = no. of Julian centuries since 1900
    #      l       = mean longitude of sun
    #      e       = equation of time (in seconds)
    #      epsilon = obliquity of the ecliptic
    #      alpha   = right ascension of the sun (apparent)
    #
    deg2rad <- pi/180.
    rad2deg <- 180./pi

    # time is computed from:
    # 	seconds since 1970 converted to days 
    #	days between 1900 and 1970 (by running old "julian" on 1 Jan 1970 00:00
    #	gmt converted to days
    #
    hour <- (times %% 86400.) / 3600.

    times <- times/86400. + 25567.5;

    # Julian centuries since 1900 
    tc <- times/36525.

    l <- (( 279.697 + 36000.769*tc) %% 360) * deg2rad

    e <- 	- (93.0 + 14.23*tc - .0144*tc*tc) * sin(l) -
    (432.5 - 3.71*tc - .2063*tc*tc) * cos(l) +
    (596.9 - 0.81*tc - .0096*tc*tc) * sin(2.*l) -
    (  1.4 + 0.28*tc) * cos(2.*l) +
    (  3.8 + 0.60*tc) * sin(3.*l) +
    ( 19.5 - 0.21*tc - .0103*tc*tc) * cos(3.*l) -
    ( 12.8 - 0.03*tc) * sin(4.*l)

    # convert seconds of time to degrees
    # ...15 degrees <- 1 hour <- 3600 seconds
    e <- e/240.               

    epsilon.rad <- atan(.43382 - .00027*tc)
    alpha.deg   <- (l * rad2deg) - e
    alpha.rad   <- alpha.deg * deg2rad

    num        <- sin(epsilon.rad) * sin(alpha.rad)
    denom      <- cos(epsilon.rad)
    decl.rad   <- atan2( num, denom )
    decl.deg   <- decl.rad * rad2deg

    # greenwich hour angle
    gha.deg   <- ( hour*15. + e + 180.) %% 360
    # local hour angle
    lha.deg   <- ( gha.deg + lon) %% 360
    # in radians too
    lha.rad   <- lha.deg * deg2rad

    list(decl=decl.rad, lha=lha.rad)
}
solar_azm_elev <- function(sunpos,lat) {
    #
    # Function     AZM_ELEV
    #
    # From sun_angle_accurate.c, converted by Steve Oncley 11 Jul 95
    #
    rad2deg <- 180./pi
    deg2rad <- pi/180.

    lat.rad <- lat * deg2rad

    decl.rad <- sunpos$decl
    lha.rad <- sunpos$lha

    num     <- sin( lat.rad )*sin(decl.rad) +
    cos( lat.rad )*cos(decl.rad)*cos(lha.rad)
    denom   <- sqrt(1-num*num)
    elev    <- atan2(num, denom) * rad2deg

    num     <- cos(decl.rad)*sin(lha.rad)
    denom   <- cos( lat.rad )*sin(decl.rad) -
    sin( lat.rad )*cos(decl.rad)*cos(lha.rad)
    x       <- -num / denom
    azm     <- atan(x) * rad2deg
    #  if (num <  0. && denom >= 0.) azm <- azm + 360.
    ix <- (num < 0.) & (denom >= 0.)
    azm[ix] <- azm[ix] + 360.
    #  if (denom < 0.) azm <- azm + 180.
    ix <- (denom < 0.)
    azm[ix] <- azm[ix] + 180.
    azm     <- azm %% 360
    #  if (azm < 0.) azm <- azm + 360.
    ix <- (azm < 0.)
    azm[ix] <- azm[ix] + 360.

    list(azm=azm, elev=elev)
}
