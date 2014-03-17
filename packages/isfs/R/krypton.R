# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
#               Copyright (C) by UCAR
# 
dat.kryptonX <- function(what,derived=TRUE,...)
{

    # krypton pathlength, in cm
    default.val <- 1.3

    warning(paste("project dat.kryptonX not found for krypton path length. dat.kryptonX returning", default.val,"cm"))

    x <- dat(expand("kh2o",what),derived=F)
    sfxs <- suffixes(x,2)

    if (nrow(x) > 1) {
        tz <- c(tspar(x)[1],tspar(x)[nrow(x)])
        z <- matrix(rep(default.val,ncol(x)*2),nrow=2,byrow=T)
    }
    else {
        tz <- tspar(x)[1]
        z <- matrix(rep(default.val,ncol(x)),nrow=1,byrow=T)
    }
    dat(nts(z,tz,
            names=paste("kryptonX",sfxs,sep=""),
            units=rep("cm",ncol(x)),
            stations=stations(x)))
}

dat.kryptonKw <- function(what,derived=TRUE,...)
{

    # krypton Kw, water vapor absorption coefficient
    default.val <- -0.122

    warning(paste("project dat.kryptonKw not found for krypton water vapor absorption coef. dat.kryptonKw returning", default.val))

    x <- dat(expand("kh2o",what),derived=F)
    sfxs <- suffixes(x,2)

    if (nrow(x) > 1) {
        tz <- c(tspar(x)[1],tspar(x)[nrow(x)])
        z <- matrix(rep(default.val,ncol(x)*2),nrow=2,byrow=T)
    }
    else {
        tz <- tspar(x)[1]
        z <- matrix(rep(default.val,ncol(x)),nrow=1,byrow=T)
    }
    dat(nts(z,tz,
            names=paste("kryptonKw",sfxs,sep=""),
            units=rep("cm",ncol(x)),
            stations=stations(x)))
}

dat.kryptonKo <- function(what,derived=TRUE,...)
{

    # krypton Ko, oxygen absorption coefficient

    x <- dat(expand("kryptonX",what))

    select <- !is.na(x) & x < 1.95
    x[select] <- -3.4e-3
    select <- !is.na(x) & x >= 1.95
    x[select] <- -1.3e-3

    sfxs <- suffixes(x,2)

    dimnames(x)[[2]] <- paste("kryptonKo",sfxs,sep="")
    x
}

dat.kryptonSep <- function(what,derived=TRUE,...)
{
    # separation, in meters between krypton and sonic anemometer w axis

    default.val <- 0.3	# 30 centimeters

    warning(paste("project dat.kryptonSep not found for krypton<->sonic separation. dat.kryptonSep returning", default.val,"meters"))

    x <- dat(expand("kh2o",what),derived=F)
    if (is.null(x)) return(x)
    sfxs <- suffixes(x,2)

    if (nrow(x) > 1) {
        tz <- c(tspar(x)[1],tspar(x)[nrow(x)])
        z <- matrix(rep(default.val,ncol(x)*2),nrow=2,byrow=T)
    }
    else {
        tz <- tspar(x)[1]
        z <- matrix(rep(default.val,ncol(x)),nrow=1,byrow=T)
    }
    dat(nts(z,tz,
            names=paste("kryptonSep",sfxs,sep=""),
            units=rep("m",ncol(x)),
            stations=stations(x)))
}

dat.o2corr <- function(what,derived=TRUE,...)
{
    # compute oxygen correction for kryptons 
    # use ko from van Dijk, Kohsiek, and de Bruin, 2002, Table 2

    # o2corr = ko/kw*Co*Mo/Mw
    #
    # Then, to use o2corr:
    # w'h2o' = w'kh2o' + o2corr * rhoDry/T * w't'
    #
    # ko = kh2o absorption coefficient for oxygen
    #    = -3.4E-3 m^3/gm-cm, for pathlength x = 1.3 cm
    #    = -1.3E-3 m^3/gm-cm, for pathlength x = 2.6 cm
    #      previous version used ko = 7.14E-3 m^3/gm-cm
    # kw = kh2o absorption coefficient for water vapor, m^3/gm-cm
    #      (specific for each laboratory calibration)
    #      previous version used kw = -0.122 m^3/gm-cm
    # Co = 0.21 = fractional concentration of O2 in air
    # Mo = 32 gm/mole for O2
    # Ma = 28.97 gm/mole for dry air
    # Co*Mo/Ma = 0.23

    if (!is.null(dpar("robust")) && dpar("robust")) corr <- 0
    else {
        # get kw and x for each kh2o
        ko <- dat(expand("kryptonKo",what))
        kw <- dat(expand("kryptonKw",what))
        kw <- conform(kw,ko)
        corr <- 0.23*ko/kw
        dimnames(corr)[[2]] <- paste("o2corr",suffixes(corr),sep="")
        corr@units <- rep("",ncol(corr))
    }
    corr
}
