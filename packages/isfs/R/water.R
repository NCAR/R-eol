# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

#
# Water vapor
# -----------
# NOTE that the following functions assume that dat("T"), dat("P"),
# and dat("RH") produce equally-dimensioned arrays of data

dat.satvp <- function(what,derived=TRUE,TC,frost=dpar("RH.ice"), enhancement=T,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    #  S function to calculate the saturation vapor pressure (mb) 
    #  of pure water vapor over a plane surface of water or ice (frost=T)
    #  as a function of temperature TC (deg C)
    #  Formulas from Buck, JAM 1981, pp 1527-1532
    #  Usage: dat("satvp")

    # formula ew_2, fit from 0C to 50C

    if (missing(TC)) TC <- dat(sub(datvar,"T",what,fixed=TRUE))

    sfx <- suffixes(TC,2)

    satvp <- 6.1121*exp(17.368*TC/(238.88+TC))

    select <- as.vector(!is.na(TC) & TC <= 0)
    TC <- as.vector(TC)[select]
    if (!is.null(frost) && !is.na(frost) && frost) 
        # formula e_i2, fit from -50C to 0C
        satvp[select] <- 6.1115*exp(22.452*TC/(272.55+TC))
    else
        # formula e_w3, fit from -40C to 0C
        satvp[select] <- 6.1121*exp(17.966*TC/(247.15+TC))

    # Multiply by an enhancement factor to account for air/h20 mixture.
    # Enhacement is also a f(P,T) but using constant=1.004 is 
    # generally commensurate with error of preceding formulas.
    if (enhancement) satvp <- 1.004*satvp

    dimnames(satvp) <- list(NULL,paste("satvp",sfx,sep=""))
    satvp@units <- rep("mb",ncol(satvp))
    satvp
}

dat.RH.ice <- function(what,derived=TRUE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x

    RH <- dat(sub(datvar,"RH",what,fixed=TRUE))
    TC <- conform(dat(sub(datvar,"T",what,fixed=TRUE)),RH)

    RH <- (RH * dat("satvp",TC=TC, frost = F))/dat("satvp",TC=TC, frost = T)
    colnames(RH) <- sub("[^.]+",datvar,colnames(RH))     # units are OK
    RH
}

dat.H2O <- function(what,derived=TRUE,RH,...)
{
    datvar <- datVar() # requested variable name, x of dat.x

    # Water vapor density (g/m^3)
    Rm <- 0.4617		# joules/gr-degK
    if (missing(RH)) RH <- dat(sub(datvar,"RH",what,fixed=TRUE))
    if (is.null(RH)) return(NULL)

    Td <- conform(dat("T"),RH)

    # eliminate super-saturation w.r.t. water
    check <- !is.na(RH) & RH > 100 
    if (any(check)) {
        warning(paste(round(100*length(RH[check])/length(RH),0), 
                "% of RH data super-saturated; set to 100%RH"))  
        RH[check] <- 100
    }
    # 100 pascals per millibar
    Pv <- dat("satvp",TC=Td) * RH		# pascals; satvp in mb, RH in %
    x <- Pv/Rm/(Td + 273.15)

    colnames(x) <- sub("[^.]+",datvar,colnames(x))
    x@units <- rep("g/m^3",ncol(x))
    x
}

dat.MR <- function(what,derived=TRUE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    # Mixing ratio (g/kg)
    RH <- dat(sub(datvar,"RH",what,fixed=TRUE))
    if (is.null(RH)) return(NULL)
    # eliminate super-saturation w.r.t. water
    test <- as.vector(RH)
    check <- !is.na(test) & test > 100 
    if (any(check)) {
        warning(paste(round(100*length(RH[check])/length(RH),0), 
                "% of RH data super-saturated; set to 100%RH"))  
        RH[check] <- 100
    }

    Td <- conform(dat("T"),RH)

    Pv <- dat("satvp",TC=Td) * RH / 100
    P <- dat("P")

    if (is.null(P)) {
        warning("No pressure data available")
        P <- dat(nts(matrix(NA_real_,ncol=ncol(Pv),nrow=nrow(Pv)),
                Pv@positions,stations=stations(Pv),
                names=paste("P",suffixes(Pv))))
    }

    if (ncol(P) > 1) P <- conform(P,Pv)

    x <- 622*Pv/(P - Pv)
    x[is.infinite(x)] <- NA_real_

    colnames(x) <- sub("[^.]+",datvar,colnames(x))
    x@units <- rep("g/kg",ncol(x))
    x
}

dat.mr <- function(what,derived=TRUE,...)
{
    # Mixing ratio (g/kg) from fast h2o (g/m^3)
    datvar <- datVar() # requested variable name, x of dat.x
    #
    # Added Nov 2009 by GDM. This calculation
    # uses a slow temperature, dat("T"), which
    # hopefully is available at or near the same
    # location as h2o.

    Rm <- 0.4617		# joules/gr-degK
    h2o <- dat(sub(datvar,"h2o",what,fixed=TRUE),...)

    Td <- conform(dat(sub(datvar,"T",what,fixed=TRUE)),h2o)

    Pv <- h2o * (Td + 273.15) * Rm / 100 # millibars
    P <- dat("P")

    if (is.null(P)) {
        warning("No pressure data available")
        P <- dat(nts(matrix(NA_real_,ncol=ncol(Pv),nrow=nrow(Pv)),
                Pv@positions,stations=stations(Pv),
                names=paste("P",suffixes(Pv))))
    }

    if (ncol(P) > 1) P <- conform(P,Pv)

    x <- 622*Pv/(P - Pv)
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("mr",suffixes(x,2),sep=""))
    x@units <- rep("g/kg",ncol(x))
    x
}

dat.Q <- function(what,derived=TRUE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    # Specific humidity (g/kg)
    RH <- dat(sub(datvar,"RH",what,fixed=TRUE))
    # eliminate super-saturation w.r.t. water
    check <- !is.na(RH@data) & RH@data > 100 
    if (any(check)) {
        warning(paste(round(100*length(RH[check])/length(RH),0), 
                "% of RH data super-saturated; set to 100%RH"))  
        RH@data[check] <- 100
    }
    Td <- conform(dat("T"),RH)

    Pv <- dat("satvp",TC=Td) * RH / 100

    P <- dat("P")
    if (is.null(P)) {
        warning("No pressure data available")
        P <- dat(nts(matrix(NA_real_,ncol=ncol(Pv),nrow=nrow(Pv)),
                Pv@positions,stations=stations(Pv),
                names=paste("P",suffixes(Pv),sep="")))
    }
    if (ncol(P) > 1) P <- conform(P,Pv)

    x <- 622*Pv/(P - 0.378*Pv)
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("Q",suffixes(x,2),sep=""))
    x@units <- rep("g/kg",ncol(x))
    x
}
dat.Tc <- function(what,derived=TRUE,...)
{

    # sonic virtual temperature (deg C)

    datvar <- datVar() # requested variable name, x of dat.x

    Td <- dat(sub(datvar,"T",what,fixed=TRUE))
    RH <- dat(sub(datvar,"RH",what,fixed=TRUE))
    RH <- conform(RH,Td)
    # eliminate super-saturation w.r.t. water
    check <- !is.na(RH) & RH > 100 
    if (any(check)) {
        warning(paste(round(100*length(RH[check])/length(RH),0), 
                "% of RH data super-saturated; set to 100%RH"))  
        RH[check] <- 100
    }


    Pv <- dat("satvp",TC=Td) * RH / 100	# mb
    P <- dat("P")

    P <- conform(P,Pv)

    # We're simply going to match suffixes and stations, ignoring height.
    # If any columns aren't matched, fill in with median of others...
    # Don't know what happens if more than one P per stn/sfx...
    sf <- suffixes(P)
    nw <- nwords(sf,sep=".")
    st <- stations(P)
    pp <- as.character(st)
    pp[st==0] <- words(sf,nw,nw)[st==0]
    sf <- suffixes(Pv)
    nw <- nwords(sf,sep=".")
    st <- stations(Pv)
    vp <- as.character(st)
    vp[st==0] <- words(sf,nw,nw)[st==0]
    im <- match(vp,pp,nomatch=0)
    i0 <- im==0
    im[i0] <- 1
    P <- P[,im]

    if (any(i0)) {
        warning(paste("Creating P at stations",
                paste(unique(stations(Pv)[im==0]),collapse=","),
                "from mean P of other stations"))
        Pa <- apply(P@data,1,mean,na.rm=T)
        P[,i0] <- Pa
        stations(P) <- stations(Pv)
    }

    x <- (Td + 273.15)*(1 + 0.32*Pv/P) - 273.15
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("Tc",suffixes(x,2),sep=""))
    x@units <- rep("degC",ncol(x))

    # Look for any non-derived Tc, such as from a 2-d sonic
    if (any(words(variables(),1,1) == "Tc")) {
        x2 <- dat(sub(datvar,"Tc",what,fixed=TRUE),derived=F)
        x <- Cbind(x,x2)
    }
    x
}

dat.Tdew <- function(what,derived=TRUE,...)
{
    # For now, this calculation ignores frost/below 0 issues
    # and simply inverts the formula for satvp above:
    #  satvp <- 6.1121*exp(17.368*TC/(238.88+TC))

    datvar <- datVar() # requested variable name, x of dat.x

    RH <- dat(sub(datvar,"RH",what,fixed=TRUE))
    Td <- conform(dat("T"),RH)
    vp <- dat("satvp",TC=Td) * RH / 100
    x <- 238.88/( (17.368/log(vp/6.1121))-1 )	
    x[is.infinite(x)] <- NA_real_

    dimnames(x) <- list(NULL,paste("Tdew",suffixes(x,2),sep=""))
    x@units <- rep("degC",ncol(x))
    x
}

dat.evap <- function(what,derived=TRUE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x

    wr <- clip(dat(sub(datvar,"w'mr'",what,fixed=TRUE)))
    wr[is.na(wr)] <- 0

    rhoDry <- dat(sub(datvar,"rhoDry",what,fixed=TRUE))
    if (inherits(rhoDry,"dat")) rhoDry <- clip(rhoDry)
    rhoDry[is.na(rhoDry)] <- 0

    evap <- wr*0
    rhoDry <- conform(rhoDry,evap)

    for (i in 1:ncol(wr)) {

        # if dpar(robust=T) dat("rhoDry") is one number
        if (length(dim(rhoDry)) == 2) rho <- rhoDry[,i]
        else rho <- rhoDry

        x <- cumsum(wr[,i]*rho)*c(0,diff(tspar(wr)))/1000
        evap[,i] <- x
    }

    colnames(evap) <- sub("[^.]+",datvar,colnames(wr))
    evap@units <- rep("mm",ncol(evap))
    evap
}

dat.h2o <- function(what,derived=TRUE,...) 
{
    datvar <- datVar() # requested variable name, x of dat.x

    robust <- dpar("robust")
    if (is.null(robust)) robust <- TRUE

    kh2o <- h2o <- NULL

    wvb <- words(variables(),1,1)

    if (any(wvb == "kh2o")) {
        h2o.name <- sub(datvar,"kh2o",what,fixed=TRUE)

        kh2o <- dat(h2o.name,derived=FALSE,avg=FALSE,smooth=FALSE)	# g/m^3
        if (!is.null(kh2o) && !robust) {
            # dat("o2corr") can return a simple 0, rather than a time series
            # In that case, conform will just return that 0 again.
            o2corr <- dat(sub(datvar,"o2corr",what,fixed=TRUE))
            if (is(o2corr,"dat")) {
                o2corr <- conform(o2corr,kh2o)
                o2corr <- approx(o2corr,xout=kh2o,method="constant",f=0,rule=2)
            }
            rhod <- conform(dat("rhoDry",avg=FALSE,smooth=FALSE),kh2o)
            kh2o <- kh2o - o2corr * rhod
        }
    }
    # read li7500 data
    if (any(wvb == "h2o")) {
        h2o.name <- sub(datvar,"h2o",what,fixed=TRUE)
        h2o <- dat(h2o.name,derived=FALSE,avg=FALSE,smooth=FALSE)
    }
    # combine
    h2o <- Cbind(kh2o,h2o)
    
    if (!is.null(h2o)) {
        sfxs <- suffixes(h2o,2)
        colnames(h2o) <- paste0("h2o",sfxs)
    }
    h2o
}

