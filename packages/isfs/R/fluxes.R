# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
#               Copyright (C) by UCAR
# 
#
# Turbulent fluxes
# ----------------
#
calc.x.t <- function(what,robust=dpar("robust"))
{
    MH2O = 18
    # compute u't', v't', or w't', as specified by the what argument
    xt.name = words(what,1,1,sep=".")	# get x't' prefix
    comp = words(xt.name,1,1,sep="'")	# get x

    xtc.name <- expand(paste(comp,"tc'",sep="'"),what)	# x'tc' where x is u,v or w
    xt <- dat(xtc.name,avg=T,smooth=T)
    sfxs <- suffixes(xt,2)

    if (!is.null(robust) && !robust) {

        xkh2o=NULL
        xh2o=NULL
        o2corr=NULL
        lo2corr=NULL
        scorr=NULL
        lscorr=NULL
        wvb = words(variables(),1,1)

        # read krypton data
        if (sum(wvb=="kh2o")>0) {
            # Assume E measured as <w'h2o'> (m/s g/m^3)
            xh.name <- expand(paste(comp,"kh2o'",sep="'"),what)# x'kh2o'

            # specify derived=F, so make sure we get the uncorrected x'kh2o'
            xkh2o <- dat(xh.name,derived=F,avg=T,smooth=T)	# m/s g/m^3
            if (!is.null(xkh2o)) {
                o2corr <- conform(dat("o2corr"),xkh2o)
                # dat("o2corr") can return a simple 0, rather than a time series
                if (!is.null(dim(o2corr)))
                    o2corr <- approx(o2corr,xout=tspar(xkh2o),method="constant",f=0,rule=2)
                else warning(paste("In calc.x.t: krypton oxygen correction (o2corr) is fixed at ",o2corr))
                scorr = dat("Scorr",which="krypton",avg=T,smooth=T)
                if (!is.null(dim(scorr))) scorr <- conform(scorr,xkh2o)
            }
        }
        # read li7500 data
        if (sum(wvb=="h2o")>0) {
            xh.name <- expand(paste(comp,"h2o'",sep="'"),what)# x'h2o'
            # specify derived=F, so that oxygen and spatial corrections
            # are not done in this call to dat
            xh2o <- dat(xh.name,derived=F,avg=T,smooth=T) # m/s mmol/m^3
            if (!is.null(xh2o)) {
                # ...convert units
                cvt = units(xh2o) == "m/s mmol/m^3"
                if (any(cvt)) {
                    xh2o[,cvt] = xh2o[,cvt]*MH2O*0.001
                    xunits = units(xh2o)
                    xunits[cvt] =  rep("m/s g/m^3",sum(cvt))
                    units(xh2o) = xunits
                }
                lo2corr = xh2o*0
                units(lo2corr) = rep("",ncol(xh2o))
                lscorr <- dat("Scorr",which="other",avg=T,smooth=T)
                if(!is.null(dim(lscorr))) lscorr <- conform(lscorr,xh2o)
            }
        }
        # combine
        #
        # correction for spatial separation between sonic and fast hygrometer
        xh2o = Cbind(xkh2o*scorr,xh2o*lscorr)

        # delete data when the fraction of expected sonic samples is lower than sfrac.min
        if (!is.null(dpar("sfrac.min")) && existsFunction("dat.sfrac")) {
            sonicbad = !is.na(xh2o) & (conform(dat("sfrac",avg=T,smooth=T),xh2o) < dpar("sfrac.min"))
            if (any(!is.na(sonicbad) & sonicbad)) {
                rmd = apply(sonicbad,2,function(x){sum(x,na.rm=T)/length(x) * 100})
                cat(paste("calc.x.t: percentage of flux data removed due to dat(\"sfrac\") < dpar(sfrac.min=",
                        dpar("sfrac.min"),"):",
                        paste(dimnames(xh2o)[[2]],"(",round(rmd,1),")",sep="",collapse=", "),"\n"))
                xh2o[sonicbad] = NA_real_
            }
        }

        o2corr = Cbind(o2corr,lo2corr)
        if (T) {
            allna = apply(xh2o,2,function(x)all(is.na(x)))
            if (!all(allna)) {
                xh2o = xh2o[,!allna]
                o2corr = o2corr[,!allna]
            }
        }

        if (any(xh2o@units != "m/s g/m^3" & xh2o@units != "m/s gm/m^3"))
            warning("units of <w'kh2o'> are not m/s g/m^3")

        # Corrections to <w'tc'> assume E measured as <w'kh2o'> (m/s g/m^3)
        # Correct <w'tc'> for <w'h2o'> per Schotanus et al., BLM, 1983
        # Correct E for <w't'> per Webb et al., QJRMS, 1980
        # Correct E for <w't'> (oxygen correction)
        # Correct E for <w'kh2o'> for spatial separation

        if (!identical(dim(xh2o),dim(xt))) {
            warning("in calc.x.t, dim(xh2o)=",paste(dim(xh2o),collapse=",")," is not equal to dim(xt)",paste(dim(xt),collapse=","),". Doing conform(xt,xh2o)")
            xt <- conform(xt,xh2o)
        }

        xh2o <- conform(xh2o,xt) * 1.e-3
        o2corr <- conform(o2corr,xt) * 1.e-3

        sfxs <- suffixes(xh2o,2)

        rho.air <- conform(dat("rho.air",avg=T,smooth=T),xt)
        Q <- conform(dat("Q",avg=T,smooth=T),xt) * 1e-3
        TK <- conform(dat("T",avg=T,smooth=T),xt) + 273.15

        if (comp == "w") {
            factor <- 0.51*(1 + Q*(1/0.622-1))
            xt <- (xt - factor * TK / rho.air * xh2o)/
            (1 + factor * (Q + o2corr * (1-Q)))
        }
        else {
            factor <- 0.51*(1 - Q)
            xt <- (xt - factor * TK / rho.air * xh2o)/
            (1 + factor * o2corr * (1-Q))
        }

        xt@data[is.infinite(xt@data)] <- NA_real_
    }

    dimnames(xt) <- list(NULL,paste(xt.name,sfxs,sep=""))
    xt@units <- rep("m/s degK",ncol(xt))
    xt
}

"dat.u't'" <- function(what,robust=dpar("robust"),...)
{
    calc.x.t(what,robust=robust)
}

"dat.v't'" <- function(what,robust=dpar("robust"),...)
{
    calc.x.t(what,robust=robust)
}

"dat.w't'" <- function(what,robust=dpar("robust"),...)
{
    calc.x.t(what,robust=robust)
}

calc.x.mr <- function(what,robust=dpar("robust"))
{
    MH2O = 18
    xmr.name = words(what,1,1,sep=".")
    comp = words(xmr.name,1,1,sep="'")
    xkh2o=NULL
    xh2o=NULL
    o2corr=NULL
    lo2corr=NULL
    scorr=NULL
    lscorr=NULL
    wvb = words(variables(),1,1)
    # read krypton data
    if (sum(wvb=="kh2o")>0) {
        # Assume E measured as <w'h2o'> (m/s g/m^3)
        xh.name <- expand(paste(comp,"kh2o'",sep="'"),what)# x'kh2o'

        # specify derived=F, so make sure we get the uncorrected x'kh2o'
        xkh2o <- dat(xh.name,derived=F,avg=T,smooth=T)	# m/s g/m^3
        if (!is.null(xkh2o)) {
            o2corr <- conform(dat("o2corr"),xkh2o)
            # dat("o2corr") can return a simple 0, rather than a time series
            if (!is.null(dim(o2corr)))
                o2corr <- approx(o2corr,xout=tspar(xkh2o),method="constant",f=0,rule=2)
            scorr <- conform(dat("Scorr",which="krypton",avg=T,smooth=T),xkh2o)
        }
    }
    # read li7500 data
    if (sum(wvb=="h2o")>0) {
        xh.name <- expand(paste(comp,"h2o'",sep="'"),what)# x'h2o'

        # specify derived=F, so that oxygen and spatial corrections
        # are not done in this call to dat
        xh2o <- dat(xh.name,derived=F,avg=T,smooth=T) # m/s mmol/m^3
        if (!is.null(xh2o)) {
            # ...convert units
            cvt = units(xh2o) == "m/s mmol/m^3"
            if (any(cvt)) {
                xh2o[,cvt] = xh2o[,cvt]*MH2O*0.001
                xunits = units(xh2o)
                xunits[cvt] =  rep("m/s g/m^3",sum(cvt))
                units(xh2o) = xunits
            }
            lo2corr = xh2o*0
            units(lo2corr) = rep("",ncol(xh2o))
            lscorr <- conform(dat("Scorr",which="other",avg=T,smooth=T),xh2o)
        }
    }
    # combine
    xh2o = Cbind(xkh2o,xh2o)
    o2corr = Cbind(o2corr,lo2corr)
    scorr = Cbind(scorr,lscorr)
    if (T) {
        allna = apply(xh2o,2,function(x)all(is.na(x)))
        if (!all(allna)) {
            xh2o = xh2o[,!allna]
            o2corr = o2corr[,!allna]
            scorr = scorr[,!allna]
        }
    }

    # o2corr <- approx(o2corr,xout=tspar(xh2o),method="constant",f=0,rule=2)

    if (any(xh2o@units != "m/s g/m^3" & xh2o@units != "m/s gm/m^3"))
        warning("units of <w'kh2o'> are not m/s g/m^3")
    sfxs <- suffixes(xh2o,2)

    if (!is.null(robust) && !robust) {

        xtc.name <- expand(paste(comp,"tc'",sep="'"),what)	# x'tc' where x is u,v or w
        # Correct <w'kh2o'> for spatial separation 
        # Correct E for <w't'> (oxygen correction)
        # Correct <w'tc'> for <w'h2o'> per Schotanus et al., BLM, 1983
        # Correct E for <w't'> per Webb et al., QJRMS, 1980

        # correction for spatial separation between sonic and fast hygrometer
        xh2o <- xh2o * scorr

        rhod <- conform(dat("rhoDry",avg=T,smooth=T),xh2o) * 1e3	# g/m^3
        xmr = xh2o/rhod
        Q <- conform(dat("Q",avg=T,smooth=T),xmr) * 1.e-3
        MR <- Q/(1-Q)
        TK <- conform(dat("T",avg=T,smooth=T),xmr) + 273.15

        xtc <- conform(dat(xtc.name,avg=T,smooth=T),xmr)

        # delete data when the fraction of expected sonic samples is lower than sfrac.min
        if (!is.null(dpar("sfrac.min")) && existsFunction("dat.sfrac")) {
            sonicbad = !is.na(xmr) & (conform(dat("sfrac",avg=T,smooth=T),xmr) < dpar("sfrac.min"))
            if (any(!is.na(sonicbad) & sonicbad)) {
                rmd = apply(sonicbad,2,function(x){sum(x)/length(x) * 100})
                cat(paste("calc.x.mr: percentage of flux data removed due to dat(\"sfrac\") < dpar(sfrac.min=",
                        dpar("sfrac.min"),"):",
                        paste(dimnames(xh2o)[[2]],"(",round(rmd,1),")",sep="",collapse=", "),"\n"))
                xmr[sonicbad] = NA_real_
            }
        }

        if (comp == "w") {
            factor <- 0.51*(1 + Q*(1/0.622-1))
            xmr <- (1 + MR/0.622)*(xmr + (MR + o2corr) * xtc / TK)/
            (1 + factor*(Q + o2corr*(1-Q)))
        }
        else {
            factor <- 0.51*(1-Q)
            xmr <- (xmr + o2corr * xtc / TK) / (1 + factor*o2corr*(1-Q))
        }
        xmr@data[is.infinite(xmr@data)] <- NA_real_
    }

    xmr <- xmr * 1.e3	# m/s g/kg

    dimnames(xmr) <- list(NULL,paste(xmr.name,sfxs,sep=""))
    xmr@units <- rep("m/s g/kg",ncol(xmr))
    xmr
}
"dat.u'mr'" <- function(what,robust=dpar("robust"),...)
{
    calc.x.mr(what,robust=robust)
}

"dat.v'mr'" <- function(what,robust=dpar("robust"),...)
{
    calc.x.mr(what,robust=robust)
}

"dat.w'mr'" <- function(what,robust=dpar("robust"),...)
{
    calc.x.mr(what,robust=robust)
}

calc.x.h2o <- function(what,robust=dpar("robust"))
{
    MH2O = 18
    xh2o.name = words(what,1,1,sep=".")
    comp = words(xh2o.name,1,1,sep="'")

    xkh2o=NULL
    xh2o=NULL
    o2corr=NULL
    lo2corr=NULL
    scorr=NULL
    lscorr=NULL
    wvb = words(variables(),1,1)
    # read krypton data
    if (sum(wvb=="kh2o")>0) {
        xh.name <- expand(paste(comp,"kh2o'",sep="'"),what)# x'kh2o'
        xkh2o <- dat(xh.name,avg=T,smooth=T)	# m/s g/m^3
        if (!is.null(xkh2o)) {
            # dat("o2corr") can return a simple 0, rather than a time series
            # In that case, conform will just return that 0 again.
            o2corr <- conform(dat("o2corr"),xkh2o)
            if (!is.null(dim(o2corr)))
                o2corr <- approx(o2corr,xout=tspar(xkh2o),method="constant",f=0,rule=2)
            scorr <- conform(dat("Scorr",which="krypton",avg=T,smooth=T),xkh2o)
        }
    }
    # read li7500 data
    if (sum(wvb=="h2o")>0) {
        xh.name <- expand(paste(comp,"h2o'",sep="'"),what)# x'h2o'
        xh2o <- dat(xh.name,derived=F,avg=T,smooth=T) # m/s mmol/m^3
        if (!is.null(xh2o)) {
            # ...convert units
            cvt = units(xh2o) == "m/s mmol/m^3"
            if (any(cvt)) {
                xh2o[,cvt] = xh2o[,cvt]*MH2O*0.001
                xunits = units(xh2o)
                xunits[cvt] =  rep("m/s g/m^3",sum(cvt))
                units(xh2o) = xunits
            }
            lo2corr = xh2o*0
            units(lo2corr) = rep("",ncol(xh2o))
            lscorr <- conform(dat("Scorr",which="other",avg=T,smooth=T),xh2o)
        }
    }
    # combine
    xh2o = Cbind(xkh2o,xh2o)

    # lo2corr is either NULL or a time series of 0's
    if (is.null(o2corr))
        o2corr = 0
    else if (is(o2corr,"nts") && is(lo2corr,"nts"))
        o2corr = Cbind(o2corr,lo2corr)

    scorr = Cbind(scorr,lscorr)
    if (T) {
        allna = apply(xh2o,2,function(x)all(is.na(x)))
        if (!all(allna)) {
            xh2o = xh2o[,!allna]
            if (is(o2corr,"nts")) o2corr = o2corr[,!allna]
            scorr = scorr[,!allna]
        }
    }
    if (any(xh2o@units != "m/s g/m^3" & xh2o@units != "m/s gm/m^3"))
        warning("units of <w'kh2o'> are not m/s g/m^3")
    sfxs <- suffixes(xh2o,2)
    # browser()

    if (!is.null(robust) && !robust) {
        xtc.name <- expand(paste(comp,"tc'",sep="'"),what)	# x'tc' where x is u,v or w
        # Correct <w'kh2o'> for spatial separation 
        # Correct <w'tc'> for <w'h2o'> per Schotanus et al., BLM, 1983
        # Correct E for <w't'> per Webb et al., QJRMS, 1980
        # Correct E for <w't'> (oxygen correction) 
        #
        # delete data when the fraction of expected sonic samples is lower than sfrac.min
        if (!is.null(dpar("sfrac.min")) && existsFunction("dat.sfrac")) {
            sonicbad = !is.na(xh2o) & (conform(dat("sfrac",avg=T,smooth=T),xh2o) < dpar("sfrac.min"))
            if (any(!is.na(sonicbad) & sonicbad)) {
                rmd = apply(sonicbad,2,function(x){sum(x)/length(x) * 100})
                cat(paste("calc.x.h2o: percentage of flux data removed due to dat(\"sfrac\") < dpar(sfrac.min=",
                        dpar("sfrac.min"),"):",
                        paste(dimnames(xh2o)[[2]],"(",round(rmd,1),")",sep="",collapse=", "),"\n"))
                xh2o[sonicbad] = NA_real_
            }
        }

        # correction for spatial separation between sonic and fast hygrometer
        xh2o <- xh2o * scorr

        rhod <- conform(dat("rhoDry",avg=T,smooth=T),xh2o) * 1e3	# g/m^3
        Q <- conform(dat("Q",avg=T,smooth=T),xh2o) * 1e-3	# kg/kg
        TK <- conform(dat("T",avg=T,smooth=T),xh2o) + 273.15

        xtc <- conform(dat(xtc.name,avg=T,smooth=T),xh2o)

        if (comp == "w") {
            factor <- 0.51*(1 + Q*(1/0.622-1))
            xh2o <- ( (1 + factor*Q) * xh2o + o2corr * xtc * rhod / TK )/
            (1 + factor*(Q + o2corr * (1-Q)))
        }
        else {
            factor <- 0.51*(1-Q)
            xh2o <- (xh2o + o2corr * xtc * rhod / TK)/
            (1 + factor* o2corr * (1-Q))
        }
        xh2o@data[is.infinite(xh2o@data)] <- NA_real_
    }

    dimnames(xh2o) <- list(NULL,paste(xh2o.name,sfxs,sep=""))
    xh2o@units <- rep("m/s g/m^3",ncol(xh2o))
    xh2o
}

"dat.u'h2o'" <- function(what,robust=dpar("robust"),...)
{
    calc.x.h2o(what,robust=robust)
}

"dat.v'h2o'" <- function(what,robust=dpar("robust"),...)
{
    calc.x.h2o(what,robust=robust)
}

"dat.w'h2o'" <- function(what,robust=dpar("robust"),...)
{
    calc.x.h2o(what,robust=robust)
}


calc.H <- function(wt,wq,Td,Q,rho)
{
    # Turbulent sensible heat flux
    # Constant values from Smithsonian Meteorological Tables:
    Cpd <- 1006	# for dry air, range is 1004.8-1007.4 from -50 to +50 C and
    # 700 to 1100 mb, J/kg-K
    Cpv <- 1857	# for water vapor, value for 0 C, range is 1850 to 1870 
    # from -50 to +50C, J/kg-K

    # No fast water covariance
    if (missing(wq)) {
        Hname <- "H.dry"
        Q <- conform(Q,wt)/1000   # kg/kg
        # Stull, eq. 10.7.1b
        Cp <- Cpd*(1 + 0.84*Q)
        x <- Cp * conform(rho,wt) * wt
    }
    else {
        Hname <- "H"
        wq <- conform(wq,wt)
        # Stull, eq. 10.7.1c or Fleagle & Businger (2nd ed) eq. 6.23, 
        # using Cpd&Cpv directly
        x <- conform(rho,wt) * (Cpd * wt + Cpv * conform(Td,wt) * wq)
    }

    if (!is.null(x)) {
        dimnames(x) <- list(NULL,paste(Hname,suffixes(wt,2),sep=""))
        x@units <- rep("W/m^2",ncol(x))
    }
    x
}
dat.H <- function(what,robust=dpar("robust"),...)
{
    # Turbulent sensible heat flux
    Td <- dat("T",avg=T,smooth=T)

    if (is.null(robust) || robust)
        Td[is.na(Td)] <- median(Td,na.rm=T)

    wq <- dat(expand("w'h2o'",what),avg=T,smooth=T)*1e-3
    wt <- dat(expand("w't'",what),avg=T,smooth=T)

    x <- calc.H(wt=wt,wq=wq,Td=Td,rho=dat("rho.air",avg=T,smooth=T))
    x
}
dat.H.dry <- function(what,robust=dpar("robust"),...)
{
    # Turbulent sensible heat flux without contribution from varying Cp 

    Q <- dat("Q",avg=T,smooth=T)
    if (is.null(robust) || robust) {
        if (all(is.na(Q)))
            Q <- 0
        else
            Q <- median(Q, na.rm=T)
    }
    x <- calc.H(wt=dat(expand("w't'",what),avg=T,smooth=T),
        Q=dat("Q",avg=T,smooth=T),rho=dat("rho.air",avg=T,smooth=T))
    x
}


dat.LE <- function(what,robust=dpar("robust"),...)
{
    # Turbulent latent heat flux
    x <- dat(expand("w'mr'",what),avg=T,smooth=T)
    sfxs <- suffixes(x,2)

    x <- conform(dat(expand("Lv",what),avg=T,smooth=T),x) *
    conform(dat("rhoDry",avg=T,smooth=T),x) * x * 1e-3

    dimnames(x) <- list(NULL,paste("LE",sfxs,sep=""))
    x@units <- rep("W/m^2",ncol(x))
    x
}
dat.Lv <- function(what,robust=dpar("robust"),...)
{
    # Latent heat of vaporatizaton of water is a weak function of temperature
    # (varies by 1% in 10 C)
    # ref:  Handbook of Hydrology, Maidment, Ed., Eq 4.2.1 
    # [I haven't actually seen this book, but have a quotation]
    # values from http://www.eos.uoguelph.ca/webfiles/wjames/wj365m04.html
    # were used previously, but conversion from calories gave slightly different
    # numerical values

    if (!is.null(robust) && !robust) {
        x <- 2501000 - 2361 * dat(expand("T",what))
        dimnames(x) <- list(NULL,paste("Lv",suffixes(x,2),sep=""))
        x@units <- rep("J/kg",ncol(x))
    }
    else x <- 2.5e6
    x
}
dat.BR <- function(what,...)
{
    # Bowen ratio
    x <- dat(expand("H",what),avg=T,smooth=T) / 
    dat(expand("LE",what),avg=T,smooth=T)
    x@data[is.infinite(x@data)] <- NA_real_

    dimnames(x) <- list(NULL,paste("BR",suffixes(x,2),sep=""))
    x@units <- rep("",ncol(x))
    x
}
dat.Scorr <- function(what,which="krypton",robust=dpar("robust"),...)
{
    # Correct covariance for spatial separation of sonic and scalar sensor
    # Assumes separation parallel to the sonic u axis and 
    # cospectrum with -2 slope in inertial subrange
    #
    # corr = exp(2*pi*n_m*x/(z-d))
    # x = spatial separation (m)
    # z = sonic height agl (m)
    # d = displacement height of wind profile (m)
    # n_m = dimensionless frequency of peak of flux cospectrum
    #     = function of z/L from Horst, BLM, projected 2008

    if (is.null(robust) || robust) {
        S = dat("heightSonic")
        sfxs <- suffixes(S, 2)
        S@data = matrix(1,nrow(S),ncol(S))
    }
    else {

        # first get the separation. Conform everything to it
        # It is then approximated to zoL later.
        if (which == "krypton") S <- dat(expand("kryptonSep",what))
        else if (which == "licor") S <- dat(expand("licor7500Sep",what))
        else S <- dat(expand("fastH2OSep",what))

        # convert separation from cm to m
        if (!is.null(units(S))) {
            cmx = !is.na(match(units(S),"cm"))
            if (any(cmx)) S[,cmx] = S[,cmx] / 100.0
        }

        if (is.null(S)) return(1)

        zoL <- conform(dat(expand("L",what),avg=T,smooth=T),S)
        sfxs <- suffixes(zoL,2)

        # need z and d for each sonic
        z <- conform(dat(expand("heightSonic",what)),S)
        z <- approx(z,xout=tspar(zoL),method="constant",f=0,rule=2)

        d <- conform(dat(expand("D",what)),z)
        # Note linear interpolation for D
        d <- approx(d,xout=tspar(z),method="linear",rule=3)
        z = z - d

        zoL <- z / zoL	

        nmx = zoL	# make a copy of the time series
        select <- !is.na(zoL@data) & zoL@data <= -0.1
        nmx@data[select] <- 0.065
        # Horst, Lenschow, 2009: Corrected nmx values, 5/11/2009
        nmx@data[!select] <- 2.31 - 2.240 / (1.015 + 0.15 * zoL@data[!select])^2


        nmy = zoL	# make a copy of the time series
        select <- !is.na(zoL@data) & zoL@data <= -0.05
        nmy@data[select] <- 0.15
        nmy@data[!select] <- 2.43 - 2.28 / (1.01 + 0.2 * zoL@data[!select])^2

        # Need u,v in instrument coordinates.
        check.windcoords()
        rcoords = dpar("coords")
        if (rcoords != "instrument") {
            on.exit(dpar(coords=rcoords),add=T)
            dpar(coords="instrument")
        }

        u = conform(dat(expand("u",what),avg=T,smooth=T),zoL)
        v = conform(dat(expand("v",what),avg=T,smooth=T),zoL)
        spd = sqrt(u^2 + v^2)

        nm = sqrt((nmx * u/spd)^2 + (nmy * v/spd)^2)
        nm = conform(nm,S)
        S <- approx(S,xout=tspar(nm),method="constant",f=0,rule=2)

        S <- exp(2 * pi * nm * S / z)
    }
    dimnames(S) = list(NULL,paste("Scorr",sfxs,sep=""))
    S@units <- rep("",ncol(S))
    S
}

dat.TKE <- function(cache=F,...)
{
    x <- sqrt(dat("u'u'")^2 + dat("v'v'")^2 + dat("w'w'")^2)/2

    dimnames(x) <- list(NULL,paste("TKE",suffixes(x,2),sep=""))
    x@units <- rep("(m/s)^2",ncol(x))
    x
}
