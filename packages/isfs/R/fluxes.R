# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.
#
# Turbulent fluxes
# ----------------
#
calc.x.t <- function(datvar,what)
{

    MH2O <- 18
    # compute u't', v't', or w't', as specified by the what argument
    comp <- words(datvar,1,1,sep="'")	# get u, v or w

    # x'tc' where x is u,v or w
    xtc.name <- sub(datvar,paste(comp,"tc'",sep="'"),what,fixed=TRUE)

    # Note avg=TRUE, which means x'tc' is computed at the
    # resolution specified in avg, and then corrections are
    # applied here, after the averaging.
    xt <- dat(xtc.name,avg=TRUE,smooth=TRUE)

    robust <- dpar("robust")
    if (is.null(robust)) robust <- TRUE

    if (robust) {   # no conversion from x'tc' to x't'
        colnames(xt) <- sub("[^.]+",datvar,colnames(xt))
        return(xt)
    }

    # Have h2o flux corrections been done?
    fcorr_done <- dpar("h2o_flux_corrected")
    fcorr_done <- !is.null(fcorr_done) && as.logical(fcorr_done)

    xkh2o <- xh2o <- o2corr <- lo2corr <- NULL

    wvb <- words(variables(),1,1)

    xh.name <- paste(comp,"kh2o'",sep="'")  # x'kh2o'

    if (!fcorr_done && any(wvb == xh.name)) {

        # Assume E measured as <w'h2o'> (m/s g/m^3)

        # derived=FALSE, but there isn't a dat.x.kh2o so it's not really necessary
        # Note avg=TRUE, which means x'kh2o' is computed at the
        # resolution specified in avg, and then the o2 and spatial
        # corrections are applied here, after the averaging.
        xh.name <- sub(datvar,xh.name,what,fixed=TRUE)
        xkh2o <- dat(xh.name,derived=FALSE,avg=TRUE,smooth=TRUE)	# m/s g/m^3

        if (!is.null(xkh2o)) {

            allna <- apply(xkh2o,2,function(x)all(is.na(x)))
            if (!all(allna)) xkh2o <- xkh2o[,!allna]

            # dat("o2corr") can return a simple zero, rather than a time series
            o2corr <- dat(sub(datvar,"o2corr",what,fixed=TRUE))
            if (is(o2corr,"dat")) {
                o2corr <- conform(o2corr,xkh2o)
                o2corr <- approx(o2corr,xout=xkh2o,method="constant",f=0,rule=2)
            }

            scorr <- dat(sub(datvar,"Scorr",what,fixed=TRUE),which="krypton",avg=TRUE,smooth=TRUE)
            if (is(scorr,"dat")) scorr <- conform(scorr,xkh2o)
            # correction for spatial separation between sonic and fast hygrometer
            if (!is.null(scorr)) xkh2o <- xkh2o * scorr
        }
    }
    # read li7500 data
    xh.name <- paste(comp,"h2o'",sep="'")   # x'h2o'

    if (any(wvb == xh.name)) {
        xh.name <- sub(datvar,xh.name,what,fixed=TRUE)
        # specify derived=FALSE, to avoid infinite loop of calling this derived function
        xh2o <- dat(xh.name,derived=FALSE,avg=TRUE,smooth=TRUE) # m/s mmol/m^3

        if (!is.null(xh2o)) {

            allna <- apply(xh2o,2,function(x)all(is.na(x)))
            if (!all(allna)) xh2o <- xh2o[,!allna]

            # ...convert units
            cvt <- units(xh2o) == "m/s mmol/m^3"
            if (any(cvt)) {
                xh2o[,cvt] <- xh2o[,cvt]*MH2O*0.001
                xunits <- units(xh2o)
                xunits[cvt] <-  rep("m/s g/m^3",sum(cvt))
                units(xh2o) <- xunits
            }

            if (!fcorr_done) {

                # no oxygen correction for licors
                if (is(o2corr,"dat")) {
                    lo2corr <- xh2o * 0.0
                    units(lo2corr) <- rep("",ncol(xh2o))
                }

                scorr <- dat(sub(datvar,"Scorr",what,fixed=TRUE),which="other",avg=TRUE,smooth=TRUE)
                if (is(scorr,"dat")) scorr <- conform(scorr,xh2o)
                # correction for spatial separation between sonic and fast hygrometer
                if (!is.null(scorr)) xh2o <- xh2o * scorr
            }
        }
    }

    if (!is.null(xkh2o)) {
        if (is.null(xh2o)) xh2o <- xkh2o
        else xh2o <- Cbind(xkh2o,xh2o)
    }

    # delete data when the fraction of expected sonic samples is lower than sfrac.min
    if (!is.null(dpar("sfrac.min")) && existsFunction("dat.sfrac")) {
        sonicbad <- !is.na(xh2o) & (conform(dat("sfrac",avg=TRUE,smooth=TRUE),xh2o) < dpar("sfrac.min"))
        if (any(!is.na(sonicbad) & sonicbad)) {
            rmd <- apply(sonicbad,2,function(x){sum(x,na.rm=TRUE)/length(x) * 100})
            cat(paste("calc.x.t: percentage of flux data removed due to dat(\"sfrac\") < dpar(sfrac.min=",
                    dpar("sfrac.min"),"):",
                    paste(colnames(xh2o),"(",round(rmd,1),")",sep="",collapse=", "),"\n"))
            xh2o[sonicbad] <- NA_real_
        }
    }

    if (any(xh2o@units != "m/s g/m^3" & xh2o@units != "m/s gm/m^3"))
        warning("units of <w'kh2o'> are not m/s g/m^3")

    TK <- conform(dat("T",avg=TRUE,smooth=TRUE),xt) + 273.15
    xh2o <- conform(xh2o,xt) * 1.e-3

    if (!identical(dim(xh2o),dim(xt))) {
        warning("in calc.x.t, dim(xh2o)=",paste(dim(xh2o),collapse=",")," is not equal to dim(xt)",paste(dim(xt),collapse=","),". Doing conform(xt,xh2o)")
        xt <- conform(xt,xh2o)
    }

    if (fcorr_done) {
        xt <- xt - 0.51 * TK * xh2o
    }
    else {

        # Corrections to <w'tc'> assume E measured as <w'kh2o'> (m/s g/m^3)
        # Correct <w'tc'> for <w'h2o'> per Schotanus et al., BLM, 1983
        # Correct E for <w't'> per Webb et al., QJRMS, 1980
        # Correct E for <w't'> (oxygen correction)
        # Correct E for <w'kh2o'> for spatial separation

        if (is(o2corr,"dat")) {
            if (!is.null(lo2corr)) o2corr <- Cbind(o2corr,lo2corr)
            o2corr <- conform(o2corr,xt) * 1.e-3
        }
        else if (is.null(o2corr)) o2corr <- 0.0

        rhoAir <- conform(dat("rhoAir",avg=TRUE,smooth=TRUE),xt)
        # cat("dim(rhoAir)=",dim(rhoAir),"\n")
        Q <- conform(dat("Q",avg=TRUE,smooth=TRUE),xt) * 1e-3

        # cat("dim(Q)=",dim(Q),"\n")
        # cat("dim(xh2o)=",dim(xh2o),"\n")
        # cat("dim(o2corr)=",dim(o2corr),"\n")
        # cat("o2corr=",o2corr,"\n")

        if (comp == "w") {
            factor <- 0.51*(1 + Q*(1/0.622-1))
            xt <- (xt - factor * TK / rhoAir * xh2o) /
                (1 + factor * (Q + o2corr * (1-Q)))
        }
        else {
            factor <- 0.51*(1 - Q)
            xt <- (xt - factor * TK / rhoAir * xh2o) /
                (1 + factor * o2corr * (1-Q))
        }
        xt@data[is.infinite(xt@data)] <- NA_real_
    }
    xt@units <- rep("m/s degK",ncol(xt))

    colnames(xt) <- sub("[^.]+",datvar,colnames(xt))
    xt
}

"dat.u't'" <- function(what,derived=TRUE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    calc.x.t(datvar,what)
}

"dat.v't'" <- function(what,derived=TRUE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    calc.x.t(datvar,what)
}

"dat.w't'" <- function(what,derived=TRUE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    calc.x.t(datvar,what)
}

calc.x.tnew <- function(datvar,what)
{
    # compute u't', v't', or w't', as specified by the what argument
    comp <- words(datvar,1,1,sep="'")	# get u,v, w

    # x'tc' where x is u,v or w
    xtc.name <- sub(datvar,paste(comp,"tc'",sep="'"),what,fixed=TRUE)

    # Note avg=TRUE, which means x'tc' is computed at the
    # resolution specified in avg, and then corrections are
    # applied here, after the averaging.
    xt <- dat(xtc.name,avg=TRUE,smooth=TRUE)

    xh.name <- paste(comp,"h2o'",sep="'")  # x'kh2o'
    xh.name <- sub(datvar,xh.name,what,fixed=TRUE)
    # specify derived=TRUE, to get corrected w'h2o'
    xh2o <- dat(xh.name,derived=TRUE,avg=TRUE,smooth=TRUE) # m/s mmol/m^3

    TK <- conform(dat("T",avg=TRUE,smooth=TRUE),xt) + 273.15

    # cat("dim(xh2o)=",dim(xh2o),"\n")
    # cat("dim(xt)=",dim(xt),"\n")
    # cat("dim(TK)=",dim(TK),"\n")

    xt <- xt - 0.51 * TK * xh2o * 1.e-3

    colnames(xt) <- sub("[^.]+",datvar,colnames(xt))
    xt@units <- rep("m/s degK",ncol(xt))
    xt
}

"dat.u'tnew'" <- function(what,derived=TRUE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    calc.x.tnew(datvar,what)
}

"dat.v'tnew'" <- function(what,derived=TRUE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    calc.x.tnew(datvar,what)
}

"dat.w'tnew'" <- function(what,derived=TRUE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    calc.x.tnew(datvar,what)
}

calc.x.mr <- function(datvar,what,derived=TRUE)
{
    robust <- dpar("robust")
    if (is.null(robust)) robust <- TRUE

    MH2O <- 18
    comp <- words(datvar,1,1,sep="'")

    xkh2o <- xh2o <- o2corr <- lo2corr <- NULL

    # Have h2o flux corrections been done?
    fcorr_done <- dpar("h2o_flux_corrected")
    fcorr_done <- !is.null(fcorr_done) && as.logical(fcorr_done)

    wvb <- words(variables(),1,1)

    # read krypton data
    xh.name <- paste(comp,"kh2o'",sep="'")  # x'kh2o'

    if (!fcorr_done && any(wvb == xh.name)) {
        # Assume E measured as <w'h2o'> (m/s g/m^3)
        xh.name <- sub(datvar,xh.name,what,fixed=TRUE)

        # specify derived=FALSE, so make sure we get the uncorrected x'kh2o'
        # Note avg=TRUE, which means x'kh2o' is computed at the
        # resolution specified in avg, and then the o2 and spatial
        # corrections are applied here, after the averaging.
        xkh2o <- dat(xh.name,derived=FALSE,avg=TRUE,smooth=TRUE)	# m/s g/m^3
        if (!is.null(xkh2o)) {

            allna <- apply(xkh2o,2,function(x)all(is.na(x)))
            if (!all(allna)) xkh2o <- xkh2o[,!allna]

            if (!robust) {
                o2corr <- dat(sub(datvar,"o2corr",what,fixed=TRUE))
                if (is(o2corr,"dat")) {
                    o2corr <- conform(o2corr,xkh2o)
                    o2corr <- approx(o2corr,xout=xkh2o,method="constant",f=0,rule=2)
                }
                scorr <- dat(sub(datvar,"Scorr",what,fixed=TRUE),which="krypton",avg=TRUE,smooth=TRUE)
                if (is(scorr,"dat")) scorr <- conform(scorr,xkh2o)
                # correction for spatial separation between sonic and fast hygrometer
                if (!is.null(scorr)) xkh2o <- xkh2o * scorr
            }
        }
    }
    # read li7500 data
    xh.name <- paste(comp,"h2o'",sep="'")  # x'h2o'

    if (any(wvb == xh.name)) {
        xh.name <- sub(datvar,xh.name,what,fixed=TRUE)

        # specify derived=FALSE, so that oxygen and spatial corrections
        # are not done in this call to dat
        xh2o <- dat(xh.name,derived=FALSE,avg=TRUE,smooth=TRUE) # m/s mmol/m^3
        if (!is.null(xh2o)) {

            allna <- apply(xh2o,2,function(x)all(is.na(x)))
            if (!all(allna)) xh2o <- xh2o[,!allna]

            # ...convert units
            cvt <- units(xh2o) == "m/s mmol/m^3"
            if (any(cvt)) {
                xh2o[,cvt] <- xh2o[,cvt]*MH2O*0.001
                xunits <- units(xh2o)
                xunits[cvt] <-  rep("m/s g/m^3",sum(cvt))
                units(xh2o) <- xunits
            }
            if (!fcorr_done && !robust) {
                # no oxygen correction for licors
                if (is(o2corr,"dat")) {
                    lo2corr <- xh2o * 0.0
                    units(lo2corr) <- rep("",ncol(xh2o))
                }

                scorr <- dat(sub(datvar,"Scorr",what,fixed=TRUE),which="other",avg=TRUE,smooth=TRUE)
                if (is(scorr,"dat")) scorr <- conform(scorr,xh2o)
                # correction for spatial separation between sonic and fast hygrometer
                if (!is.null(scorr)) xh2o <- xh2o * scorr
            }
        }
    }
    # combine
    if (!is.null(xkh2o)) {
        if (is.null(xh2o)) xh2o <- xkh2o
        else xh2o <- Cbind(xkh2o,xh2o)
    }

    if (any(xh2o@units != "m/s g/m^3" & xh2o@units != "m/s gm/m^3"))
        warning("units of <w'kh2o'> are not m/s g/m^3")
    sfxs <- suffixes(xh2o,2)

    rhod <- conform(dat("rhoDry",avg=TRUE,smooth=TRUE),xh2o) * 1e3	# g/m^3
    xmr <- xh2o / rhod

    if (!fcorr_done && !robust) {

        if (is(o2corr,"dat")) {
            if (!is.null(lo2corr)) o2corr <- Cbind(o2corr,lo2corr)
            o2corr <- conform(o2corr,xh2o)
        }
        else if (is.null(o2corr)) o2corr <- 0.0

        # x'tc' where x is u,v or w
        xtc.name <- sub(datvar,paste(comp,"tc'",sep="'"),what,fixed=TRUE)
        # Correct <w'kh2o'> for spatial separation  (done above)
        # Correct E for <w't'> (oxygen correction)
        # Correct <w'tc'> for <w'h2o'> per Schotanus et al., BLM, 1983
        # Correct E for <w't'> per Webb et al., QJRMS, 1980

        Q <- conform(dat("Q",avg=TRUE,smooth=TRUE),xmr) * 1.e-3
        MR <- Q/(1-Q)
        TK <- conform(dat("T",avg=TRUE,smooth=TRUE),xmr) + 273.15

        xtc <- conform(dat(xtc.name,avg=TRUE,smooth=TRUE),xmr)

        # delete data when the fraction of expected sonic samples is lower than sfrac.min
        if (!is.null(dpar("sfrac.min")) && existsFunction("dat.sfrac")) {
            sonicbad <- !is.na(xmr) & (conform(dat("sfrac",avg=TRUE,smooth=TRUE),xmr) < dpar("sfrac.min"))
            if (any(!is.na(sonicbad) & sonicbad)) {
                rmd <- apply(sonicbad,2,function(x){sum(x)/length(x) * 100})
                cat(paste("calc.x.mr: percentage of flux data removed due to dat(\"sfrac\") < dpar(sfrac.min=",
                        dpar("sfrac.min"),"):",
                        paste(colnames(xh2o),"(",round(rmd,1),")",sep="",collapse=", "),"\n"))
                xmr[sonicbad] <- NA_real_
            }
        }

        if (comp == "w") {
            factor <- 0.51*(1 + Q*(1/0.622-1))
            xmr <- (1 + MR/0.622)*(xmr + (MR + o2corr) * xtc / TK) /
                (1 + factor*(Q + o2corr*(1-Q)))
        }
        else {
            factor <- 0.51*(1-Q)
            xmr <- (xmr + o2corr * xtc / TK) / (1 + factor*o2corr*(1-Q))
        }
    }

    xmr@data[is.infinite(xmr@data)] <- NA_real_

    xmr <- xmr * 1.e3	# m/s g/kg

    colnames(xmr) <- sub("[^.]+",datvar,colnames(xmr))
    xmr@units <- rep("m/s g/kg",ncol(xmr))
    xmr
}
"dat.u'mr'" <- function(what,derived=TRUE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    calc.x.mr(datvar,what)
}

"dat.v'mr'" <- function(what,derived=TRUE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    calc.x.mr(datvar,what)
}

"dat.w'mr'" <- function(what,derived=TRUE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    calc.x.mr(datvar,what)
}

calc.x.h2o <- function(datvar,what)
{
    robust <- dpar("robust")
    if (is.null(robust)) robust <- TRUE

    MH2O <- 18
    comp <- words(datvar,1,1,sep="'")

    xkh2o <- xh2o <- o2corr <- lo2corr <- NULL

    fcorr_done <- dpar("h2o_flux_corrected")
    fcorr_done <- !is.null(fcorr_done) && as.logical(fcorr_done)

    wvb <- words(variables(),1,1)

    # read krypton data
    xh.name <- paste(comp,"kh2o'",sep="'")  # x'kh2o'

    if (!fcorr_done && any(wvb == xh.name)) {
        xh.name <- sub(datvar,xh.name,what,fixed=TRUE)

        # Note avg=TRUE, which means x'kh2o' is computed at the
        # resolution specified in avg, and then the o2 and spatial
        # corrections are applied here, after the averaging.
        xkh2o <- dat(xh.name,derived=FALSE,avg=TRUE,smooth=TRUE)	# m/s g/m^3
        if (!is.null(xkh2o) && !robust) {
            # dat("o2corr") can return a simple 0, rather than a time series
            # In that case, conform will just return that 0 again.
            if (is.null(o2corr)) o2corr <- dat(sub(datvar,"o2corr",what,fixed=TRUE))
            if (is(o2corr,"dat")) {
                o2corr <- conform(o2corr,xkh2o)
                o2corr <- approx(o2corr,xout=xkh2o,method="constant",f=0,rule=2)
            }
            scorr <- dat(sub(datvar,"Scorr",what,fixed=TRUE),which="krypton",avg=TRUE,smooth=TRUE)
            if (is(scorr,"dat")) scorr <- conform(scorr,xkh2o)
            # correction for spatial separation between sonic and fast hygrometer
            if (!is.null(scorr)) xkh2o <- xkh2o * scorr
        }
    }
    # read li7500 data
    xh.name <- paste(comp,"h2o'",sep="'")  # x'h2o'

    if (any(wvb == xh.name)) {
        xh.name <- sub(datvar,xh.name,what,fixed=TRUE)# x'h2o'
        xh2o <- dat(xh.name,derived=FALSE,avg=TRUE,smooth=TRUE) # m/s mmol/m^3
        if (!is.null(xh2o)) {
            # ...convert units
            cvt <- units(xh2o) == "m/s mmol/m^3"
            if (any(cvt)) {
                xh2o[,cvt] <- xh2o[,cvt]*MH2O*0.001
                xunits <- units(xh2o)
                xunits[cvt] <-  rep("m/s g/m^3",sum(cvt))
                units(xh2o) <- xunits
            }
            if (!fcorr_done && !robust) {

                # no oxygen correction for licors
                if (is(o2corr,"dat")) {
                    lo2corr <- xh2o * 0.0
                    units(lo2corr) <- rep("",ncol(xh2o))
                }

                scorr <- dat(sub(datvar,"Scorr",what,fixed=TRUE),which="other",avg=TRUE,smooth=TRUE)
                if (is(scorr,"dat")) scorr <- conform(scorr,xh2o)
                # correction for spatial separation between sonic and fast hygrometer
                if (!is.null(scorr)) xh2o <- xh2o * scorr
            }
        }
    }
    # combine
    if (!is.null(xkh2o)) {
        if (is.null(xh2o)) xh2o <- xkh2o
        else xh2o <- Cbind(xkh2o,xh2o)
    }

    if (any(xh2o@units != "m/s g/m^3" & xh2o@units != "m/s gm/m^3"))
        warning("units of <w'h2o'> are not m/s g/m^3")
    # browser()

    if (!fcorr_done && !robust) {

        sfxs <- suffixes(xh2o,2)

        if (is(o2corr,"dat")) {
            if (!is.null(lo2corr)) o2corr <- Cbind(o2corr,lo2corr)
            o2corr <- conform(o2corr,xh2o)
        }
        else if (is.null(o2corr)) o2corr <- 0.0

        # Correct <w'kh2o'> for spatial separation 
        # Correct <w'tc'> for <w'h2o'> per Schotanus et al., BLM, 1983
        # Correct E for <w't'> per Webb et al., QJRMS, 1980
        # Correct E for <w't'> (oxygen correction) 
        #
        # delete data when the fraction of expected sonic samples is lower than sfrac.min
        if (!is.null(dpar("sfrac.min")) && existsFunction("dat.sfrac")) {
            sonicbad <- !is.na(xh2o) & (conform(dat("sfrac",avg=TRUE,smooth=TRUE),xh2o) < dpar("sfrac.min"))
            if (any(!is.na(sonicbad) & sonicbad)) {
                rmd <- apply(sonicbad,2,function(x){sum(x)/length(x) * 100})
                cat(paste("calc.x.h2o: percentage of flux data removed due to dat(\"sfrac\") < dpar(sfrac.min=",
                        dpar("sfrac.min"),"):",
                        paste(colnames(xh2o),"(",round(rmd,1),")",sep="",collapse=", "),"\n"))
                xh2o[sonicbad] <- NA_real_
            }
        }

        rhod <- conform(dat("rhoDry",avg=TRUE,smooth=TRUE),xh2o) * 1e3	# g/m^3
        Q <- conform(dat("Q",avg=TRUE,smooth=TRUE),xh2o) * 1e-3	# kg/kg
        TK <- conform(dat("T",avg=TRUE,smooth=TRUE),xh2o) + 273.15

        # x'tc' where x is u,v or w
        xtc.name <- sub(datvar,paste(comp,"tc'",sep="'"),what,fixed=TRUE)
        xtc <- conform(dat(xtc.name,avg=TRUE,smooth=TRUE),xh2o)

        # cat("dim(rhod)=",dim(rhod),"\n")
        # cat("dim(Q)=",dim(Q),"\n")
        # cat("dim(TK)=",dim(TK),"\n")
        # cat("dim(xtc)=",dim(xtc),"\n")
        # cat("dim(o2corr)=",dim(o2corr),"\n")
        # cat("o2corr=",o2corr,"\n")


        if (comp == "w") {
            factor <- 0.51*(1 + Q*(1/0.622-1))
            xh2o <- ( (1 + factor*Q) * xh2o + o2corr * xtc * rhod / TK ) /
                (1 + factor*(Q + o2corr * (1-Q)))
        }
        else {
            factor <- 0.51*(1-Q)
            xh2o <- (xh2o + o2corr * xtc * rhod / TK) /
                (1 + factor* o2corr * (1-Q))
        }
        xh2o@data[is.infinite(xh2o@data)] <- NA_real_
        colnames(xh2o) <- sub("[^.]+",datvar,colnames(xh2o))
        xh2o@units <- rep("m/s g/m^3",ncol(xh2o))
    }

    xh2o
}

"dat.u'h2o'" <- function(what,derived=TRUE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    calc.x.h2o(datvar,what)
}

"dat.v'h2o'" <- function(what,derived=TRUE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    calc.x.h2o(datvar,what)
}

"dat.w'h2o'" <- function(what,derived=TRUE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    calc.x.h2o(datvar,what)
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
        colnames(x) <- sub("[^.]+",Hname,colnames(x))
        x@units <- rep("W/m^2",ncol(x))
    }
    x
}
dat.H <- function(what,derived=TRUE,...)
{
    # Turbulent sensible heat flux

    datvar <- datVar() # requested variable name, x of dat.x
    robust <- dpar("robust")
    if (is.null(robust)) robust <- TRUE

    Td <- dat("T",avg=TRUE,smooth=TRUE)

    if (robust)
        Td[is.na(Td)] <- median(Td,na.rm=TRUE)

    wq <- dat(sub(datvar,"w'h2o'",what,fixed=TRUE),avg=TRUE,smooth=TRUE)*1e-3
    wt <- dat(sub(datvar,"w't'",what,fixed=TRUE),avg=TRUE,smooth=TRUE)

    x <- calc.H(wt=wt,wq=wq,Td=Td,rho=dat("rhoAir",avg=TRUE,smooth=TRUE))
    x
}

dat.H.dry <- function(what,derived=TRUE,...)
{
    # Turbulent sensible heat flux without contribution from varying Cp 

    datvar <- datVar() # requested variable name, x of dat.x

    robust <- dpar("robust")
    if (is.null(robust)) robust <- TRUE

    Q <- dat("Q",avg=TRUE,smooth=TRUE)
    if (robust) {
        if (all(is.na(Q)))
            Q <- 0
        else
            Q <- median(Q, na.rm=TRUE)
    }
    wt <- dat(sub(datvar,"w't'",what,fixed=TRUE),avg=TRUE,smooth=TRUE)
    x <- calc.H(wt=wt,Q=Q,rho=dat("rhoAir",avg=TRUE,smooth=TRUE))
    x
}

dat.LE <- function(what,derived=TRUE,...)
{
    # Turbulent latent heat flux
    datvar <- datVar() # requested variable name, x of dat.x
    x <- dat(sub(datvar,"w'mr'",what,fixed=TRUE),avg=TRUE,smooth=TRUE)
    sfxs <- suffixes(x,2)

    x <- conform(dat(sub(datvar,"Lv",what,fixed=TRUE),avg=TRUE,smooth=TRUE),x) *
	conform(dat("rhoDry",avg=TRUE,smooth=TRUE),x) * x * 1e-3

    colnames(x) <- sub("[^.]+",datvar,colnames(x))
    x@units <- rep("W/m^2",ncol(x))
    x
}
dat.Lv <- function(what,derived=TRUE,...)
{
    # Latent heat of vaporatizaton of water is a weak function of temperature
    # (varies by 1% in 10 C)
    # ref:  Handbook of Hydrology, Maidment, Ed., Eq 4.2.1 
    # [I haven't actually seen this book, but have a quotation]
    # values from http://www.eos.uoguelph.ca/webfiles/wjames/wj365m04.html
    # were used previously, but conversion from calories gave slightly different
    # numerical values
    datvar <- datVar() # requested variable name, x of dat.x

    robust <- dpar("robust")
    if (is.null(robust)) robust <- TRUE

    if (!robust) {
        x <- 2501000 - 2361 * dat(sub(datvar,"T",what,fixed=TRUE))
        colnames(x) <- sub("[^.]+",datvar,colnames(x))
        x@units <- rep("J/kg",ncol(x))
    }
    else x <- 2.5e6
    x
}
dat.BR <- function(what,derived=TRUE,...)
{
    # Bowen ratio
    datvar <- datVar() # requested variable name, x of dat.x
    x <- dat(sub(datvar,"H",what,fixed=TRUE),avg=TRUE,smooth=TRUE) / 
    dat(sub(datvar,"LE",what,fixed=TRUE),avg=TRUE,smooth=TRUE)
    x@data[is.infinite(x@data)] <- NA_real_

    colnames(x) <- sub("[^.]+",datvar,colnames(x))
    x@units <- rep("",ncol(x))
    x
}
dat.Scorr <- function(what,derived=TRUE,which="krypton",...)
{
    # Correct covariance for spatial separation of sonic and scalar sensor
    # Assumes separation parallel to the sonic u axis and 
    # cospectrum with -2 slope in inertial subrange
    #
    # corr <- exp(2*pi*n_m*x/(z-d))
    # x <- spatial separation (m)
    # z <- sonic height agl (m)
    # d <- displacement height of wind profile (m)
    # n_m <- dimensionless frequency of peak of flux cospectrum
    #     <- function of z/L from Horst, BLM, projected 2008

    datvar <- datVar() # requested variable name, x of dat.x

    robust <- dpar("robust")
    if (is.null(robust)) robust <- TRUE

    if (robust) {
        S <- dat("heightSonic")
        sfxs <- suffixes(S, 2)
        S@data <- matrix(1,nrow(S),ncol(S))
    }
    else {

        # first get the separation. Conform everything to it
        # It is then approximated to zoL later.
        if (which == "krypton") S <- dat(sub(datvar,"kryptonSep",what,fixed=TRUE))
        else if (which == "licor") S <- dat(sub(datvar,"licor7500Sep",what,fixed=TRUE))
        else S <- dat(sub(datvar,"fastH2OSep",what,fixed=TRUE))

        if (is.null(S)) return(1)

        # convert separation from cm to m
        if (!is.null(units(S))) {
            cmx <- !is.na(match(units(S),"cm"))
            if (any(cmx)) S[,cmx] <- S[,cmx] / 100.0
        }


        zoL <- conform(dat(sub(datvar,"L",what,fixed=TRUE),avg=TRUE,smooth=TRUE),S)
        sfxs <- suffixes(zoL,2)

        # need z and d for each sonic
        z <- conform(dat(sub(datvar,"heightSonic",what,fixed=TRUE)),S)
        z <- approx(z,xout=zoL,method="constant",f=0,rule=2)

        d <- conform(dat(sub(datvar,"D",what,fixed=TRUE)),z)
        # Note linear interpolation for D
        d <- approx(d,xout=z,method="linear",rule=3)
        z <- z - d

        zoL <- z / zoL	

        nmx <- zoL	# make a copy of the time series
        select <- !is.na(zoL@data) & zoL@data <= -0.1
        nmx@data[select] <- 0.065
        # Horst, Lenschow, 2009: Corrected nmx values, 5/11/2009
        nmx@data[!select] <- 2.31 - 2.240 / (1.015 + 0.15 * zoL@data[!select])^2


        nmy <- zoL	# make a copy of the time series
        select <- !is.na(zoL@data) & zoL@data <= -0.05
        nmy@data[select] <- 0.15
        nmy@data[!select] <- 2.43 - 2.28 / (1.01 + 0.2 * zoL@data[!select])^2

        # Need u,v in instrument coordinates.
        check.windcoords()
        rcoords <- dpar("coords")
        if (rcoords != "instrument") {
            on.exit(dpar(coords=rcoords),add=TRUE)
            dpar(coords="instrument")
        }

        u <- conform(dat(sub(datvar,"u",what,fixed=TRUE),avg=TRUE,smooth=TRUE),zoL)
        v <- conform(dat(sub(datvar,"v",what,fixed=TRUE),avg=TRUE,smooth=TRUE),zoL)
        spd <- sqrt(u^2 + v^2)

        nm <- sqrt((nmx * u/spd)^2 + (nmy * v/spd)^2)
        nm <- conform(nm,S)
        S <- approx(S,xout=nm,method="constant",f=0,rule=2)

        S <- exp(2 * pi * nm * S / z)
    }
    colnames(S) <- sub("[^.]+",datvar,colnames(S))
    S@units <- rep("",ncol(S))
    S
}

dat.TKE <- function(what,derived=TRUE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    x <- (dat("u'u'",avg=TRUE,smooth=TRUE) +
        dat("v'v'",avg=TRUE,smooth=TRUE) +
        dat("w'w'",avg=TRUE,smooth=TRUE))/2

    colnames(x) <- sub("[^.]+",datvar,colnames(x))
    x@units <- rep("(m/s)^2",ncol(x))
    x
}

"dat.w'co2'" <- function(what,...)
{
    #
    datvar <- datVar() # requested variable name, x of dat.x

    robust <- dpar("robust")
    if (is.null(robust)) robust <- TRUE

    wc <- dat("w'co2'",avg=TRUE,smooth=TRUE,derived=FALSE)

    # Have the flux corrections been done?
    fcorr_done <- dpar("co2_flux_corrected")
    fcorr_done <- !is.null(fcorr_done) && as.logical(fcorr_done)

    # Correct, if desired
    if (!fcorr_done && !robust) {

        # Directly from Webb, Pearman, Leuning, 1980, QJRMS, 106, 85-100.
        #
        MCO2 <- 44.01 # Ref: Wikipedia
        mu <- 28.97/18

        # Get data and convert to kg/m^3 (haven't put in ppmV yet, but open-paths don't use)
        rhoc <- dat(sub(datvar,"co2",what,fixed=TRUE),avg=TRUE,smooth=TRUE)
        uc <- units(rhoc)
        cf <- rep(NA,length(uc))
        cf[uc=="kg/m^3"] <- 1
        cf[uc=="g/m^3"] <- 1e-3
        cf[uc=="mmol/m^3"] <- 1e-6 * MCO2
        rhoc@data <- t(t(rhoc@data)*cf)

        wc <- conform(wc,rhoc)
        wc@data <- t(t(wc@data)*cf)

        TK <- conform(dat("T",avg=TRUE,smooth=TRUE),wc) + 273.15
        rhoa <- conform(dat("rhoDry",avg=TRUE,smooth=TRUE),wc)
        rhov <- 0.001*conform(dat("H2O",avg=TRUE,smooth=TRUE),wc)
        # the following will pick up Webb, spatial sep, etc. corrections for w't' and w'h2o'
        wt <- conform(dat("w't'",avg=TRUE,smooth=TRUE),wc)

        # we're now using h2o from the IRGA itself, which should be fine
        wh2o <- 0.001*conform(dat("w'h2o'",avg=TRUE,smooth=TRUE),wc)

        # now the correction
        scorr <- dat(sub(datvar,"Scorr",what,fixed=TRUE),which="other",avg=TRUE,smooth=TRUE)
        if (is(scorr,"dat")) scorr <- conform(scorr,wc)
        wc <- wc*scorr + mu*(rhoc/rhoa)*wh2o + (1 + mu*rhov/rhoa)*(rhoc/TK)*wt    

        # convert back to original units (do we really want to do this?)
        wc@data <- t(t(wc@data)/cf)
    }
    wc
}
