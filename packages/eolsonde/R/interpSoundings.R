# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsonde" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

interpSoundings <- function(sdngs, xname, ynames, xlim=NULL, nstep=100)
{
    res <- list()
    xout <- NULL
    xunits <- NULL
    xreversed <- NULL
    xm1 <- NULL
    xm2 <- NULL

    xmin <- Inf
    xmax <- -Inf

    if (!is.list(sdngs))
        sdngs <- list(sdng=sdngs)

    # First scan soundings to determine:
    #   xreversed: whether xin is increasing or decreasing.
    #       xout will be created with the same trend.
    #   xunits: determine units of x variable and warn if they
    #       are not consistent
    #   xmin,xmax: which are used to create xout if xlim is NULL
    for (sname in names(sdngs)) {

        sdng <- sdngs[[sname]]

        vnames <- colnames(sdng)
        if (is.na(match(xname,vnames))) stop(paste(xname,"not found in",sname))

        xin <- clip(sdng[,xname])
        xtunits <- eolts::units(xin)
        xok <- !is.na(xin@data[,1]) & !is.infinite(xin@data[,1])
        if (sum(xok) < 10) next
        xin <- xin@data[xok,]   # no longer a time series

        xmin <- min(xmin,xin)
        xmax <- max(xmax,xin)

        nm <- length(xin) / 10
        if (nm < 2) nm <- length(xin) / 2
        m1 <- mean(xin[1:nm],trim=0.2)
        m2 <- mean(tail(xin,nm),trim=0.2)
        xtreversed <- m1 > m2

        if (abs(m1-m2) > 1.e-4) {
            if (is.null(xreversed)) {
                xreversed <- xtreversed
                xm1 <- m1
                xm2 <- m2
            } else {
                if (xreversed != xtreversed) {
                    warning(paste0("Trend of ",xname,
                        " seems to be changing over these soundings. ",
                        sname,": mean(first 10%)=",signif(m1,4),
                        ", mean(last 10%)=",signif(m2,4),
                        ", previous=",signif(xm1,4),", ",signif(xm2,4)))
                    # If this difference of means is larger, use it.
                    if (abs(m1-m2) > abs(xm1 - xm2)) {
                        xreversed <- xtreversed
                        xm1 <- m1
                        xm2 <- m2
                    }
                }
            }
        }
        else {
            warning(paste0(xname," in ",sname," appears to be constant.",
                " mean(first 10%)=",signif(m1,4),
                ", mean(last 10%)=",signif(m2,4),
                ", stddev=",signif(sd(xin),4)))
        }
        if (is.null(xunits)) {
            xunits <- xtunits
        } else if (xunits != xtunits) {
            warning(paste("units of",xname,"are changing:",xunits,"!=",xtunits))
        }
    }
    if (is.null(xlim)) {
        if (is.infinite(xmin)) {
            warning(paste("No data found for",xname,"in any input sounding"))
            return(res) # empty list
        }
        xlim <- c(xmin,xmax)
    }
    else xlim <- sort(xlim)

    xout <- seq(from=xlim[1],to=xlim[2],length=nstep)

    # interpolate over log of pressure
    if (xunits == "mb") xiout <- log(xout)
    else xiout <- xout

    if (xreversed) {
        # make xout have same upward/downward trend as xin
        xout <- rev(xout)
        xiout <- rev(xiout)
    }

    for (sname in names(sdngs)) {

        sdng <- sdngs[[sname]]

        vnames <- colnames(sdng)

        xin <- sdng[,xname]

        xok <- !is.na(xin@data[,1]) & !is.infinite(xin@data[,1])
        if (sum(xok) < 5) next
        xin <- xin[xok,]

        tin <- positions(xin)
        dt <- deltat(xin)["trimmed.mean"]

        # interpolate over log of pressure
        if (xunits == "mb") xin <- log(xin)

        rmat <- matrix(NA,ncol=length(ynames)+1,nrow=length(xout),
                dimnames=list(NULL,c(xname,ynames)))

        rmat[,xname] <- xout

        for (yname in ynames) {

            if (is.na(match(yname,vnames))) stop(paste(yname,"not found in",sname))

            # first interpolate yin to same times as xin
            yin <- clip(sdng[,yname])
            yin <- align(yin,tin,how="interp",matchtol=dt*10)

            if (sum(!is.na(yin)) > 2)
                tryCatch(
                    rmat[,yname] <- approx(xin@data[,1],yin@data[,1],xiout)$y,
                    error=function(e)
                    {
                        # msg <- paste(e)
                        # don't print out error message:
                        # "need at least two non-NA values to interpolate"
                        # User has been warned above in check of means
                        if (all(!grepl("need at least two",format(e),fixed=TRUE)))
                            warning(paste(format(e),collapse=": "),": ",sname,", ",xname,call.=FALSE)
                        NA_real_
                    }
                )
            NULL
        }

        t0  <- as.numeric(tin[1])
        tn  <- as.numeric(tail(tin,1))
        tx <- utime(t0 + seq(from=0,to=tn-t0,length.out=length(xout)))

        yunits <- eolts::units(sdng[,ynames])

        rmat <- dat(nts(rmat,tx,units=c(xunits,yunits)))
        res[sname] <- rmat
        NULL
    }
    res
}
