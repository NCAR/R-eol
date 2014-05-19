# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsdng" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

interpSounding <- function(xs,xname,yname)
{
    res <- list()
    yout <- NULL

    for (sname in names(xs)) {
        x <- xs[[sname]]
        vnames <- colnames(x)

        if (is.na(match(xname,vnames))) stop(paste(xname,"not found in",sname))
        if (is.na(match(yname,vnames))) stop(paste(yname,"not found in",sname))

        xtunits <- eolts::units(x[,xname])
        ytunits <- eolts::units(x[,yname])

        # interpolate over log of pressure
        if (is.null(yout)) {
            xunits <- xtunits
            yunits <- ytunits
            if (yunits == "mb") {
                yout <- seq(5,1200,5)
                yiout <- log(yout)
            }
            else {
                yout <- seq(0,30000,75)
                yiout <- yout
            }
        }
        else {
            if (xunits != xtunits)
                stop(paste("units of",xname,"are changing,",xunits,"!=",xtunits))
            if (yunits != ytunits)
                stop(paste("units of",yname,"are changing,",yunits,"!=",ytunits))
        }

        yin <- as.vector(x@data[,yname])
        if (yunits == "mb") yin <- log(yin)

        yok <- !is.na(yin) & !is.infinite(yin) & substring(x@data[,"sta"],1,1) == "S"
        if (sum(yok) < 10) next
        yin <- yin[yok]

        # compute means of first and last 10% of yin, and
        # compare, in order to guess whether yin is increasing or decreasing
        nm <- length(yin) / 10
        if (nm < 2) nm <- length(yin) / 2

        m1 <- mean(yin[1:nm])
        m2 <- mean(tail(yin,nm))

        xr <- matrix(NA,ncol=length(xname)+1,nrow=length(yout),
                dimnames=list(NULL,c(xname,yname)))

        xin <- x@data[yok,xname]

        if (m1 > m2) {
            # reverse yout so that it has the same trend as yin
            if (sum(!is.na(xin)) > 1)
                xr[,xname] <- approx(yin,xin,rev(yiout))$y
            xr[,yname] <- rev(yout)
        }
        else {
            if (sum(!is.na(xin)) > 1)
                xr[,xname] <- approx(yin,xin,yiout)$y
            xr[,yname] <- yout
        }

        tin <- as.numeric(positions(x))[yok]
        t0  <- tin[1]
        tn  <- tail(tin,1)
        # tin <- tin - t0
        # tx <- utime(t0 + replace.nas(approx(1:length(tin),tin,1:length(yout))$y,FALSE))
        tx <- utime(t0 + seq(from=0,to=tn-t0,length.out=length(yout)))

        xr <- dat(nts(xr,tx,units=c(xunits,yunits)))
        res[sname] <- xr
        # browser()
    }
    res
}
