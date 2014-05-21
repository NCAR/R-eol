# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsdng" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

interpSoundings <- function(sdngs,xname,ynames)
{
    res <- list()
    xout <- NULL
    xunits <- NULL
    xreversed <- NULL

    if (!is.list(sdngs))
        sdngs <- list(sdng=sdngs)

    for (sname in names(sdngs)) {

        sdng <- sdngs[[sname]]

        vnames <- colnames(sdng)
        if (is.na(match(xname,vnames))) stop(paste(xname,"not found in",sname))

        xin <- as.vector(sdng@data[,xname])

        xok <- !is.na(xin) & !is.infinite(xin)
        if (sum(xok) < 10) next
        xin <- xin[xok]

        # compute means of first and last 10% of xin, and
        # compare, in order to guess whether xin is increasing or decreasing
        nm <- length(xin) / 10
        if (nm < 2) nm <- length(xin) / 2

        m1 <- mean(xin[1:nm],trim=0.2)
        m2 <- mean(tail(xin,nm),trim=0.2)

        if (is.null(xout)) {
            xunits <- eolts::units(sdng[,xname])
            xreversed <- m1 > m2
            if (xunits == "mb") {
                xout <- seq(5,1200,5)
                xiout <- log(xout)
            }
            else {
                xout <- seq(0,30000,75)
                xiout <- xout
            }
            if (xreversed) {
                # make xout have same upward/downward trend as xin
                xout <- rev(xout)
                xiout <- rev(xiout)
            }
        }
        else {
            xtunits <- eolts::units(sdng[,xname])
            if (xunits != xtunits)
                stop(paste("units of",xname,"are changing,",xunits,"!=",xtunits))
            xtreversed <- m1 > m2
            if (xreversed != xtreversed)
                stop(paste("upward/downward trend of",xname,"seems to be changing over these soundings"))
        }

        # interpolate over log of pressure
        if (xunits == "mb") xin <- log(xin)

        rmat <- matrix(NA,ncol=length(ynames)+1,nrow=length(xout),
                dimnames=list(NULL,c(xname,ynames)))

        rmat[,xname] <- xout

        for (yname in ynames) {

            if (is.na(match(yname,vnames))) stop(paste(yname,"not found in",sname))

            yin <- sdng@data[xok,yname]

            if (sum(!is.na(yin)) > 1)
                rmat[,yname] <- approx(xin,yin,xiout)$y
            NULL
        }

        tin <- as.numeric(positions(sdng))[xok]
        t0  <- tin[1]
        tn  <- tail(tin,1)
        tx <- utime(t0 + seq(from=0,to=tn-t0,length.out=length(xout)))

        yunits <- eolts::units(sdng[,ynames])

        rmat <- dat(nts(rmat,tx,units=c(xunits,yunits)))
        res[sname] <- rmat
        NULL
    }
    res
}
