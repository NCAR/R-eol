# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolts" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

test.nts <- function()
{
    t1 <- utime("2013 10 13 01:02:03.4")
    nr <- 20
    nc <- 3
    dt <- 10
    tx <- seq(from=t1,by=dt,length=nr)
    dns <- paste0("c",1:nc)
    xunits <- paste0("u",1:nc)

    x <- nts(matrix(1:(nr*nc),nrow=nr,dimnames=list(NULL,dns)), tx,xunits)

    checkTrue(class(x)[1] == "nts")
    checkEquals(dim(x),c(nr,nc))
    checkEquals(dimnames(x)[[2]],dns)
    checkEquals(units(x),xunits)

    checkEquals(as.numeric(x[3,1]),3)
    checkEquals(as.vector(x[2:3,2]),c(nr+2,nr+3))
    checkEquals(as.numeric(x[2:3,2]),c(nr+2,nr+3))
    checkEquals(as.numeric(positions(x[3,])),as.numeric(t1+20),tolerance=0)
    checkEquals(dim(x[1:3,2]),c(3,1))

    # check of deltat
    xdt <- rep(dt,6)
    names(xdt) <- c("","median","min","max","mean","trimmed.mean")
    checkEquals(deltat(x),xdt)

    # check of indexing rows with utime
    ti <- c(t1 + dt/2, t1 + 2 * dt + dt/2)
    checkEquals(class(x[ti,])[1],"nts")
    checkEquals(dim(x[ti,]),c(2,nc))

    # index and operation
    xx <- x[,1] + x[,2]
    checkEquals(class(xx)[1],"nts")
    checkEquals(dim(xx),c(nr,1))
    checkEquals(dimnames(xx)[[2]],dns[1])
    checkEquals(units(xx),xunits[1])
    checkEquals(as.numeric(xx[3,1]),3 + 3 + nr)
    checkEquals(as.numeric(positions(xx[3,])),as.numeric(t1+20),tolerance=0)

    xx <- x[x < 5]
    # xx is not an nts
    checkEquals(class(xx)[1],"integer")    # 1:N is integer
    checkEquals(xx,1:4)

    xx <- x[-2,]
    checkEquals(class(xx)[1],"nts")
    checkEquals(nrow(xx), nr - 1)

    # operation where operands have different number of rows
    xxx <- xx * x
    checkEquals(xxx@data,(x[-2,]^2)@data)

    # using one nts to index another. 
    # generate shorter nts with slightly different times
    nry <- nr - 5
    ncy <- nc
    # different times than x
    ty <- tx[3:(nry+2)] + runif(nry) - 0.5
    y <- nts(matrix(runif(nry*ncy), nrow=nry),ty, names=paste0("y",1:ncy),
        units=paste0("u",1:ncy))

    # Times are matched within options("dt.eps")
    # default dt.eps is 1/10 of deltat(x) which is 1 in this test
    # save default, probably NULL
    dt.eps <- options("dt.eps")

    # this check should all be TRUE, so no change in length returned
    n1 <- length(x[y < 1.5])
    checkEquals(n1, prod(dim(y)))

    # length of result should be less than length of x
    n1 <- length(x[y < 0.5])
    checkTrue(n1 < prod(dim(x)))

    # tighten time comparison limit, should match fewer elements
    options(dt.eps=dt/100)
    n2 <- length(x[y < 0.5])
    checkTrue(n2 < prod(dim(x)), "length(x[y < 0.5]), dt.eps=dt/100")
    checkTrue(n2 < n1, "length(x[y < 0.5]) decreasing, dt.eps=dt/100")

    options(dt.eps=dt.eps)
    xx <- x
    checkEquals(sum(is.na(xx)), 0)

    xx[y < 0.5] <- xx[y < 0.5] + NA
    n1 <- sum(is.na(xx))
    # should have matched some elements
    checkTrue(n1 > 0)

    # tighten time comparison limit, should match fewer elements
    options(dt.eps=dt / 100)
    xx <- x
    xx[y < 0.5] <- xx[y < 0.5] + NA
    n2 <- sum(is.na(xx))
    checkTrue(n2 < n1, "sum(is.na(xx))")

    # reset back to original value
    options(dt.eps)

    # add weights and stations
    wts <- matrix(rep(seq(from=1.0,by=0.1,length=nc-1),rep(nr,nc-1)),nrow=nr)
    wtmap <- rep(1,nc)
    wtmap[nc] <- nc - 1

    x <- nts(matrix(1:(nr*nc),nrow=nr,dimnames=list(NULL,dns)), tx,xunits,
        weights=wts,weightmap=wtmap,stations=1:nc)

    # check that weights are indexed properly
    checkEquals(x[,nc]@weights,wts[,nc-1,drop=F])
    checkEquals(x[,nc-2]@weights,wts[,1,drop=F])

    checkEquals(stations(x[,2:3]),2:3)

    # align: align/interpolate a time series to a new positions
    t1n <- floor(t1)
    txn <- seq(from=t1n,by=dt,length=nr)
    dtxx <- t1 - t1n

    # tx    x[,1]  txn     xx[,1]
    #  0.4   1      0       NA
    # 10.4   2      10      1.96
    # 20.4   3      20      2.96
    # 30.4   4      30      3.96
    # 40.4   5      40      4.96
    xx <- align(x,txn,how="interp",matchtol=dt/2)
    checkEquals(class(xx)[1],"nts")
    checkEquals(dim(xx),c(nr,nc))
    checkTrue(all(is.na(xx[1,])))
    checkTrue(all(abs((x[-1,]-xx[-1,])-(dtxx/dt)) < .00001))

    # tx    x[,1]   txn   xx[,1]
    #  0.4   1      0       NA
    # 10.4   2      10      1.96
    # 20.4   3      20      2.96
    #                       
    # 40.4   5      40      4.96
    xx <- align(x[-4,],txn,how="interp",matchtol=dt/2)
    checkEquals(class(xx)[1],"nts")
    # should be as above, but no value for time 3
    checkEquals(dim(xx),c(nr-1,nc))
    checkTrue(all(is.na(xx[1,])))
    checkTrue(all(abs((x[c(-1,-4),]-xx[-1,])-(dtxx/dt)) < .00001))

    xx <- align(x,txn,how="before",error.how="NA",matchtol=dt/2)
    checkEquals(class(xx)[1],"nts")
    checkEquals(dim(xx),c(nr,nc))
    checkEquals(x@data,xx@data)

    xx <- Rbind(x[1:as.integer(nr/2),],x[(as.integer(nr/2)+1):nr,])
    checkEquals(class(xx)[1],"nts")
    checkEquals(dim(x),dim(xx))
    checkEquals(x@data,xx@data)
    for (ic in 1:nc) {
        checkEquals(x[,ic]@weights,xx[,ic]@weights)
    }

    xxx <- Cbind(x,xx)
    checkEquals(class(xxx)[1],"nts")
    checkEquals(nrow(xxx),nrow(x))
    checkEquals(ncol(xxx),ncol(x) + ncol(xx))
    checkEquals(x@data,xxx@data[,1:nc])
    checkEquals(xx@data,xxx@data[,(nc+1):(nc*2)])

    xxx <- Cbind(x,xx,xx)
    checkEquals(class(xxx)[1],"nts")
    checkEquals(nrow(xxx),nrow(x))
    checkEquals(ncol(xxx),ncol(x) + 2 * ncol(xx))
    checkEquals(x@data,xxx@data[,1:nc])
    checkEquals(xx@data,xxx@data[,(nc+1):(nc*2)])
    checkEquals(xx@data,xxx@data[,(nc*2+1):(nc*3)])

    # approx x to txn times, which are earlier by 0.4 sec
    xx <- approx(x,xout=txn,method="constant",f=0,rule=2)
    # checkEquals(class(xx)[1],"nts")
    checkEquals(dim(xx),c(nr,nc))
    # first value of x will be in rows 1 and 2 of new timeseries
    # last value of x won't exist in new timeseries. 
    checkTrue(all(abs((x@data[-nr,]-xx@data[-1,])) < .00001))
    checkTrue(all(abs((xx@data[1,]-xx@data[2,])) < .00001))

    # TODO: seriesMerge

    return()
}
