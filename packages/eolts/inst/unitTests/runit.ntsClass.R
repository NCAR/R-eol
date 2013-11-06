test.nts = function()
{
    t1 = utime("2013 10 13 01:02:03.4")
    nr = 5
    nc = 3
    dt = 10
    tx = seq(from=t1,by=dt,length=nr)
    dns <- paste("c",1:nc,sep="")
    xunits <- paste("u",1:nc,sep="")

    x = nts(matrix(1:(nr*nc),nrow=nr,dimnames=list(NULL,dns)), tx,xunits)

    checkEquals(dim(x),c(nr,nc))
    checkEquals(dimnames(x)[[2]],dns)
    checkEquals(units(x),xunits)

    checkEquals(as.numeric(x[3,1]),3)
    checkEquals(as.vector(x[2:3,2]),c(nr+2,nr+3))
    checkEquals(as.numeric(x[2:3,2]),c(nr+2,nr+3))
    checkEquals(positions(x[3,]),t1+20)
    checkEquals(dim(x[1:3,2]),c(3,1))

    # check of deltat
    xdt = rep(dt,6)
    names(xdt) = c("","median","min","max","mean","trimmed.mean")
    checkEquals(deltat(x),xdt)

    # check of indexing rows with utime
    ti = c(t1 + dt/2, t1 + 2 * dt + dt/2)
    checkEquals(dim(x[ti,]),c(2,nc))

    # index and operation
    xx = x[,1] + x[,2]
    checkEquals(dim(xx),c(nr,1))
    checkEquals(dimnames(xx)[[2]],dns[1])
    checkEquals(units(xx),xunits[1])
    checkEquals(as.numeric(xx[3,1]),3 + 3 + nr)
    checkEquals(positions(xx[3,]),t1+20)

    xx = x[x < 5]
    # xx is not an nts
    checkEquals(class(xx),"integer")    # 1:N is integer
    checkEquals(xx,1:4)

    xx = x[-2,]

    # operation where operands have different number of rows
    xxx = xx * x
    checkEquals(xxx@data,(x[-2,]^2)@data)

    # add weights and stations
    wts = matrix(rep(seq(from=1.0,by=0.1,length=nc-1),rep(nr,nc-1)),nrow=nr)
    wtmap = rep(1,nc)
    wtmap[nc] = nc - 1

    x = nts(matrix(1:(nr*nc),nrow=nr,dimnames=list(NULL,dns)), tx,xunits,
        weights=wts,weightmap=wtmap,stations=1:nc)

    # check that weights are indexed properly
    checkEquals(x[,nc]@weights,wts[,nc-1,drop=F])
    checkEquals(x[,nc-2]@weights,wts[,1,drop=F])

    checkEquals(stations(x[,2:3]),2:3)



    # align: align/interpolate a time series to a new positions
    t1n = floor(t1)
    txn = seq(from=t1n,by=dt,length=nr)
    dtxx = t1 - t1n

    # tx    x[,1]  txn     xx[,1]
    #  0.4   1      0       NA
    # 10.4   2      10      1.96
    # 20.4   3      20      2.96
    # 30.4   4      30      3.96
    # 40.4   5      40      4.96
    xx = align(x,txn,how="interp",matchtol=dt/2)
    checkEquals(dim(xx),c(nr,nc))
    checkTrue(all(is.na(xx[1,])))
    checkTrue(all(abs((x[-1,]-xx[-1,])-(dtxx/dt)) < .00001))

    # tx    x[,1]   txn   xx[,1]
    #  0.4   1      0       NA
    # 10.4   2      10      1.96
    # 20.4   3      20      2.96
    #                       
    # 40.4   5      40      4.96
    xx = align(x[-4,],txn,how="interp",matchtol=dt/2)
    # should be as above, but no value for time 3
    checkEquals(dim(xx),c(nr-1,nc))
    checkTrue(all(is.na(xx[1,])))
    checkTrue(all(abs((x[c(-1,-4),]-xx[-1,])-(dtxx/dt)) < .00001))


    # TODO: seriesMerge, Cbind, Rbind

    return()
}
