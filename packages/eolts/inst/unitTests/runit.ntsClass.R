test.nts = function()
{
    t1 = utime("2013 10 13 01:02:03.4")
    nr = 4
    nc = 3
    dtby = 10
    tx = seq(from=t1,by=dtby,length=nr)
    dns <- c("a","b","c")
    xunits <- c("x","y","z")

    x = nts(matrix(1:(nr*nc),nrow=nr,dimnames=list(NULL,dns)), tx,xunits)

    checkEquals(dim(x),c(nr,nc))
    checkEquals(dimnames(x)[[2]],dns)
    checkEquals(units(x),xunits)

    checkEquals(as.numeric(x[3,1]),3)
    checkEquals(as.vector(x[2:3,2]),c(6,7))
    checkEquals(as.numeric(x[2:3,2]),c(6,7))
    checkEquals(positions(x[3,]),t1+20)

    xdt = rep(dtby,6)
    names(xdt) = c("","median","min","max","mean","trimmed.mean")

    checkEquals(deltat(x),xdt)

    checkEquals(dim(x[1:3,2]),c(3,1))

    xx = x[,1] + x[,2]
    checkEquals(dim(xx),c(nr,1))
    checkEquals(dimnames(xx)[[2]],dns[1])
    checkEquals(units(xx),xunits[1])

    checkEquals(as.numeric(xx[3,1]),10)
    checkEquals(positions(xx[3,]),t1+20)

    xx = x[x < 5]
    checkEquals(class(xx),"integer")    # 1:12 is integer
    checkEquals(xx,1:4)

    xx = x[-2,]

    xxx = xx * x
    checkEquals(xxx@data,(x[-2,]^2)@data)

    return()
}
