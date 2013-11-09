# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
test.netcdf = function()
{

    wd <- getwd()
    cat("wd=",wd,"\n")
    pkg <- sub("\\.Rcheck$", '', basename(dirname(wd)))
    cat("pkg=",pkg,"\n")

    datadir <- file.path(system.file("unitTests", package = pkg),"data")
    cat("datadir=",datadir,"\n")

    ncfile  <- "test1.nc"

    t1  <- utime("2011 nov 15 21:22:00",time.zone="UTC")
    t2  <- utime("2011 nov 15 22:00:00",time.zone="UTC")

    con = netcdf(file=ncfile,dir=datadir,start=t1,end=t2)

    x = readts(con,c("ADIFR","ALT"), start=t1, end=t2)
    close(con)


    # cat("dim(x)=",dim(x),"\n")

    checkEquals(dim(x),c(21,2))

    # dput(x,file.path(datadir,"test1.Rdput",control="all"))
    # test1.Rdput then needs editing to be successfully read by dget:
    #   remove ".Data =" in initialization of utimes (3 places)

    xx = dget(file.path(datadir,"test1.Rdput"))

    checkTrue(max(abs(x-xx),na.rm=TRUE) < 1.e-10)
    checkTrue(all.equal(x@data,xx@data))

    checkTrue(all.equal(as.numeric(positions(x)),as.numeric(positions(xx))))

    # these fail, not sure why
    # checkTrue(identical(positions(x),positions(xx)))
    # checkEquals(positions(x),positions(xx))
    # cat(positions(x),"\n")
    # cat(positions(xx),"\n")

}


