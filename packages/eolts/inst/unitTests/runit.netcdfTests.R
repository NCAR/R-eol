# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolts" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

test.netcdf <- function()
{

    wd <- getwd()
    cat("wd=",wd,"\n")
    pkg <- sub("\\.Rcheck$", '', basename(dirname(wd)))
    cat("pkg=",pkg,"\n")

    datadir <- file.path(system.file("unitTests", package=pkg),"data")
    cat("datadir=",datadir,"\n")

    ncfile  <- "test1.nc"

    t1  <- utime("2011 nov 15 21:22:00",time.zone="UTC")
    t2  <- utime("2011 nov 15 22:00:00",time.zone="UTC")

    con <- netcdf(file=ncfile,dir=datadir,start=t1,end=t2)

    x <- readts(con,c("ADIFR","ALT"), start=t1, end=t2)
    close(con)


    # cat("dim(x)=",dim(x),"\n")

    checkEquals(dim(x),c(21,2))

    # dput(x,file.path("/tmp","test1.Rdput"),control="all")
    # test1.Rdput then needs editing to be successfully read by dget:
    #   remove ".Data =" in initialization of utimes (3 places)

    xx <- dget(file.path(datadir,"test1.Rdput"))

    checkTrue(max(abs(x-xx),na.rm=TRUE) < 1.e-10)
    checkTrue(all.equal(x@data,xx@data))

    checkTrue(all.equal(as.numeric(positions(x)),as.numeric(positions(xx))))

    # these fail, not sure why
    # checkTrue(identical(positions(x),positions(xx)))
    # checkEquals(positions(x),positions(xx))
    # cat(positions(x),"\n")
    # cat(positions(xx),"\n")

}

test.netcdf2 <- function()
{

    wd <- getwd()
    cat("wd=",wd,"\n")
    pkg <- sub("\\.Rcheck$", '', basename(dirname(wd)))
    cat("pkg=",pkg,"\n")

    datadir <- file.path(system.file("unitTests", package=pkg),"data")
    cat("datadir=",datadir,"\n")

    ncfile  <- "test_%Y%m%d.nc"

    t1  <- utime("2012 oct 10 19:00:00",time.zone="US/Mountain")
    t2  <- utime("2012 oct 11 20:00:00",time.zone="US/Mountain")

    con <- netcdf(file=ncfile,dir=datadir,start=t1,end=t2)

    on.exit(close(con))

    # test read of variables by their netcdf name and their short_name
    x <- readts(con,c("u_1m","u_10m_M"), start=t1, end=t2)
    cat("dim(x)=",dim(x),"\n")
    checkEquals(dim(x),c(300,22))

    x <- readts(con,c("u.1m","u.10m.M"), start=t1, end=t2,stns=0:21)
    cat("dim(x)=",dim(x),"\n")
    checkEquals(dim(x),c(300,22))

    # readts does not do any striding over the station dimension,
    # returning all stations between the min and the max non-zero stations
    # specified, so the stns sequence should increment by 1.
    x <- readts(con,c("u.1m","u_10m_M"), start=t1, end=t2,
        stns=c(0,seq(from=3,length=5)))
    cat("dim(x)=",dim(x),"\n")
    checkEquals(dim(x),c(300,6))

    # test2.Rdput then needs editing to be successfully read by dget:
    #   remove ".Data =" in initialization of utimes (3 places)
    # dput(x,file.path("/tmp","test2.Rdput"),control="all")
    xx <- dget(file.path(datadir,"test2.Rdput"))
    checkTrue(max(abs(x-xx),na.rm=TRUE) < 1.e-10)
    checkTrue(all.equal(x@data,xx@data))

}

