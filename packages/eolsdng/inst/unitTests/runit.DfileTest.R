# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsdng" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

test_dfile <- function()
{

    wd <- getwd()
    cat("wd=",wd,"\n")
    pkg <- sub("\\.Rcheck$", '', basename(dirname(wd)))
    cat("pkg=",pkg,"\n")

    datadir <- file.path(system.file("unitTests", package=pkg),"data","D-file")
    cat("datadir=",datadir,"\n")

    options(time.zone="UTC")

    dfile <- file.path(datadir,"D20080815_172011_P.1")
    cat("dfile=",dfile,"\n")

    sdng <- readDFile(dfile,sta_clean=FALSE)
    cat("dim(sdng)=",paste(dim(sdng),collapse=","),"\n")
    cat("colnames(sdng)=",paste(colnames(sdng),collapse=","),"\n")

    checkEquals(dim(sdng),c(1176-6,17))

    nr <- nrow(sdng)

    cat("t1=",format(positions(sdng)[1],format="%Y %m %d %H:%M:%OS"),"\n")
    checkEquals(positions(sdng)[1],utime("2008 08 15 17:20:11.36"))

    cat("t2=",format(positions(sdng)[nr],format="%Y %m %d %H:%M:%OS"),"\n")
    checkEquals(positions(sdng)[nr],utime("2008 08 15 17:29:45.82"))

    checkEquals(as.numeric(sdng[1,"p"]),391.4)
    checkEquals(as.numeric(sdng[1,"gps.alt"]),7752.0)

    checkTrue(is.na(as.numeric(sdng[nr,"p"])))
    checkEquals(as.numeric(sdng[nr,"gps.alt"]),67.01)


    # read dfile with sta_clean=TRUE

    sdng <- readDFile(dfile,sta_clean=TRUE)
    cat("dim(sdng)=",paste(dim(sdng),collapse=","),"\n")
    cat("colnames(sdng)=",paste(colnames(sdng),collapse=","),"\n")

    checkEquals(dim(sdng),c(1176-27,17))

    nr <- nrow(sdng)

    cat("t1=",format(positions(sdng)[1],format="%Y %m %d %H:%M:%OS"),"\n")
    checkEquals(positions(sdng)[1],utime("2008 08 15 17:20:11.82"))

    cat("t2=",format(positions(sdng)[nr],format="%Y %m %d %H:%M:%OS"),"\n")
    checkEquals(positions(sdng)[nr],utime("2008 08 15 17:29:45.82"))

    checkTrue(is.na(as.numeric(sdng[1,"p"])))
    checkTrue(is.na(as.numeric(sdng[1,"gps.alt"])))

    checkTrue(is.na(as.numeric(sdng[nr,"p"])))
    checkEquals(as.numeric(sdng[nr,"gps.alt"]),67.01)

}
