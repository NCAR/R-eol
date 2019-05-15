# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsonde" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

test_dfile <- function(pkg="eolsonde")
{

    datadir <- file.path(system.file("unitTests", package=pkg),"data","D-file")
    cat("datadir=",datadir,"\n")

    options(time.zone="UTC")

    dfile <- file.path(datadir,"D20080815_172011_P.1.gz")
    cat("dfile=",dfile,"\n")

    dpar(checkSondeStatus=FALSE,sondeRecords=c("A","P","S"))

    sdng <- readDFile(dfile)
    cat("dim(sdng)=",paste(dim(sdng),collapse=","),"\n")
    cat("colnames(sdng)=",paste(colnames(sdng),collapse=","),"\n")

    checkEquals(dim(sdng),c(1176-6,17))

    nr <- nrow(sdng)

    # readDFile sorts the time series

    cat("t1=",format(positions(sdng)[1],format="%Y %m %d %H:%M:%OS"),"\n")
    checkEquals(positions(sdng)[1],utime("2008 08 15 17:20:01.82"),tolerance=0)

    cat("t2=",format(positions(sdng)[nr],format="%Y %m %d %H:%M:%OS"),"\n")
    checkEquals(positions(sdng)[nr],utime("2008 08 15 17:29:45.82"),tolerance=0)

    checkTrue(is.na(as.numeric(sdng[1,"P"])))
    checkEquals(as.numeric(sdng[20,"P"]),391.4)

    checkTrue(is.na(as.numeric(sdng[1,"Alt_gps"])))
    checkEquals(as.numeric(sdng[20,"Alt_gps"]),7752.0)

    checkTrue(is.na(as.numeric(sdng[nr,"P"])))
    checkEquals(as.numeric(sdng[nr,"Alt_gps"]),67.01)


    # read dfile with checkSondeStatus=TRUE
    dpar(checkSondeStatus=TRUE,sondeRecords="S")

    sdng <- readDFile(dfile)
    cat("dim(sdng)=",paste(dim(sdng),collapse=","),"\n")
    cat("colnames(sdng)=",paste(colnames(sdng),collapse=","),"\n")

    checkEquals(dim(sdng),c(1176-27,17))

    nr <- nrow(sdng)

    cat("t1=",format(positions(sdng)[1],format="%Y %m %d %H:%M:%OS"),"\n")
    checkEquals(positions(sdng)[1],utime("2008 08 15 17:20:11.82"),tolerance=0)

    cat("t2=",format(positions(sdng)[nr],format="%Y %m %d %H:%M:%OS"),"\n")
    checkEquals(positions(sdng)[nr],utime("2008 08 15 17:29:45.82"),tolerance=0)

    checkTrue(is.na(as.numeric(sdng[1,"P"])))
    checkTrue(is.na(as.numeric(sdng[1,"Alt_gps"])))

    checkTrue(is.na(as.numeric(sdng[nr,"P"])))
    checkEquals(as.numeric(sdng[nr,"Alt_gps"]),67.01)

}
