# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsonde" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

test_qcfile <- function()
{

    wd <- getwd()
    cat("wd=",wd,"\n")
    pkg <- sub("\\.Rcheck$", '', basename(dirname(wd)))
    cat("pkg=",pkg,"\n")

    datadir <- file.path(system.file("unitTests", package=pkg),"data","qc.eol")
    cat("datadir=",datadir,"\n")

    options(time.zone="UTC")

    qcfile <- file.path(datadir,"D20080815_172011_PQC.eol")
    cat("qcfile=",qcfile,"\n")

    sdng <- readQCFile(qcfile)
    cat("dim(sdng)=",paste(dim(sdng),collapse=","),"\n")
    cat("colnames(sdng)=",paste(colnames(sdng),collapse=","),"\n")

    checkEquals(dim(sdng),c(648-14,14))

    nr <- nrow(sdng)

    cat("t1=",format(positions(sdng)[1],format="%Y %m %d %H:%M:%OS"),"\n")
    checkEquals(positions(sdng)[1],utime("2008 08 15 17:20:10.00"))

    cat("t2=",format(positions(sdng)[nr],format="%Y %m %d %H:%M:%OS"),"\n")
    checkEquals(positions(sdng)[nr],utime("2008 08 15 17:29:46.04"))

    checkEquals(as.numeric(sdng[1,"p"]),391.4)
    checkEquals(as.numeric(sdng[1,"gps.alt"]),7752.0)

    checkTrue(is.na(as.numeric(sdng[nr-1,"p"])))
    checkEquals(as.numeric(sdng[nr,"p"]),1009.16)

    checkEquals(as.numeric(sdng[nr-1,"gps.alt"]),67.01)
    checkTrue(is.na(as.numeric(sdng[nr,"gps.alt"])))
}
