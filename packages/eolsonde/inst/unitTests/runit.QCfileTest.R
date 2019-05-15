# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsonde" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

test_qcfile <- function(pkg="eolsonde")
{

    datadir <- file.path(system.file("unitTests", package=pkg),"data","qc.eol")
    cat("datadir=",datadir,"\n")

    options(time.zone="UTC")

    qcfile <- file.path(datadir,"D20080815_172011_PQC.eol.gz")
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

    checkEquals(as.numeric(sdng[1,"P"]),391.4)
    checkEquals(as.numeric(sdng[1,"Alt_gps"]),7752.0)

    checkTrue(is.na(as.numeric(sdng[nr-1,"P"])))
    checkEquals(as.numeric(sdng[nr,"P"]),1009.16)

    checkEquals(as.numeric(sdng[nr-1,"Alt_gps"]),67.01)
    checkTrue(is.na(as.numeric(sdng[nr,"Alt_gps"])))
}
