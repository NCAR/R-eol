# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsonde" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

readSoundings <- function(
    dir=c(file.path(Sys.getenv("SONDE_ROOT"),"projects",Sys.getenv("PROJECT"),
        Sys.getenv("PLATFORM"),"data",Sys.getenv("DATASET")),
        Sys.getenv("SONDE_DATA")),
    file=c("D%Y%m%d_%H%M%S_P"),
    start=dpar("start"),end=dpar("end"),sta_clean=TRUE
    )
{

    files <- list()

    for (dx in dir) {
        files <- append(files,lapply(file,function(fpat)
            {
                # convert time specification to a regular expression
                fre <- gsub("%Y","[12][0-9]{3}",fpat,fixed=TRUE)
                fre <- gsub("%m","[01][0-9]",fre,fixed=TRUE)
                fre <- gsub("%d","[0-3][0-9]",fre,fixed=TRUE)
                fre <- gsub("%H","[0-2][0-9]",fre,fixed=TRUE)
                fre <- gsub("%M","[0-5][0-9]",fre,fixed=TRUE)
                fre <- gsub("%S","[0-5][0-9]",fre,fixed=TRUE)
                # cat("fs=",paste(fs,collapse=","),"\n")

                # If no time descriptors in fpat, return the list of files
                fs <- list.files(path=dx,pattern=fre)

                if (fre == fpat) {
                    fs
                } else {
                    if (length(fs) == 0)
                        fs <- list.files(path=dx,pattern=paste0(fre,".*"))
                    lapply(fs,function(f)
                        {
                            # Parse file names to get start times
                            # Any that fail to parse return NA, and we'll ignore them.
                            ft <- as.numeric(strptime(f,format=fpat,tz="UTC"))
                            if (is.null(ft)) {
                                # try removing possible REs after the %S
                                fpat <- sub("%S.*","%S",fpat)
                                ft <- as.numeric(strptime(f,format=fpat,tz="UTC"))
                            }
                            if (!is.na(ft) && ft >= start && ft <= end) list(f=f,t=utime(ft),d=dx)
                            else NULL
                        }
                    )
                }
            }
        ))
    }

    # browser()

    # remove top level names, the file patterns
    files <- unlist(files,recursive=FALSE)

    files <- files[!sapply(files,is.null)]

    # get file names, remove duplicates
    fnames <- unlist(sapply(files,function(f){f$f}))
    files <- files[match(unique(fnames),fnames)]

    ftimes <- sapply(files,function(fx) { fx$t })
    fi <- order(ftimes)
    files <- files[fi]

    sdngs <- lapply(files,function(fx)
        {
            fp <- file.path(fx$d,fx$f)
            # read first string of the file. If it contains "AVAPS", call readDFile
            ftype <- scan(fp,nmax=1,what="")
            if (grepl("AVAPS",ftype,fixed=TRUE)) {
                sdng <- readDFile(fp,sta_clean=sta_clean)
                srec <- sum(substr(sdng@data[,"sta"],1,1) == "S")
                ptuok <- sum(substr(sdng@data[,"sta"],1,2) == "S0")
                gpsok <- sum(substr(sdng@data[,"sta"],1,3) == "S00"
                        | substr(sdng@data[,"sta"],1,3) == "S10")
                if (sta_clean) {
                    cat("read",fp,", dim=",paste(dim(sdng),collapse=","),
                        ",#PTUOK=",ptuok,",#GPSOK=",gpsok,"\n")
                } else {
                    prec <- sum(substr(sdng@data[,"sta"],1,1) == "P")
                    arec <- sum(substr(sdng@data[,"sta"],1,1) == "A")
                    cat("read",fp,", dim=",paste(dim(sdng),collapse=","),
                        ",#A=",arec,",#P=",prec,",#S=",srec,
                        ",#PTUOK=",ptuok,",#GPSOK=",gpsok,"\n")
                }
            } else {
                sdng <- readQCFile(fp)
                cat("read",fp,", dim=",paste(dim(sdng),collapse=","),"\n")
            }
            sdng
        }
    )
    if (length(sdngs) == 0) warning("No soundings found")
    names(sdngs) <- sapply(files,function(fx) { fx$f })
    sdngs
}
