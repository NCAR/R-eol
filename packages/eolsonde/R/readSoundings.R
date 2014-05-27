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
    file=c(
        "D%Y%m%d_%H%M%S_P\\.[0-9]+",
        "D%Y%m%d_%H%M%S_P\\.[0-9]+.gz",
        "D%Y%m%d_%H%M%S\\.[0-9]+",
        "D%Y%m%d_%H%M%S\\.[0-9]+.gz"),
    start=dpar("start"),end=dpar("end"),sta_clean=TRUE
    )
{

    res <- list()
    for (dx in dir) {
        for (fx in file) {
            # convert time specification to a regular expression
            fpat <- gsub("%Y","[12][0-9]{3}",fx,fixed=TRUE)
            fpat <- gsub("%m","[01][0-9]",fpat,fixed=TRUE)
            fpat <- gsub("%d","[0-3][0-9]",fpat,fixed=TRUE)
            fpat <- gsub("%H","[0-2][0-9]",fpat,fixed=TRUE)
            fpat <- gsub("%M","[0-5][0-9]",fpat,fixed=TRUE)
            fpat <- gsub("%S","[0-5][0-9]",fpat,fixed=TRUE)
            # cat("f=",paste(dx,fpat,sep=.Platform$file.sep),"\n")
            files <- list.files(path=dx,pattern=fpat)
            # cat("files=",paste(files,collapse=","),"\n")
            if (length(files) > 0) {

                # In order to parse the time from a file name, remove everything
                # after the %S
                timepat <- sub("%S.*","%S",fx)

                # Parse file names to get start times
                # Any that fail to parse return NA, and we'll ignore them.
                ftimes <- as.numeric(strptime(files,format=timepat,tz="UTC"))
                files <- files[!is.na(ftimes)]
                ftimes <- ftimes[!is.na(ftimes)]
                ftimes <- utime(ftimes)
                fi <- order(ftimes)

                ftimes <- ftimes[fi]
                files <- files[fi]

                fwithin <- ftimes >= start & ftimes <= end

                if (any(fwithin)) files <- files[fwithin]
                else {
                    warning(paste(length(files),"files found in",dir,
                        " but none with times between ",
                        format(start,format="%Y %b %d %H:%M:%S",time.zone="GMT"),"and",
                        format(end,format="%Y %b %d %H:%M:%S %Z",time.zone="GMT")))
                    next
                }

                # cat("files=",paste(files,collapse=","),"\n")

                for (fx in files) {
                    # browser()
                    # return a list of soundings
                    res[[fx]] <- readDFile(paste(dx,fx,sep=.Platform$file.sep),sta_clean=sta_clean)

                    srec <- sum(substr(res[[fx]]@data[,"sta"],1,1) == "S")
                    ptuok <- sum(substr(res[[fx]]@data[,"sta"],1,2) == "S0")
                    gpsok <- sum(substr(res[[fx]]@data[,"sta"],1,3) == "S00"
                            | substr(res[[fx]]@data[,"sta"],1,3) == "S10")
                    if (sta_clean) {
                        cat("read",fx,", nrow=",nrow(res[[fx]]),
                            ",#PTUOK=",ptuok,",#GPSOK=",gpsok,"\n")
                    } else {
                        prec <- sum(substr(res[[fx]]@data[,"sta"],1,1) == "P")
                        arec <- sum(substr(res[[fx]]@data[,"sta"],1,1) == "A")
                        cat("read",fx,", nrow=",nrow(res[[fx]]),
                            ",#A=",arec,",#P=",prec,",#S=",srec,
                            ",#PTUOK=",ptuok,",#GPSOK=",gpsok,"\n")
                    }
                }
            }
        }
    }
    if (length(res) == 0) warning("No sounding files found")
    res
}
