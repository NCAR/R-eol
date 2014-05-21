# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsonde" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

readSoundings <- function(
    dir=c(paste(Sys.getenv("SONDE_ROOT"),"projects",Sys.getenv("PROJECT"),
        Sys.getenv("PLATFORM"),Sys.getenv("DATASET"),sep=.Platform$file.sep),
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
                # determine last file whose time is less than start
                before <- ftimes < start
                if (any(before)) {
                    before <- max(seq(along=files)[before])
                    fwithin[before] <- TRUE
                }
                if (any(fwithin)) files <- files[fwithin]
                else warning(paste("no files found in",dir,"between ",
                        format(start,format="%Y %b %d %H:%M:%S",time.zone="GMT"),"and",
                        format(end,format="%Y %b %d %H:%M:%S %Z",time.zone="GMT")))

                cat("files=",paste(files,collapse=","),"\n")

                for (fx in files) {
                    # browser()
                    # return a list of soundings
                    res[[fx]] <- readDFile(paste(dx,fx,sep=.Platform$file.sep),sta_clean=sta_clean)
                }
            }
        }
    }
    res
}
