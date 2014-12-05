# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsonde" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

readSoundings <- function(
    dir=c(file.path(Sys.getenv("SONDE_ROOT"),"projects",Sys.getenv("SPROJECT"),
        Sys.getenv("SPLATFORM"),"data",Sys.getenv("SDATASET")),
        Sys.getenv("SONDE_DATA")),
    file=c("D%Y%m%d_%H%M%S_P"),
    start=dpar("start"),end=dpar("end")
    )
{

    files <- list()
    timeMisMatch <- NULL

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
                    # Add a trailing wildcard if no match
                    if (length(fs) == 0)
                        fs <- list.files(path=dx,pattern=paste0(fre,".*"))
                    lapply(fs,function(f)
                        {
                            # Parse file names to get start times
                            # Any that fail to parse return NA, and we'll ignore them.
                            ft <- as.numeric(strptime(f,format=fpat,tz="UTC"))
                            if (is.na(ft)) {
                                # try removing possible REs after the %S
                                fpat <- sub("%S.*","%S",fpat)
                                ft <- as.numeric(strptime(f,format=fpat,tz="UTC"))
                            }
                            if (!is.na(ft) && ft >= start && ft <= end)
                                list(f=f,t=utime(ft),d=dx)
                            else {
                                timeMisMatch <<- append(timeMisMatch,f)
                                NULL
                            }
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

    if (length(files) == 0) {
        if (length(timeMisMatch) > 0)
            warning(paste0(length(timeMisMatch),
                    " files matched ",paste(file,collapse=","),
                    " in ",paste(dir,collapse=","),
                    " but none are within times: ",
                    eolts::format(start,"%Y %m %d %H:%M:%S"),"-",
                    eolts::format(end,"%Y %m %d %H:%M:%S"),". For example=",timeMisMatch[1]))
        else warning(paste("no files matching",paste(file,collapse=", "),"in directories",
                paste(dir,collapse=", ")))
        return(NULL)
    }

    # get file names, remove duplicates
    fnames <- unlist(sapply(files,function(f){f$f}))
    files <- files[match(unique(fnames),fnames)]

    ftimes <- sapply(files,function(fx) { fx$t })
    fi <- order(ftimes)
    files <- files[fi]

    sdngs <- lapply(files,function(fx)
        {
            fp <- file.path(fx$d,fx$f)
            # shortened name for messages
            fpshort <- ifelse(nchar(fp) > 45,
                paste0("...",substring(fp,nchar(fp)-45)),fp)
            # read first string of the file. If it contains "AVAPS", call readDFile
            ftype <- scan(fp,nmax=1,what="",quiet=TRUE)
            if (grepl("AVAPS",ftype,fixed=TRUE)) {
                sdng <- readDFile(fp)

                ptuok <- sum((sdng@data[,"status"] %% 10) < 2,na.rm=TRUE)  # 00, 01
                gpsok <- sum(sdng@data[,"status"] %% 2 == 0,na.rm=TRUE)    # 00, 10

                if (dpar("checkSondeStatus")) {
                    cat(paste0(fpshort,": (",paste(dim(sdng),collapse="x"),
                        "), #PTUOK=",ptuok,", #GPSOK=",gpsok),"\n")
                } else {
                    tendig <- sdng@data[,"status"] %/% 10
                    arec <- sum(tendig == 0,na.rm=TRUE)
                    prec <- sum(tendig == 1,na.rm=TRUE)
                    srec <- sum(tendig == 2,na.rm=TRUE)
                    cat(paste0(fpshort,": (",paste(dim(sdng),collapse="x"),
                        "), #A=",arec,", #P=",prec,", #S=",srec,
                        ", #PTUOK=",ptuok,", #GPSOK=",gpsok),"\n")
                }
            } else {
                sdng <- readQCFile(fp)
                cat(paste0(fpshort,": (",paste(dim(sdng),collapse="x"),")"),"\n")
            }
            sdng
        }
    )
    names(sdngs) <- sapply(files,function(fx) { fx$f })
    sdngs
}
