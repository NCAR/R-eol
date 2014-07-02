# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolts" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

setClass("netcdf",
    slots=c(
        file="character",
        dir="character",
        start="utime",
        end="utime",
        lenfile="integer",
        timeNames="character",
        server="character",
        interval="numeric",
        cdlfile="character",
        cppPtr="raw"
        ),
    prototype=list(
        file=Sys.getenv("NETCDF_FILE"),
        dir=Sys.getenv("NETCDF_DIR"),
        start=utime(0),
        end=utime(0),
        lenfile=86400L,
        timeNames=c("time","Time"),
        server=Sys.getenv("NETCDF_SERVER"),
        interval=300,
        cdlfile="",
        cppPtr=raw(8))
)

netcdf <- function(
    file=Sys.getenv("NETCDF_FILE"),
    dir=Sys.getenv("NETCDF_DIR"),
    start=dpar("start"),
    end=dpar("end"),
    lenfile=dpar("lenfile"),
    timeNames=c("time","Time"),
    server=Sys.getenv("NETCDF_SERVER"),
    interval=300,
    cdlfile="")
{
    if (is.null(lenfile)) lenfile <- 0
    obj <- new("netcdf",
        file=file,dir=dir,
        start=start,end=end,
        lenfile=as.integer(lenfile),
        timeNames=timeNames,
        server=server,interval=interval,cdlfile=cdlfile)

    if (lenfile == 0) {
        # Scan directory for files, then
        # if file contains any time descriptors, parse the presumed start
        # times from the file names
        hastimefmt <- any(sapply(c("%Y","%m","%d","%b","%H","%M","%S"),
                function(x,str){grepl(x,str,fixed=TRUE)},str=file))

        if (hastimefmt) {
            nmformat <- gsub("%Y","[12][0-9]{3}",file,fixed=TRUE)
            nmformat <- gsub("%m","[01][0-9]",nmformat,fixed=TRUE)
            nmformat <- gsub("%d","[0-3][0-9]",nmformat,fixed=TRUE)
            nmformat <- gsub("%H","[0-2][0-9]",nmformat,fixed=TRUE)
            nmformat <- gsub("%M","[0-5][0-9]",nmformat,fixed=TRUE)
            nmformat <- gsub("%S","[0-5][0-9]",nmformat,fixed=TRUE)

            files <- list.files(dir,nmformat)

            # Parse file names to get start times
            # Any that fail to parse return NA, and we'll ignore them.
            ftimes <- as.numeric(strptime(files,file,tz="GMT"))
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
        }
        else if (length(file) == 1)
            files <- list.files(dir,file)
        else
            files <- file
    }
    else {
        if (lenfile == 31 * 86400)
            times <- monthly(from=utime(start,time.zone="GMT"),to=end-1)
        else times <- seq(from=utime(floor(start/lenfile)*lenfile),to=end-1,by=lenfile)
        files <- unique(format(times,format=as(file,"character"),time.zone="GMT"))
    }
    if (length(files) == 0) stop(paste("no files found in",dir,"matching",file))

    .Call("open_netcdf",obj,files,cdlfile,300L,300L,PACKAGE="eolts")
}

setClass("netcdfVariable",
    slots=c(name="character", mode="character",
        nctype="character", dimension="integer", attributes="list"),
    prototype=list(
        name=character(),mode=character(),nctype=character(),dimension=integer(),
  	attributes=list())
)

if (!isGeneric("format")) setGeneric("format")
setMethod("format","netcdf",
    function(x,...) {
        paste(class(x),
            ", dir=",x@dir,
            ", file=",paste(x@file,collapse=","),
            ", cppPtr=",paste(x@cppPtr,collapse=""))
    }
)

setMethod("show","netcdf",
    function(object) cat(format(object),"\n")
)

setMethod("format","netcdfVariable",
    function(x) {
        paste(class(x),
            ", name=",x@name,
            ", mode=",x@mode,
            ", nctype=",x@nctype,
            ", dimensions=(",
              paste(names(x@dimension),"(",
                    as.vector(x@dimension),")",collapse=", ",sep=""),")",
            ", attributes=(",
              paste(names(x@attributes),"(",
                sapply(x@attributes,function(x)paste(x,collapse=",")),")",collapse=",",sep=""),")",
            "\n",sep="")
    }
)

setMethod("show","netcdfVariable",
    function(object) cat(format(object),"\n")
)

if (!isGeneric("is.open")) {
    setGeneric("is.open",function(con) standardGeneric("is.open"))
}
setMethod("is.open",
    signature(con="netcdf"),
    function(con)
    {
        .Call("is_netcdf_open",con,PACKAGE="eolts")
    }
)

if (!isGeneric("close")) setGeneric("close")

setMethod("close",
    signature(con="netcdf"),
    function(con)
    {
        .Call("close_netcdf",con,PACKAGE="eolts")
        NULL
    }
)

setMethod("stations",
    signature(x="netcdf"),
    function(x)
    {
        .Call("get_stations",x,PACKAGE="eolts")
    }
)

setGeneric("readnc",function(con,variables,start,count,...)
    standardGeneric("readnc"))

setMethod("readnc",
    signature(con="netcdf",variables="character",start="integer",count="integer"),
    function(con,variables,start,count,...)
    {
        x <- .Call("read_netcdf",con,variables,start,count,PACKAGE="eolts")
        if (length(x) == 1) x <- x[[1]]
        x
    }
)

setMethod("readnc",
    signature(con="netcdf",variables="character",start="missing",count="missing"),
    function(con,variables,start,count,...)
    {
        readnc(con,variables,integer(0),integer(0))
    }
)

setMethod("readnc",
    signature(con="netcdf",variables="list",start="integer",count="integer"),
    function(con,variables,start,count,...)
    {
        if (length(variables) > 0 && class(variables[[1]]) == "netcdfVariable") {
            variables <- sapply(variables,function(x)x@name)
            readnc(con,variables,start,count,...)
        }
        else stop("invalid \"variables\" argument")
    }
)

setMethod("readnc",
    signature(con="netcdf",variables="list",start="missing",count="missing"),
    function(con,variables,start,count,...)
    {
        readnc(con,variables,start=integer(0),count=integer(0),...)
    }
)

setMethod("readnc",
    signature(con="netcdf",variables="missing",start="missing",count="missing"),
    function(con,variables,start,count,...)
    {
        x <- .Call("read_global_attrs",con,PACKAGE="eolts")
        x
    }
)

setGeneric("readts",function(con,variables,start,end,...)
    standardGeneric("readts"))

setMethod("readts",
    signature(con="netcdf",variables="character",start="utime",end="utime"),
    function(con,variables,start,end,...)
    {
        dots <- list(...)

        if (!hasArg(timevar) || is.null((timevar <- dots$timevar)))
          timevar <- c("time","Time","time_offset")

        if (!hasArg(basetime) || is.null((basetime <- dots$basetime)))
          basetime <- "base_time"

        if (!hasArg(stns) || is.null((stns <- dots$stns)))
          stns <- dpar("stns")

        if (!hasArg(time.format) || is.null((time.format <- dots$time.format)))
          time.format <- getOption("time.out.format")

        if (!hasArg(time.zone) || is.null((time.zone <- dots$time.zone)))
          time.zone <- getOption("time.zone")

        ncverbose <- dpar("ncverbose")

        x <- .External("read_netcdf_ts",con,variables,start,end,
            stns,timevar,basetime,as.character(time.zone),
            as.integer(ncverbose),PACKAGE="eolts")

        # utime slot from read_netcdf_ts is a single utime, with
        # a vector of numeric values. Change it to a vector
        # of utimes, each with one numeric value.
        positions(x) <- utime(positions(x))
        deltat(x) <- deltat(x)
        start(x) <- start
        end(x) <- end
        x@time.format <- time.format
        x@time.zone <- time.zone
        x
    }
)

setMethod("readts",
    signature(con="netcdf",variables="character",start="missing",end="missing"),
    function(con,variables,start,end,...)
    {
      readts(con,variables,start=dpar("start"),end=dpar("end"),...)
    }
)

setMethod("readts",
    signature(con="netcdf",variables="list",start="utime",end="utime"),
    function(con,variables,start,end,...)
    {
        if (length(variables) > 0 && class(variables[[1]]) == "netcdfVariable") {
            variables <- sapply(variables,function(x)x@name)
            readts(con,variables,start,end,...)
        }
        else stop("invalid \"variables\" argument")
    }
)

setMethod("readts",
    signature(con="netcdf",variables="list",start="missing",end="missing"),
    function(con,variables,start,end,...)
    {
        readts(con,variables,start=dpar("start"),end=dpar("end"),...)
    }
)

if (!isGeneric("variables",where=1))
    setGeneric("variables",function(con,...) standardGeneric("variables"))

setMethod("variables",
    signature(con="netcdf"),
    function(con,...)
    {
        args <- list(...)
        all <- FALSE
        if (hasArg(all)) all <- args$all

        names.only <- TRUE
        if (hasArg(names.only)) names.only <- args$names.only

        x <- .Call("get_variables",con,all,PACKAGE="eolts")
        # browser()
        if (names.only) {
            x <- sapply(x,function(x)
            {
               sn <- x@attributes$"short_name"
               if (!is.null(sn)) sn
               else x@name
            })
        }
        if (length(x) == 0) x <- NULL
        x
    }
)

setMethod("variables",
    signature(con="missing"),
    function(...)
    {
        args <- list(...)
        all <- FALSE
        if (hasArg(all)) all <- args$all

        if (all) vname <- "variables.all"
        else vname <- "variables"

        if (!check.cache(vname)) {
            iod <- netcdf()
            on.exit(close(iod))
            vars <- variables(iod,...)

            cache.object(vname,vars)
        }
        get.cache.object(vname)
    }
)
