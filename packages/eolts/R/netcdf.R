#
#               Copyright (C) by UCAR
# 

setClass("netcdf",
    slots=c(
        file="character",
        dir="character",
        start="utime",
        end="utime",
        lenfile="integer",
        server="character",
        interval="numeric",
        cdlfile="character",
        cppPtr="raw"
        ),
    prototype=list(
        file=Sys.getenv("SURF_NETCDF_FILE"),
        dir=Sys.getenv("SURF_NETCDF_DIR"),
        start=utime(0),
        end=utime(0),
        lenfile=as.integer(86400),
        server="",
        interval=300,
        cdlfile="",
        cppPtr=raw(8))
)

netcdf = function(
        file=Sys.getenv("SURF_NETCDF_FILE"),
        dir=Sys.getenv("SURF_NETCDF_DIR"),
        start=dpar("start"),
        end=dpar("end"),
        lenfile=dpar("lenfile"),
        server="",
        interval=300,
        cdlfile="")
{
    if (is.null(lenfile)) lenfile = 86400
    obj = new("netcdf",
        file=file,dir=dir,
        start=start,end=end,
        lenfile=as.integer(lenfile),
        server=server,interval=interval,cdlfile=cdlfile)

    if (FALSE) {
    sfile = file
    substring(sfile,"%y") <- "%02y"
    substring(sfile,"%m") <- "%02m"
    substring(sfile,"%d") <- "%02d"
    substring(sfile,"%j") <- "%03D"
    substring(sfile,"%H") <- "%02H"
    substring(sfile,"%M") <- "%02M"
    substring(sfile,"%S") <- "%02S"

    obj@filenamefmt = file
    obj@lenfile = as(lenfile,"integer")

    obj@interval = as(interval,"numeric")

    obj@server = as.character(server)

    obj@start = as(start,"utime")
    obj@end = as(end,"utime")
    }

    if (lenfile == 31 * 86400) times = monthly(from=utime(start,zone="GMT"),to=end-1)
    else times = utime(seq(from=floor(start/lenfile)*lenfile,to=end-1,by=lenfile))

    obj@file = unique(format(times,format=as(file,"character"),TZ="GMT"))

    .Call("open_netcdf",obj,cdlfile,300,300,PACKAGE="eolts")
}

setClass("netcdfVariable",
    slots=c(name="character", mode="character",
        nctype="character", dimension="integer", attributes="list"),
    prototype=list(name=character(),mode=character(),nctype=character(),dimension=integer(),
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

# We could make these methods instead of functions.
# Calling them "read" would be using a common name, and might
# conflict with future releases of S, or other extensions.
# (As of Splus v6, read is not used.)
# Since all methods must have the same signature (unless we use ...)
# we probably shouldn't use the name "read" here.

setGeneric("readnc",function(con,variables,start,count,...)
    standardGeneric("readnc"))

setMethod("readnc",
    signature(con="netcdf",variables="character",start="integer",count="integer"),
    function(con,variables,start,count,...)
    {
        x = .Call("read_netcdf",con,variables,start,count,PACKAGE="eolts")
        if (length(x) == 1) x = x[[1]]
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
            variables = sapply(variables,function(x)x@name)
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

setGeneric("readts",function(con,variables,start,end,...)
    standardGeneric("readts"))

setMethod("readts",
    signature(con="netcdf",variables="character",start="utime",end="utime"),
    function(con,variables,start,end,...)
    {
        dots <- list(...)
        if (!hasArg(timevar) || is.null((timevar <- dots$timevar)))
          timevar <- c("time","time_offset")
        if (!hasArg(basetime) || is.null((basetime <- dots$basetime)))
          basetime <- "base_time"
        if (!hasArg(stns) || is.null((stns <- dots$stns)))
          stns <- dpar("stns")

        if (!hasArg(time.format) || is.null((time.format <- dots$time.format)))
          time.format = options("time.out.format")[[1]]

        if (!hasArg(time.zone) || is.null((time.zone <- dots$time.zone)))
          time.zone = options("time.zone")[[1]]

        x = .External("read_netcdf_ts",con,variables,start,end,
            stns,timevar,basetime,PACKAGE="eolts")

        # utime slot from read_netcdf_ts is a single utime, with
        # a vector of numeric values. Change it to a vector
        # of utimes, each with one numeric value.
        positions(x) = utime(positions(x))
        deltat(x) = deltat(x)
        start(x) = start
        end(x) = end
        x@time.format = time.format
        x@time.zone = time.zone
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
            variables = sapply(variables,function(x)x@name)
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
        args = list(...)
        all = FALSE
        if (hasArg(all)) all = args$all

        names.only = TRUE
        if (hasArg(names.only)) names.only = args$names.only

        x = .Call("get_variables",con,all,PACKAGE="eolts")
        # browser()
        if (names.only) {
            x = sapply(x,function(x)
            {
               sn = x@attributes$"short_name"
               if (!is.null(sn)) sn
               else x@name
            })
        }
        x
    }
)

setMethod("variables",
    signature(con="missing"),
    function(...)
    {
        args = list(...)
        all = FALSE
        if (hasArg(all)) all = args$all

        if (all) vname = "variables.all"
        else vname = "variables"

        if (!check.cache(vname)) {
            iod <- netcdf()
            on.exit(close(iod))
            vars <- variables(iod,...)

            cache.object(vname,vars)
        }
        get.cache.object(vname)
    }
)


if (FALSE) {
setMethod("variables",
    signature(con="netcdf"),
    function(con,...)
    {
        invisible(.Call("get_variables",con,FALSE,PACKAGE="eolts"))
    }
)

setMethod("variables",
    signature(con="missing",all="logical"),
    function(all)
    {
        if (all) vname = "variables.all"
        else vname = "variables"

        if (!check.cache(vname)) {
            iod <- netcdf()
            on.exit(close(iod))
            vars <- variables(iod,all)

            cache.object(vname,vars)
        }
        get.cache.object("variables")
    }
)

setMethod("variables",
    signature(con="missing",all="missing"),
    function()
    {
        variables(all=FALSE)
    }
)
}

