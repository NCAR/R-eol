# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolts" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

# POSIX time class, with basic representation of numeric seconds
# since Jan 1, 1970 00:00 UTC
setClass("utime",
    contains=c("positionsCalendar", "numeric")
)

setMethod("initialize",
    "utime",
    function(.Object,val=as.numeric(Sys.time()))
    {
        .Object@.Data <- val
        .Object
    }
)

# constructor for utime, from character, POSIXct, timeDate or numeric
utime <- function(val=as.numeric(Sys.time()),
    in.format=getOption("time.in.format"),
    time.zone=getOption("time.zone"))
{
    # cat(paste("in utime(), class(val)=",class(val),", length(val)=",length(val),"\n"))

    res <- NULL
    if (is.null(val) || length(val) == 0) return(res)

    if (is(val,"numeric")) {
        res <- new("utime",val)
    }
    else if (is.character(val)) {
        # like setAs("character","utime") but in.format is an argument
        if (is.null(time.zone)) time.zone <- getOption("time.zone")
        if (is.null(time.zone)) time.zone <- "UTC"

        if (length(val) == 1 && val == "now") {
            return(as(Sys.time(),"utime"))
        }
        for (i in 1:length(in.format)) {
            # cat("trying format",in.format[[i]],"\n")
            res <- as.numeric(strptime(val,format=in.format[[i]],tz=time.zone))
            if (any(!is.na(res))) {
                break
            }
        }
        if (any(is.na(res))) {
            badt <- is.na(res)
            if (length(val) == 1) warning(paste0("\"",val,"\" not parsable with any in.format=c(",
                    paste("\"",unlist(in.format),"\"",sep="",collapse=", "),"). Setting to NA"))
            else warning(paste0(sum(badt)," dates (such as \"",val[badt][1],"\") not parsable with any in.format=c(",
                    paste("\"",unlist(in.format),"\"",sep="",collapse=", "),"). Setting to NA"))
        }
        res <- new("utime", res)
    }
    else if (is(val,"POSIXct")) {
        res <- utime(as.numeric(val))
    }
    else if (is(val,"timeDate")) {  # splusTimeDate::timeDate
        res <- as(val,"utime")
    }
    else if (is(val,"utime")) {
        res <- val
    }
    else if (is.list(val)) {
        time.zone <- as.character(val$TZ)
        if (is.null(time.zone)) time.zone <- getOption("time.zone")
        if (length(time.zone) == 0) time.zone <- "UTC"

        # Unfortunately have to convert to character string and then parse.
        # Doesn't seem to be a way to construct a POSIXlt from a list
        # of time, date fields.
        if (!is.null(val$yday) && is.null(val$mon) && is.null(val$day)) {
            res <- utime(paste(val$year,val$yday-1,val$hour,val$min,val$sec),
                in.format="%Y%j%H%M%OS",time.zone = time.zone)
        }
        else {
            res <- utime(paste(val$year,val$mon,val$day,
                val$hour,val$min,val$sec),
                in.format="%Y%m%d%H%M%OS", time.zone=time.zone)
        }
    }
    else {
        res <- new("utime",as.numeric(val))
    }
    res
}

# coerce from numeric to utime
setAs("numeric","utime",
    function(from)
    {
        res <- new("utime",val=from)
        res
    }
)

# coerce from utime to POSIXct
setAs("utime","POSIXct",
    function(from)
    {
        time.zone <- getOption("time.zone")
        if (is.null(time.zone)) time.zone <- "UTC"

        as.POSIXct(from@.Data,tz=time.zone,origin=structure(0,class="POSIXct"))
    }
)

# coerce from POSIXct to utime
setAs("POSIXct","utime",
    function(from)
    {
        # cat("in setAs POSIXct utime\n")
        utime(as.numeric(from))
    }
)

# coerce from splusTimeDate::timeDate to utime
setAs("timeDate","utime",
    function(from) {
        utime((from@columns[[1]] - 3653) * 86400 + from@columns[[2]] / 1000)
    }
)

# coerce from utime to timeDate
setAs("utime","timeDate",
    function(from)
    {
        if (any(bad <- is.na(from@.Data)))
            splusTimeDate::timeDate(julian=as.integer(floor(from@.Data/86400) + 3653),
                ms=as.integer(ifelse(bad,0,round((from@.Data %% 86400) * 1000,digits=3))))
        else {
            # cat("utime=",from@.Data[2],"\n")
            # cat("julian=",floor(from@.Data[2]/86400) + 3653,"\n")
            # cat("ms=",round((from@.Data[2] %% 86400.0) * 1000,digits=3),"\n")
            splusTimeDate::timeDate(julian=as.integer(floor(from@.Data/86400) + 3653),
                ms=as.integer(round((from@.Data %% 86400.0) * 1000,digits=3)))
        }
    }
)

# coerce from utime to POSIXct to character
setAs("utime","character",
    function(from) 
    {
        format <- getOption("time.out.format")
        time.zone <- getOption("time.zone")
        if (is.null(time.zone)) time.zone <- "UTC"

        from <- as(from,"POSIXct")
        attr(from,"tzone") <- time.zone
        format(from,format=format,tz=time.zone)
    }
)

# coerce from character to POSIXct to utime
setAs("character","utime",
    function(from)
    {
        in.format <- getOption("time.in.format")
        time.zone <- getOption("time.zone")
        if (is.null(time.zone)) time.zone <- "UTC"

        if (length(from) == 1 && from == "now") {
            return(as(Sys.time(),"utime"))
        }
        for (i in 1:length(in.format)) {
            # cat("trying format",in.format[[i]],"\n")
            res <- strptime(from,format=in.format[[i]],tz=time.zone)
            if (!any(is.na(res))) {
                res <- new("utime",as.numeric(res))
                break
            }
        }
        if (any(is.na(res))) {
            if (length(from) == 1) warning(paste0("\"",from,"\" not parsable with any in.format=c(",
                    paste("\"",unlist(in.format),"\"",sep="",collapse=", "),")"))
            else warning(paste("some dates not parsable with any in.format=c(",
                    paste("\"",unlist(in.format),"\"",sep="",collapse=", "),")"))
        }
        res
    }
)

# Convert from utime to a list.  Note that a utime object
# does not contain a time zone, and the function specified
# in the setAs is only passed a from argument, and so the
# time zone of the conversion is taken from getOption("time.zone")
setAs("utime","list",
    function(from)
    {
        time.zone <- getOption("time.zone")
        if (is.null(time.zone)) time.zone <- "UTC"

        # 
        to <- as.POSIXlt(as(from,"POSIXct"),tz=time.zone)
        list(year=to$year + 1900,
            mon=to$mon + 1,
            day=to$mday,
            hour=to$hour,
            min=to$min,
            sec=to$sec,
            yday=to$yday + 1,
            TZ=time.zone
        )
    }
)

# Convert from a list to utime.
setAs("list","utime",
    function(from)
    {
        # cat("setAs list method for utime","\n")
        if (is.null(from$TZ))
            utime(from)
        else
            utime(from, time.zone=from$TZ)
    }
)
as.list.utime <- function(x, ...)
{

    if (hasArg(time.zone)) time.zone <- list(...)$time.zone
    else time.zone <- getOption("time.zone")

    # cat("as.list.utime, time.zone=", time.zone,"\n")

    to <- as.POSIXlt(as(x,"POSIXct"),tz=time.zone)
    list(year=to$year + 1900,
        mon=to$mon + 1,
        day=to$mday,
        hour=to$hour,
        min=to$min,
        sec=to$sec,
        yday=to$yday + 1,
        TZ=time.zone
    )
}

setGeneric("as.list",function(x) standardGeneric("as.list"))
setMethod("as.list",signature(x="utime"),
    function(x, ...)
    {
        if (hasArg(time.zone)) time.zone <- list(...)$time.zone
        else time.zone <- getOption("time.zone")
        # cat("as.list method for utime, time.zone=", time.zone,"\n")

        to <- as.POSIXlt(as(x,"POSIXct"),tz=time.zone)
        list(year=to$year + 1900,
            mon=to$mon + 1,
            day=to$mday,
            hour=to$hour,
            min=to$min,
            sec=to$sec,
            yday=to$yday + 1,
            TZ=time.zone
        )
    }
)

setMethod("format","utime",
    function(x,...)
    {
        if (hasArg(format)) format <- list(...)$format
        else format <- getOption("time.out.format")

        if (hasArg(time.zone)) time.zone <- list(...)$time.zone
        else time.zone <- getOption("time.zone")
        if (is.null(time.zone)) time.zone <- "UTC"

        x <- as(x,"POSIXct")
        attr(x,"tzone") <- time.zone
        format(x,format=format,tz=time.zone)
    }
)

format_utime <- function(x, format=getOption("time.out.format"),
    time.zone=getOption("time.zone"))
{
    if (is.numeric(x)) x <- utime(x)
    if (is.null(time.zone)) time.zone <- "UTC"
    x <- as(x,"POSIXct")
    attr(x,"tzone") <- time.zone
    format(x,format=format,tz=time.zone)
}

setMethod("show",signature(object="utime"),
    function(object)
    {
        # cat("in show utime\n")
        time.zone <- getOption("time.zone")
        if (is.null(time.zone)) time.zone <- "UTC"

        print.default(format(object,format=getOption("time.out.format"),
               time.zone=time.zone),quote=FALSE)
    }
)

setGeneric("monthly",function(from,to) standardGeneric("monthly"))

setMethod("monthly",signature(from="utime",to="utime"),
    function(from,to)
    {
        res <- NULL
        while (1) {
          ul <- as.list(from)
          ul$day <- 1     # first day of month
          ul$hour <- ul$min <- ul$sec <- 0
          from <- utime(ul)
          if(from > to) break
          res <- c(res, from)
          from <- utime(from + 32 * 86400)
        }
        utime(res)
    }
)

diff.utime <- function(x,...) diff(x@.Data)
# setMethod("diff","utime",function(x,...) diff(x@.Data))

# setGeneric("diff",function(x) standardGeneric("diff"))
# setMethod("diff","utime",function(x,...)
#     {
#         diff(x@.Data)
#     }
# )

setMethod( "c", signature(x="utime"),
    function(x, ...)
    {
        arglist <- list(...)
        if(length(arglist)==0) return(x)
        lens <- sapply(arglist, length)
        if(!any(lens > 0)) {
            return(x)
        }
        arglist <- arglist[lens > 0]
        if(length(arglist) > 1)
            c(c(x, arglist[[1]]), do.call("c", arglist[-1]))
        else {
            y <- arglist[[1]]
            if (class(y) != class(x)) y <- as(y,class(x))
            x@.Data <- c(x@.Data,y@.Data)
            x
        }
    }
)

setMethod("[",signature(x="utime"),
    function(x,i) 
    {
        # cat(paste("in utime [, nargs=",nargs(),"\n"))
        utime(x@.Data[i])
    }
)

setReplaceMethod("[",signature(x="utime",i="ANY",j="missing",value="utime"),
    function(x,i,value)
    {
        # i <- list(...)[[1]]
        x@.Data[i] <- value@.Data
        x
    }
)

setReplaceMethod("[",signature(x="utime",i="ANY",j="missing",value="numeric"),
    function(x,i,value)
    {
        # i <- list(...)[[1]]
        x@.Data[i] <- value
        x
    }
)
setReplaceMethod("[",signature(x="utime",i="ANY",j="missing",value="ANY"),
    function(x,i,value)
    {
        # i <- list(...)[[1]]
        x@.Data[i] <- utime(value)@.Data
        x
    }
)

setMethod("Ops",signature(e1="utime",e2="utime"),
    function(e1,e2)
    {
        # Ops are the usual binary operators: 
        # +,-,*,^,%%,%/%,/,==,>,<,!=,<=,>=,&,|
        # Do ?Ops
        # cat("Ops(utime,utime) .Generic=",.Generic,"\n")
        # the most usual Ops for 2 utimes is subtraction, which 
        # should return a numeric, not a utime.
        callGeneric(e1@.Data,e2@.Data)
    }
)

setMethod("Ops",signature(e1="utime",e2="numeric"),
    function(e1,e2)
    {
        # the most usual Ops for a utime and a numeric is
        # addition or subtracting a number of seconds,
        # where we want to return a utime.
        # Otherwise one may divide, then truncate and
        # multiply to convert to an even boundary, such as hour or day.
        # It could remain as a utime for all those operations,
        # but it seems best to make the result of a divide or multiply
        # a numeric, which must be converted back into a utime.
        # cat("Ops(utime,numeric): .Generic=",.Generic,"\n")
        if (.Generic == "+" || .Generic == "-") {
            e1@.Data <- callGeneric(e1@.Data,e2)
            e1
        }
        else callGeneric(e1@.Data,e2)
    }
)

setMethod("Ops",signature(e1="numeric",e2="utime"),
    function(e1,e2)
    {
        # the most usual Ops for a numeric and a utime is
        # addition, where we want to return a utime. Otherwise
        # return a numeric.
        # cat("Ops(numeric,utime): .Generic=",.Generic,"\n")
        if (.Generic == "+") {
            e2@.Data <- callGeneric(e1,e2@.Data)
            e2
        }
        else callGeneric(e1,e2@.Data)
    }
)

setMethod("Math",signature(x="utime"),
    function(x)
    {
        # from ?Math: abs, sign,sqrt, ceiling,floor
        # The most common use is probably ceiling or floor,
        # where we'll return a utime
        # cat("Math(utime): .Generic=",.Generic,"\n")

        x@.Data <- callGeneric(x@.Data)
        x
    }
)

setMethod("Summary",signature(x="utime"),
    function(x,...,na.rm=FALSE)
    {
        x@.Data <- callGeneric(x@.Data,...,na.rm=na.rm)
        x
    }
)

setMethod("prod",signature(x="utime"),
    function(x,...,na.rm=FALSE)
    {
        callGeneric(x@.Data)
    }
)

setMethod("sum",signature(x="utime"),
    function(x,...,na.rm=FALSE)
    {
        callGeneric(x@.Data)
    }
)

setMethod("any",signature(x="utime"),
    function(x,...,na.rm=FALSE)
    {
        callGeneric(x@.Data)
    }
)

setMethod("all",signature(x="utime"),
    function(x,...,na.rm=FALSE)
    {
        callGeneric(x@.Data)
    }
)

setMethod("summary",signature(object="utime"),
    function(object,format=getOption("time.out.format"),
        time.zone=getOption("time.zone"),...)
    {
        if (is.null(time.zone)) time.zone <- "UTC"

        nas <- is.na( object )
        tmp <- as( object[!nas], "numeric")
        ret <- as( quantile( tmp, c( 0, .25, .5, .75, 1 )), "utime" )
        ret <- c( ret, as( tmp, "utime" ))
        ret <- format( ret, format=format,time.zone=time.zone)
        ret <- ret[c(1,2,3,6,4,5)]
        names( ret ) <- c( "Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max" )
        if( any( nas ))
          ret[ "NAs" ] <- sum( nas )

        ret <- matrix( ret, nrow = 1, dimnames = list( "", names( ret )))
        oldClass( ret ) <- "table"
        ret

    }
)

if (FALSE) {
    # not needed, default seq with from=utime returns utimes
seq.utime <- function(...)
{
    # cat("seq.utime\n")
    # from,to,by=((to-from)/(length-1)),length=NULL)
    from <- lenarg <- to <- by <- along.with <- NULL
    args <- list(...)

    if (hasArg(from)) from <- args$from

    if (hasArg(to)) to <- args$to

    if (hasArg(length)) lenarg <- args$length
    else if (hasArg(along.with)) lenarg <- length(args$along.with)

    if (hasArg(by)) by <- args$by

    if (!is.null(from)) {
        if (is.null(to))
            utime(seq(from=as.numeric(from),by=by,length=lenarg))
        else if (is.null(lenarg))
            utime(seq(from=as.numeric(from),to=as.numeric(to),by=by))
        else
            utime(seq(from=as.numeric(from),to=as.numeric(to),length=lenarg))
    }
    else if (!is.null(to)) {
        utime(seq(to=as.numeric(to),by=by,length=lenarg))
    }
    else 1:lenarg
}
}
