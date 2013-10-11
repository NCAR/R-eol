#
#               Copyright (C) by UCAR
# 

# POSIX time class, with basic representation of numeric seconds
# since Jan 1, 1970 00:00 UTC
setClass("utime",
    contains=c("positionsCalendar", "numeric"),
    prototype=prototype(0)
)

setMethod("initialize",
    "utime",
    function(.Object,val=99)
    {
        .Object@.Data = val
        .Object
    }
)

# constructor for utime, from character, POSIXct, timeDate or numeric
utime = function(val=0,in.format=options("time.in.format")[[1]],time.zone=options("time.zone")[[1]])
{
    # cat(paste("in utime(), class(val)=",class(val),", length(val)=",length(val),"\n"))

    res = NULL

    if (is(val,"numeric")) {
        if (is.null(time.zone)) time.zone = ""

        res = new("utime",val)
    }
    else if (is(val,"utime")) {
        res = val
    }
    else if (is(val,"POSIXct")) {
        res = as(val,"utime")
    }
    else if (is(val,"timeDate")) {  # splusTimeDate::timeDate
        res = as(val,"utime")
    }
    else if (is.character(val)) {
        if (length(val) == 1 && val == "now") res = as(Sys.time(),"utime")
        else res = as(as.POSIXct(val,format=in.format,tz=time.zone),"utime")
        if (any(is.na(res@.Data))) {
            res = as(timeDate(val,
                in.format=splusTimeDate::timeDateOptions("time.in.format")[[1]],
                zone=splusTimeDate::timeDateOptions("time.zone")[[1]]),"utime")
        }
        if (any(is.na(res@.Data))) {
            if (length(val) == 1) warning(paste(val,"not parsable with in.format=",
                    in.format))
            else warning(paste("some dates not parsable with in.format=",in.format))
        }
    }
    else if (is.null(val)) {
        cat("null val\n")
        res = new("utime",0)
    }
    res
}

if (FALSE) {
    # this is createed automatically
    setAs("utime","numeric",function(from) from@.Data)
}

# coerce from numeric to utime
setAs("numeric","utime",
    function(from)
    {
        format = options("time.out.format")[[1]]
        if (is.null(format)) format = ""

        res = new("utime",val=from,format=format,time.zone=Sys.timezone());
        # res@.Data = from

        res
    }
)

# coerce from utime to timeDate
setAs("utime","timeDate",
    function(from)
    {
        # the timeDate constructor chokes if ms is NA, so set it to 0.
        if (any(bad <- is.na(from@.Data)))
            splusTimeDate::timeDate(julian=floor(from@.Data/86400) + 3653,
                ms=ifelse(bad,0,round((from@.Data %% 86400) * 1000,digits=3)),
                format=options("time.out.format")[[1]],
                zone=options("time.zone")[[1]])

        else {
            # cat("utime=",from@.Data[2],"\n")
            # cat("julian=",floor(from@.Data[2]/86400) + 3653,"\n")
            # cat("ms=",round((from@.Data[2] %% 86400.0) * 1000,digits=3),"\n")
            splusTimeDate::timeDate(julian=floor(from@.Data/86400) + 3653,
                ms=round((from@.Data %% 86400.0) * 1000,digits=3),
                format=options("time.out.format")[[1]],
                zone=options("time.zone")[[1]])
        }
    }
)

# coerce from utime to POSIXct
setAs("utime","POSIXct",
    function(from)
    {
        as.POSIXct(from@.Data,origin=structure(0,class="POSIXct"))
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

# coerce from utime to POSIXct to character
setAs("utime","character",
    function(from) as(as(from,"POSIXct"),"character"))

# coerce from character to POSIXct to utime
setAs("character","utime",
    function(from) as(as(from,"POSIXct"),"utime"))

setAs("utime","list",
    function(from)
    {
        to = as.POSIXlt(as(from,"POSIXct"),"POSIXlt")
        list(year=to$year + 1900,
            mon=to$mon + 1,
            day=to$mday,
            hour=to$hour,
            min=to$min,
            sec=to$sec,
            yday=to$yday + 1,
            TZ=options("time.zone")[[1]]
            )
    }
)

# coerce from list to utime
setAs("list","utime",
    function(from)
    {
        # Splus doesn't have a %D or %j for day of year input parsing.
        # It does have a %D to output the day of year...
        if (!is.null(from$yday) && is.null(from$mon) && is.null(from$day)) {
            utime(paste(from$year,from$yday-1,from$hour,from$min,from$sec),
                in.format="%Y%j[%H[%M[%S[.%N]]]]",time.zone=as.character(from$TZ))
        }
        else {
            utime(paste(from$year,from$mon,from$day,
                from$hour,from$min,from$sec),
                in.format="%Y%m%d[%H[%M[%S[.%N]]]]",time.zone=as.character(from$TZ))
        }
    }
)

setMethod("format","utime",
    function(x,...)
    {
        if (hasArg(format)) format = list(...)$format
        else format = options("time.out.format")[[1]]

        if (hasArg(time.zone)) time.zone = list(...)$time.zone
        else if (hasArg(TZ)) time.zone = list(...)$TZ
        else time.zone = options("time.zone")[[1]]

        if (is.list(time.zone)) time.zone = unlist(time.zone)

        if (FALSE) {
            x = as(x,"timeDate")

            x@format = format
            x@time.zone = time.zone
            format(x)
        }
        else {
            x = as(x,"POSIXct")
            format(x,format=format,tz=time.zone)
        }
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

setMethod("show",signature(object="utime"),
    function(object)
    {
        # cat("in show utime\n")
        print.default(format(object,format=options("time.out.format")[[1]],
               time.zone=options("time.zone")[[1]]),quote=FALSE)
    }
)

setMethod("diff","utime",function(x,...) diff(x@.Data))

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
            y = arglist[[1]]
            if (class(y) != class(x)) y = as(y,class(x))
            x@.Data = c(x@.Data,y@.Data)
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
        callGeneric(e1@.Data,e2@.Data)
    }
)

setMethod("Ops",signature(e1="utime",e2="numeric"),
    function(e1,e2)
    {
        callGeneric(e1@.Data,e2)
    }
)

setMethod("Ops",signature(e1="numeric",e2="utime"),
    function(e1,e2)
    {
        callGeneric(e1,e2@.Data)
    }
)

setMethod("Math",signature(x="utime"),
    function(x)
    {
        x@.Data <- callGeneric(x@.Data)
        x
    }
)

setMethod("Summary",signature(x="utime"),
    function(x,...,na.rm=FALSE)
    {
        x@.Data <- callGeneric(x@.Data)
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


utime.now <- function() {
    utime(.C("utime_now",sec=integer(1))$sec)
}

