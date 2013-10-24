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
    function(.Object,val=as.numeric(Sys.time()))
    {
        .Object@.Data = val
        .Object
    }
)

# constructor for utime, from character, POSIXct, timeDate or numeric
utime = function(val=as.numeric(Sys.time()),
    in.format=options("time.in.format")[[1]],
    time.zone=options("time.zone")[[1]])
{
    # cat(paste("in utime(), class(val)=",class(val),", length(val)=",length(val),"\n"))

    res = NULL
    if (is(val,"numeric")) {
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
        else for (i in 1:length(in.format)) {
            # cat("trying format",in.format[[i]],"\n")
            res = as.POSIXct(val,format=in.format[[i]],tz=time.zone)
            if (!any(is.na(res))) {
                res = as(res,"utime")
                break
            }
        }
        if (any(is.na(res))) {
            if (length(val) == 1) warning(paste(val,"not parsable with any in.format=",
                    unlist(in.format)))
            else warning(paste("some dates not parsable with in.format=",in.format))
        }
    }
    else if (is.null(val)) {
        res = new("utime")
    }
    res
}

if (FALSE) {
    # this is created automatically
    setAs("utime","numeric",function(from) from@.Data)
}

# coerce from numeric to utime
setAs("numeric","utime",
    function(from)
    {
        res = new("utime",val=from)
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
        time.zone = options("time.zone")[[1]]
        if (time.zone == "") time.zone = "UTC"

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

# coerce from utime to POSIXct to character
setAs("utime","character",
    function(from) 
    {
        time.zone = options("time.zone")[[1]]
        from = as(from,"POSIXct")
        attr(from,"tzone") = time.zone
        as(from,"character")
    }
)

# coerce from character to POSIXct to utime
setAs("character","utime",
    function(from)
    {
        time.zone = options("time.zone")[[1]]
        as(as(from,"POSIXct",tz=time.zone),"utime")
    }
)

setAs("utime","list",
    function(from)
    {
        time.zone = options("time.zone")[[1]]
        to = as.POSIXlt(as(from,"POSIXct",tz=time.zone),tz=time.zone)
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
            attr(x,"tzone") = time.zone
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

setMethod("summary",signature(object="utime"),
    function(object,format=options("time.out.format")[[1]],time.zone=options("time.zone")[[1]],...)
    {
        nas <- is.na( object )
        tmp <- as( object[!nas], "numeric")
        ret <- as( quantile( tmp, c( 0, .25, .5, .75, 1 )), "utime" )
        ret <- c( ret, as( tmp, "utime" ))
        ret = format( ret, format=format,time.zone=time.zone)
        ret <- ret[c(1,2,3,6,4,5)]
        names( ret ) <- c( "Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max" )
        if( any( nas ))
          ret[ "NAs" ] <- sum( nas )

        ret <- matrix( ret, nrow = 1, dimnames = list( "", names( ret )))
        oldClass( ret ) <- "table"
        ret

    }
)




