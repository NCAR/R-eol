
.onLoad = function(libname,pkgname)
{
    options(time.in.format="%Y %m %d %H:%M:%S",
            time.out.format="%Y %02m %02d %02H:%02M:%02S %Z",
            time.zone=Sys.timezone())
    cat(paste(pkgname,"::.onLoad\n",
        "  options(\"time.in.format\")=\"",options("time.in.format")[[1]],"\"\n",
        "  options(\"time.out.format\")=\"",options("time.out.format")[[1]],"\"\n",
        "  options(\"time.zone\")=\"",options("time.zone")[[1]],"\"\n",sep=""))
}
setClass("utime",
    contains="numeric",
    slots=c(format="character",time.zone="character")
)

setMethod("initialize",
    "utime",
    function(.Object,format=options("time.out.format")[[1]],time.zone=options("time.zone")[[1]])
    {
        .Object@.Data = 0
        .Object@format = format
        .Object@time.zone = time.zone
        .Object
    }
)

utime = function(val=0,in.format=options("time.in.format")[[1]],format=options("time.out.format")[[1]],time.zone=options("time.zone")[[1]])
{
    if (is(val,"POSIXct")) {
        res = as(val,"utime")
        if (is.null(time.zone)) time.zone = attr(val,"tzone")
    }
    else if (is(val,"timeDate")) {  # splusTimeDate::timeDate
        res = as(val,"utime")
        if (is.null(time.zone)) time.zone = val@time.zone
    }
    else {
        if (is.null(time.zone)) time.zone = ""
        if (is.character(val)) {
            if (length(val) == 1 && val == "now") res = as(Sys.time(),"utime")
            else res = as(as.POSIXct(val,format=in.format,tz=time.zone),"utime")
            if (any(is.na(res@.Data))) {
              if (length(val) == 1) warning(paste(val,"not parsable with in.format=",
                      in.format))
              else warning(paste("some dates not parsable with in.format=",in.format))
            }
        }
        else res = as(val,"utime")
    }
    res@format = format
    res@time.zone = time.zone
    res
}

#
setAs("utime","numeric",function(from) from@.Data)

# coerce from numeric to utime
setAs("numeric","utime",
    function(from)
    {
        ret = new("utime");
        ret@.Data = from
        ret@format = options("time.out.format")[[1]]
        ret@time.zone = Sys.timezone()
        ret
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
                format=from@format,zone=from@time.zone)
        else {
            # cat("utime=",from@.Data[2],"\n")
            # cat("julian=",floor(from@.Data[2]/86400) + 3653,"\n")
            # cat("ms=",round((from@.Data[2] %% 86400.0) * 1000,digits=3),"\n")
            splusTimeDate::timeDate(julian=floor(from@.Data/86400) + 3653,
                  ms=round((from@.Data %% 86400.0) * 1000,digits=3),
                  format=from@format,zone=from@time.zone)
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
        to = utime(as.numeric(from))
        to@time.zone = attr(from,"tzone")
        to
    }
)

if (FALSE) {
as.utime.POSIXct = function(from)
{
    to = utime(as.numeric(from))
    to@time.zone = attr(from,"tzone")
}
}

# coerce from timeDate to utime
setAs("timeDate","utime",
    function(from)
      utime((from@columns[[1]] - 3653) * 86400 + from@columns[[2]] / 1000,format=from@format,zone=from@time.zone))

# coerce from utime to POSIXct to character
setAs("utime","character",
    function(from) as(as(from,"POSIXct"),"character"))

# coerce from character to POSIXct to utime
setAs("character","utime",
    function(from) as(as(from,"POSIXct"),"utime"))

setAs("utime","list",
    function(from)
    {
        to = as(as(from,"POSIXct"),"POSIXlt")
        list(year=to$year,
            mon=to$mon,
            day=to$mday,
            hour=to$hour,
            min=to$min,
            sec=to$sec,
            yday=to$yday + 1,
            TZ=from@time.zone
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
                in.format="%Y%j[%H[%M[%S[.%N]]]]",zone=as.character(from$TZ))
        }
        else {
            utime(paste(from$year,from$mon,from$day,
                from$hour,from$min,from$sec),
                in.format="%Y%m%d[%H[%M[%S[.%N]]]]",zone=as.character(from$TZ))
        }
    }
)

setMethod("format","utime",
    function(x,...)
    {
        if (hasArg(format)) format = list(...)$format
        else format = x@format
        if (hasArg(time.zone)) time.zone = list(...)$time.zone
        else if (hasArg(TZ)) time.zone = list(...)$TZ
        else time.zone = x@time.zone
        if (is.list(time.zone)) time.zone = unlist(time.zone)
        x = as(x,"timeDate")
        x@format = format
        x@time.zone = time.zone
        format(x)
    }
)

if (isGeneric("monthly",where=1)) removeGeneric("monthly",where=1)

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

setMethod("show","utime",function(object) show(as(object,"timeDate")))

setMethod("diff","utime",function(x,...) diff(x@.Data))

if (FALSE) {
setMethod("[","utime",
    function(x,...,drop=F) utime(x@.Data[...])
)

setMethod("[<-",signature(x="utime",i="ANY",j="missing",value="utime"),
    function(x,i,value)
    {
        # i <- list(...)[[1]]
        x@.Data[i] <- value@.Data
        x
    }
)

setMethod("[<-",signature(x="utime",i="ANY",j="missing",value="numeric"),
    function(x,i,value)
    {
        # i <- list(...)[[1]]
        x@.Data[i] <- value
        x
    }
)
setMethod("[<-",signature(x="utime",i="ANY",j="missing",value="ANY"),
    function(x,i,value)
    {
        # i <- list(...)[[1]]
        x@.Data[i] <- utime(value)@.Data
        x
    }
)

}

setMethod("Ops",signature(e1="utime",e2="numeric"),
    function(e1,e2)
    {
        e1@.Data = callGeneric(e1@.Data,e2)
        e1
    }
)

setMethod("Ops",signature(e1="numeric",e2="utime"),
    function(e1,e2)
    {
      e2@.Data = callGeneric(e1,e2@.Data)
      e2
    }
)
utime.now <- function() {
    utime(.C("utime_now",sec=integer(1))$sec)
}

# if (isGeneric("abline",where=1)) removeGeneric("abline",where=1)
# setGeneric("abline",function(a,...) standardGeneric("abline"))
setMethod("abline",signature(a="utime"),
    function(a,...)
    {
        sc <- plot.ats.scale
        v <- (as.numeric(a) - sc$off) / sc$scale
        abline.default(v=v,...)
        a
    }
)

tlocator <- function(n=1,type="n",...)
{
    if (n == 1) 
        cat("Use mouse to select a time on the plot ... ")
    else
        cat("Use mouse to select times on the plot.\nClick middle button, or both buttons on two button mouse to terminate ... ")

    times <- utime(locator(n,type=type,...)$x * plot.ats.scale$scale + plot.ats.scale$off)
    cat("done\n")
    times
}

