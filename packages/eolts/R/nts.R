# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolts" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

setClass("nts",
    contains="timeSeries",
    slots=c(
        weights="matrix",
        weightmap="integer",
        stations="integer",
        deltat="numeric",
        wss="numeric",
        time.format="character",
        time.zone="character"
        ),
    prototype=list(
        weights=matrix(0,ncol=0,nrow=0),
        weightmap=integer(0),
        stations=integer(0),
        deltat=numeric(0),
        wss=numeric(0),
        time.format=getOption("time.out.format"),
        time.zone=getOption("time.zone")
        )
    )

nts <- function(data,positions,
    units,
    names,
    weights=NULL,
    weightmap=NULL,
    stations=NULL,
    wss=NULL,
    time.format=getOption("time.out.format"),
    time.zone=getOption("time.zone"))
{
    ret <- new("nts")

    if(missing(positions) || missing(data)) return(ret)

    # The only requirement for the data slot in splusTimeSeries 
    # is that splusTimeSeries::is.rectangular(data) return TRUE
    # We'll convert vectors to 1 column matrices
    if (is.vector(data)) ret@data <- matrix(data,ncol=1)
    else ret@data <- data

    ret@positions <- as(positions,"utime")

    # ret@positions@format <- getOption("time.out.format")

    if (length(ret@positions) != nrow(ret@data))
        stop(paste("length(positions)=",length(positions),
                " not equal to nrow(data)=",nrow(ret@data)))

    if(!missing(units)) {
        ret@units <- as(units, "character")

        if (length(ret@units) != ncol(ret@data)) {
            lu <- length(ret@units)
            nc <- ncol(ret@data)
            warning(paste("length of units (",lu,
                    ") is not equal to number of columns in data (",nc,")",sep=""))
            if (nc < lu) ret$units[1:nc]
            else ret@units <- c(ret@units,rep("?",nc-lu))
        }
    }
    else ret@units <- rep("",ncol(ret@data))

    if (!missing(names)) colnames(ret@data) <- names

    if (!is.null(weights)) {
        ret@weights <- weights
        ret@weightmap <- as.integer(weightmap)
    }

    if (is.null(stations) || length(stations) == 0)
        stations <- rep(0L,ncol(ret@data))
    else if (!is.integer(stations)) stations <- as.integer(stations)

    ret@stations <- stations

    # Window squared and summed: if data is windowed (commonly
    # done in spectral analysis) wss is the sum of the
    # squared window coefficients * N. If a square window of
    # all 1's, then wss = N^2
    # We're assuming same window for all columns of this nts
    if (!is.null(wss)) res@wss <- wss

    deltat(ret) <- deltat(ret)	# this actualy does something!

    if (length(ret@positions)) {
        ret@start.position <- utime(ret@positions[1])
        ret@end.position <- utime(ret@positions[length(ret@positions)])
    }

    ret@time.format <- time.format
    ret@time.zone <- time.zone
    ret
}

setMethod("as.numeric", signature(x="nts"),
    function(x,...)
    {
        as.numeric(x@data,...)
    }
    )

setAs("nts","numeric",
    function(from)
    {
        as.numeric(from@data)
    }
    )

setAs("nts","matrix",
    function(from)
    {
        from@data
    }
    )

setMethod("as.logical", signature(x="nts"),
    function(x,...)
    {
        as.logical(x@data,...)
    }
    )

setAs("nts","logical",
    function(from)
    {
        as.logical(from@data)
    }
    )

as.logical.nts <- function(x,...)
{
    as.logical(x@data,...)
}

as.double.nts <- function(x,...)
{
    as.numeric(x@data,...)
}

as.vector.nts <- function(x,mode="any")
{
    as.vector(x@data,mode=mode)
}

as.matrix.nts <- function(x,...)
{
    x@data
}

"!.nts" <- function(x)
{
    x@data <- !x@data
    x
}

setMethod("!", signature(x="nts"),
    function(x) {
        "!.nts"(x)
    }
    )

setMethod("is.na",signature(x="nts"),
    function(x)
    {
        x@data <- is.na(x@data)
        x
    }
    )

setMethod("is.finite",signature(x="nts"),
    function(x)
    {
        x@data <- is.finite(x@data)
        x
    }
    )

setMethod("is.infinite",signature(x="nts"),
    function(x)
    {
        x@data <- is.infinite(x@data)
        x
    }
    )

setMethod("start", signature(x="nts"),
    function(x,...) {
        t1 <- utime(x@start.position)
        if (length(t1) == 0) {
            if (length(x@positions) > 0) t1 <- x@positions[1]
            else t1 <- utime(0)
        }
        t1
    }
    )

setMethod("end", signature(x="nts"),
    function(x,...)
    {
        t2 <- utime(x@end.position)
        if (length(t2) == 0) {
            if (length(x@positions) > 0) t2 <- x@positions[length(x@positions)]
            else t2 <- utime(0)
        }
        t2
    }
    )

setGeneric("start<-",function(x,value) standardGeneric("start<-"))

setReplaceMethod("start",signature(x="nts",value="utime"),
    function(x,value)
    {
        # cat ("start<- for nts\n")
        x@start.position <- value
        x
    }
    )

setGeneric("end<-",function(x,value) standardGeneric("end<-"))

setReplaceMethod("end",signature(x="nts",value="utime"),
    function(x,value)
    {
        x@end.position <- value
        x
    }
    )

setGeneric("units",function(x) standardGeneric("units"))

setMethod("units", signature(x="nts"),
    function(x)
    {
        x@units
    }
    )

setGeneric("units<-",function(x,value) standardGeneric("units<-"))

setReplaceMethod("units",signature(x="nts",value="character"),
    function(x,value)
    {
        if (length(value) != ncol(x@data))
            warning(paste("Length of units (",length(units),") is not equal to number of data columns (",ncol(x@data),")."))
        x@units <- value
        x
    }
    )

setGeneric("tspar",function(x) standardGeneric("tspar"))
setMethod("tspar", signature(x="nts"),
    function(x)
    {
        if (length(x@positions)) utime(x@positions)
        else stop("Empty time series")
    }
    )

setMethod("positions", signature(object="nts"),
    function(object)
    {
        object@positions
    }
    )

setReplaceMethod("positions", signature(object="nts",value="utime"),
    function(object,value)
    {
        object@positions <- value
        object
    }
    )

setReplaceMethod("positions", signature(object="nts",value="ANY"),
    function(object,value)
    {
        object@positions <- as(value,"utime")
        object
    }
    )


setMethod("dim", signature(x="nts"),
    function(x)
    {
        dim(x@data)
    }
    )

setMethod("dimnames", signature(x="nts"),
    function(x)
    {
        dimnames(x@data)
    }
    )

setMethod("colnames", signature(x="nts"),
    function(x)
    {
        colnames(x@data)
    }
    )

setMethod("rownames", signature(x="nts"),
    function(x)
    {
        rownames(x@data)
    }
    )

setReplaceMethod("dim", signature(x="nts",value="numeric"),
    function(x,value)
    {
        dim(x@data) <- value
        x@positions <- x@positions[1:value[1]]
        x
    }
    )

setReplaceMethod("dimnames", signature(x="nts",value="list"),
    function(x,value)
    {
        # Workaround for strange situation in R:
        # if x@data is a data.frame,
        # dimnames(x@data) <- list(NULL,c("a","b")) will
        # coerce both list elements to character before the
        # assignment.  as.character(NULL) is character(0) which is
        # an invalid dimname.
        # So do the rownames and colnames separately
        rownames(x@data) <- value[[1]]
        colnames(x@data) <- value[[2]]
        x
    }
    )

setReplaceMethod("colnames", signature(x="nts",value="character"),
    function(x,value)
    {
        colnames(x@data) <- value
        x
    }
    )

setReplaceMethod("rownames", signature(x="nts",value="character"),
    function(x,value)
    {
        rownames(x@data) <- value
        x
    }
    )


setGeneric("stations",function(x) standardGeneric("stations"))

setMethod("stations", signature(x="nts"),
    function(x)
    {
        x@stations
    }
    )

setGeneric("stations<-",function(x,value) standardGeneric("stations<-"))

setReplaceMethod("stations", signature(x="nts",value="integer"),
    function(x,value)
    {
        x@stations <- value
        x
    }
    )

setReplaceMethod("stations", signature(x="nts",value="ANY"),
    function(x,value)
    {
        nv <- names(value)
        if (is.null(nv)) nv <- rep("",length(value))
        if (!is.integer(value)) {
            if (is.numeric(value)) value <- round(value)
            value <- as.integer(value)
        }
        names(value) <- nv
        x@stations <- value
        x
    }
    )

setMethod("deltat", signature(x="utime"),
    function(x,...)
    {
        # cat(paste("deltat of utime, length(x)=",length(x),"\n"))
        if (length(x) > 1) {
            dts <- diff(x)
            rdt <- range(dts,na.rm=TRUE)
            mindt <- signif(rdt[1],6)
            maxdt <- signif(rdt[2],6)
            meddt <- signif(median(dts),6)
            mendt <- signif(mean(dts),6)
            tmendt <- signif(mean(dts,trim=.05),6)
            # Guess at the most likely deltat
            #
            # We don't use the median deltat, because sometimes we get
            # a bi-modal deltat distribution from a sensor.
            # Perhaps due to data buffering we see deltats of
            # .03 and .15 second from a 10hz sensor, that is actually
            # sampling at 10hz, but the assigned time tags are
            # a little wacky because of sample buffering.
            #
            # In this case we want to use a mean deltat, but that is thrown
            # off by outliers (gaps).  A trimmed mean screens out outlier deltats,
            # but slightly mis-estimates the bi-modal case.
            # So if we see a big difference between the trimmed mean and
            # median deltats, but the trimmed mean and the mean agree
            # pretty well, then we use the mean, otherwise the trimmed mean.

            if (any(is.na(c(tmendt,meddt,mendt)))) {
                warning("missing data in positions slot (times of time series)")
                bestdt <- NA_real_
            }
            else if (tmendt == 0 || (abs(meddt-tmendt)/tmendt > .1 &&
                    abs(mendt-tmendt)/tmendt < .05)) bestdt <- mendt
        else bestdt <- tmendt

        dt <- c(bestdt,meddt,mindt,maxdt,mendt,tmendt)
        }
        else dt <- rep(NA_real_,6)
        names(dt) <- c("","median","min","max","mean","trimmed.mean")
        dt
    }
    )

setGeneric("deltat",function(x,...) standardGeneric("deltat"))

setMethod("deltat", signature(x="nts"),
    function(x,...)
    {
        # cat(paste("deltat of nts, length(x@deltat)=",length(x@deltat),"\n"))
        if (length(dt <- x@deltat) == 0)
            dt <- deltat(x@positions)
        # cat("deltat dt=",paste(dt,collapse=","),"\n")
        dt
    }
    )

setMethod("deltat", signature(x="timeDate"),
    function(x,...)
    {
        deltat(as(x,"utime"))
    }
    )


setGeneric("deltat<-",function(x,value) standardGeneric("deltat<-"))

setReplaceMethod("deltat", signature(x="nts",value="numeric"),
    function(x,value)
    {
        if (length(names(value)) == 0) names(value) <- rep("",length(value))
        x@deltat <- value
        x
    }
    )

match.nts <- function(e1,e2,dt=getOption("dt.eps"),...)
{
    d1 <- deltat(e1)
    d2 <- deltat(e2)

    if (!is.null(d1) && nrow(e1) > 1 && !is.na(d1["min"]) &&
        d1["min"] < 0) warning("First time series is not ordered. match.nts results are probably not correct")

    if (!is.null(d2) && nrow(e2) > 1 && !is.na(d2["min"]) &&
        d2["min"] < 0) warning("Second time series is not ordered. match.nts results are probably not correct")


    e1 <- as.numeric(as(e1@positions,"utime"))
    e2 <- as.numeric(as(e2@positions,"utime"))

    if (is.null(dt)) 
        if (is.na(dt <- min(d1[1],d2[1]) * .01)) dt <- 1

    # avoid warnings of non-unique indices if there are repeated
    # timetags but the two sets are identical
    if (dt == 0. && length(e1) == length(e2) && all(e1==e2))
        return(seq(along=as.vector(e1)))

    # deltat() for a one-row time series is null, min(NULL) is NA
    if (dt == 0.) return(match(e1,e2,nomatch=0))

    match.delta(e1,e2,dt)
}

match.delta <- function(e1,e2,delta,cond="nearest")
{
    icond <- if (cond == "nearest") 1L else if (cond == "first") 0L 
        else stop(paste("cond=",cond,"should be either nearest or first"))
    .Call("match_within",e1,e2,as.numeric(delta),icond,PACKAGE="eolts")
}

setMethod("[",signature(x="nts"),
    function(x,i,j,...)
    {
        ## drop argument is ignored, forced to FALSE

        # cat(paste("in [ nts, nargs=",nargs(),
        #           ", missing(i)=",missing(i),
        #           ", missing(j)=",missing(j),"\n"))

        # x[]           nargs()==2, !has.i, !has.j
        # x[i]          nargs()==2, has.i, !has.j
        # x[i,]         nargs()==3, has.i, !has.j
        # x[,j]         nargs()==3, !has.i, has,j
        # x[i,j]        nargs()==3, has.i, has.j

        # with only one indice, x[i] is no longer an nts.
        # Samples from each observation time are selectively removed.
        # However, there seems to be a bug in timeSeries, such that
        # x[i] returns the ith row of the time series. So
        # we'll just index the data slot
        # Note that if you index an nts with only one argument (x[i]) you don't
        # get an nts back.
        has.i <- !missing(i)
        if (nargs() == 2) {
            if (!has.i) return(x)
            else {
                if (is(i,"nts")) i <- i@data
                return(x@data[i])
            }
        }
        has.j <- !missing(j)

        nr <- nrow(x)
        nc <- ncol(x)

        # row index i can be an nts or a pair of utimes.
        # Convert these to integer vectors, and then call "[" again.
        if (has.i && length(i) > 0) {
            # cat(paste("in [ nts, length(i)=",length(i),"\n"))

            # Note that if you index an nts with only one argument (x[i]) you don't
            # get an nts back. Cannot, in general, return a time series or matrix,
            # because samples from each observation time are selectively removed.

            if (is(i,"nts")) {
                # i is an nts, but is not the only argument (x[i,] or x[i,j]),
                #	use its times to subsample x.
                #	If it is logical, just use column 1 (this is debateable)
                if (is.logical(i@data)) {
                    ii <- !is.na(as.logical(i@data[,1])) & as.logical(i@data[,1])
                    if (!any(ii)) {
                        warning("i[,1] is all false")
                        return(numeric(0))
                    }
                    i <- i[ii,1]
                }
                i[] <- TRUE

                # for each time in i, its position in x
                i <- match.nts(i,x)
                if (all(i==0)) warning("Time series are not synchronized. No matching observations found.  Increase 'options(dt.eps=seconds)'")
                i <- i[i & !duplicated(i)]
            }
            else if(is(i, "utime") || is(i[1], "utime")) {
                if (length(dim(i)) != 2) i <- matrix(as.numeric(i),nrow=2)
                else i <- as.matrix(as.numeric(i))

                timex <- as.numeric(x@positions)

                lt <- outer(timex,i[2,],"<=")	# length(timex) X 1 X ncol(i)
                gt <- outer(timex,i[1,],">=")	# length(timex) X 1 X ncol(i)
                cond <- ((lt & gt) %*% rep(1,ncol(i))) > 0
                i <- seq(along=cond)[cond]

                if(length(i)==0 || all(i==0)) {
                    warning("time series object has zero length")
                    return(numeric(0))
                }
            }
        }

        timex <- x@positions

        weights <- x@weights
        weightmap <- x@weightmap
        stations <- x@stations
        units <- x@units
        t1 <- start(x)
        t2 <- end(x)

        newdt <- F

        # we have selected rows, so recompute deltat
        if (has.i && length(i) > 0) newdt <- T
        else i <- 1:nr

        if (!has.j || length(j) == 0) j <- 1:nc
        else if (is.character(j)) j <- match(j,dimnames(x)[[2]],nomatch=0)

        class.x <- class(x)
        ts.class <- "timeSeries"
        attr(ts.class,"package") <- "splusTimeSeries"
        class(x) <- ts.class
        # cat("calling [ on timeSeries\n")
        x <- x[i,j,drop=FALSE]
        # cat("called [ on timeSeries\n")
        class(x) <- class.x
        x@positions <- utime(x@positions)

        if (!length(x)) {
            warning("time series object has zero length")
            return(x)
        }

        if (length(weightmap) > 0) {
            weightmap <- weightmap[j]
            if (length(weightmap) == 0) {
                weights <- matrix(ncol=0,nrow=0)
                weightmap <- integer(0)
            }
            else if (length(weights) > 0) {
                uw <- unique(weightmap)
                uw <- uw[uw!=0]
                # Remap weights
                weightmap <- match(weightmap,uw,nomatch=0)
                weights <- weights[i,uw,drop=FALSE]
            }
        }
        else weights <- matrix(ncol=0,nrow=0)

        if (!is.null(weights)) {
            x@weights <- weights
            x@weightmap <- weightmap
        }

        stations(x) <- stations[j]
        x@units <- units[j]

        # recompute deltat
        if (newdt) {
            x@deltat <- numeric(0)
            x@start.position <- numeric(0)
            x@end.position <- numeric(0)
            deltat(x) <- deltat(x)
            start(x) <- start(x)
            end(x) <- end(x)
        }
        else {
            start(x) <- t1
            end(x) <- t2
        }
        x
    }
    )

setReplaceMethod("[",signature(x="nts",value="nts"),
    function(x,i,j,...,value)
    {
        # cat(paste("in [<- nts, nargs=",nargs(),
        #           ", missing(i)=",missing(i),
        #           ", missing(j)=",missing(j),"\n"))

        # x[] <- y      nargs()==3, !has.i, !has.j
        # x[i] <- y     nargs()==3, has.i, !has.j
        # x[i,] <- y    nargs()==4, has.i, !has.j
        # x[,j] <- y    nargs()==4, !has.i, has,j
        # x[i,j] <- y   nargs()==4, has.i, has.j
        has.i <- !missing(i)
        has.j <- !missing(j)

        if (!has.i && !has.j) {
            value <- value@data
            x@data[] <- value
            return(x)
        }

        nr <- nrow(x)
        nc <- ncol(x)

        if (!has.i) i <- 1:nr

        # cat("length(i)=",length(i)," length(j)=",length(j)," length(value)=",length(value),"\n")

        if (is(i,"nts")) {
            if (nargs() == 3) {
                if (is.logical(i@data)) {
                    # If called as "x[i] <- y", and i is a logical nts, then
                    # match the i and x nts's, and select those matching for
                    # which i is true
                    ii <- matrix(F,ncol=nc,nrow=nr)
                    xi <- match.nts(x,i)
                    ii[xi != 0,] <- i@data[xi,]
                    i <- as.vector(ii)
                }
                else {
                    warning("When doing \"x[i] <- value\" , and i is a timeseries, then i should be time series of logical values. Converting i to an integer matrix.")
                    i <- as.integer(i@data)
                }
            }
            else {
                # treat column of the i timeseries as a set of selection times
                i <- i[,1]
                if (is.logical(i@data)) {
                    # If called as "x[i,] <- y", and i is a logical nts, then use first
                    # column of i
                    i <- i[!is.na(i@data) & i@data,]
                    # for each time in i, its position in x
                    i <- match.nts(i,x)
                } else {
                    i <- match.nts(i,x)
                    if (all(i==0)) warning("Time series are not synchronized. No matching observations found.  Increase 'options(dt.eps=seconds)'")
                }
                i <- i[i!=0 & !duplicated(i)]
            }
        }
        else if(is(i, "utime")) {
            timex <- as.numeric(utime(x@positions))

            if (length(dim(i)) != 2) i <- matrix(as.numeric(i),nrow=2)
            else i <- as.matrix(i)
            lt <- outer(timex,i[2,],"<=")	# length(timex) X 1 X ncol(i)
            gt <- outer(timex,i[1,],">=")	# length(timex) X 1 X ncol(i)
            cond <- ((lt & gt) %*% rep(1,ncol(i))) > 0
            i <- seq(along=cond)[cond]
            if(any(!i)) return(NULL)
        }

        if (!has.j) {
            j <- NULL
            if (nargs() == 4 && !identical(dim(i),dim(x))) j <- 1:nc
        }

        if (is.logical(i) && any(is.na(i))) i[is.na(i)] <- FALSE
        if (is.character(j)) j <- match(j,dimnames(x)[[2]],nomatch=0)
        if (is.logical(j) && any(is.na(j))) j[is.na(j)] <- FALSE

        # If updating all rows of x, then update detrend parameters, units
        # and weights
        if (nargs()==4 && length(seq(along=i)[i]) == nr) {
            # cat("doing more\n")
            detrend.x <- NULL
            if (!is.null(nrow(value)) && nr != nrow(value))
                warning(paste("nrows of x (",nr,") are not equal to nrows of value (",
                        nrow(value),")",sep=""))

            # detrend.new <- attr(value,"detrend")
            detrend.new <- NULL

            if (!is.null(detrend.x) || !is.null(detrend.new)) {
                if (is.null(detrend.new)) {
                    if (is.matrix(value)) {
                        detrend.new <- matrix(0,nrow=2,ncol=ncol(value),
                            dimnames=list(c("Intercept","X"),dimnames(value)[[2]]))
                    }
                    else {
                        detrend.new <- matrix(0,nrow=2,ncol=1,
                            dimnames=list(c("Intercept","X")," "))
                    }
                }
                if (is.null(detrend.x))
                    detrend.x <- matrix(0,nrow=2,ncol=nc,
                        dimnames=list(c("Intercept","X"),dimnames(x)[[2]]))
                detrend.x[,j,drop=FALSE] <- detrend.new
            }

            # weights
            if ((length(weights.new <- value@weights) > 0 &&
                    any(wm.new <- (wmap.new <- value@weightmap) != 0)) ||
                length(x@weights) > 0) { 

                weights.x <- x@weights
                wmap.x <- x@weightmap

                if (length(weights.x) > 0) {
                    wmap.x[j] <- 0L
                    wm <- wmap.x != 0	# untouched columns with weights
                    if (length(weights.new) > 0 && nrow(weights.x) != nrow(weights.new)) {
                        # if (nr != nrow(value))
                        # warning(paste("nrows of x (",nr,") are not equal to nrows of value (",
                        # nrow(value),")",sep=""))

                        weights.new <- matrix(0,ncol=0,nrow=0)
                        wmap.new[] <- 0L
                        wm.new[] <- F
                    }
                    if (any(wm)) {
                        uwm <- unique(wmap.x[wm])
                        weights.x <- weights.x[,uwm,drop=FALSE]
                        wmap.x <- match(wmap.x,uwm,nomatch=0L)
                    }
                    else weights.x <- matrix(0,ncol=0,nrow=0)
                }
                else wmap.x <- rep(0L,nc)

                if (length(weights.new) > 0) {
                    if (length(weights.x) == 0) weights.x <- weights.new
                    else  weights.x <- cbind(weights.x,weights.new)
                    wmap.new[wm.new] <- wmap.new[wm.new] + as.integer(max(wmap.x))
                    wmap.x[j] <- wmap.new
                }

                x@weights <- weights.x
                x@weightmap <- wmap.x
            }
        }

        value <- value@data
        # browser()

        if (is.null(j)) x@data[i] <- value
        else x@data[i,j] <- value
        x
    }
    )

setReplaceMethod("[",signature(x="nts",value="vector"),
    function(x,i,j,...,value)
    {
        # cat(paste("in [<- nts, nargs=",nargs(),
        #           ", missing(i)=",missing(i),
        #           ", missing(j)=",missing(j),"\n"))

        # x[] <- y      nargs()==3, !has.i, !has.j
        # x[i] <- y     nargs()==3, has.i, !has.j
        # x[i,] <- y    nargs()==4, has.i, !has.j
        # x[,j] <- y    nargs()==4, !has.i, has,j
        # x[i,j] <- y   nargs()==4, has.i, has.j
        has.i <- !missing(i)
        has.j <- !missing(j)

        if (!has.i && !has.j) {
            x@data[] <- value
            return(x)
        }

        nr <- nrow(x)
        nc <- ncol(x)
        if (!has.i) i <- 1:nr

        if (is(i,"nts")) {
            if (nargs() == 3) {
                if (is.logical(i@data)) {
                    # If called as "x[i] <- y", and i is a logical nts, then
                    # match the i and x nts's, and select those matching for
                    # which i is true
                    ii <- matrix(FALSE,ncol=nc,nrow=nr)
                    xi <- match.nts(x,i)
                    ii[xi != 0,] <- i@data[xi,]
                    i <- as.vector(ii)
                }
                else {
                    warning("When doing \"x[i] <- value\" , and i is a timeseries, then i should be time series of logical values. Converting i to an integer matrix.")
                    i <- as.integer(i@data)
                }
            }
            else {
                # treat column of the i timeseries as a set of selection times
                i <- i[,1]
                if (is.logical(i@data)) {
                    # If called as "x[i,] <- y", and i is a logical nts, then use first
                    # column of i
                    i <- i[!is.na(i@data) & i@data,]
                    # for each time in i, its position in x
                    i <- match.nts(i,x)
                } else {
                    i <- match.nts(i,x)
                    if (all(i==0)) warning("Time series are not synchronized. No matching observations found.  Increase 'options(dt.eps=seconds)'")
                }
                i <- i[i!=0 & !duplicated(i)]
            }
        }
        else if(is(i, "utime")) {
            timex <- as.numeric(x@positions)

            if (length(dim(i)) != 2) i <- matrix(as.numeric(i),nrow=2)
            else i <- as.matrix(i)
            lt <- outer(timex,i[2,],"<=")	# length(timex) X 1 X ncol(i)
            gt <- outer(timex,i[1,],">=")	# length(timex) X 1 X ncol(i)
            cond <- ((lt & gt) %*% rep(1,ncol(i))) > 0
            i <- seq(along=cond)[cond]
            if(any(!i)) return(NULL)
        }

        if (!has.j) {
            j <- NULL
            if (nargs() == 4 && !identical(dim(i),dim(x))) j <- 1:nc
        }

        if (is.logical(i) && any(is.na(i))) i[is.na(i)] <- FALSE
        if (is.character(j)) j <- match(j,dimnames(x)[[2]],nomatch=0)
        if (is.logical(j) && any(is.na(j))) j[is.na(j)] <- FALSE

        if (is.null(j)) x@data[i] <- value
        else x@data[i,j] <- value
        x
    }
    )


setMethod("Math",signature(x="nts"),
    function(x)
    {
        x@data <- callGeneric(x@data)
        x
    }
    )

setMethod("mean",signature(x="nts"),
    function(x,trim=0,na.rm=TRUE,...)
    {
        mean(x@data,trim=trim,na.rm=na.rm,...)
    }
    )

setMethod("var",signature(x="nts"),
    function(x,y=NULL,na.rm=TRUE,use)
    {
        var(x@data,na.rm=na.rm,use=use)
    }
    )

setMethod("cumsum",signature(x="nts"),
    function(x)
    {
        x@data = matrix(apply(x@data,2,cumsum),ncol=ncol(x),dimnames=dimnames(x@data))
        x
    }
    )

setMethod("atan2",signature(y="nts",x="nts"),
    function(y,x)
    {
        if (length(tol <- getOption("dt.eps")) == 0) {
            d1 <- deltat(y)
            d2 <- deltat(x)

            if (!is.null(d1) && nrow(y) > 1 && !is.na(d1["min"]) &&
                d1["min"] < 0) warning("First time series is not ordered. align results are probably not correct")

            if (!is.null(d2) && nrow(x) > 1 && !is.na(d2["min"]) &&
                d2["min"] < 0) warning("Second time series is not ordered. align results are probably not correct")

            # If dt.eps is not defined, use .1 of deltat.
            # deltat() for a one-row time series is null, min(NULL) is NA
            if (is.na(tol <- min(d1[1],d2[1]) * .1)) tol <- 1.0;
        }

        p1 <- positions(y)
        p2 <- positions(x)

        # make union of positions. Taken from default method for seriesMerge
        if (!identical(p1,p2)) {
            if(is(p1, "utime") != is(p2,"utime"))
                stop(paste("Cannot mix series types. Positions(",
                        class(p1),",",class(p2),") are not comparable"))

            # call C function to find out alignment
            # returns a list of ( indexes for NA, indexes to drop, subscript or
            # interpolation weights )

            # points in p1 which exceed deltat tolerance of p2
            nuke.p1 <- .Call("utime_align", p1, p2, c("NA", "NA"), tol, PACKAGE="eolts")

            # points in p2 which exceed deltat tolerance of p1
            nuke.p2 <- .Call("utime_align", p2, p1, c("NA", "NA"), tol, PACKAGE="eolts")

            # cat("length(p1) before nuke=",length(p1),"\n")
            # cat("length(p2) before nuke=",length(p2),"\n")
            # browser()

            # points in p1 which are within deltat tolerance of p2
            p1 <- p1[nuke.p1[[3]][!nuke.p1[[1]] & !nuke.p1[[2]]]]

            # points in p2 which are within deltat tolerance of p1
            p2 <- p2[nuke.p2[[3]][!nuke.p2[[1]] & !nuke.p2[[2]]]]

            if (length(p1) == 0) stop("no timetags in e1 are within ",tol,"of e2")
            if (length(p2) == 0) stop("no timetags in e2 are within ",tol,"of e1")

            # cat("length(p1) after nuke=",length(p1),", tol=",tol,"\n")
            # cat("length(p2) after nuke=",length(p2),", tol=",tol,"\n")

            # create the union of p1 and p2, discard points that are close
            dt <- min(d1[1] * .5,d2[1] * .5, tol)
            tol2 <- dt
            p1 <- sort(c(p1, p2))
            ok <- abs(diff(p1)) > tol2
            if(length(ok) && !all(ok)) {
                # make it the right length
                ok <- c(ok, T)
                p1 <- p1[ok]
            }
            # cat("length(p1) after unionPositions=",length(p1),", dt=",dt,"\n")
            if (length(p1) == 0) return(NULL)

            x <- align(x, p1, how="NA",matchtol=tol)
            x@deltat <- numeric(0)
            x@weights <- matrix(ncol=0,nrow=0)
            x@weightmap <- integer(0)

            y <- align(y, p1, how="NA",matchtol=tol)
            y@deltat <- numeric(0)
            y@weights <- matrix(ncol=0,nrow=0)
            y@weightmap <- integer(0)
        }

        nr1 <- nrow(y)
        nr2 <- nrow(x)
        nc1 <- ncol(y)
        nc2 <- ncol(x)

        if (nr1 == nr2 && nc1 != nc2) {
            if ((nc1 %% nc2) == 0)		# nc2 divides evenly into nc1
                y@data <- atan2(y@data, as.vector(x@data))
            else if ((nc2 %% nc1) == 0)		# nc1 divides evenly into nc2
                y@data <- atan2(as.vector(y@data), x@data)
            else y@data <- atan2(y@data, x@data)
        }
        else y@data <- atan2(y@data, x@data)

        # remove weights when computing atan2 of two nts
        y@weights <- matrix(ncol=0,nrow=0)
        y@weightmap <- integer(0)
        y
    }
    )

setMethod("atan2",signature(y="nts",x="ANY"),
    function(y,x)
    {
        y@data <- atan2(y@data,x)
        x
    }
    )

setMethod("atan2",signature(y="ANY",x="nts"),
    function(y,x)
    {
        x@data <- atan2(y,x@data)
        x
    }
    )

# Override Ops method for nts.
setMethod("Ops",signature(e1="nts",e2="nts"),
    function(e1, e2)
    {
        # cat(".Generic=",.Generic," .Signature=",.Signature,"\n")

        d1 <- deltat(e1)
        d2 <- deltat(e2)
        s1 <- stations(e1)
        s2 <- stations(e2)

        dt.eps <- getOption("dt.eps") # seconds
        if (length(dt.eps) == 0) {
            if (!is.null(d1) && nrow(e1) > 1 && !is.na(d1["min"]) &&
                d1["min"] < 0) warning("First time series is not ordered. align results are probably not correct")

            if (!is.null(d2) && nrow(e2) > 1 && !is.na(d2["min"]) &&
                d2["min"] < 0) warning("Second time series is not ordered. align results are probably not correct")

            # deltat() for a one-row time series is NA, min(NULL) is NA
            if (is.na(dt.eps <- min(d1[1],d2[1]) * .1)) dt.eps <- 1
        }
        tol <- dt.eps
        p1 <- positions(e1)
        p2 <- positions(e2)

        # make union of positions. Taken from default method for seriesMerge
        if (!identical(p1,p2)) {
            if(is(p1, "positionsCalendar") != is(p2,"positionsCalendar"))
                stop(paste("Cannot mix series types. Positions(",
                        class(p1),",",class(p2),") are not comparable"))

            # call C function to find out alignment
            # returns a list of ( indexes for NA, indexes to drop, subscript or
            # interpolation weights )

            # points in p1 which exceed deltat tolerance of p2
            nuke.p1 <- .Call("utime_align", p1, p2, c("NA", "NA"), tol, PACKAGE="eolts")

            # points in p2 which exceed deltat tolerance of p1
            nuke.p2 <- .Call("utime_align", p2, p1, c("NA", "NA"), tol, PACKAGE="eolts")

            # cat("length(p1) before nuke=",length(p1),"\n")
            # cat("length(p2) before nuke=",length(p2),"\n")
            # browser()

            # points in p1 which are within deltat tolerance of p2
            p1 <- p1[nuke.p1[[3]][!nuke.p1[[1]] & !nuke.p1[[2]]]]

            # points in p2 which are within deltat tolerance of p1
            p2 <- p2[nuke.p2[[3]][!nuke.p2[[1]] & !nuke.p2[[2]]]]

            if (length(p1) == 0) stop("no timetags in e1 are within ",tol,"of e2")
            if (length(p2) == 0) stop("no timetags in e2 are within ",tol,"of e1")

            # cat("length(p1) after nuke=",length(p1),", dt.eps=",dt.eps,"\n")
            # cat("length(p2) after nuke=",length(p2),", dt.eps=",dt.eps,"\n")

            # create the union of p1 and p2, discard points that are close
            dt <- min(d1[1]*.5,d2[1]*.5,dt.eps)
            tol2 <- dt
            p1 <- sort(c(p1, p2))
            ok <- abs(diff(p1)) > tol2
            if(length(ok) && !all(ok)) {
                # make it the right length
                ok <- c(ok, T)
                p1 <- p1[ok]
            }
            # cat("length(p1) after unionPositions=",length(p1),", dt=",dt,"\n")
            if (length(p1) == 0) return(NULL)

            # browser()
            e1 <- align(e1, p1, how="NA",matchtol=tol)
            e1@deltat <- numeric(0)
            e1@weights <- matrix(ncol=0,nrow=0)
            e1@weightmap <- integer(0)

            e2 <- align(e2, p1, how="NA",matchtol=tol)
            e2@deltat <- numeric(0)
            e2@weights <- matrix(ncol=0,nrow=0)
            e2@weightmap <- integer(0)
        }

        nr1 <- nrow(e1)
        nr2 <- nrow(e2)
        nc1 <- ncol(e1)
        nc2 <- ncol(e2)

        if (nr1 == nr2 && nc1 != nc2) {
            if ((nc1 %% nc2) == 0)		# nc2 divides evenly into nc1
                e1@data <- callGeneric(e1@data, as.vector(e2@data))
            else if ((nc2 %% nc1) == 0)		# nc1 divides evenly into nc2
                e1@data <- callGeneric(as.vector(e1@data), e2@data)
            else e1@data <- callGeneric(e1@data, e2@data)
        }
        else e1@data <- callGeneric(e1@data, e2@data)

        # remove weights when multiplying two nts together

        # If weights are identical then keep them, otherwise discard
        if (length((wts1 <- e1@weights)) > 0 && length((wts2 <- e2@weights)) > 0) {
            wm1 <- e1@weightmap
            wm2 <- e2@weightmap
            # if weights are identical then keep e1 weights
            if (!identical(wts1[,wm1],wts2[,wm2])) {
                e1@weights <- matrix(ncol=0,nrow=0)
                e1@weightmap <- integer(0)
            }
        }
        else {
            e1@weights <- matrix(ncol=0,nrow=0)
            e1@weightmap <- integer(0)
        }
        if (length(s1) > length(s2)) {
            ns <- names(s1)
            nx <- max(length(s2)/length(s1),1)
            s1 <- rep(s1,nx)
        }
        else {
            ns <- names(s2)
            nx <- max(length(s1)/length(s2),1)
            s1 <- rep(s2,nx)
        }
        names(s1) <- rep(ns,nx)
        stations(e1) <- s1
        e1
    }
    )

setMethod("Ops",signature(e1="nts",e2="timeSeries"),
    function(e1,e2)
    {
        # cat(".Generic=",.Generic," .Signature=",.Signature,"\n")
        sx <- e1@stations
        wx <- e1@weights
        wmx <- e1@weightmap
        class.e1 <- class(e1)
        class(e1) <- "timeSeries"
        e1 <- callGeneric(e1,e2)
        class(e1) <- class.e1
        stations(e1) <- sx
        e1@weights <- wx
        e1@weightmap <- wmx
        e1
    }
    )
setMethod("Ops",signature(e1="nts",e2="ANY"),
    function(e1,e2)
    {
        # cat(".Generic=",.Generic," .Signature=",.Signature,"\n")
        e1@data <- callGeneric(e1@data,e2)
        if (length(e1@data) == 0) e1 <- NULL
        e1
    }
    )

setMethod("Ops",signature(e1="timeSeries",e2="nts"),
    function(e1,e2)
    {
        # cat(".Generic=",.Generic," .Signature=",.Signature,"\n")
        sx <- e2@stations
        wx <- e2@weights
        wmx <- e2@weightmap
        class.e2 <- class(e2)
        class(e2) <- "timeSeries"
        e2 <- callGeneric(e1,e2)
        class(e2) <- class.e2
        stations(e2) <- sx
        e2@weights <- wx
        e2@weightmap <- wmx
        e2
    }
    )
setMethod("Ops",signature(e1="ANY",e2="nts"),
    function(e1,e2)
    {
        # cat(".Generic=",.Generic," .Signature=",.Signature,"\n")
        e2@data <- callGeneric(e1,e2@data)
        if (length(e2@data) == 0) e2 <- NULL
        e2
    }
    )
setMethod("Ops",signature(e1="nts",e2="missing"),
    function(e1,e2)
    {
        # cat(".Generic=",.Generic," .Signature=",.Signature,"\n")
        sx <- e1@stations
        wx <- e1@weights
        wmx <- e1@weightmap
        class.e1 <- class(e1)
        class(e1) <- "timeSeries"
        e1 <- callGeneric(e1)
        class(e1) <- class.e1
        stations(e1) <- sx
        e1@weights <- wx
        e1@weightmap <- wmx
        e1
    }
    )

setMethod("%*%",signature(x="nts",y="matrix"),
    function(x, y)
    {
        x@data <- callGeneric(x@data,y)
        x
    }
)

setMethod("%*%",signature(x="matrix",y="nts"),
    function(x, y)
    {
        y@data <- callGeneric(x,y@data)
        y
    }
)

# Transpose of an nts becomes a matrix
setMethod("t",signature(x="nts"),
    function(x)
    {
        t(x@data)
    }
)

setMethod("diff",signature(x="nts"),
    function(x)
    {
        y <-  x[-1,]
        y@data <- diff(x@data)
        y
    }
)

setMethod("Complex",signature(z="nts"),
    function(z) {
        z@data <- callGeneric(z@data)
        z
    }
    )

setMethod("Summary",signature(x="nts"),
    function(x,...,na.rm=TRUE)
    {
        if (all(is.na(x@data))) callGeneric(x@data,...,na.rm=FALSE)
        else callGeneric(x@data,...,na.rm=TRUE)
    }
    )

setGeneric("seriesMerge")
# setGeneric("seriesMerge",function(x1,x2,...) standardGeneric("seriesMerge"))

setMethod("seriesMerge",signature(x1="nts",x2="nts"),
    function(x1,x2,...,pos="union")
    {

        args <- list(...)

        is_ts <- unlist(sapply(args, function(x) {
                if (.hasSlot(x,"positions"))
                    TRUE
                else
                    FALSE
                        }))
        ndots <- sum(is_ts)

        if (hasArg(how)) {
            if ("how" %in% names(args)) how <- args$how
        }
        else how <- "NA"

        if (hasArg(error.how)) {
            if ("error.how" %in% names(args)) error.how=args$error.how
        }
        else error.how <- "NA"

        if (hasArg(matchtol)) {
            if ("matchtol" %in% names(args)) matchtol <- args$matchtol
        }
        else {
            if (hasArg(dt) && "dt" %in% names(args)) matchtol <- args$dt
            else matchtol <- min(deltat(x1)[1],deltat(x2)[1]) * .5
        }
        if (length(matchtol) < 1 || is.na(matchtol)) matchtol <- 1	# 1 second

        if (hasArg(suffixes)) {
            if ("suffixes" %in% names(args)) suffixes <- args$suffixes
        }
        else suffixes <- paste(".", 1:(ndots + 2), sep="")

        # cat("seriesMerge, nts, nts,length(args)=",length(args),
        #     ", names(args)=", paste(names(args),collapse=","),
        #     ", hasArg=", hasArg(error.how),
        #     ", args$error.how=",args$error.how,
        #     ", error.how=",error.how,"\n")

        args <- args[is_ts]

        nc1 <- ncol(x1)
        nc2 <- ncol(x2)
        n1 <- dimnames(x1)[[2]]
        n2 <- dimnames(x2)[[2]]

        s1 <- stations(x1)
        if (length(s1) == 0) s1 <- rep(NA_integer_,nc1)
        s2 <- stations(x2)
        if (length(s2) == 0) s2 <- rep(NA_integer_,nc2)

        if (length(x1@weights) > 0)
            w1 <- nts(x1@weights,x1@positions,rep("",ncol(x1@weights)))
        else w1 <- matrix(ncol=0,nrow=0)

        if (length(x2@weights) > 0)
            w2 <- nts(x2@weights,x2@positions,rep("",ncol(x2@weights)))
        else w2 <- matrix(ncol=0,nrow=0)

        wm1 <- x1@weightmap
        wm2 <- x2@weightmap

        if (length(pos) == 1 && pos == "union") {
            tstart <- min(start(x1),start(x2))
            tend <- max(end(x1),end(x2))
        }

        class.x1 <- class(x1)
        class(x1) <- "timeSeries"
        class(x2) <- "timeSeries"

        # cat("calling seriesMerge, timeSeries, timeSeries\n")
        x1 <- splusTimeSeries::seriesMerge(x1,x2,pos=pos,
            how=how,error.how=error.how,matchtol=matchtol/86400,suffixes=suffixes[1:2])
        class(x1) <- class.x1

        # seriesMerge adds arg numbers to dimnames: "a" + "b" -> "a1" "b2". Remove them
        dimnames(x1) <- list(NULL,c(n1,n2))

        # Perhaps x1 has weights and x2 doesn't, or vv
        wmap <- integer(0)

        ncw1 <- ncw2 <- 0

        if (length(w1) > 0) {
            ncw1 <- ncol(w1)
            wmap <- wm1
        }

        if (length(w2) > 0) {
            ncw2 <- ncol(w2)
            if (length(wmap) == 0) wmap <- rep(0,nc1)
            wmap <- c(wmap,wm2 + ncw1)
        }
        else if (length(wmap) > 0) wmap <- c(wmap,rep(0,nc2))

        if (ncw1 + ncw2 > 0) {
            if (ncw1 > 0) {
                if (ncw2 > 0) {
                    if (nrow(w1) == nrow(w2) && nrow(w1) == nrow(x1)) w1 <- cbind(w1@data,w2@data)
                    else {
                        w1 <- seriesMerge(w1,w2,
                            pos=pos,how=how,error.how=error.how,
                            matchtol=matchtol,suffixes=suffixes[1:2])@data
                    }
                }
                else w1 <- align(w1,positions(x1),how=how,matchtol=matchtol)@data

            }
            else w1 <- align(w2,positions(x1),how=how, matchtol=matchtol)@data

            if (nrow(w1) != nrow(x1)) stop(paste("nrow(x1@weights)=",nrow(w1),
                    " is not equal to nrow(x1)=",nrow(x1),sep=""))

        }

        x1@weights <- w1
        x1@weightmap <- as.integer(wmap)
        s1 <- c(s1,s2)
        stations(x1) <- s1

        if (length(pos) == 1 && pos == "union") {
            start(x1) <- tstart
            end(x1) <- tend
        }
        else {
            # reset start and end time of not doing union
            start(x1) <- positions(x1)[1]
            end(x1) <- tail(positions(x1),1)
        }

        i <- 1
        while( i <= length(args)) {
            x1 <- seriesMerge(x1,args[[i]],pos="union",how=how,error.how=error.how,
                matchtol=matchtol,suffixes=suffixes[c(1,i+2)])
            i <- i + 1
        }
        x1
    }
)

setMethod("seriesMerge",signature(x1="nts",x2="timeSeries"),
    function(x1,x2,...)
    {
        # convert second arg to nts
        x2 <- nts(x2@data,x2@positions,x2@units)
        seriesMerge(x1,x2,...)
    }
)

setMethod("seriesMerge",signature(x1="timeSeries",x2="nts"),
    function(x1,x2,...)
    {
        # convert first arg to nts
        x1 <- nts(x1@data,x1@positions,x1@units)
        seriesMerge(x1,x2,...)
    }
)

setMethod("seriesMerge",signature(x1="nts",x2="numeric"),
    function(x1,x2,...)
    {

        args <- list(...)

        is_ts <- unlist(sapply(args, function(x) {
                if (.hasSlot(x,"positions"))
                    TRUE
                else
                    FALSE
                    }))
        ndots <- sum(is_ts)

        if (hasArg(how)) how <- args$how
        else how <- NULL

        if (hasArg(error.how)) error.how <- args$error.how
        else error.how <- NULL

        if (hasArg(matchtol)) matchtol <- args$matchtol
        else {
            if (hasArg(dt)) matchtol <- args$dt
            else matchtol <- min(deltat(x1)[1],deltat(x2)[1]) * .1
        }
        if (is.na(matchtol)) matchtol <- 1	# 1 second

        if (hasArg(suffixes)) suffixes <- args$suffixes
        else suffixes <- paste(".", 1:(ndots + 2), sep="")

        args <- args[is_ts]

        nc1 <- ncol(x1)
        w1 <- x1@weights
        wm1 <- x1@weightmap
        s1 <- stations(x1)
        if (length(s1) == 0) s1 <- rep(NA_integer_,nc1)

        if (is.matrix(x2)) {
            nx2 <- dimnames(x2)[[2]]
            nc2 <- ncol(x2)
        }
        else {
            nx2 <- "x2"
            nc2 <- 1
        }
        if (length(wm1) > 0) wm1 <- c(wm1,rep(0,nc2))
        s1 <- c(s1,rep(0,nc2))

        x2 <- nts(cbind(x1@data,x2),x1@positions,weights=w1,weightmap=wm1,
            stations=s1)
        dimnames(x2) <- list(NULL,c(dimnames(x1)[[2]],nx2))

        i <- 1
        while( i <= length(args)) {
            x2 <- seriesMerge(x2,args[[i]],
                how=how,error.how=error.how,matchtol=matchtol,suffixes=suffixes[c(1,i+2)])
            i <- i + 1
        }
        x2
    }
    )

setMethod("is.finite",signature(x="nts"),
    function(x)
    {
        x@data <- is.finite(x@data)
        x
    }
    )

setGeneric("seriesConcat",function(x1,x2,...) standardGeneric("seriesConcat"))

setMethod("seriesConcat",signature(x1="nts",x2="nts"),
    function(x1,x2,...)
    {

        n1 <- dimnames(x1)[[2]]
        n2 <- dimnames(x1)[[2]]

        if (!identical(n1,n2)) warning("column names of x1 do no match column names of x2")

        p1 <- x1@positions
        p2 <- x2@positions

        wm1 <- x1@weightmap
        if (length(wm1) == 0) wm1 <- rep(0,ncol(x1))
        wm2 <- x2@weightmap
        if (length(wm2) == 0) wm2 <- rep(0,ncol(x2))

        weightmap <- matrix(c(wm1,wm2),nrow=2,byrow=TRUE)

        # non-zeros in this result  are for columns that have at least
        # one weight
        weightmap <- as.integer(as.vector(t(c(1,1)) %*% weightmap))

        weights <- matrix(ncol=0,nrow=0)

        if (any(weightmap != 0)) {

            # create an ordered weightmap
            weightmap[weightmap > 0] <- 1:length(seq(along=weightmap)[weightmap > 0])

            w1 <- matrix(NA_integer_,nrow=nrow(x1),ncol=max(weightmap))
            if (any(wm1 != 0)) w1[,weightmap[wm1 != 0]] <- x1@weights[,wm1[wm1 != 0]]

            w2 <- matrix(NA_integer_,nrow=nrow(x2),ncol=max(weightmap))
            if (any(wm2 != 0)) w2[,weightmap[wm2 != 0]] <- x2@weights[,wm2[wm2 != 0]]

            weights <- rbind(w1,w2)
        } 
        else weightmap <- integer(0)

        x1@data <- rbind(x1@data,x2@data)
        positions(x1) <- c(p1,p2)
        end(x1) <- end(x2)

        x1@weightmap <- weightmap
        x1@weights <- weights

        if (length(list(...)) > 0) x1 <- seriesConcat(x1,...)
        x1
    }
)

if (FALSE) {
    setGeneric("rbind")
    setMethod("rbind",signature(x1="nts",x2="nts"),
        function(x1,x2,...) seriesConcat(x1,x2,...)
        )
    setGeneric("cbind")
    setMethod("cbind",signature(x1="nts",x2="nts"),
        function(x1,x2,pos="union",...) seriesMerge(x1,x2,pos=pos,...)
        )

    setMethod("cbind",signature(x1="ANY",x2="nts"),
        function(x1,x2,...)
        {
            if (is.null(x1)) x2
            else stop("unsupported argument type x1")
        }
        )

    setMethod("cbind",signature(x1="nts",x2="ANY"),
        function(x1,x2,...)
        {
            if (is.null(x2)) x1
            else stop("unsupported argument type x2")
        }
        )
}

setGeneric("Rbind",function(x1,x2,...) standardGeneric("Rbind"))
setMethod("Rbind",signature(x1="nts",x2="nts"),
    function(x1,x2,...) seriesConcat(x1,x2,...)
    )

setGeneric("Cbind",function(x1,x2,...) standardGeneric("Cbind"))

setMethod("Cbind",signature(x1="nts",x2="nts"),
    function(x1,x2,...,pos="union")
        seriesMerge(x1,x2,...,pos=pos)
    )

setMethod("Cbind",signature(x1="ANY",x2="nts"),
    function(x1,x2,...)
    {
        if (is.null(x1)) Cbind(x2,...)
        else stop("unsupported argument type x1")
    }
    )

setMethod("Cbind",signature(x1="nts",x2="ANY"),
    function(x1,x2,...)
    {
        if (is.null(x2)) Cbind(x1,...)
        else stop("unsupported argument type x2")
    }
    )

setGeneric("average",function(x,...) standardGeneric("average"))

setMethod("average", signature=(x="nts"),
    function(x,avgperiod,outinterval,
        method="mean",use.weights=TRUE,simple=TRUE)
    {
        if (!simple) stop("average on an nts cannot do non-simple averaging. Non-simple averaging must be done on a dat object")
        cat(paste("average x=",dimnames(x)[[2]],
                ", period=",avgperiod,
                ", outinterval=",outinterval,
                ", use.weights=",use.weights,"\n",sep=""))
        t1 <- start(x)
        t2 <- end(x)
        if (method=="mean") {
            x <- .Call("ntsaverage",
                x,
                avgperiod,
                outinterval,
                use.weights,PACKAGE="eolts")
        }
        else if (method=="median") {
            x <- .Call("ntsaverage_median",
                x,
                avgperiod,
                outinterval,
                use.weights,PACKAGE="eolts")
        }
        # positions slot from above C functions is "numeric"
        positions(x) <- as(positions(x),"utime")
        start(x) <- t1
        end(x) <- t2
        x@deltat <- numeric(0)
        deltat(x) <- deltat(x)
        x
    }
    )

setMethod("show",signature(object="nts"),
    function(object)
    {
        # stolen almost verbatim from getMethod("show",sig="seriesVirtual")

        # print out the data by making a table
        if(!numRows(object@data)) {
            cat("NULL data\n")
            return()
        }
        cols <- unlist(lapply(1:numCols(object@data), function(i, x)
                subscript2d(x,,i),object@data))

        time.zone <- object@time.zone
        if (time.zone == "") time.zone <- format(object@positions[1],format="%Z")

        cols <- c(format(object@positions,format=object@time.format,time.zone=object@time.zone), cols)
        cols <- matrix(cols, ncol=numCols(object@data) + 1)
        colnames <- colIds(object@data)
        if(is.null(colnames) || !length(colnames))
            colnames <- 1:numCols(object@data)
        dimnames(cols) <- list(rep("", numRows(cols)), c(
                paste("Time(",time.zone,")",sep=""),
                colnames))
        oldClass(cols) <- "table"
        print(cols,na.print="NA",quote=FALSE)
    }
)

setGeneric("approx")
setMethod("approx",signature(x="nts"),
    function(x,y=NULL,xout,method="linear",n=NA,yleft,yright,rule=1,f=0,ties=mean)
    {
        nc <- ncol(x)
        if (inherits(xout,"nts")) xout <- as.numeric(tspar(xout))
        else if (inherits(xout,"utime")) xout <- as.numeric(xout)
        else if (inherits(xout,"timeDate")) xout <- as.numeric(utime(xout))

        dnames <- dimnames(x)[[2]]
        dunits <- x@units
        stns <- stations(x)

        # Remove time 0 from times.  We're seeing round off errors in
        # approx if they are left as seconds since Jan 1970.
        xoff <- xout[1]

        yout <- matrix(NA_real_,ncol=nc,nrow=length(xout),dimnames=list(NULL,dnames))
        if (nc > 1) {
            for (i in 1:nc) {
                if (any(xx <- is.na(x[,i]))) {
                    if (!all(xx)) {
                        xx <- x[!xx,i]
                        if (nrow(xx) == 1 && method == "constant") xx <- xx[c(1,1),]
                        if (nrow(xx) > 1)
                            yout[,i] <- approx(x=as.numeric(tspar(xx))-xoff,
                                y=xx@data,xout=xout-xoff,method=method,rule=rule,f=f)$y
                    }
                }
                else {
                    if (nrow(x) == 1 && method == "constant") x <- x[c(1,1),]
                    if (nrow(x) > 1)
                        yout[,i] <- approx(x=as.numeric(tspar(x))-xoff,
                            y=x@data[,i],xout=xout-xoff,method=method,rule=rule,f=f)$y
                }
                NULL
            }
        }
        else {
            if (any(xx <- is.na(x))) {
                if (!all(xx)) {
                    xx <- x[!xx,]
                    if (nrow(xx) == 1 && method == "constant") xx <- xx[c(1,1),]
                    if (nrow(xx) > 1)
                        yout[] <- approx(x=as.numeric(tspar(xx))-xoff,
                            y=xx@data,xout=xout-xoff,method=method,rule=rule,f=f)$y
                }
            }
            else {
                if (nrow(x) == 1 && method == "constant") x <- x[c(1,1),]
                if (nrow(x) > 1)
                    yout[] <- approx(x=as.numeric(tspar(x))-xoff,
                        y=x@data,xout=xout-xoff,method=method,rule=rule,f=f)$y
            }
        }
        as(nts(yout,xout,units=dunits,stations=stns),class(x))
    }
)

setGeneric("align",function(x,pos,how,error.how,matchtol) standardGeneric("align"))

setMethod("align",signature="nts",
    function(x,pos,how,error.how,matchtol)
    {
        if (missing(how)) how <- "NA"
        if (missing(error.how)) error.how <- "NA"

        if (!missing(matchtol)) tol <- matchtol
        else tol <- getOption("dt.eps")

        if (length(tol) == 0) tol <- deltat(pos)["trimmed.mean"] * .1

        if (inherits(pos,"nts")) pos <- positions(pos)
        # else if (inherits(pos,"utime")) pos <- as(pos,"timeDate")

        # put in time order
        xi <- order(positions(x))
        if (any(diff(xi) < 0)) {
            warning("Correcting for un-ordered time series")
            x <- x[xi,]
        }

        xi <- order(pos)
        if (any(diff(xi) < 0)) {
            warning("Correcting for un-ordered pos")
            pos <- pos[xi]
        }

        class.x <- class(x)
        stations.x <- stations(x)
        class(x) <- "timeSeries"
        if (how == "interp") {

            # output times that are within tol of input time series
            xpos <- positions(splusTimeSeries::align(x,pos,how="drop",
                    error.how="drop",matchtol=tol/86400))

            # interpolate x to pos
            x <- splusTimeSeries::align(x,pos,how=how,error.how=error.how,
                matchtol=0)

            # result time series of interpolated values which are not
            # interpolated over more than tol.
            x <- splusTimeSeries::align(x,xpos,how="drop",error.how="drop",matchtol=tol/86400)
            class(x) <- class.x
            x@weights <- matrix(0,ncol=0,nrow=0)
            x@weightmap <- integer(0)
        }
        else {
            x <- splusTimeSeries::align(x,pos,how=how,error.how=error.how,matchtol=tol/86400)
            class(x) <- class.x
            x@weights <- matrix(0,ncol=0,nrow=0)
            x@weightmap <- integer(0)
        }
        x@positions <- utime(x@positions)
        x@deltat <- numeric(0)
        deltat(x) <- deltat(x)
        x@start.position <- x@positions[1]
        x@end.position <- x@positions[length(x@positions)]
        stations(x) <- stations.x
        x
    }
)

setGeneric("replace.nas",function(x,warn) standardGeneric("replace.nas"))

setMethod("replace.nas",signature=signature("nts","logical"),
    function(x,warn=TRUE)
    {
        if (any(is.na(x@data)))
            x@data <- replace.nas(x@data,warn=warn)
        x
    }
    )

setMethod("replace.nas",signature=signature("nts","missing"),
    function(x)
    {
        replace.nas(x,T)
    }
    )

setMethod("replace.nas",signature=signature(x="matrix",warn="logical"),
    function(x,warn=FALSE)
    {
        if (any(is.na(x))) {
            nc <- ncol(x)
            nr <- nrow(x)
            dns <- dimnames(x)[[2]]     # names of data columns
            na.cnt <- t(rep(1,nr)) %*% is.na(x)
            na.cols <- (1:nc)[na.cnt > 0]
            if (is.null(dns)) dns <- paste("col",1:nc,sep="")

            if (warn) warning(paste("Missing data in",dns[na.cols],
                    ". Replacing",na.cnt[na.cols],"NAs out of",nr,"total points with previous non-NA\n",collapse=" "))
            x <- apply(x,2,replace.nas,F)
        }
        x
    }
    )

setMethod("replace.nas",signature=signature(x="numeric",warn="logical"),
    function(x,warn=FALSE)
    {
        n <- length(x)
        nax <- is.na(x)
        if (!any(nax)) return(x)

        snax <- cumsum(nax)                   # count of NA's so far

        msnax <- as.integer(!nax) * snax      # put 0's where there are NA's

        dmsnax <- c(0,diff(msnax))            # steps in NA count, after each group
        dmsnax[dmsnax < 0] <- 0

        no0 <- seq(along=dmsnax)[dmsnax > 0]

        dno0 <- diff(c(0,dmsnax[no0]))        # for each continuous group
        # of NAs, the number in each group
        xdmsnax <- rep(0,n)
        xdmsnax[no0] <- dno0                  # just after a continuous group
        # the number in the group
        cdmsnax <- cumsum(xdmsnax)

        cdmsnax[!nax] <- 0                    # number of NA's before this group

        xx <- snax - cdmsnax                  # how many earlier is a non NA
        xx[!nax] <- 0

        i <- seq(along=nax) - xx              # where the previous non-NA is
        # 0's indicate first one is NA
        # In case sequence starts with NA's
        fn <- i[!nax][1]
        i[i==0] <- fn

        y <- x[i]
        y
    }
    )

setMethod("window", signature(x="nts"),
    function(x,...)
    {
        args <- list(...)
        if (!is.null(args$type)) type <- args$type
        else type <- "hamming"

        nr <- nrow(x)

        switch(type,
            hamming = w <- .54 - .46 * cos(2. * pi * (0:(nr-1)) / (nr-1)),
            parzen = w <- 1 - abs((0:(nr-1) - .5 * (nr-1)) / .5 / (nr+1)),
            welch = w <- 1 - ((0:(nr-1) - .5 * (nr-1)) / .5 / (nr+1))^2,
            hanning = w <- .5 * (1. - cos(2. * pi * (0:(nr-1)) / (nr-1))),
            square = w <- rep(1,nr),
            stop("Unknown window type")
            )
        x <- x * w
        # "window squared and summed" normalization factor
        x@wss <- sum(w*w) * nr
        invisible(x)
    }
    )

setGeneric("trend",function(x,use.lsfit) standardGeneric("trend"))

setMethod("trend", signature(x="nts",use.lsfit="logical"),
    function(x,use.lsfit)
    {
        nc <- ncol(x)
        #
        # Using the weights attribute does not work, unless we're
        # willing to use a for loop, since lsfit only allows one
        # weight per time.
        #
        # wtm <- attr(x,"weightmap")
        # coef <- lsfit(times,matrix(x,ncol=nc),wt=wts[,wtm])$coef

        if (use.lsfit) {
            times <- as.numeric(tspar(x))
            times <- times - times[1]

            coef <- lsfit(times,x@data)$coef
            matrix(as.vector(coef),nrow=2,
                dimnames=list(dimnames(coef)[[1]],dimnames(x)[[2]]))
        }
        else {
            if (removenasfirst <- F) {
                non.nas <- (is.na(x) %*% rep(1,nc)) == 0
                x <- x[non.nas,]
            }

            nr <- nrow(x)
            times <- as.numeric(tspar(x))

            if (nr < 2) {
                a <- rep(NA_real_,nc)
                b <- rep(NA_real_,nc)
            }
            else {
                times <- times - times[1]

                # Don't use weights
                x@weights <- matrix(ncol=0,nrow=0)
                x@weightmap <- integer(0)
                mean.x <- apply(x@data,2,mean,na.rm=TRUE)

                mean.t <- rep(mean(times),nc)
                mean.t2 <- rep(mean(times*times),nc)

                mean.tx <- apply(x@data * matrix(rep(times,nc),ncol=nc),2,mean,na.rm=TRUE)

                a <- (mean.tx - mean.t * mean.x) / (mean.t2 - mean.t^2)
                b <- mean.x - a * mean.t

            }
            matrix(c(b,a),nrow=2,byrow=TRUE,
                dimnames=list(c("Intercept","X"),dimnames(x)[[2]]))
        }
    }
    )

setMethod("trend",signature(x="nts",use.lsfit="missing"),
    function(x,use.lsfit) {
        trend(x,F)
    }
    )

setGeneric("detrend",function(x,trnd) standardGeneric("detrend"))

setMethod("detrend", signature(x="nts",trnd="matrix"),
    function(x,trnd)
    {
        nr <- dim(x)[1]
        nc <- dim(x)[2]
        times <- as.numeric(tspar(x))
        times <- times - times[1]

        x <- x - ((times %o% as.vector(trnd["X",])) +
            matrix(rep(trnd["Intercept",],nr),ncol=nc,byrow=TRUE))

        # attr(x,"detrend") <- trnd
        invisible(x)
    }
    )

setMethod("detrend",signature(x="nts",trnd="missing"),
    function(x,trnd)
    {
        trnd <- trend(x)
        detrend(x,trnd)
    }
    )

setGeneric("lag",function(x) standardGeneric("lag"))

setMethod("lag",signature(x="nts"),
    function(x,...)
    {
        if (missing(seconds)) stop("must specify seconds argument to lag")

        # This function is only useful when applying different lags
        # to the columns of a multi-column time series.
        # Usually one is probably better off doing simply:
        #     tspar(x) <- tspar(x) + seconds
        #
        nr <- nrow(x)
        nc <- ncol(x)
        dt <- deltat(x)

        if (length(seconds) != nc)
            stop(paste("length(seconds) must be equal to the number of columns in time series:",nc))

        if (length(dt) > 1) {
            mindt <- dt["min"]
            maxdt <- dt["max"]
            dt <- dt[1]

            # lenient sampling checks
            if (!is.na(mindt) && !is.na(maxdt) &&
                (mindt < dt * .75 || maxdt > dt * 1.25)) {
                warning("data is not evenly sampled.  lag function will not apply lags properly")
            }
        }
        nlag <- round(seconds / dt)

        for (i in 1:nc) {
            if (abs(n <- nlag[i]) >= nr) {
                warning(paste("lag=",seconds[i]," seconds is greater than length of time series."))
                x@data[,i] <- NA_real_               # shift them outa here
            }
            else if (n > 0) {
                x@data[1:(nr-n),i]    <- x@data[(n+1):nr,i]
                x@data[(nr-n+1):nr,i] <- NA_real_
            }
            else if (n < 0) {
                x@data[(-n+1):nr,i]    <- x@data[1:(nr+n),i]
                x@data[1:(-n),i] <- NA_real_
            }
        }

        maxlag <- max(nlag[abs(nlag)<nr])
        minlag <- min(nlag[abs(nlag)<nr])

        n1 <- 1
        if (!is.na(minlag) && minlag < 0) n1 <- -minlag + 1
        n2 <- nr
        if (!is.na(maxlag) && maxlag > 0) n2 <- nr - maxlag
        x[n1:n2,]
    }
    )

setGeneric("write",function(x,...) standardGeneric("write"))

setMethod("write", signature(x="ANY"),
    function(x,file="data",ncolumns=if(is.character(x)) 1 else 5, append=FALSE,sep=" ")
    {
        base::write(x,file=file,ncolumns=ncolumns,append=append,sep=sep)
    }
    )

setMethod("write", signature(x="nts"),
    function(x,file="data",ncolumns=if(is.character(x)) 1 else 5, append=FALSE,sep=" ",...)
    {
        if (hasArg(digits)) how <- args$digits
        else digits <- 6

        ts <- format(x@positions,format=x@time.format,time.zone=x@time.zone)
        x <- signif(x,digits=digits)
        x <- apply(cbind(ts,x@data),1,paste,collapse="\t")
        write(x,file=file,append=append,sep=sep)
    }
    )

write.nts <- function(x,file="data",append=FALSE,sep=" ",digits=6)
{
    ts <- format(x@positions,format=x@time.format,time.zone=x@time.zone)
    x <- signif(x,digits=digits)
    x <- apply(cbind(ts,x@data),1,paste,collapse=sep)
    write(x,file=file,append=append,sep=sep)
}

setMethod( "summary","nts", function( object, ... )
    {
        ps <- summary( object@positions,format=object@time.format,time.zone=object@time.zone )
        # assume summary() always returns a 1-row table; want matrix
        oldClass(ps) <- NULL
        ds <- lapply( 1:numCols(object@data),
            function(i, x)
            {
                ret <- summary(subscript2d(x,,i))
                oldClass(ret) <- NULL
                ret
            }, object@data )

        lens <- sapply( ds, "numCols" )
        lens <- c( numCols(ps), lens )
        maxlen <- max( lens )

        # make all summaries into name : value character strings

        pastenames <- function( tab, newlen )
        {
            ret <- paste( format( dimnames(tab)[[2]] ), ":",  format( tab ),
                "  ", sep="" )
            length( ret ) <- newlen
            ret
        }

        allcols <- cbind( pastenames( ps, maxlen ),
            sapply( ds, pastenames, maxlen ))

        colnames <- names(object@data)
        if( is.null( colnames ))
            colnames <- 1:numCols(object@data)
        dimnames( allcols ) <- list( rep("",maxlen),
            c( "Positions", colnames ))
        oldClass( allcols ) <- "table"
        allcols
    }
)
