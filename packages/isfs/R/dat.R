# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

.isfsEnv <- new.env(parent=emptyenv())

setClass("dat",contains="nts")

dat <- function(what,derived=TRUE,cache=getOption("dcache"),
    avg,smooth=FALSE,...)
{

    if (inherits(what,"nts")) return(as(what,"dat"))

    # Averaging is controlled by two parameters:
    # 1. the "avg" dpar parameter:
    #    dpar(avg=c(nonsimple.period,simple.period))
    #    or
    #    dpar(avg=simple.period)
    # 2. the avg=TRUE/FALSE parameter to this dat function.
    # 
    # If dpar("avg") is non-zero, and avg=TRUE in the dat call,
    # then averaging is done.
    #
    # The avg argument to dat defaults to FALSE, unless this is the
    # top call to dat in which case it defaults to TRUE.
    # We detect the top call to dat by checking
    # sys.nframe, and storing/removing its value on .isfsEnv.
    # 
    # If "what" is a derived variable, in which case this
    # dat will call another dat, then avg is not
    # passed to the derived dat (defaulting to FALSE), but this
    # function will average the result.
    #
    # This scheme results in derived variables being computed from
    # non-reduced data, then averaged after the derivation:
    # i.e. lazy, at the last minute, averaging.
    # To override this behavior within a dat function specify avg=TRUE 
    # in the dat calls to the input variables:
    # For example, this dat function does the default:
    #     dat.X <- function(what,...) {
    #       dat("Y") ^ 2
    #     }
    # and results in  X <- average(Y^2), if dpar(avg=nonzero)
    #
    # This function:
    #     dat.X <- function(what,...) {
    #       dat("Y",avg=TRUE,smooth=TRUE) ^ 2
    #     }
    # would result in X <- (average(Y))^2
    #
    # So each dat function must consider if it wants to work with
    # averaged inputs or average the result.
    #
    # The same treatment applies to the smooth argument to dat
    # except that it always defaults to FALSE, even in the top
    # frame, i.e. the user must specify it if they want smoothing.
    # However dat functions can set it to true in their calls to
    # dat to get input variables (typically used to smooth turbulent
    # input variables).
    #
    # Like averaging, smoothing is accomplished by computing means.
    # However specifying smooth=TRUE, and a smooth time period in
    # dpar results in simple running averages of the selected data
    # variables, with an output interval equal to the current deltaT
    # of the data.
    # Specifying avg=TRUE and the avg time periods in dpar results in
    # more complicated processing, where for example, averages of
    # second moments will include the deviation of the means.
    #
    # Caching and smoothing/averaging: save data on cache before
    # smoothing or averaging.  Therefore cached data is
    # not smoothed.
    #

    nframe <- sys.nframe()
    if (!exists(".dat.topframe",envir=.isfsEnv)) {
        assign(".dat.topframe",nframe,envir=.isfsEnv)
        on.exit(remove(".dat.topframe",envir=.isfsEnv))
    }

    if (missing(avg)) {
        if (nframe == get(".dat.topframe",envir=.isfsEnv)) avg <- TRUE
        else avg <- FALSE
    }
    avgper <- dpar("avg")
    if (is.null(avgper) || identical(avgper,NA_real_) || identical(avgper,FALSE))
        avg <- FALSE
    simple.avg <- FALSE
    if (avg && length(avgper) == 1) {
        avgper <- c(avgper,avgper)
        simple.avg <- TRUE
    }

    smoothper <- dpar("smooth")
    if (is.null(smoothper) || identical(smoothper,NA_real_) ||
        identical(smoothper,FALSE)) smooth <- FALSE

    if (derived) {
        # if passed more than one name, returns a list
        if (length(what) > 1) {
            names(what) <- what
            # average/smooth these variables before the Cbind
            x <- lapply(what,
                function(what,...) {
                    x <- dat(what,derived=derived,cache=cache,...)
                    if (smooth || avg)
                        x <- smooth_avg_dat(x,smooth,smoothper,avg,simple.avg,avgper)
                    x
                },...)
            x <- x[!unlist(lapply(x,is.null))]
            if (length(x) == 0) return(NULL)
            else if (length(x) == 1) x <- x[[1]]
            else if (length(x) == 2) x <- Cbind(x[[1]],x[[2]])
            else  {
                xx <- Cbind(x[[1]],x[[2]])
                for (i in seq(from=3,to=length(x))) xx <- Cbind(xx,x[[i]])
                x <- xx
            }
            return(x)
        }

        # Look for a dat function  dat.what
        datFuncName <- paste("dat",what,sep=".")
        if (exists(datFuncName,mode="function")) {
            # browser()
            f <- get(datFuncName,mode="function",pos=1,inherits=TRUE)
            x <- f(what=what,derived=derived,cache=cache,...)
            if (inherits(x,"nts")) {
                x <- select(x,stns=dpar("stns"),hts=dpar("hts"),sfxs=dpar("sfxs"),
                    sites=dpar("sites"))
                if (smooth || avg)
                    x <- smooth_avg_dat(x,smooth,smoothper,avg,simple.avg,avgper)
            }
            return(x)
        }
        else {
            # This covers the situation if what is something like
            # "spd.a.b.c".  We want to execute dat.spd, which may
            # return  spd.x, spd.y.z and spd.a.b.c. Then we look
            # for the "a.b.c" suffix in the dimnames and return the
            # matching columns
            nw <- nwords(what,sep=".")
            iw <- nw - 1
            while (iw > 0) {
                datFuncName <- paste("dat",words(what,1,iw),sep=".")
                if (exists(datFuncName,mode="function")) {
                    f <- get(datFuncName,mode="function",pos=1,inherits=TRUE)
                    x <- f(what=what,derived=derived,cache=cache,...)

                    if (inherits(x,"nts")) {
                        sfx <- words(what,2,sep=".")
                        dn <- dimnames(x)[[2]]
                        mx <- words(dn,2,nw) == sfx
                        if (!any(mx)) {
                            warning(
                                paste("No dimnames matching",sfx,"in",
                                    paste(dn,collapse=",")))
                            return(NULL)
                        }
                        x <- x[,mx]
                        x <- select(x,stns=dpar("stns"),hts=dpar("hts"),sfxs=dpar("sfxs"),
                            sites=dpar("sites"))
                        if (smooth || avg)
                            x <- smooth_avg_dat(x,smooth,smoothper,avg,simple.avg,avgper)
                    }
                    return(x)
                }
                iw <- iw - 1
            }
        }
    }

    if (length(what) == 1) {
        if (check_cache(what)) {
            if (avg || smooth) return(smooth_avg_dat(get_cache_object(what),
                    smooth,smoothper,avg,simple.avg,avgper))
        else return(get_cache_object(what))
        }
    }

    # sort names alphabetically.  This should help avoid problems
    # of mismatching data:  x <- dat("X") * dat("Y"), where
    # because of the way things are stored in the netcdf file:
    # X could be read as  X.a, X.b and Y in the opposite order: Y.b Y.a

    dnames <- sort(lookup(what,verbose=FALSE))
    if ((nnames <- length(dnames)) == 0) return(NULL)

    if (nnames == 1) {
        if (check_cache(dnames)) {
            if (smooth || avg) return(smooth_avg_dat(get_cache_object(dnames),
                    smooth,smoothper,avg,simple.avg,avgper))
            else return(get_cache_object(dnames))
        }
    }

    T1 <- dpar("start")
    if (is.null(T1)) stop("dpar(\"start\") has not been set")
    T2 <- dpar("end")
    stns <- dpar("stns")

    lenfile <- dpar("lenfile")
    if(is.null(lenfile)) iod <- netcdf()
    else iod <- netcdf(lenfile=lenfile)

    x <- readts(iod,variables=dnames,start=T1,end=T2,stns=stns)
    close(iod)

    x <- as(x,"dat")

    # browser()
    x <- select(x,stns=stns,hts=dpar("hts"),sfxs=dpar("sfxs"),sites=dpar("sites"))

    # if (!is.null(dpar("chksum")) && dpar("chksum")) x <- chksumck(x)

    if (length(what) == 1 && !is.null(x) &&
        (is.null(cache) || cache)) cache_object(what,x)

    if (smooth || avg)
        x <- smooth_avg_dat(x,smooth,smoothper,avg,simple.avg,avgper)

    x
}

datVar <- function()
{
    # When called from within a dat.x function, returns "x"

    # parent.frame(2) is the frame of dat(). Within dat(),
    # datFuncName is the name of the function that is
    # being called.
    env <- parent.frame(2)
    if (exists("datFuncName",envir=env)) {
        func <- get("datFuncName",env)
    }
    else {
        # looks like dat.X is being called directly
        func <- strsplit(deparse(sys.call(-1)),"(",fixed=TRUE)[[1]][1]
    }
    sub("dat.","",func)
}

smooth_avg_dat <- function(x,smooth,smoothper,avg,simple.avg,avgper)
{
    if (!inherits(x,"dat")) return(x)
    dt <- deltat(x)[1]
    if (smooth && !is.null(smoothper) && !is.na(dt) && dt < smoothper)
        x <- average(x,smoothper,dt,method="mean",simple=TRUE)
    if (avg && !is.na(dt) && dt < avgper[2]) {
        dns <- dimnames(x)[[2]]
        # do the initial simple=FALSE averaging on second moments
        if (!simple.avg && any(regexpr("'",dns) != -1) && dt < avgper[1]) {
            x <- average(x,avgper[1],avgper[1],method="mean",simple=FALSE)
            dt <- deltat(x)[1]
        }
        if (!is.na(dt) && dt < avgper[2])
            x <- average(x,avgper[2],avgper[2],method="mean",simple=TRUE)
    }
    x
}

derived <- function()
{
    dname <- "derivedISFSFunctions"
    if (check_cache(dname)) return(get_cache_object(dname))

    attchd <- search()
    # look through attached packages and objects for "package.isfs" and
    # one containing "file:" and "projects".
    mtch <- (grepl("file:",attchd,fixed=TRUE) & grepl("projects",attchd,fixed=TRUE)) |
    grepl("package:isfs",attchd,fixed=TRUE)

    x <- NULL
    for (pos in seq(along=mtch)[mtch]) {
        dfunc <- objects(pattern="^dat\\.",pos=pos)
        for (f in dfunc)
            if (exists(f,mode="function")) x <- c(x,words(f,sep=".",2))
    }

    x <- unique(x)
    ignore <- c("default","dat","variables","derived","chksumOK")
    lf <- match(x,ignore,nomatch=0) == 0
    x <- x[lf]
    cache_object(dname,x)
    x
}

setGeneric("select",function(x,...) standardGeneric("select"))

setMethod("select",signature(x="dat"),
    function(x,...)
    {
        dots <- list(...)
        if (hasArg(stns) && length((stns <- dots$stns)) > 0) {
            xstns <- stations(x)
            if (length(xstns) == ncol(x)) {
                sm <- match(xstns,stns,nomatch=0)
                sm <- seq(along=sm)[sm!=0][sort.list(sm[sm!=0])]
                if (length(sm) > 0) x <- x[,sm]
                else {
                    warning(paste(paste(dimnames(x)[[2]],collapse=","),
                            ": no selected stations (",paste(stns,collapse=","),
                            ") found in stations(x)=c(",paste(xstns,collapse=","),
                            "). select(x,stns) returning NULL"))
                    return(NULL)
                }
            }
        }
        if (hasArg(hts) && length((hts <- dots$hts)) > 0) {
            # Select heights (exactly)
            hts <- dots$hts
            if (!is.null(xhts <- heights(x)) && length(xhts) == ncol(x) && !all(is.na(xhts)) ) {

                # An exact match is susceptible to precision errors.
                # Match a height if it is within 1.e-5 meters

                # this returns a matrix of the function applied to all pairings
                # of an elements from xhts and hts,
                # with nrows=length(xhts),ncols=length(hts)
                hdiff <- outer(xhts,hts,function(x,y) abs(x-y))
                hdiff <- !is.na(hdiff) & hdiff < 1.e-5
                htsmatch <-
                    matrix(1:length(hts),nrow=length(xhts),ncol=length(hts),byrow=TRUE)[hdiff]
                xhtsmatch <-
                    matrix(1:length(xhts),nrow=length(xhts),ncol=length(hts))[hdiff]

                # exact match.  The only reason to do this is to get the
                # NA matches.
                sm <- match(xhts,hts,nomatch=0)

                # Add the approximate matching elements to the exact match
                sm[xhtsmatch] <- htsmatch

                # return heights in same sequence as hts
                sm <- seq(along=sm)[sm!=0][sort.list(sm[sm!=0])]
                if (length(sm) > 0) x <- x[,sm]
                else {
                    warning(paste(paste(dimnames(x)[[2]],collapse=","),
                            ": no selected heights (",paste(hts,collapse=","),
                            ") found in heights(x)=c(",paste(xhts,collapse=","),
                            "). select(x,hts) returning NULL"))
                    return(NULL)
                }
            }
        }
        if (hasArg(sfxs) && length((sfxs <- dots$sfxs)) > 0) {
            # Select sfxs
            # remove leading dots
            ldots <- substring(sfxs,1,1) == "."
            if (any(ldots)) sfxs[ldots] <- substring(sfxs[ldots],2)

            ns <- length(sfxs)
            nws <- nwords(sfxs,sep=".")

            dn <- dimnames(x)[[2]]
            nd <- length(dn)
            nwd <- nwords(dn,sep=".")

            nws <- rep(nws,rep(nd,ns))
            ssfxs <- rep(sfxs,rep(nd,ns))

            dn <- rep(dn,ns)
            nwd <- rep(nwd,ns)

            dn <- words(dn,nwd-nws+1,nwd)

            sm <- matrix((match(dn,ssfxs,nomatch=0) - 1) %/% ns + 1,nrow=nd)
            sm <- apply(sm,1,function(x)x[x!=0][1])

            # return suffixes in same sequence as sfxs
            sm <- seq(along=sm)[!is.na(sm)][sort.list(sm[!is.na(sm)])]
            if (length(sm) > 0) x <- x[,sm]
            else {
                warning(paste("no selected suffixes (",paste(sfxs,collapse=","),
                        ") found in dimnames(x)[[2]]=c(",paste(dimnames(x)[[2]],collapse=","),
                        "). select(x,sfxs) returning NULL"))
                return(NULL)
            }
        }
        if (hasArg(sites) && length((sites <- dots$sites)) > 0) {
            # Select sites for station 0 variables
            dsites <- sites(dimnames(x)[[2]])
            stns <- stations(x)

            sm <- match(dsites,sites)
            sm[stns > 0] <- NA  # remove wrong matches if a variable is from a station
            if (any(!is.na(sm) | (stns > 0))) {
                # variables are already in station order
                # return station 0 variables in same sequence as sites
                sl <- sort.list(sm,na.last=TRUE)
                sl <- sl[!is.na(sort(sm,na.last=TRUE))]
                sl <- c(sl,(1:ncol(x))[stns > 0])
                x <- x[,sl]
            }
            else {
                warning(paste("no selected sites (",paste(sites,collapse=","),
                        ") found in dimnames(x)[[2]]=c(",paste(dimnames(x)[[2]],collapse=","),
                        "). select(x,sites) returning NULL"))
                x <- NULL
            }
        }
        x
    }
    )

setGeneric("suffixes",function(x,...) standardGeneric("suffixes"))

setMethod("suffixes",signature="dat",
    function(x,first=2,leadch=".")
    {
        # function to strip words from front of dimnames(x)[[2]]
        # first is first word to save, e.g. for a dimname of "T.hygt.1.5m", 
        # suffixes returns ".hygt.1.5m" (first=2)
        # leadch is the character prepended to result
        x <- dimnames(x)[[2]]
        suffixes(x,first=first,leadch=leadch)
    }
    )
setMethod("suffixes",signature="character",
    function(x,first=2,leadch=".")
    {
        if (length(first) == 1) first <- rep(first,length(x))
        x <- words(x,first, nwords(x,sep="."),sep=".")
        x[x!=""] <- paste(leadch, x[x!=""], sep="")
        x
    }
    )

expand <- function(x,y,first=2)
{
    paste(x,suffixes(y,first=first),sep="")
}

setGeneric("clip", function(x1,...) standardGeneric("clip"))

setMethod("clip",signature(x1="dat"),
    function(x1,...)
    {
        for (i in 1:ncol(x1)) {
            cn <- colnames(x1[,i])
            if (!is.null(cl <- clip(cn)) && (is.null(cl$clip) || cl$clip)) {
                cmin <- cl$min
                cmax <- cl$max
                clip.x <- x1@data[,i] < cmin & !is.na(x1@data[,i])
                if (any(clip.x)) x1@data[clip.x,i] <- NA_real_
                clip.x <- x1@data[,i] > cmax & !is.na(x1@data[,i])
                if (any(clip.x)) x1@data[clip.x,i] <- NA_real_
            }
        }
        x1
    }
    )


setMethod("clip",signature(x1="character"),
    function(x1,...)
    {
        # Set/Get global clip limits
        # cliplimits <- function(vname,clip,cmin,cmax)
        dots <- list(...)

        if (!exists(".clip.limits",envir=.isfsEnv)) clip.limits <- list()
        else clip.limits <- get(".clip.limits",envir=.isfsEnv)

        if (nargs() == 1) {

            # list indexing [[]] operator will match partial strings
            # we want exact matches over the number of words in x
            # If x is a.b.c.d, this code will first try to find clip limits for
            # a.b.c.d, then a.b.c, a.b, then a, returning NULL if a is not found

            if (length(x1) > 1) {
                names(x) <- x1
                cl <- lapply(x1,clip)
            }
            else {
                if (length(clip.limits) == 0) return(NULL)
                nw <- nwords(x1,sep=".")
                lc <- length(clip.limits)
                cl <- F
                for (iw in nw:1) {
                    cl <- words(names(clip.limits),rep(1,lc),rep(iw,lc),sep=".")
                    cl <- cl == words(x1,1,iw,sep=".")
                    if (any(cl)) break
                }
                if (!any(cl)) return(NULL)
                cl <- clip.limits[cl]
                if (length(cl) > 1)
                    warning(paste("multiple clip limits found for \"",x1,"\": \"", paste(names(cl),collapse="\",\""),"\". Using \"",names(cl[1]),"\"",sep=""))
                cl <- cl[[1]]

            }
            return(cl)
        }

        if (hasArg(clip)) clip <- dots$clip
        else clip <- dots[[1]]

        if (hasArg(cmin)) cmin <- dots$cmin
        else if (length(dots) > 1) cmin <- dots[[2]]
        else cmin <- NULL

        if (hasArg(cmax)) cmax <- dots$cmax
        else if (length(dots) > 2) cmax <- dots[[3]]
        else cmax <- NULL

        oldclip <- clip.limits[[x1]]

        if (is.logical(clip)) {
            if (!is.null(cmin) && length(cmin)==2 && is.null(cmax)) {
                cmax <- cmin[2]
                cmin <- cmin[1]
            }
            else if (is.null(cmin) || is.null(cmax)) {
                if (!is.null(oldclip)) {
                    if (is.null(cmin)) cmin <- oldclip$min
                    if (is.null(cmax)) cmax <- oldclip$max
                } else {
                    if (clip) stop("clip limits must be specified when clip==TRUE")
                    cmin <- cmax <- NULL
                }
            }
        }
        else if (is.numeric(clip)) {
            if (length(clip) == 2) {
                cmax <- clip[2]
                cmin <- clip[1]
                clip <- T
            }
            else if (!is.null(cmin)) {
                cmax <- cmin
                cmin <- clip
                clip <- T
            }
        }

        clip.limits[[x1]] <- list(min=cmin,max=cmax,clip=clip)

        assign(".clip.limits",clip.limits,envir=.isfsEnv)
        oldclip
    }
    )

setMethod("clip",signature(x1="missing"),
    function(x1,...)
    {
        if (!exists(".clip.limits",envir=.isfsEnv)) list()
        else get(".clip.limits",envir=.isfsEnv)
    }
    )

setGeneric("heights",function(x) standardGeneric("heights"))

setMethod("heights","ANY",
    function(x)
    {
        if (is.matrix(x)) x <- dimnames(x)[[2]]
        if (!is.character(x)) stop("x is not character")

        sapply(x,
            function(x)
            {
                m <- regexec("\\.([0-9]+\\.?[0-9]*)(c?m)",x)[[1]]
                if (length(m) < 3 || m[1] == -1) return(NA_real_)
                ht <- as.numeric(substr(x,m[2],m[2]+attr(m,"match.length")[2]-1))
                mc <- substr(x,m[3],m[3]+attr(m,"match.length")[3]-1)
                # if height is in cm, treat it as a depth: convert to meters, flip sign
                if (mc == "cm") ht <- -ht / 100.
                ht
            }
            )
    }
    )

setMethod("heights","dat",
    function(x)
    {
        heights(dimnames(x)[[2]])
    }
    )

setGeneric("sites",function(x) standardGeneric("sites"))

setMethod("sites","ANY",
    function(x)
    {
        if (is.matrix(x)) x <- dimnames(x)[[2]]
        if (!is.character(x)) stop("x is not character")
        sapply(x,
            function(x)
            {
                m <- regexec("(\\.[0-9]+\\.?[0-9]*(c?m))?(\\.[^.]+)?$",x)[[1]]
                if (length(m) < 4 || m[1] == -1) return("")
                substr(x,m[4]+1,m[4]+attr(m,"match.length")[4]-1)
            }
            )
    }
    )

setMethod("sites","dat",
    function(x)
    {
        sites(dimnames(x)[[2]])
    }
    )

setReplaceMethod("dimnames", signature(x="dat",value="list"),
    function(x,value)
    {
        dimnames(as(x,"nts")) <- value
        x
    }
    )

setGeneric("crack",function(x,tbreak)
    standardGeneric("crack"))

setMethod("crack",signature(x="dat"),
    function(x,tbreak=900)
    {
        ts <- positions(x)
        tdiff <- diff(ts)
        brks <- seq(along=tdiff)[tdiff > tbreak]
        brks2 <- brks

        insert.rec <- x[1,]
        insert.rec[1,] <- NA_real_

        for (i in seq(along=brks)) {
            j <- brks[i]
            k <- brks2[i]
            positions(insert.rec) <- ts[j] + tbreak / 2
            x <- Rbind(x[1:k,],insert.rec,x[(k+1):nrow(x),])
            brks2 <- brks2 + 1
            T
        }
        x
    }
    )

setMethod("seriesConcat",signature(x1="dat",x2="dat"),
    function(x1,x2,...)
    {
        class(x1) <- "nts"
        class(x2) <- "nts"
        x1 <- seriesConcat(x1,x2,...)
        class(x1) <- "dat"
        x1
    }
    )

setMethod("average", signature="dat",
    function(x,...,simple=TRUE)
    {
        cat("average x=",dimnames(x)[[2]]," args=",unlist(list(...)),"simple=",simple,"\n")

        # simple <- list(...)$simple
        # if (is.null(simple)) simple <- T

        if (simple) {
            class(x) <- "nts"
            x <- average(x,...,simple=simple)
        }
        else {

            dns <- dimnames(x)[[2]]

            # Preallocate result time series
            # Create dummy time series, with no NAs,
            # average this to find out the maximum length
            # of the results
            xa <- x[,1]
            xa[] <- 1
            class(xa) <- "nts"
            xa <- average(xa,...,simple=TRUE)
            wts <- matrix(0,ncol=ncol(x),nrow=nrow(xa))
            xa <- dat(nts(matrix(NA_real_,ncol=ncol(x),nrow=nrow(xa),dimnames=list(NULL,dns)),
                    tspar(xa), units=x@units,weightmap=1:ncol(x),
                    weights=wts,stations=stations(x)))

            for (dn in unique(dns)) {
                xcol <- !is.na(match(dns,dn))
                dnames <- components(dn)
                nw <- length(dnames)

                # First-order moment, just take normal average
                if (nw == 1) {
                    xx <- x[,xcol]
                    class(xx) <- "nts"
                    av <- average(xx,...,simple=TRUE)
                    xa[,xcol] <- av
                    # I believe the above assignment does this
                    # wts[,xcol] <- av@weights
                    # xa@weights <- wts
                }
                else if (nw == 2) {
                    xx <- x[,xcol]

                    # Create a time series containing 1.0 when covariance exists,
                    # otherwise NA.
                    # Multiply the individual component means by this time series before
                    # re-computing the covariance. This corrects errors if there
                    # is different data coverage in the components.
                    fx <- xx
                    fx@data[!is.na(fx@data)] <- 1.0

                    # Second-order moment, add in variance of first-order moments

                    # If this average is being called from dat, then
                    # in the following dat call, avg will default to F.
                    if (any(mx <- (dns == dnames[1]))) x1 <- x[,mx]
                    else x1 <- dat(dnames[1],avg=FALSE,smooth=FALSE)
                    x1 <- conform(x1,xx)

                    wm <- x1@weightmap
                    if (length(wm) > 0) {
                        xw <- nts(x1@weights,positions(x1)) * fx
                        naw <- is.na(xw@data)
                        if (any(naw)) xw@data[naw] <- 0
                    }
                    x1 <- x1 * fx
                    if (length(wm) > 0) {
                        # in above operation, if weights differ, they are
                        # removed. Restore them
                        x1@weights <- xw@data
                        x1@weightmap <- wm
                    }

                    class(x1) <- "nts"

                    if (any(mx <- (dns == dnames[2]))) x2 <- x[,mx]
                    else x2 <- dat(dnames[2],avg=FALSE,smooth=FALSE)
                    x2 <- conform(x2,xx)

                    wm <- x2@weightmap
                    if (length(wm) > 0) {
                        xw <- nts(x2@weights,positions(x2)) * fx
                        naw <- is.na(xw@data)
                        if (any(naw)) xw@data[naw] <- 0
                    }
                    x2 <- x2 * fx
                    if (length(wm) > 0) {
                        # in above operation, if weights differ, they are
                        # removed. Restore them
                        x2@weights <- xw@data
                        x2@weightmap <- wm
                    }

                    class(x2) <- "nts"

                    if (length(xx@weightmap) > 0) {
                        naw <- is.na(xx@weights * fx@data)
                        if (any(naw)) xx@weights[naw] <- 0
                    }

                    weights_ok <- TRUE
                    if (length(xx@weightmap) > 0 && length(x1@weightmap) > 0 &&
                        !identical(as.numeric(xx@weights[,xx@weightmap]),
                            as.numeric(x1@weights[,x1@weightmap]))) {
                        warning(paste("average: weights of",dimnames(xx)[[2]],"differ from",dimnames(x1)[[2]]))
                        weights_ok <- FALSE
                    }
                    else if (length(xx@weightmap) > 0 && length(x2@weightmap) > 0 &&
                        !identical(as.numeric(xx@weights[,xx@weightmap]),
                            as.numeric(x2@weights[,x2@weightmap]))) {
                        warning(paste("average: weights of",dimnames(xx)[[2]],"differ from",dimnames(x2)[[2]]))
                        weights_ok <- FALSE
                    }
                    if (!weights_ok) {
                        bad <- (xx@weights != x1@weights) | (xx@weights != x2@weights)
                        warning(paste0(sum(bad)," (",round(sum(bad)/length(bad)*100,3),
                                "%) samples with differing weights were set to NA"))
                        x1[bad] <- NA_real_
                        x1@weights[bad] <- 0
                        x2[bad] <- NA_real_
                        x2@weights[bad] <- 0
                        xx[bad] <- NA_real_
                        xx@weights[bad] <- 0
                    }

                    class(xx) <- "nts"

                    av <- average(xx + x1 * x2,...,simple=TRUE) - 
                        average(x1,...,simple=TRUE) * average(x2,...,simple=TRUE)
                    xa[,xcol] <- av
                }
                else if (nw == 3) {
                    # Third-order moment, this gets complicated!
                    xx <- x[,xcol]
                    fx <- xx
                    fx@data[!is.na(fx@data)] <- 1.0

                    if (any(mx <- (dns == dnames[1]))) x1 <- x[,mx]
                    else x1 <- dat(dnames[1],avg=FALSE,smooth=FALSE)
                    x1 <- conform(x1,xx)

                    wm <- x1@weightmap
                    if (length(wm) > 0) {
                        xw <- nts(x1@weights,positions(x1)) * fx
                        naw <- is.na(xw@data)
                        if (any(naw)) xw@data[naw] <- 0
                    }
                    x1 <- x1 * fx
                    if (length(wm) > 0) {
                        x1@weights <- xw@data
                        x1@weightmap <- wm
                    }
                    class(x1) <- "nts"

                    if (any(mx <- (dns == dnames[2]))) x2 <- x[,mx]
                    else x2 <- dat(dnames[2],avg=FALSE,smooth=FALSE)
                    x2 <- conform(x2,xx)

                    wm <- x2@weightmap
                    if (length(wm) > 0) {
                        xw <- nts(x2@weights,positions(x2)) * fx
                        naw <- is.na(xw@data)
                        if (any(naw)) xw@data[naw] <- 0
                    }
                    x2 <- x2 * fx

                    if (length(wm) > 0) {
                        x2@weights <- xw@data
                        x2@weightmap <- wm
                    }

                    class(x2) <- "nts"

                    if (any(mx <- (dns == dnames[3]))) x3 <- x[,mx]
                    else x3 <- dat(dnames[3],avg=FALSE,smooth=FALSE)
                    x3 <- conform(x3,xx)

                    wm <- x3@weightmap
                    if (length(wm) > 0) {
                        xw <- nts(x3@weights,positions(x3)) * fx
                        naw <- is.na(xw@data)
                        if (any(naw)) xw@data[naw] <- 0
                    }
                    x3 <- x3 * fx
                    if (length(wm) > 0) {
                        x3@weights <- xw@data
                        x3@weightmap <- wm
                    }
                    class(x3) <- "nts"

                    if (length(xx@weightmap) > 0) {
                        naw <- is.na(xx@weights * fx@data)
                        if (any(naw)) xx@weights[naw] <- 0
                    }

                    # variable names, 'w' from 'w.99m.moon'
                    vnames <- words(dnames,first=1,last=1,sep='.')
                    # rest of variable names: '99m.moon'
                    vsuffixes <- unique(words(dnames,first=2,sep='.'))

                    if (length(vsuffixes) != 1) {
                        stop(paste("multiple suffixes of variables=",paste(vsuffixes,collapse=','),"Cannot compute 3rd order moments",sep=" "))
                    }

                    dx1x2 <- paste(vnames[1],"'",vnames[2],"'",".",vsuffixes,sep="")

                    x1x2 <- conform(dat(dx1x2),xx)
                    wm <- x1x2@weightmap
                    xw <- nts(x1x2@weights,positions(x1x2)) * fx
                    naw <- is.na(xw@data)
                    if (any(naw)) xw@data[naw] <- 0
                    x1x2 <- x1x2 * fx
                    x1x2@weights <- xw@data
                    x1x2@weightmap <- wm
                    class(x1x2) <- "nts"

                    dx1x3 <- paste(vnames[1],"'",vnames[3],"'",".",vsuffixes,sep="")
                    x1x3 <- conform(dat(dx1x3),xx)
                    wm <- x1x3@weightmap
                    xw <- nts(x1x3@weights,positions(x1x3)) * fx
                    naw <- is.na(xw@data)
                    if (any(naw)) xw@data[naw] <- 0
                    x1x3 <- x1x3 * fx
                    x1x3@weights <- xw@data
                    x1x3@weightmap <- wm
                    class(x1x3) <- "nts"

                    dx2x3 <- paste(vnames[2],"'",vnames[3],"'",".",vsuffixes,sep="")
                    x2x3 <- conform(dat(dx2x3),xx)
                    wm <- x2x3@weightmap
                    xw <- nts(x2x3@weights,positions(x2x3)) * fx
                    naw <- is.na(xw@data)
                    if (any(naw)) xw@data[naw] <- 0
                    x2x3 <- x2x3 * fx
                    x2x3@weights <- xw@data
                    x2x3@weightmap <- wm
                    class(x2x3) <- "nts"

                    class(xx) <- "nts"

                    if (!identical(as.numeric(xx@weights),as.numeric(x1@weights))) {
                        warning(paste("average: weights of",dimnames(xx)[[2]],"differ from",dimnames(x1)[[2]]))
                        weights_ok <- FALSE
                    }
                    else if (!identical(as.numeric(xx@weights),as.numeric(x2@weights))) {
                        warning(paste("average: weights of",dimnames(xx)[[2]],"differ from",dimnames(x2)[[2]]))
                        weights_ok <- FALSE
                    }
                    else if (!identical(as.numeric(xx@weights),as.numeric(x3@weights))) {
                        warning(paste("average: weights of",dimnames(xx)[[2]],"differ from",dimnames(x3)[[2]]))
                        weights_ok <- FALSE
                    }

                    if (!weights_ok) {
                        bad <- (xx@weights != x1@weights) | (xx@weights != x2@weights) |
                                (xx@weights != x3@weights)
                        warning(paste0(sum(bad)," (",round(sum(bad)/length(bad)*100,3),
                                "%) samples with differing weights were set to NA"))
                        x1[bad] <- NA_real_
                        x1@weights[bad] <- 0
                        x2[bad] <- NA_real_
                        x2@weights[bad] <- 0
                        x3[bad] <- NA_real_
                        x3@weights[bad] <- 0
                        x1x2[bad] <- NA_real_
                        x1x2@weights[bad] <- 0
                        x1x3[bad] <- NA_real_
                        x1x3@weights[bad] <- 0
                        x2x3[bad] <- NA_real_
                        x2x3@weights[bad] <- 0
                        xx[bad] <- NA_real_
                        xx@weights[bad] <- 0
                    }
                    x1a <- average(x1,...,simple=TRUE)
                    x2a <- average(x2,...,simple=TRUE)
                    x3a <- average(x3,...,simple=TRUE)

                    # browser()

                    av <- average(xx + x1*x2x3 + x2*x1x3 + x3*x1x2 + x1*x2*x3,...,simple=TRUE) - 
                        x1a * average(x2x3,...,simple=TRUE) -
                        x2a * average(x1x3,...,simple=TRUE) -
                        x3a * average(x1x2,...,simple=TRUE) -
                        x1a * x2a * x3a
                    xa[,xcol] <- av
                    # wts[,xcol] <- attr(av,"weights")
                    # attr(xa,"weights") <- wts
                }
                else 
                    stop("Give me a break! I can't yet average higher than 3rd order moments!\n")
            }

            x <- xa
        }
        class(x) <- "dat"
        x
    }
)

other_dat_func <- function(what,whine=TRUE)
{
    nd <- length(search())

    name <- paste("dat",what,sep=".")

    while (name != "dat") {
        for (i in 1:nd)
            if (exists(name,where=i,inherits=FALSE))
                for (j in (i+1):nd) if (exists(name,where=j)) return(get(name,pos=j))

        nw <- nwords(name,sep=".")
        if (nw == 1) break
        name <- words(name,1,nw-1,sep=".")
    }
    if (whine)
        stop(paste("An alternate version of",paste("dat",what,sep="."),"was not found on search list"))
    NULL
}

d_by_dt <- function(x,dtmax=NULL,lag=2,differences=1,time=0)
{
    # Compute time derivative (per second) of a time series:
    #    dx/dt(i) <- (x(i+1) - x(i-1)) / (t(i+1) - t(i-1))
    #
    nr <- nrow(x)
    nc <- ncol(x)
    stns <- stations(x)

    # arbitrary
    if (is.null(dtmax)) dtmax <- deltat(x)[1] * 10

    ts <- as.numeric(positions(x))

    dx <- diff(x@data,lag=lag)
    dt <- diff(ts,lag=lag)

    # result of diff is lag points shorter
    if (time == 0) {
        # middle times
        mx <- -((nr-lag+1):nr)
        ts <- ts[mx] + dt / 2
    }
    else if (time == -1) {
        # first times
        mx <- -((nr-lag+1):nr)
        ts <- ts[mx]
    }
    else if (time == 1) {
        # end times
        mx <- -(1:lag)
        ts <- ts[mx]
    }

    keep <- dt > 0 & dt < dtmax
    dx <- dx[keep,]
    dt <- dt[keep]
    ts <- ts[keep]

    dunits <- sapply(x@units,function(x) {
        if (x=="") "s-1" 
        else paste(x,"s-1")
    })
    dnames <- paste0("d_", words(colnames(x),1,1), "_by_dt",
        sapply(words(colnames(x),2),function(x) {
            if (x == "") x
            else paste0(".",x)
    }))

    x <- dat(nts(matrix(as.vector(dx/dt),ncol=nc,dimnames=list(NULL,dnames)),
            as(ts,"utime"),units=dunits,stations=stns))
    #
    # recursively call again if differences > 1
    if (differences > 1) x <- d_by_dt(x,dtmax=dtmax,lag=lag,differences=differences-1,time=time)
    x
}

d.by.dt <- function(x,dtmax=NULL,lag=2,differences=1,time=0)
    # old name for this function
{
    d_by_dt(x,dtmax=dtmax,lag=lag,differences=differences,time=time)
}
