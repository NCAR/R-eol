# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolts" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

# Set function called from dpar to be used when any of the parameters
# listed in .dparParams are set.
setDpar <- function(parm,vals)
{
    if (length(parm) != 1) stop("parm must be of length 1")
    pname <- names(parm)
    pval <- parm[[1]]

    vals[[pname]] <- pval

    # If any time parameter is specified, adjust others
    timeNames <- c("lenday","lenhr","lenmin","lensec","start","end")

    mx <- match(pname,timeNames,nomatch=0)

    if (mx == 1) {  # lenday
        vals$lenhr <- pval * 24
        vals$lenmin <- pval * 24 * 60
        vals$lensec <- pval * 86400
        if (!is.null(vals$start)) vals$end = vals$start + vals$lensec
    }
    else if (mx == 2) { # lenhr
        vals$lenday <- pval / 24
        vals$lenmin <- pval * 60
        vals$lensec <- pval * 3600
        if (!is.null(vals$start)) vals$end = vals$start + vals$lensec
    }
    else if (mx == 3) { # lenmin
        vals$lenday <- pval / (24 * 60)
        vals$lenhr <- pval / 60
        vals$lensec <- pval * 60
        if (!is.null(vals$start)) vals$end = vals$start + vals$lensec
    }
    else if (mx == 4) { # lensec
        vals$lenday <- pval / 86400
        vals$lenhr <- pval / 3600
        vals$lenmin <- pval / 60
        if (!is.null(vals$start)) vals$end = vals$start + vals$lensec
    }
    else if (mx == 5) { # start
        if (!is.null(vals$start)) vals$end = vals$start + vals$lensec
    }
    else if (mx == 6) { # end
        if (!is.null(vals$start)) {
            lensec = as.numeric(vals$end - vals$start)
            vals$lensec <- lensec
            vals$lenday <- lensec / 86400
            vals$lenhr <- lensec / 3600
            vals$lenmin <- lensec / 60
        }
    }
    vals
}

.dparParams <- list(
    start=list(type="utime",setf=setDpar,default=NULL,flushCache=TRUE),
    end=list(type="utime",setf=setDpar,default=NULL,flushCache=TRUE),
    lenday=list(type="numeric",setf=setDpar,default=NULL,flushCache=TRUE),
    lenhr=list(type="numeric",setf=setDpar,default=NULL,flushCache=TRUE),
    lenmin=list(type="numeric",setf=setDpar,default=NULL,flushCache=TRUE),
    lensec=list(type="numeric",setf=setDpar,default=86400,flushCache=TRUE),
    lenfile=list(type="numeric",setf=setDpar,default=86400,flushCache=FALSE),
    # verbosity level when reading NetCDF variables
    # 0=quiet, 1=show variable names and times, 2=show vars, filenames, start, count indices
    ncverbose=list(type="integer",setf=setDpar,default=1,flushCache=FALSE)
)

dpar <- function(...,save_cache=FALSE)
{
    dpardefs <- get(".dparParams",envir=.eoltsEnv)

    if (!exists(".dpar",envir=.eoltsEnv)) {
        set <- sapply(dpardefs,function(x){ !is.null(x$default) })
        dparcurr <- lapply(dpardefs[set],function(x)x$default)
        names(dparcurr) <- names(dpardefs[set])
        assign(".dpar",dparcurr,envir=.eoltsEnv)
    }
    else dparcurr <- get(".dpar",envir=.eoltsEnv)

    if(nargs() == 0) return(dparcurr)

    args <- list(...)

    if(is.null(names(args))) {
        # no named parameters passed, i.e. not in the form  dpar(a=99)
        # therefore we are setting values using a list:
        #	dpar(list(a=99,b=12))
        # or it is a query:
        #  dpar(c("a","b"))
        arg <- args[[1]]

        amode <- mode(arg)
        if (amode == "list") args <- arg
        else if (amode == "character") {
            if (any((mn <- match(arg,names(dpardefs),nomatch=0)) == 0)) {
                if (length(arg[mn==0]) == 1)
                    stop(paste(arg[mn==0],"is not a dpar parameter. Choose from:",paste(names(dpardefs),collapse=", ")))
                else
                    stop(paste(paste(arg[mn==0],collapse=","),"are not dpar parameters. Choose from:",paste(names(dpardefs),collapse=", ")))
            }

            res <- lapply(arg,function(an)
                {
                    if (!is.null(dparcurr[[an]]))  {
                        ddef <- dpardefs[[an]]
                        as(dparcurr[[an]],ddef$type)
                    }
                    else dparcurr[[an]]
                }
            )
            if (length(res) == 1) return(res[[1]])
            names(res) <- arg
            return(res)
        }
        else  stop(paste("invalid argument:", arg))
    }
    else {
        if(mode(unlist(args, recursive = F)) == "list")
            stop("dpar(name=list()) is illegal")
    }

    if(length(args) == 0) return()
    if(is.null(names(args))) stop("parameters must be given by name")

    # convert $month to $mon
    if (any(mn <- (names(args)=="month"))) names(args)[mn] <- "mon"

    # allow sloppy users to specify other names for the station parameter
    stn.alts <- c("stn","stations","station")
    if (any((mn <- match(stn.alts,names(args),nomatch=0))!=0)) names(args)[mn] <- "stns"

    flushCache <- FALSE

    oldvals <- lapply(names(args),function(an)
        {
            ddef <- dpardefs[[an]]
            if (is.null(ddef))
                stop(paste(an,"is not a dpar parameter:",paste(names(dpardefs),collapse=",")))
            oldval <- dparcurr[[an]]
            if (!is.null(args[[an]])) {
                val <- as(args[[an]],ddef$type)
                args[[an]] <-  val
            }
            else val <- args[[an]]

            dparcurr <<- ddef$setf(args[an],dparcurr)
            if (!identical(oldval,val) && ddef$flushCache) flushCache <<- TRUE
            oldval
        }
    )
    names(oldvals) <- names(args)

    # user has specified any of the data.selection.names or data.opt.names,
    # or changed the start or length or stations, then remove
    # all cached dat objects
    if (!save_cache && flushCache) clear_cache()

    assign(".dpar",dparcurr,envir=.eoltsEnv)

    invisible(oldvals)
}

dpar.next <- function()
{
    if (is.null(dpar("start"))) invisible(dpar.now())
    else {
        t1 = dpar("start") + dpar("lensec")
        invisible(dpar(start=t1))
    }
}
dpar.prev <- function()
{
    if (is.null(dpar("start"))) invisible(dpar.now())
    else {
        t1 = dpar("start") - dpar("lensec")
        invisible(dpar(start=t1))
    }
}
dpar.now <- function()
{

    if (is.null(dpar("lensec"))) dpar(lenday=1)

    len = dpar("lensec")

    res <- 60
    if (len > 3600) res <- 300

    t1 <- utime("now")
    t1 <- (t1 %/% res) * res

    t1 <- t1 - len

    invisible(dpar(start=t1))
}

