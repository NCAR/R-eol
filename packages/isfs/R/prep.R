# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

HAVE_PREP <- FALSE

setClass("prep",
    slots=c(
	variables="character",
	rate="numeric",
	cppPtr="raw"
    ),
    prototype=list(
	variables="",
        rate=0,
	cppPtr=raw(8)
    )
)

prep <- function(variables,rate=0)
{
    if (!HAVE_PREP) stop("sorry prep not available")
    obj = new("prep",variables=variables,rate=rate)
    .Call("open_prep",obj,PACKAGE="isfs")
}

setMethod("close",
    signature(con="prep"),
    function(con)
    {
        .Call("close_prep",con,PACKAGE="isfs")
        NULL
    }
)

setGeneric("pid",function(con) standardGeneric("pid"))

setMethod("pid",
    signature(con="prep"),
    function(con)
    {
        .Call("prep_pid",con,PACKAGE="isfs")
    }
)

setMethod("readts",
    signature(con="prep",variables="missing",start="utime",
              end="utime"),
    function(con,variables,start,end,...)
    {

        pid <- pid(con)
        if (pid <= 0) {
            #      vsn = unix("prep -v")
            newversion =  regexpr("^Version:",system("prep -v",intern=TRUE)) == 1
            # old version of prep doesn't report Version
            if (length(newversion)==0) newversion = F
            if (newversion) {
                runargs <- c("-D",paste(con@variables,collapse=","),
                    "-B",format(start,format="%Y %m %d %H%M%OS",time.zone="GMT"),
                    "-C")
                env = ""
            }
            else {
                ops <- system(paste("getops",
                    format(start,format="%Y %m %d %H%M%OS",time.zone="GMT")),intern=TRUE)

                runargs <- c("-D",paste(con@variables,collapse=","),
                    "-B",format(start,format="%Y %m %d %H%M%OS",time.zone="GMT"),
                    "-f","-C")
                env <- paste("OPS=",ops,sep="")
            }

            # If the user passes a non-zero rate, use it when exec'ing prep.
            # otherwise don't pass a -r option to prep.
            if (con@rate > 0.) runargs <- c("-r",con@rate,runargs)

            prog <- "prep"

            if (!newversion) {
                com <- paste(sep="","OPS=",ops," getunits -c ",
                    paste(con@variables,sep=" ",collapse=" "))
                units <- eval(parse(text=paste("c(",system(com,intern=TRUE),")")))
            }
            else units = character(0)

            cat("Executing: prep",runargs,"\n")
            # cat(env,"\n");

            pid <- .Call("start_prep",con,prog,runargs,env,units,PACKAGE="isfs")
            # cat("prep pid=",pid,"\n")

            if (pid < 0) stop(paste("error executing",prog))
        }
        else {
            # send a SIGCONT signal to the process
            # cat("sending SIGCONT to prep\n")
            .Call("cont_sig_prep",con,PACKAGE="isfs")
        }

        period <- end - start

        # If user is asking for the same length of time as the last request,
        # try to read the same number of points.  This is so that one can do spectral
        # analysis on successive periods and look at the same frequencies.
        if (period == .Call("prep_get_period",con,PACKAGE="isfs"))
              nrows <- .Call("prep_get_nrows",con,PACKAGE="isfs")
        else nrows <- 0L

        # cat("nrows=",nrows,"\n")

        x <- .Call("read_prep",con,start,end,as.integer(nrows),PACKAGE="isfs")

        # cat("dim(x)=",dim(x),"\n")
        # browser()

        positions(x) = utime(positions(x))
        stations(x) <- as.integer(rep(0,ncol(x)))
        # start(x) = start(x)
        # end(x) = end(x)
        x@time.format = getOption("time.out.format")
        x@time.zone = getOption("time.zone")

        nrows <- nrow(x)
        if (nrows <= 1L) {
          if (nrows == 1L) {
            warning(paste("One point time series read from",format(start),"to",format(end)))
            return(invisible(x))
          }
          warning(paste("No data found from",format(start),"to",format(end)))
          return(NULL)
        }
        # In the future, try to read an even number of records - help fft efficiency
        if (nrows > 100L && nrows %% 2L) {
          nrows <- nrows - 1L
          x <- x[1:nrows,]
        }

        # If data is evenly sampled, save period and number of records
        # read, so we can try to read the same number next time
        # if the user asks for the same length of time.
        dt = deltat(x)
        # cat("dt=",paste(dt,collapse=","),"\n")
        if (con@rate > 0. && abs(dt[1] - 1/con@rate) < .1/con@rate) {
          dt[1] = 1/con@rate
          deltat(x) = dt
        }
        # browser()
        # cat('(dt["max"] - dt["min"])=',(dt["max"] - dt["min"])," dt[1]=",dt[1],
        #   " nrows=",nrows," period=",period,"\n",
        #   " ((nrows * dt[1]) / period)=",((nrows * dt[1]) / period),"\n")
        if ((dt["max"] - dt["min"]) < 1.5 * dt[1] &&
              ((nrows * dt[1]) / period) > .95)
        {
          # Set nrows the first time we get evenly sampled data
          if (.Call("prep_get_nrows",con,PACKAGE="isfs") <= 0) {
            # cat("prep_set_nrows, nrows=",nrows,"\n")
            .Call("prep_set_nrows",con,as.integer(nrows),PACKAGE="isfs")
            # cat("prep_set_period, period=",period,"\n")
            .Call("prep_set_period",con,period,PACKAGE="isfs")
          }
        }
        else .Call("prep_set_period",con,-1., PACKAGE="isfs")

        x@data[!is.na(x@data) & x@data > 1.e36] <- NA

        # send a SIGSTOP signal to the process
        # cat("sending SIGSTOP to prep\n")
        .Call("stop_sig_prep",con,PACKAGE="isfs")

        invisible(dat(x))
    }
)

setMethod("readts",
    signature(con="prep",variables="missing",start="character",end="character"),
    function(con,variables,start,end,...)
    {
        t1 = utime(start)
        t2 = utime(end)
        readts(con, start=t1,end=t2)
    }
)

setMethod("readts",
    signature(con="prep",variables="missing",start="missing",end="missing"),
    function(con,variables,start,end,...)
    {
        readts(con, start=dpar("start"),end=dpar("end"))
    }
)

