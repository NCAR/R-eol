# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolts" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

dpar <- function(...,save.cache=F)
{
    # dpar function.  Argument handling stolen from par()

    if (!exists(".dpar",envir=.eoltsEnv)) {

        # here is where the defaults are set:
        dpar.list <- list(
                          robust=F,
                          accelgrav=9.81,
                          vonKarman=0.4,
                          lensec=86400,
                          ncverbose=1
                          # sites=character(0),
                          # sfxs=character(0),
                          # stns=integer(0),
                          # hts=numeric(0)
                          )
        assign(".dpar",dpar.list,envir=.eoltsEnv)
    }
    else dpar.list <- get(".dpar",envir=.eoltsEnv)

    if(nargs() == 0) return(dpar.list)

    dpar.list.orig <- dpar.list

    newvals <- list(...)

    # time can be set with start and end
    time.names <- c("start","end","lensec")

    time.len.names <- c("lenday","lenhr","lenmin")
    all.time.len.names <- c("lenday","lenhr","lenmin","lensec")

    # Changing any of the data.selection.names or data.opt.names
    # will result in the cache being flushed.
    # TODO: check if "sonic","temp" or "h2o" are used and necessary to keep
    data.selection.names <- c("stns","sonic","h2o","temp","hts","sfxs","sites")
    data.selection.modes <- c("numeric","character","character","character","numeric","character","character")
    data.opt.names <- c(
        "chksum",   # screen data by looking at associated checksums?
        "lat","lon",# used by dat("az.sun"), dat("el.sun")
        "avg",      # for simple averages, one value, average interval in seconds
                    # for non-simple averages, two values, non-simple interval, simple interval
        "smooth",   # smoothing period, in seconds
        "ncverbose" # verbosity level when reading NetCDF variables
                    # 0=quiet, 1=show variable names and times, 2=show vars, filenames, start, count indices
    )

    # Changing any deriv.opt.names does not result in the cache being
    # flushed, since derived variables are not kept in the cache.
    deriv.opt.names <- c(
         "accelgrav",	# in case we put a PAM on the moon
         "azimuth.boom",
         "colorplots",
         "coords",
         "datacoords",
         "declination",
         "emissivity.sfc",
         "pyrgeometer.B",
         "pyrgeometer.swcor",# Pyrgeometer shortwave correction factor
         "robust",		# if true, use simpliest algorithms for computing
         # derived quantities, which should be more
         # robust since they have fewer dependencies
         "specificheat",
         "vonKarman",
         "wind.speed",	# variable to use for wind speed in dat.winds
         "bulk.speed",	# variable to use for wind speed in dat.Cd...
         "RH.ice",		# compute RH wrt ice?
         "good.samples",	# minimum acceptable sonic sampling %age
         "platform",		# platform name, used for labeling plots
         "lenfile",		# length of NetCDF file, often 86400 secs
         "gradientevalz",    # height to evaluate gradient functions
         "sfrac.min",        # minimum fraction of sonic samples, for editing fluxes
         "sonic_h2o_separation_corrected",  # have x'h2o' been corrected for sensor separation?
         "sonic_co2_separation_corrected",  # have x'co2' been corrected for sensor separation?
         # has Gsoil been corrected for difference between heat conductivity of
         # soil and calibration medium?
         "gsoil_philip_corrected"  
    )

    set.names <- c(time.len.names,data.selection.names,data.opt.names,
                   deriv.opt.names,time.names)
    # can't fetch any of time.len.names. Can fetch "lensec" in time.names
    get.names <- c(data.selection.names,data.opt.names,deriv.opt.names,time.names)

    # 
    if(is.null(names(newvals))) {
        # no named parameters passed, i.e. not in the form  dpar(a=99)
        # therefore we are setting values using a list:
        #	dpar(list(a=99,b=12))
        # or it is a query:
        #  dpar(c("a","b"))
        arg <- newvals[[1]]

        amode <- mode(arg)
        if (amode == "list") newvals <- arg
        else if (amode == "character") {
            if (any((mn <- match(arg,get.names,nomatch=0))==0)) {
                if (any(!is.na(match(arg,time.len.names)))) str <- 'Use "lensec" to query the time length'
                else str <- ""

                if (sum(mn==0) > 1) stop(paste(paste(arg[mn==0],collapse=","),"are not readable dpar parameters.",str))
                else stop(paste(arg[mn==0],"is not a readable dpar parameter.",str))
            }
            if (length(arg) > 1) {
                # return NULL for those values not found
                mn <- !is.na(match(arg,names(dpar.list)))
                return(c(dpar.list[arg[mn]],sapply(arg[!mn],function(x){NULL})))
            }
            return(dpar.list[[arg]])
        }
        else  stop(paste("invalid argument:", arg))
    }
    else {
        if(mode(unlist(newvals, recursive = F)) == "list")
            stop("dpar(name=list()) is illegal")
    }
    if(length(newvals) == 0) return()
    inames <- names(newvals)
    if(is.null(inames)) stop("parameters must be given by name")

    # convert $month to $mon
    if (any(mn <- (inames=="month"))) {
        newvals$mon <- newvals$month
        inames[mn] <- "mon"
    }

    # allow sloppy users to specify other names for the station parameter
    stn.alts <- c("stn","stations","station")
    if (any((mn <- match(stn.alts,inames,nomatch=0))!=0)) {
        names(newvals)[mn] <- "stns"
        inames <- names(newvals)
    }

    imodes <- sapply(newvals,function(x)mode(x))

    mn <- match(inames,data.selection.names,nomatch=0)
    if (any(mn != 0)) {
        bad <- imodes[mn!=0] != "NULL" & imodes[mn!=0] != data.selection.modes[mn]
        if (any(bad))
            stop(paste("dpar ",paste(data.selection.names[mn][bad],collapse=","),
                    "must be",
                paste(data.selection.modes[mn][bad],collapse=","),", or NULL"))
    }

    mn <- match(inames,set.names,nomatch=0)
    if (any(mn == 0)) 
        stop(paste(inames[mn==0][1],"is not a known dpar parameter"))

    inames <- unique(set.names[mn])

    dpar.list[inames] <- newvals[inames]
    if (any(inames == "day") || any(inames == "mon"))
        dpar.list$yday <- NULL
    else if (any(inames == "yday")) {
        dpar.list$mon <- NULL
        dpar.list$day <- NULL
    }

    # make sure start is a utime
    if (!is.null(dpar.list$start)) dpar.list$start = utime(dpar.list$start)

    # If any length parameter is specified,
    # zero out lengths not specified
    if (any(match(all.time.len.names,inames,nomatch=0) != 0)) {
        len.not <- all.time.len.names[match(all.time.len.names,inames,nomatch=0) == 0]
        dpar.list[len.not] = 0
        dpar.list$lensec = dpar.list$lenday * 86400 + dpar.list$lenhr * 3600 +
            dpar.list$lenmin * 60 + dpar.list$lensec
        # length specified, overwrite end
        if (!is.null(dpar.list$start)) dpar.list$end = dpar.list$start + dpar.list$lensec

        # remove lenday,lenhr,lenmin from values to be returned
        inames <- inames[is.na(match(inames,time.len.names))]
        # add lensec
        if (is.na(match("lensec",inames))) inames <- c(inames,"lensec")
    }
    else if (match("end",inames,nomatch=0) != 0) {
        # end specified, but not length
        # end may be a character string, convert to utime.
        dpar.list$end <- utime(dpar.list$end)
        if (!is.null(dpar.list$start)) {
            dpar.list$lensec = as.numeric(dpar.list$end - dpar.list$start)
        }
    }
    # remove lenday,lenhr,lenmin
    dpar.list[time.len.names] <- NULL

    if (!is.null(dpar.list$start)) dpar.list$end = dpar.list$start + dpar.list$lensec

    dpar.list$end <- utime(dpar.list$end)	# make sure it is a utime

    if (FALSE) {
        # Read available stations from NetCDF file
        if (!is.null(dpar.list$start) && !exists(".all_stations",envir=.eoltsEnv)) {
            iwarn <- options(warn=-1)
            # cat("calling netcdf\n")
            iod <- netcdf(start=dpar.list$start,end=dpar.list$end)
            all_stations <- stations(iod)
            close(iod)
            options(iwarn)
            # cat("all_stations=",paste(all_stations,collapse=","),"\n")
            assign(".all_stations",all_stations,envir=.eoltsEnv)
            if (is.null(dpar.list$stns)) dpar.list$stns <- all_stations
        }

        if ("stns" %in% inames) {
            if (exists(".all_stations",envir=.eoltsEnv)) {
                all_stations = get(".all_stations",envir=.eoltsEnv)
                if (length(dpar.list$stns) == 0) dpar.list$stns = all_stations
                else dpar.list$stns = all_stations[match(dpar.list$stns,all_stations,
                                                 nomatch=0)]
            }
        }
    }

    # user has specified any of the data.selection.names or data.opt.names,
    # or changed the start or length or stations, then remove
    # all cached dat objects
    if (!save.cache && (
         any(match(inames,data.selection.names,nomatch=0) != 0) ||
         any(match(inames,data.opt.names,nomatch=0) != 0) ||
         !identical(dpar.list$start,dpar.list.orig$start) ||
         !identical(dpar.list$lensec,dpar.list.orig$lensec) ||
         any(is.na(match(dpar.list$stns,dpar.list.orig$stns)))))
        clear.cache()


    assign(".dpar",dpar.list,envir=.eoltsEnv)

    mx <- is.na(match(inames,names(dpar.list.orig)))
    res <- list()
    if (any(mx)) res <- sapply(inames[mx],function(x)NULL,USE.NAMES=TRUE)
    res <- c(res,dpar.list.orig[inames[!mx]])
    invisible(res)
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

