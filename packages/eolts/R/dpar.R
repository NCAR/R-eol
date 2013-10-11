#
#               Copyright (C) by UCAR
# 

dpar <- function(...,save.cache=F)
{
    # dpar function.  Argument handling stolen from par()

    if (!exists(".dpar",envir=.eoltsEnv)) {

        # here is where the defaults are set:
        dpar.list <- list(
                          robust=F,
                          accelgrav=9.81,
                          vonKarman=0.4,
                          start=utime("now"),
                          lenday=1
                          )
        assign(".dpar",dpar.list,envir=.eoltsEnv)
    }
    else dpar.list <- get(".dpar",envir=.eoltsEnv)

    if(nargs() == 0) return(dpar.list)

    dpar.list.orig <- dpar.list

    temp <- list(...)

    utime.fields <- c("year","mon","day","yday","hour","min","sec")
    utime.names <- c(utime.fields,"TZ")

    # Other members returned from or as.list.utime, which we'll ignore
    ignore.names <- c("msec","dow","cmonth","isdst","zonediff")

    # or time can be set with start and end
    time.names <- c("start","end")

    # names that can be queried, they are ignored if setting
    query.names <- c("length")

    time.len.names <- c("lenday","lenhr","lenmin","lensec")

    # Changing any of the data.selection.names or data.opt.names
    # will result in the cache being flushed.
    data.selection.names <- c("stns","sonic","h2o","temp","hts","sfxs","sites")
    data.opt.names <- c(
        "chksum",		# screen data by looking at associated checksums?
        "lat","lon",	# used by dat("az.sun"), dat("el.sun")
        "avg",		# for simple averages,
        #     one value, average interval in seconds
        # for non-simple averages,
        #     two values, non-simple interval, simple interval
        "smooth"		# smoothing period, in seconds
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
         "lenfile",		# obscure, used if NetCDF file is > 1 day long
         "gradientevalz",    # height to evaluate gradient functions
         "fh2os",            # vector of fast h2o sensors (used by calc.o2corr)
         "sonichts",         # heights of sonics at each station - useful in case heights are not in variable names
         "sfrac.min"         # minimum fraction of sonic samples, for editing fluxes
         )

    set.names <- c(utime.names,time.len.names,data.selection.names,data.opt.names,
                   deriv.opt.names,time.names)
    ok.names <- c(set.names,ignore.names,query.names)
    get.names <- c(set.names,query.names)

    # 
    if(is.null(names(temp))) {
        # no named parameters passed, i.e. not in the form  dpar(a=99)
        # therefore we are setting values using a list:
        #	dpar(list(a=99,b=12))
        # or it is a query:
        #  dpar("a")
        arg <- temp[[1]]

        amode <- mode(arg)
        if (amode == "list") temp <- arg
        else if (amode == "character") {

            if (any((mn <- match(arg,get.names,nomatch=0))==0))
                stop(paste(arg[mn==0][1],"is not a known dpar parameter"))
            else if (length(arg) > 1) {
                temp <- dpar.list[arg]
                if (any(mn <- match(arg,names(dpar.list),nomatch=0) == 0)) 
                    names(temp) <- arg
                return(temp)
            }
            else return(dpar.list[[arg]])
        }
        else  stop(paste("invalid argument:", arg))
    }
    else {
        if(mode(unlist(temp, recursive = F)) == "list")
            stop("dpar(name=list()) is illegal")
    }
    if(length(temp) == 0) return()
    inames <- names(temp)
    if(is.null(inames)) stop("parameters must be given by name")

    # convert $month to $mon
    if (any(mn <- (inames=="month"))) {
        temp$mon <- temp$month
        inames[mn] <- "mon"
    }

    # allow sloppy users to specify other names for the station parameter
    stn.alts <- c("stn","stations","station")
    if (any((mn <- match(stn.alts,inames,nomatch=0))!=0)) {
        names(temp)[mn] <- "stns"
        inames <- names(temp)
    }

    mn <- match(inames,ok.names,nomatch=0)
    if (any(mn == 0)) 
        stop(paste(inames[mn==0][1],"is not a known dpar parameter"))

    mn <- match(inames,set.names,nomatch=0)
    if (all(mn==0)) return(NULL)
    inames <- unique(set.names[mn])

    dpar.list[inames] <- temp[inames]
    if (any(inames == "day") || any(inames == "mon"))
        dpar.list$yday <- NULL
    else if (any(inames == "yday")) {
        dpar.list$mon <- NULL
        dpar.list$day <- NULL
    }

    # set timezone
    if (any(inames == "TZ")) options(time.zone=dpar.list$TZ)
    else dpar.list$TZ <- unlist(options("time.zone"))

    # If user has passed any utime fields (year, day, etc)
    # set time from them, include TZ when setting
    if (any(match(utime.fields,inames,nomatch=0)) != 0)
        dpar.list$start <- utime(dpar.list[utime.names])

    if (is.null(dpar.list$start)) dpar.list$start <- utime("now")
    else dpar.list$start <- utime(dpar.list$start)	# make sure it is a utime

    dpar.list[utime.names] <- as.list(utime(dpar.list$start))[utime.names]

    # If any length parameter is specified,
    # zero out lengths not specified
    if (any(match(time.len.names,inames,nomatch=0) != 0)) {
        len.not <- time.len.names[match(time.len.names,inames,nomatch=0) == 0]
        dpar.list[len.not] <- 0
        dpar.list$length <- dpar.list$lenday * 86400 + dpar.list$lenhr * 3600 +
        dpar.list$lenmin * 60 + dpar.list$lensec
        # length specified, overwrite end
        dpar.list$end <- dpar.list$start + dpar.list$length
    }
    else if (match("end",inames,nomatch=0) != 0) {
        # end specified, but not length
        # end may be a character string, convert to utime.
        dpar.list$end <- utime(dpar.list$end)
        dpar.list$length <- as.numeric(dpar.list$end - dpar.list$start)
        dpar.list$lensec <- 0
        dpar.list$lenmin <- 0
        dpar.list$lenhr <- 0
        dpar.list$lenday <- 0
        if (dpar.list$length == 86400) dpar.list$lenday <- 1
        else dpar.list$lensec <- dpar.list$length
    }
    else {
        # Neither length or end specified, set length if null,
        # then set end from start and length
        if (is.null(dpar.list$length)) {
            dpar.list$length <- 86400
            dpar.list$lensec <- 0
            dpar.list$lenmin <- 0
            dpar.list$lenhr <- 0
            dpar.list$lenday <- 1
        }
        dpar.list$end <- dpar.list$start + dpar.list$length
    }
    dpar.list$end <- utime(dpar.list$end)	# make sure it is a utime

    # if (is.null(dpar.list$length) || is.na(dpar.list$length)
    # 	|| dpar.list$length <= 0) {
    #     dpar.list$lensec <- 0
    #     dpar.list$lenmin <- 0
    #     dpar.list$lenhr <- 0
    #     dpar.list$lenday <- 1
    #     dpar.list$length <- 86400
    #   }

    # Read available stations from NetCDF file
    if (!exists(".all.stations",envir=.eoltsEnv)) {
        iwarn <- options(warn=-1)
        # cat("calling netcdf\n")
        iod <- netcdf(start=dpar.list$start,end=dpar.list$end)
        all.stations <- stations(iod)
        close(iod)
        options(iwarn)
        if (length(all.stations) > 0) assign(".all.stations",all.stations,envir=.eoltsEnv)
        if (is.null(dpar.list$stns)) dpar.list$stns <- all.stations
    }
    else all.stations = get(".all.stations",envir=.eoltsEnv)

    if (any(inames == "stns") && length(all.stations) > 0)
        # dpar.list$stns <- all.stations[dpar.list$stns]
        dpar.list$stns <- all.stations[match(dpar.list$stns,all.stations,
                                             nomatch=0)]

    # user has specified any of the data.selection.names or data.opt.names,
    # or changed the start or length or stations, then remove
    # all cached dat objects
    if (!save.cache && (
         any(match(inames,data.selection.names,nomatch=0) != 0) ||
         any(match(inames,data.opt.names,nomatch=0) != 0) ||
         !identical(dpar.list$start,dpar.list.orig$start) ||
         !identical(dpar.list$length,dpar.list.orig$length) ||
         any(is.na(match(dpar.list$stns,dpar.list.orig$stns)))))
        clear.cache()


    assign(".dpar",dpar.list,envir=.eoltsEnv)

    temp <- dpar.list.orig[inames]
    if (any(mn <- match(inames,names(dpar.list.orig),nomatch=0) == 0)) 
        names(temp) <- inames
    invisible(temp)
}
dpar.next <- function()
{
    t1 <- dpar("start") + dpar("length")

    l1 <- as.list(t1)
    if (l1$year < 200) l1$year <- l1$year + 1900
    utime.names <- c("year","mon","day","yday","hour","min","sec","TZ")
    n <- utime.names[unique(match(names(l1),utime.names,nomatch=0))]
    l1 <- l1[n]
    invisible(dpar(l1))
}
dpar.prev <- function()
{
    t1 <- dpar("start") - dpar("length")

    l1 <- as.list(t1)
    if (l1$year < 200) l1$year <- l1$year + 1900
    utime.names <- c("year","mon","day","yday","hour","min","sec","TZ")
    n <- utime.names[unique(match(names(l1),utime.names,nomatch=0))]
    l1 <- l1[n]
    invisible(dpar(l1))
}
dpar.now <- function()
{
    dexists <- exists(".dpar",envir=.eoltsEnv)

    len <- NULL
    if (!dexists) assign(".dpar",list(),envir=.eoltsEnv)
    else len <- dpar("length")
    if (is.null(len)) len <- 86400

    res <- 60
    if (len > 3600) res <- 300

    t1 <- utime("now")
    t1 <- (t1 %/% res) * res

    t1 <- t1 - len

    l1 <- as.list(t1)
    if (l1$year < 200) l1$year <- l1$year + 1900
    utime.names <- c("year","mon","day","yday","hour","min","sec","TZ")
    n <- utime.names[unique(match(names(l1),utime.names,nomatch=0))]
    l1 <- l1[n]
    invisible(dpar(l1))
}

