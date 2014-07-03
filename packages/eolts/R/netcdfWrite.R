# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolts" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

HAVE_NC_SERVER <- FALSE

setGeneric("writets",function(con,x,...) standardGeneric("writets"))

setMethod("writets",
    signature(con="netcdf",x="nts"),
    function(con,x,...) {

        # not supported on all architectures
        if (!HAVE_NC_SERVER) stop("sorry, writets not available. No nc_server_rpc support")

        dots <- list(...)

        history <- NULL
        if (hasArg(history)) history <- dots$history

        fillvalue <- 1.e37
        if (hasArg(fillvalue)) fillvalue <- dots$fillvalue

        max.station <- NULL
        if (hasArg(max.station)) max.station <- dots$max.station

        interval <- con@interval

        if (is.null(dt <- deltat(x)[1]) || is.na(dt) || dt <= 0.0) dt <- interval
        if ((interval %% dt) != 0) {
            ndt <- round(interval / dt)
            if (ndt < 1) ndt <- 1
            dt <- interval / ndt
        }

        if (!is.null(history) && nzchar(history)) {
            if (length(history) > 1)
                history <- paste(paste(history,collapse="\n"),"\n",sep="")
            if (substring(history,nchar(history)) != "\n")
                history <- paste(history,"\n",sep="")

            .External("write_history_ns", con,as.character(history),
                PACKAGE="eolts")
        }

        x[is.na(x)] <- fillvalue

        # browser()

        if (!is.null(stns <- stations(x)) && length(stns) > 0) {
            if (length(stns) != ncol(x))
                stop(paste("length of stations(x)=(",length(stns),") not equal to ncol(x)=",ncol(x),sep=""))

            sv <- !is.na(stns) & stns != 0
            if (any(sv)) {
                if (is.null(max.station)) {
                    # Read available stations from NetCDF file
                    if (exists(".projectEnv") &&
                        exists(".all_stations",envir=(pe <- get(".projectEnv")))) {
                        all_stations <- get(".all_stations",envir=get(".projectEnv"))
                    }
                    else {
                        # iwarn <- options(warn=-1)
                        all_stations <- stations(con)
                        # options(iwarn)
                        if (exists(".projectEnv"))
                            assign(".all_stations",all_stations,envir=get(".projectEnv"))
                    }
                    max.station <- max(all_stations)
                    # cat("max.station=",max.station,"\n")
                }

                xx <- x[,sv]		# variables with station dimension
                stns <- stns[sv]
                # Write one station at a time
                for (stn in unique(stns)) {
                    is <- stns == stn
                    xxx <- xx[,is]

                    # Do variables with same weight map
                    if (is.null(wm <- xxx@weightmap) || length(wm) == 0)
                        wm <- rep(0,ncol(xxx))

                    for (wc in unique(wm)) {
                        ws <- wc == wm
                        xxxx <- xxx[,ws]
                        # cat("dimnames(xxxx)=",paste(dimnames(xxxx)[[2]],collapse=","),
                        #     ", stations(xxxx)=",paste(stations(xxxx),collapse=","),
                        #     ",wm=",paste(xxxx@weightmap,collapse=","),"\n")
                        .External("write_ts_ns",
                            con,
                            xxxx,
                            "station",	# non-time dimension names
                            max.station,	# size of non-time dimensions
                            dt,
                            fillvalue,
                            PACKAGE="eolts")
                    }
                }
            }
            if (any(!sv)) x <- x[,!sv]	# variables without station dimension
            else x <- NULL
        }
        if (!is.null(x)) {

            # Do variables with same weight map
            if (is.null(wm <- x@weightmap) || length(wm) == 0)
                wm <- rep(0,ncol(x))

            for (wc in unique(wm)) {
                ws <- wc == wm
                xx <- x[,ws]
                .External("write_ts_ns",
                    con,
                    xx,
                    character(0),	# non-time dimension names
                    integer(0),	# size of non-time dimensions
                    dt,
                    fillvalue,
                    PACKAGE="eolts")
            }
        }
        invisible(NULL)
    }
)

setMethod("writets",
    signature(con="netcdf",x="missing"),
    function(con,x,...) {

        # not supported on all architectures
        if (!HAVE_NC_SERVER) stop("sorry, writets not available. No nc_server_rpc support")

        if (!hasArg(history) && !hasArg(gattrs)) stop("history and gattrs arguments not found")
            
        dots <- list(...)
        if ("history" %in% names(dots)) {
            history <- dots$history
            if (length(history) > 0 && nzchar(history)) {
                if (substring(history,nchar(history)) != "\n")
                    history <- paste0(history,"\n")
                .External("write_history_ns", con,as.character(history),
                    PACKAGE="eolts")
            }
        }

        if ("gattrs" %in% names(dots)) {
            gattrs <- dots$gattrs
            .External("write_global_attrs_ns", con,as.list(gattrs), PACKAGE="eolts")
        }
        invisible(NULL)
    }
)

