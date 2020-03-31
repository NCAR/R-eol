# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolts" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

setGeneric("plot")
setMethod("plot",signature(x="nts",y="missing"),
    function(x,...)
    {
        # cat("in plot nts\n")
        invisible(plot.nts(x, ...))
    }
)

setMethod("plot",signature(x="nts",y="nts"),
    function(x,y,xlab,ylab,...)
    {
        if (missing(xlab)) xlab <- deparse(substitute(x))
        if (missing(ylab)) ylab <- deparse(substitute(y))
        invisible(plot(x@data, y@data,xlab=xlab,ylab=ylab, ...))
    }
)

setMethod("plot",signature(x="nts",y="numeric"),
    function(x,y,xlab,ylab,...)
    {
        if (missing(xlab)) xlab <- deparse(substitute(x))
        if (missing(ylab)) ylab <- deparse(substitute(y))
        invisible(plot(x@data, y,xlab=xlab,ylab=ylab, ...))
    }
)

plot.nts <- function(x, type="l", xlab=TRUE, xlim, ylab, ylim,
	xaxs="i", xaxt, yaxt,
        col, pch, lty, time.zone=x@time.zone, axes=TRUE, log="",
        cex=1.0, ...)
{

    # get the axes argument. 
    # The axes argument is separate, not part of ..., because
    # we pass ... to points(), and axes is not a valid
    # argument for points.
    plotaxes <- axes

    args <- list(...)

    nc <- dim(x)[2]

    # This is no longer a fatal error.  plot.nas will be called to
    # create the plot frame.
    # if ((all.is.na <- all(is.na(x[,1])))) warning("all data in timeseries is NA")
    all.is.na <- all(is.na(x[,1]))

    # yaxt values: n(none), s(standard), t(time), l(log)
    # defaults to "s" after graphics device is created.
    # par("yaxt")	plot(yaxt)	plot(log)	par("yaxt") after plot()
    #	s	missing				s
    #	s	missing		y		l log plot
    #	s	l				s std plot, yaxt arg ignored
    #	s	l		y		s log plot, problems for points
    #	s	s				s std plot
    #	s	s		y		s log plot, problems for points
    #	s	n		 		s std plot, no yaxis
    #	s	n		y 		s log plot, no yaxis,
    #						  problems for points
    #  						  must set yaxt="l"
    #	l	missing			        s std plot, par value ignored
    #	l	missing		y	        l log plot
    #	l	l		 		l std plot, problem for points
    #						  log is taken of points data
    #						  but plotted in std axis
    #						  must do yaxt="s" for plot
    #	l	l		y		l log plot
    #	l	s		 		l std plot, problem for points
    #						  log is taken of points data
    #						  but plotted in std axis
    #	l	s		y 		l log plot
    #	l	n		 		l std plot, no yaxis,
    #						  problems for points
    #	l	n		y 		l log plot, no yaxis
    #	n	missing		 		n no y axis, but ylab
    #	n	missing		y		n log plot, no y axis
    #						  not l, problem for points
    #						  must use yaxt="l"
    #     n	l				n std plot
    #	n	l		y		n log plot
    #	n	s		 		n std plot
    #	n	s		y		n log plot
    #	n	n		 		n std plot no yaxis
    #	n	n		y		n log plot no yaxis
    #
    # Conclusions:
    #   only time yaxt function parameter is useful is when it
    #   is "n"
    #   do par(yaxt="s") unless it is "n"
    #   

    if (par("xaxt") != "n") par(xaxt="s")
    if (missing(xaxt)) xaxt = par("xaxt")

    logy = any(substring(log,1:nchar(log),1:nchar(log)) == "y")
    if (par("yaxt") != "n") {
        if (logy) par(yaxt="l")
        else par(yaxt="s")
    }
    if (missing(yaxt)) yaxt = par("yaxt")

    if (missing(ylim)) {
        ylim <- range(x,na.rm=T)	# may have more than one column
        if (any(is.na(ylim)) || any(is.infinite(ylim))) ylim <- c(0,1)	# fake it
    }

    tx <- x@positions

    if (missing(col)) {
        if (nc > 1) col <- 1:8		# default for matplot()
        else col <- par("col")
    }
    if (missing(pch)) {
        if (nc > 1) pch <-
            c("1","2","3","4","5","6","7","8","9","0","A","B","C","D","E")
        else pch <- par("pch")
    }
    if (missing(lty)) {
        if (nc > 1) lty <- 1:5
        lty <- par("lty")
    }

    #
    # xaxs:
    #	r	default, plot limits are data limits extended by 4%
    #		  on each end, plots limits are not necessarily
    #		  nice numbers
    #	s	standard axis.  Extend axes to nice numbers (not implemented in R)
    #	e	extended axis.  Extend axes to nice numbers, with
    #		  axis at least 1 character width from data limits (not implemented in R)
    #	i	internal labels, don't extend axes, label at nice numbers
    #		internal labels means first label is probably to right of
    #		first plotted point
    #	d	don't change from previous value (not implemented in R, but implemented here)
    #	

    if (xaxs == "d") {      # don't rescale
        if (! exists(".timeaxis_params",envir=.eoltsEnv)) {
            stop("timeaxis_params does not exist. Cannot plot with xaxs='d'")
        }
        tparams <- get(".timeaxis_params",envir=.eoltsEnv)
        x0 <- tparams$toffset
        xlim.scaled <- par("usr")[c(1,2)]
        xrange <- xlim.scaled + x0
    }
    else {
        if (missing(xlim)) xrange <- range(tx)
        else {
            xrange <- xlim
            x <- x[utime(xlim),]
            tx <- x@positions
        }
        tparams <- timeaxis_setup(xrange[1], xrange[2],
            time.zone=time.zone, cex=cex)
        x0 <- xrange[1]
        xlim.scaled <- xrange - x0
    }

    xaxs <- "i"

    tlabcex <- tparams$tlabcex

    if (missing(ylab)) {
        if (nc > 1) {
            # name of y variable
            xexpr <- substitute(x)
            ylab <- deparse(xexpr)
        }
        else {
            if (is.null(dunits <- attr(x,"dunits")))
                ylab <- dimnames(x)[[2]][1]
            else
                ylab <- paste0(dimnames(x)[[2]][1],
                     "(", attr(x,"dunits")[1], ")")
        }
    }

    # plot first column
    if (all.is.na)
        plot.nas(type=type, axes=plotaxes, xlim=xlim.scaled, xlab="",
            ylim=ylim, ylab=ylab, xaxs=xaxs, xaxt="n", yaxt=yaxt,
            col=col[1], pch=pch[1], lty=lty[1], cex=cex, ...)
    else
        plot(tx-x0, x@data[,1], type=type, axes=plotaxes,
            xlim=xlim.scaled, xlab="", ylim=ylim, ylab=ylab, xaxs=xaxs,
            xaxt="n", yaxt=yaxt, col=col[1], pch=pch[1], lty=lty[1],
            log=log,cex=cex, ...)

    if (plotaxes) {
        if (xaxt != "n") {
            timeaxis(1, labels=TRUE, tick=TRUE, xlab=xlab,
                time.zone=time.zone, date.too=TRUE, line=0, outer=FALSE,...)

            timeaxis(3, labels=FALSE, tick=TRUE, xlab="",
                time.zone=time.zone, line=0, outer=FALSE,...)
        }
    }

    if (nc > 1) {
        ncolor <- length(col)
        nlty <- length(lty)
        npch <- length(pch)

        for (i in 2:nc) points(x[,i],col=col[(i-1)%%ncolor+1],
            pch=pch[(i-1)%%npch+1],type=type,...)
    }
   
    invisible(tparams)
}

timeaxis <- function(side, labels=TRUE, tick=TRUE, xlab=labels,
    time.zone=getOption("time.zone"), date.too=labels, line=0, outer=FALSE,...)
{
    if (! exists(".timeaxis_params",envir=.eoltsEnv)) {
        stop("timeaxis_params does not exist. Cannot add time axis to ordinary plot")
    }
 
    tparams = get(".timeaxis_params",envir=.eoltsEnv)

    x0 <- tparams$toffset

    # major tics
    xmajtics <- tparams$majtics

    if (labels) {
        tlabels <- format(xmajtics,format=tparams$format,time.zone=time.zone)
        # Add timezone to middle label
        mid <- length(tlabels) / 2 + 1
        tfmt <- paste(tlabels[mid],"%Z")
        tlabels[mid] <- format(xmajtics[mid],format=tfmt,time.zone=time.zone)
        if (date.too && x0 > 86400. && tparams$extraformat != "") {
            tfmt <- paste0(tlabels[1],"\n",tparams$extraformat)
            tlabels[1] <- format(xmajtics[1],format=tfmt,time.zone=time.zone)
        }
    }
    else tlabels <- F

    tcl <- par("tcl")
    minor_tcl <- tcl * 0.6

    tlabcex <- tparams$tlabcex

    # check if there is room in margin, if not silently use outer margin
    if (labels && !outer) {
        mex <- par("mex")
        mar <- par("mar")
        lines.avail <- mar[side] * mex / tlabcex	# in units of tlabcex
        label.space <- (par("mgp")[2] + line + 1) * mex / tlabcex
        if (label.space > lines.avail) {
            outer <- TRUE
            line <- -mar[side] + line
        }
    }

    # labels and major tick marks
    axis(side,at=xmajtics-x0, labels=tlabels,tick=tick,
        cex=tlabcex, xaxt="s",line=line,outer=outer,
        hadj=0.5, padj=c(0.6,rep(0,length(tlabels)-1)))

    # minor ticks
    if (tick && is.na(par("tck"))) {
        axis(side,at=tparams$mintics-x0, labels=FALSE,
            tick=tick,tcl=minor_tcl, xaxt="s",...)
    }

    if (labels) {
        if (is.logical(xlab)) {
            if (xlab) xlab <- paste0("Time(", tparams$majunits, "), tic=",
                tparams$minunits)
            else xlab <- ""
        }
        if (xlab != "") title(xlab=xlab,cex=tlabcex)
    }
}

setGeneric("lines")
setMethod("lines",signature(x="nts"),
    function(x,y,...)
    {
        if (!missing(y)) invisible(lines(x@data,y,...))
        else invisible(lines.nts(x,...))
    }
)

lines.nts <- function(x,...)
{
    if (dim(x)[2] > 1) stop("Cannot plot more than one column in a time series. Do lines(x[,j])")

    tx <- x@positions
    if (! exists(".timeaxis_params",envir=.eoltsEnv))
          sc = list(scale=1.,toffset=0.)
    else sc = get(".timeaxis_params",envir=.eoltsEnv)

    tx <- tx - sc$toffset
    lines(tx,x@data,...)
}

setGeneric("points")
setMethod("points",signature(x="nts"),
    function(x,y,...)
    {
        if (!missing(y)) invisible(points(x@data,y,...))
        else invisible(points.nts(x,...))
    }
)

points.nts <- function(x,...)
{
    if (dim(x)[2] > 1) stop("Cannot plot more than one column in a time series. Do points(x[,j])")
    tx <- x@positions

    if (! exists(".timeaxis_params",envir=.eoltsEnv))
          sc = list(scale=1.,toffset=0.)
    else sc = get(".timeaxis_params",envir=.eoltsEnv)

    tx <- tx - sc$toffset
    points(tx,x@data,...)
}

# approximate time label deltas, each about .5 of preceding.
xlabel.deltas <- c(
    # 1:3, 4y           2y           1y        
       4*316224.e2, 2*316224.e2, 316224.e2,
    # 4:8, 6 mon      3 mon  1 mon      14d      7d
       158112.e2, 79056.e2, 30*86400, 14*86400, 7*86400,
    # 9:14, 4d    2d    1d       12h     6h        4h    2h
       4*86400, 2*86400, 86400, 12*3600, 6*3600, 4*3600, 2*3600,
    # 15:22, 1h 30m 10m 5m   2m  1m  30s  10s
       3600, 1800, 600, 300, 120, 60, 30,  10,
    # 23:34 seconds
       5.,  2.,  1.,  .5,  .25,  .1, .05, .025, .01, .005,.0025,.001)
         
# vc=varies, calculated, given first time and length of plot
# vc*=varies, but good enough for plotting, treat as fixed
#           delta    adjusted     ftic                     mdelta
# kt
# 1         4 y       4 year,vc*  Jan 1 of next year,vc     year, vc*
# 2         2 y       2 year,vc*  Jan 1 of next year,vc     4 month, vc*
# 3         1 y       1 year,vc*  Jan 1 of next year,vc     2 month, vc*
# 4         6 m       6 month,vc  day 1 of next month,vc    1 month, vc
# 5         3 m       3 month,vc  day 1 of next month,vc    1 month, vc
# 6         1 m       1 month,vc  day 1 of next month,vc    week, vc
# 7         14 d      same        00:00:00 (LT) of next day, vc 2 day
# 8         7  d      same        00:00:00 (LT) of next day, vc 1 day
# 9         4  d      same        00:00:00 (LT) of next day,vc  1 day
# 10        2  d      same        00:00:00 (LT) of next day,vc  12 hours
# 11        1  d      same        00:00:00 (LT) of next day,vc  6 hours
# 12        12 h      same        00 or 12:00:00 (LT) of next day,vc  3 hours
# 13        4 h       same        *:00:00 (LT) next hour,vc     1 hours
# 14        2 h       same        *:00:00 (LT) next hour,vc     30 min
# 15        1 h       same        *:00:00 (LT) next hour,vc     15 min
# 16-34               same        next even deltat              deltat/n

# Number of minor tics per delta (12 month/year, etc).
xlabel.nminor <- c(
    4, 6, 6,        # 4,2,1 y
    NA, NA, NA,     # 6,3,1 mon, calculated
    7, 7, 4, 4, 4,  # 14d:2d, 7d:1d, 4d:1d, 2d:12h, 1d:6h
    4,6,4,4,4,      # 12h:3h, 6h:1h, 4h:1h, 2h:30min, 1h:15min,
    6,5,5,          # 30min:5min, 10min:2min, 5min:1min,
    4,4,6,5,        # 2min:30sec, 1min:15sec, 30sec:5sec, 10sec:2sec
    5,4,4,          # 5sec:1sec, 2sec:0.5sec, 1sec:0.25sec,
    5,5,4,          # 0.5sec:0.1sec, 0.25sec:0.05sec, 0.1sec:0.025sec,
    5,5,4,          # 0.05sec:0.01sec, 0.025sec:0.005sec, 0.01sec:0.0025sec,
    5,5,4           # 0.005sec:0.001sec, 0.0025sec:0.0005sec, 0.001sec:0.00025sec)
    )

# Units per minor tic.
xlabel.minunits <- c(
     "YEAR", "4MONTH", "2 MONTH",
     "MONTH", "MONTH", "5 DAY",
     "2 DAY", "DAY", "DAY","12 HR","6 HR",
     "3 HR", "HR", "HR", "30 MIN", "15 MIN", "5 MIN", "2 MIN", "MIN",
     "30 SEC", "15 SEC","5 SEC","2 SEC",
     "SEC","0.5 SEC","0.25 SEC",
     "0.1 SEC","0.05 SEC","0.025 SEC",
     "0.01 SEC","0.005 SEC","0.0025 SEC",
     "0.001 SEC","0.0005 SEC","0.00025 SEC")

# format of major tic labels
xlabel.tformats <- c(
    "%Y",	    #  1 YYYY
    "%Y %b %d",     #  2 YYYY MON DAY
    "%b %d %H%M",   #  3    MON DAY HH:MM
    "%b %d %H%M",   #  4    MON DAY HH:MM
    "%H:%M",	    #  5        HH:MM
    "%H:%M:%S",	    #  6        HH:MM:SS
    "%H:%M:%OS1",   #  7        HH:MM:SS.M
    "%H:%M:%OS2",   #  8        HH:MM:SS.MM
    "%H:%M:%OS3",   #  9        HH:MM:SS.MMM
    "%H:%M:%OS4")   #  10       HH:MM:SS.MMMM

# contents of major tic labels
xlabel.majunits <- c(
     "YEAR", "YR MON DAY", "MON DAY HHMM",
     "MON DAY HHMM", "HHMM",
     "HHMMSS", "HHMMSS", "HHMMSS", "HHMMSS", "HHMMSS")

# Format of extra label on first major tic mark, plotted below regular label
xlabel.extraformats <- c(
    "%Z",		#  1 add timezone to year
    "%Z",		#  2 add timezone to date
    "%Y %Z",		#  3 add year, timezone to month,time
    "%Y %Z",		#  4 add year, timezone to month,time
    "%Y %b %d %Z",	#  5 add date to time
    "%Y %b %d %Z",	#  6 add date to time
    "%Y %b %d %Z",	#  7 add date to time
    "%Y %b %d %Z",	#  8 add date to time
    "%Y %b %d %Z",	#  9 add date to time
    "%Y %b %d %Z")	#  10 add date to time

#  Mapping of delta index to format
xlabel.tltype <- c(
    1,1,1,		# 4-1 year          YYYY
    2,2,2,  		# 6,3,1 month       YY MON DAY
    3,3,3,3,3,          # 14day-1day        MON DAY
    4,4,4,4,4,          # 12hour-1hour      MON DAY HHMM
    5,5,5,5,5,          # 30min - 1 min     HHMM
    6,6,6,6,6,          #  30sec-1sec       HHMMSS
    7,	                #  .5sec            HHMMSS.M
    8,               	#  .25sec           HHMMSS.MM
    7,	                #  .1sec            HHMMSS.M
    9,9,9,              # .05 - .01 sec
    10,10,10)	        # .005,.001 sec

timeaxis_setup <- function(t1, t2, time.zone=getOption("time.zone"),
    cex=par("cex"))
{
    # Compute scaling factor and offset for scaling time axis.

    # par("usr") is stored as a float, and an offset must
    # be removed from times so that precision is not lost.

    # Determine nice intervals and a format for labeling an X axis with
    # time labels.

    # Note: on interactive output devices par("pin") is the initial
    # size of the motif window. If the user resizes the window,
    # par("pin") may not change

    # character expansion of x axis labels
    # number of 12 character labels on xaxis
    cwidth <- par("cin")[1]	# character width in inches
    pwidth <- par("pin")[1]	# plot width in inches 

    tlabcex <- cex * par("cex")

    nlab <- pwidth / (cwidth * 12)
    if (nlab < 4) {
        tlabcex <- tlabcex * nlab / 4
        nlab <- 4
    }
    else if (nlab > 6) {
        tlabcex <- tlabcex * nlab / 6
        if (tlabcex > 1.0) tlabcex <- 1.0
        nlab <- 6
    }

    # minimum number of seconds for a time label
    deltat <- (t2 - t1) / nlab

    kt <- length(xlabel.deltas[xlabel.deltas >= deltat])
    if (kt == 0) kt <- 1

    # return:
    #   majtics, mintics (utimes)
    #   majunits, minunits (strings)
    #   tformat, extraformat (strings)

    t1l <- as.list(t1,time.zone=time.zone)
    if (kt < 4) {       # 4,2,1 year major tic
        t1l$year <- t1l$year + 1
        t1l$mon <- t1l$day <- t1l$yday <- 1
        t1l$hour <- t1l$min <- t1l$sec <- 0
        fmajtic <- as(t1l,"utime")
        # since a year isn't exactly 366 days, these aren't exactly correct
        # but close enough for plotting
        majtics <- utime(seq(from=fmajtic,to=t2,by=xlabel.deltas[kt]))
        mindt <- xlabel.deltas[kt] / xlabel.nminor[kt]
        ntic <- floor((fmajtic - t1) / mindt)
        mintics <- utime(seq(from=fmajtic-ntic*mindt,to=t2,by=mindt))
    }
    else if (kt == 4) {         # 6 month major tic, 1 month minor tic
        t1l$day <- t1l$yday <- 1
        t1l$hour <- t1l$min <- t1l$sec <- 0
        ttic <- as(t1l,"utime") + 31 * 86400    # first tic at day 1, next month
        majtics <- mintics <- NULL

        i <- 0
        while (TRUE) {
            t1l <- as.list(ttic, time.zone="time.zone")
            t1l$day <- t1l$yday <- 1
            t1l$hour <- t1l$min <- t1l$sec <- 0
            ttic <- as(t1l,"utime")
            if (ttic > t2) break
            if ((i %% 6) == 0) majtics <- c(majtics, ttic)     # day 1 of next month
            else mintics <- c(mintics, ttic)     # day 1 of next month
            ttic <- ttic + 31 * 86400
            i <- i + 1
        }
        majtics <- utime(majtics)
        mintics <- utime(mintics)
    }
    else if (kt == 5) {         # 3 month major tic, 1 month minor tic
        t1l$day <- t1l$yday <- 1
        t1l$hour <- t1l$min <- t1l$sec <- 0
        ttic <- as(t1l,"utime") + 31 * 86400
        majtics <- mintics <- NULL
        i <- 0
        while (TRUE) {
            t1l <- as.list(ttic, time.zone=time.zone)
            t1l$day <- t1l$yday <- 1
            ttic <- as(t1l,"utime")
            if (ttic > t2) break
            if ((i %% 3) == 0) majtics <- c(majtics, ttic)     # day 1 of next month
            else mintics <- c(mintics, ttic)     # day 1 of next month
            ttic <- ttic + 31 * 86400
            i <- i + 1
        }
        majtics <- utime(majtics)
        mintics <- utime(mintics)
    }
    else if (kt == 6) {         # 1 month major tic, 5 day minor tic
        t1l$day <- t1l$yday <- 1
        t1l$hour <- t1l$min <- t1l$sec <- 0
        ttic <- as(t1l,"utime")
        majtics <- mintics <- NULL

        maxdt <- 31 * 86400 # adjusted to day 1 of each month
        mindt <- 5 * 86400
        nmintics <- floor(maxdt/mindt)

        mintics <- ttic + 1:nmintics * mindt
        mintics <- mintics[mintics > t1]

        ttic <- ttic + 31 * 86400

        while (TRUE) {
            t1l <- as.list(ttic, time.zone=time.zone)
            t1l$day <- t1l$yday <- 1
            t1l$hour <- t1l$min <- t1l$sec <- 0
            ttic <- as(t1l,"utime")
            if (ttic > t2) break
            majtics <- c(majtics, ttic)     # day 1 of next month
            mintics <- c(mintics, ttic + 1:nmintics * mindt)
            ttic <- ttic + maxdt
        }
        majtics <- utime(majtics)
        mintics <- utime(mintics)
    }
    else if (kt < 12) {         # 14 days to 1 day
        t1l$hour <- t1l$min <- t1l$sec <- 0
        # 00:00 of next day
        ttic <- as(t1l,"utime") + 86400

        t1l <- as.list(ttic, time.zone=time.zone)
        t1l$hour <- t1l$min <- t1l$sec <- 0 # in case of daylight time crossover
        ttic <- as(t1l,"utime")
        
        maxdt <- xlabel.deltas[kt]
        mindt <- maxdt / xlabel.nminor[kt]

        majtics <- utime(seq(from=ttic, to=t2, by=maxdt))

        ntic <- floor((ttic - t1) / mindt)
        mintics <- utime(seq(from=ttic-ntic*mindt,to=t2,by=mindt))
    }
    else if (kt == 12) { # 12 h   00 or 12:00:00 (LT) of next day,vc  3 hours
        t1l$hour <- t1l$min <- t1l$sec <- 0
        ttic <- as(t1l,"utime") + 86400
        if (ttic - 12 * 3600 > t1) ttic <- ttic - 12 * 3600

        maxdt <- xlabel.deltas[kt]
        mindt <- maxdt / xlabel.nminor[kt]

        majtics <- utime(seq(from=ttic, to=t2, by=maxdt))
        ntic <- floor((ttic - t1) / mindt)
        mintics <- utime(seq(from=ttic-ntic*mindt,to=t2,by=mindt))
    }
    else if (kt < 16) { # 4 h to 1 h
        t1l$min <- t1l$sec <- 0
        ttic <- as(t1l,"utime") + 3600

        maxdt <- xlabel.deltas[kt]
        mindt <- maxdt / xlabel.nminor[kt]

        majtics <- utime(seq(from=ttic, to=t2, by=maxdt))
        mintics <- utime(seq(from=ceiling(t1/mindt)*mindt, to=t2, by=mindt))
    }
    else { # everything else, simple math
        maxdt <- xlabel.deltas[kt]
        mindt <- maxdt / xlabel.nminor[kt]

        majtics <- utime(seq(from=ceiling(t1/maxdt) * maxdt, to=t2, by=maxdt))
        mintics <- utime(seq(from=ceiling(t1/mindt) * mindt, to=t2, by=mindt))
    }

    labtype <- xlabel.tltype[kt]


    res <- list(toffset=t1, majtics=majtics, mintics=mintics,
        majunits=xlabel.majunits[labtype],
        minunits=xlabel.minunits[kt],
        format=xlabel.tformats[labtype],
        extraformat=xlabel.extraformats[labtype],
        tlabcex=tlabcex)

    assign(".timeaxis_params", res, envir=.eoltsEnv)
    res
}

error.bar.nts <- function(x,lower,upper,incr=T,bar.ends=T,gap=T,add=F,
	horizontal=F,ylim=NULL,...)
{
    n <- nrow(x)
    if (incr) lower <- x - abs(lower)
    else lower <- rep(lower,length=n)

    if (missing(upper)) upper <- 2 * x - lower
    else {
      if (incr) upper <- x + upper
      else upper <- rep(upper,length=n)
    }

    if (is.null(ylim)) ylim <- range(c(lower,upper),na.rm=T)

    if (!add) plot(x,ylim=ylim,...)
    
    if (! exists(".timeaxis_params",envir=.eoltsEnv))
        sc <- list(scale=1.,toffset=0.)
    else sc <- get(".timeaxis_params",envir=.eoltsEnv)

    tx <- x@positions - sc$toffset

    if(gap)
            gap <- 0.75 * par("cxy")[2]
    draw <- upper - x > gap
    segments(tx[draw], x[draw] + gap, tx[draw], upper[draw])
    draw <- x - lower > gap
    segments(tx[draw], x[draw] - gap, tx[draw], lower[draw])
    if(bar.ends) {
            size.bar <- par("cxy")[1]
            segments(tx - size.bar, upper, tx + size.bar, upper)
            segments(tx - size.bar, lower, tx + size.bar, lower)
    }
}
# little function which can be used to create a plot, even
# if all data is NAs. It just generates a point off scale, and
# pretends to plot it.  xlim,ylim must not be NAs
#
plot.nas <- function(xlim,ylim,...)
{
    x <- xlim[1] - .1 * (xlim[2] - xlim[1])
    y <- ylim[1] - .1 * (ylim[2] - ylim[1])
    plot(x,y,xlim=xlim,ylim=ylim,err=-1,...)
}

setGeneric("abline")

# method.skeleton(abline,signature(v="utime"))
setMethod("abline",signature(a="missing",b="missing",h="missing",v="utime"),
    function(v,...)
    {
        # cat("in abline, v=utime\n")
        if (! exists(".timeaxis_params",envir=.eoltsEnv))
            sc <- list(scale=1.,toffset=0.)
        else sc <- get(".timeaxis_params",envir=.eoltsEnv)
        vx <- as.numeric(v) - sc$toffset
        graphics::abline(v=vx,...)
        v
    }
)

setMethod("abline",signature(a="ANY",b="ANY",h="ANY",v="ANY"),
    function(a=NULL,b=NULL,h=NULL,v=NULL,...)
    {
        graphics::abline(a=a,b=b,h=h,v=v,...)
    }
)

tlocator <- function(n=1,type="n",...)
{
    if (n == 1) 
        cat("Use mouse to select a time on the plot ... ")
    else
        cat("Use mouse to select times on the plot.\nClick middle button, or both buttons on two button mouse to terminate ... ")

    if (! exists(".timeaxis_params",envir=.eoltsEnv)) {
        stop("timeaxis_params does not exist.")
    }
    tparams <- get(".timeaxis_params",envir=.eoltsEnv)

    times <- utime(locator(n,type=type,...)$x + tparams$toffset)
    cat("done\n")
    times
}

horiz_legend <- function(x,y,legend,col=NULL,lty=NULL,marks=NULL,
    cex=1.0,bty,xaxt="s",yaxt="s")
{
    # this makes a more compact legend than legend()
    uxy <- par("usr")
    if (is.null(cex)) cex <- par("cex")
    # cxy <- par("cxy") * cex / cex
    cxy <- c(strwidth("X"),strheight("X")) * cex / cex

    # cat("cex=",cex,",cex=",cex,"\n")
    # cat("cxy=",cxy,",dy=",(uxy[4]-uxy[3]),",nrows=",(uxy[4]-uxy[3])/cxy[2],"\n")

    nl <- length(legend)

    if (is.null(col)) col <- rep(1,nl)
    if (is.null(lty)) lty <- rep(1,nl)

    # space for line or point in legend
    ptln.size <- cxy[1]
    # Only put lines in legend if they differ
    if (is.null(marks) && length(unique(lty)) == 1) ptln.size <- 0
    # browser()

    maxlw <- (max(nchar(legend)) + 1) * cxy[1]  # maximum legend width
    if (ptln.size > 0) maxlw <- maxlw + 3 * cxy[1]

    nc <- floor((uxy[2] - uxy[1]) / maxlw)
    nr <- ceiling(nl / nc)

    # Lay it out column by column (takes less vertical space)
    y0 <- y - 1.0 * cxy[2]
    ir <- 0
    putx  <- if(par("xlog")) function(x){10^x} else function(x)x
    puty  <- if(par("ylog")) function(x){10^x} else function(x)x

    for (i in 1:nl) {
        y <- y0 - ir * 1.0 * cxy[2]
        if (ptln.size > 0) {
            if (is.null(marks))
              lines(putx(c(x,x+ptln.size*2)),puty(c(y,y)),col=col[i],lty=lty[i],
                      xaxt=xaxt,yaxt=yaxt,cex=cex)
            else
              points(putx(x+ptln.size),puty(y),col=col[i],pch=marks[i],xaxt=xaxt,yaxt=yaxt,cex=cex)
        }

        text(putx(x+2.5*ptln.size),puty(y),legend[i],col=col[i],adj=0,cex=cex,
            xaxt=xaxt,yaxt=yaxt)
        ir <- ir + 1
        if (ir == nr) {
            nc <- nc - 1        # columns left
            nr <- ceiling((nl-i) / nc)
            ir <- 0
            x <- x + maxlw
        }
    }
    NULL
}

label_times <- function(t1,t2, annotate, adj, col=1, year=T, print=T,
    cex=par("cex"), time.zone=getOption("time.zone"),...)
{
    # function to print start (t1) and end (t2) times of data
    # on the upper left of a plot (or as specified by 'adj')
    # t1 <- start(data)
    # t2 <- end(data)
    t1.list <- as.list(t1, time.zone=time.zone)
    t2.list <- as.list(t2, time.zone=time.zone)
    if (t1.list$mon != t2.list$mon | t1.list$day != t2.list$day |
        t1.list$year != t2.list$year) {
        if (year)
            if (t1.list$year != t2.list$year)
                times <- paste(format(t1, format = "%Y %b %d %H:%M",
                        time.zone=getOption("time.zone")), "-",
                    format(t2, format = "%Y %b %d %H:%M  %Z",
                        time.zone=getOption("time.zone")))
            else
                times <- paste(format(t1, format = "%Y %b %d %H:%M",
                        time.zone=getOption("time.zone")), "-",
                    format(t2, format = "%b %d %H:%M  %Z",
                        time.zone=getOption("time.zone")))
            else
                times <- paste(format(t1, format = "%b %d %H:%M",
                        time.zone=getOption("time.zone")), "-",
                    format(t2, format = "%b %d %H:%M  %Z",
                        time.zone=getOption("time.zone")))
    }
    else {
        if (year)
            times <- paste(format(t1, format = "%Y %b %d, %H:%M",
                    time.zone=getOption("time.zone")), "-",
                format(t2, format = "%H:%M  %Z",
                    time.zone=getOption("time.zone")))
        else
            times <- paste(format(t1, format = "%b %d, %H:%M",
                    time.zone=getOption("time.zone")), "-",
                format(t2, format = "%H:%M  %Z",
                    time.zone=getOption("time.zone")))
    }

    if (!missing(annotate))
        times = paste0(times, ", ", annotate)

    if (print) {
        if (missing(adj))
            adj=0
        par.old = par(col=col)
        on.exit(par(par.old))
        mtext(times, side = 3, adj = adj, line = 0.1,cex=cex,...)
    }
    invisible(times)
}

stamp <- function (string = date(), print = TRUE, plot = TRUE, cex=par("cex"),...) 
{
    if (print) 
        cat(string)
    if (plot && .Device != "null device") {
        mar <- par("mar")[[1]]
        usr <- par("usr")
        plt <- par("plt")
        at <- usr[2] + (usr[2] - usr[1]) * (1 - plt[2]) / (plt[2] - plt[1]) -
            0.6 * strwidth("m",cex=cex)
        mtext(string, side = 1, adj = 1, line = mar - 1, at = at,cex=cex,...)
    }
}

