# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
#               Copyright (C) by UCAR
# 
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

plot.nts <- function(x,type="l",xlab,xlim,ylab,ylim,
	xaxs="i",xaxt,yaxt,
	tck=par("tck"),
        col,pch,lty,time.zone=x@time.zone,axes=T,log="",...)
{

    # get the axes argument. 
    # The axes argument is separate, not part of ..., because
    # we pass ... to points(), and axes is not a valid
    # argument for points.
    plotaxes <- axes

    args <- list(...)

    if (is.null(cex <- args$cex)) cex <- par("cex")

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
        if (! exists(".plot.nts.scale",envir=.eoltsEnv)) {
            stop("plot.nts.scale does not exist. Cannot plot with xaxs='d'")
        }
        plot.nts.scale <- get(".plot.nts.scale",envir=.eoltsEnv)
        scalef <- plot.nts.scale$scale
        x0 <- plot.nts.scale$off
        xlim.scaled <- par("usr")[c(1,2)]
        xrange <- xlim.scaled * scalef + x0
    }
    else {
        if (missing(xlim)) xrange <- range(tx)
        else {
            xrange <- xlim
            x <- x[utime(xlim),]
            tx <- x@positions
        }
    }

    xaxs <- "i"

    cwidth <- par("cin")[1]	# character width in inches
    pwidth <- par("pin")[1]	# plot width in inches 

    # Note: If the output device is motif(), par("pin") is the initial
    # size of the motif window. If the user resizes the window,
    # par("pin") does not change

    # character expansion of x axis labels
    # number of 12 character labels on xaxis
    tlabcex <- par("cex")
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
    if (is.null(cex)) cex <- tlabcex

    tlab <- xlabel.nts((xrange[2] - xrange[1]) / nlab)
    # cat("cwidth=",cwidth," pwidth=",pwidth," nlab=",nlab,
    #   " tlabcex=",tlabcex," cex=",par("cex"),"\n")

    if (missing(xlab)) {
        # No sense in time labels without axis
        if (plotaxes && xaxt != "n")
            xlab <- paste(sep="","Time(",tlab$units,"), tic=",tlab$munits)
        else xlab <- ""
    }
    else if (is.logical(xlab)) {
        if (xlab) xlab <- paste(sep="","Time(",tlab$units,"), tic=",tlab$munits)
        else xlab <- ""
    }
    if (missing(ylab)) {
        if (nc > 1) {
            xexpr <- substitute(x)
            ylab <- deparse(xexpr)
        }
        else {
            if (is.null(dunits <- attr(x,"dunits")))
                ylab <- dimnames(x)[[2]][1]
            else
                ylab <- paste(dimnames(x)[[2]][1],"(",attr(x,"dunits")[1],")",sep="")
        }
    }
    xrange <- switch(xaxs,
          # regular axis, extend axis to next minor tic mark
          r=c((ceiling(xrange[1] / tlab$mdelta)-1) * tlab$mdelta,
            (floor(xrange[2] / tlab$mdelta)+1) * tlab$mdelta),
          # Standard and extended axis, expand to next major tic mark
          s=c((ceiling(xrange[1] / tlab$fdelta)-1) * tlab$fdelta,
            (floor(xrange[2] / tlab$fdelta)+1) * tlab$fdelta),
          e=c((ceiling(xrange[1] / tlab$fdelta)-1) * tlab$fdelta,
            (floor(xrange[2] / tlab$fdelta)+1) * tlab$fdelta),
          # internal axis labels, leave limits alone.
          i=xrange,
          # direct axis, don't change limits
          d=xrange,
          stop("Invalid value for xaxs"))

    # Scale the data, since par("usr") seems to be stored as float
    if (xaxs != "d") {
        scalef <- tlab$delta
        x0 <- xrange[1]
        xlim.scaled <- (xrange-x0)/scalef
    }

    if (all.is.na) plot.nas(type=type,axes=plotaxes,xlim=xlim.scaled,xlab="",
          ylim=ylim,ylab=ylab,xaxs=xaxs,xaxt="n",yaxt=yaxt,tck=tck,
          col=col[1],pch=pch[1],lty=lty[1],...)
    else if (nc > 1) 
        plot((tx-x0)/scalef,x@data[,1],type=type,axes=plotaxes,
            xlim=xlim.scaled,xlab="",ylim=ylim,ylab=ylab,xaxs=xaxs,
            xaxt="n",yaxt=yaxt,tck=tck,col=col[1],pch=pch[1],lty=lty[1],log=log,...)
    else
        plot((tx-x0)/scalef,x@data,type=type,axes=plotaxes,
            xlim=xlim.scaled,xlab="",ylim=ylim,ylab=ylab,xaxs=xaxs,
            xaxt="n",yaxt=yaxt,tck=tck,col=col[1],pch=pch[1],lty=lty[1],log=log,...)

    if (plotaxes) {
        if (xaxt != "n") {
            # First major tick
            xftic <- ceiling(xrange[1] / tlab$fdelta) * tlab$fdelta
            xtics <- utime(seq(from=xftic,to=xrange[2],by=tlab$delta))
            tlabels <- format(xtics,format=tlab$format,time.zone=time.zone)

            # Add timezone to middle label
            mid <- length(tlabels) / 2 + 1
            tfmt <- paste(tlabels[mid],"%Z")
            tlabels[mid] <- format(xtics[mid],format=tfmt)

            # labwidth <- min(nchar(tlabels))
            # nlab <- pwidth / (cwidth * labwidth * 2)

            # if (nlab < 4) tlabcex <- par("cex") * nlab / 4
            # else if (nlab > 10) {
            #   tlabcex <- par("cex") * nlab / 10
            #   if (tlabcex > 1.0) tlabcex <- 1.0
            #  }

            # cat("cwidth=",cwidth," pwidth=",pwidth," nlab=",nlab,
            # 	" tlabcex=",tlabcex," labwidth=",labwidth,"\n")

            if (xlab != "" && x0 > 86400.0 && tlab$extraformat != "") {
                tfmt <- paste(sep="",tlabels[1],"\n",tlab$extraformat)
                tlabels[1] <- format(xtics[1],format=tfmt)
            }

            # User can set tck to 1 to get grid lines
            # default is NA
            if (is.na(tck)) tck = -0.01
            mtck <- 2 * tck
            if (tck > .5) mtck <- tck

            outer <- F
            line <- 0
            mex <- par("mex")
            mar <- par("mar")
            lines.avail <- mar[1] * mex / cex		# in units of cex
            label.space <- (par("mgp")[2] + line + 1) * mex / cex

            if (label.space > lines.avail) {
                outer <- T
                line <- -mar[1] + line
                mtck <- mtck / par("mfrow")[1]
            }

            # Try to make major tic marks noticeable
            axis(1,at=(xtics-x0)/scalef,labels=tlabels,tck=mtck,cex=cex,xaxt="s",
              outer=outer,line=line,hadj=0.5,padj=c(0.6,rep(0,length(tlabels)-1)))
            axis(3,at=(xtics-x0)/scalef,labels=F,tck=mtck,xaxt="s")

            if (mtck < 1) {
                # First minor tick
                xfmtic <- xftic - tlab$delta
                xmtics <- seq(from=xfmtic,to=xrange[2],by=tlab$mdelta)
                axis(1,at=(xmtics-x0)/scalef,labels=F,tck=tck,xaxt="s")
                axis(3,at=(xmtics-x0)/scalef,labels=F,tck=tck,xaxt="s")
            }
        }
    }

    plot.nts.scale <- list(scale=scalef,off=x0,tlab=tlab,cex=cex)
    assign(".plot.nts.scale",plot.nts.scale,envir=.eoltsEnv)

    if (plotaxes && xlab != "") timelabel(xlab=xlab,cex=cex)

    if (nc > 1) {
        ncolor <- length(col)
        nlty <- length(lty)
        npch <- length(pch)

        for (i in 2:nc) points(x[,i],col=col[(i-1)%%ncolor+1],
            pch=pch[(i-1)%%npch+1],type=type,...)
    }
   
    invisible(plot.nts.scale)
}
timeaxis <- function(side,labels=T,tick=T,time.zone,
	cex=NULL,date.too=F,line=0,outer=F,...)
{
    if (! exists(".plot.nts.scale",envir=.eoltsEnv)) {
        stop("plot.nts.scale does not exist. Cannot add time axis to ordinary plot")
    }
 
    plot.nts.scale = get(".plot.nts.scale",envir=.eoltsEnv)

    # User can set tck to 1 to get grid lines
    tck <- par("tck")
    if (is.na(tck)) tck = -0.01
    mtck <- 2 * tck
    if (tck > .5) mtck <- tck

    tlab <- plot.nts.scale$tlab
    if (is.null(cex)) cex <- plot.nts.scale$cex
    # cat("cex=",cex," plot.nts.scale$cex=",plot.nts.scale$cex,"\n")
    scalef <- plot.nts.scale$scale

    x0 <- plot.nts.scale$off
    xlim.scaled <- par("usr")[c(1,2)]
    xrange <- xlim.scaled * scalef + x0

    # First major tick
    xftic <- ceiling(xrange[1] / tlab$fdelta) * tlab$fdelta
    xtics <- utime(seq(from=xftic,to=xrange[2],by=tlab$delta))

    if (labels) {
        tlabels <- format(xtics,format=tlab$format,time.zone=time.zone)
        # Add timezone to middle label
        mid <- length(tlabels) / 2 + 1
        tfmt <- paste(tlabels[mid],"%Z")
        tlabels[mid] <- format(xtics[mid],format=tfmt,time.zone=time.zone)
        if (date.too && tlab$extraformat != "") {
            tfmt <- paste(sep="",tlabels[1],"\n",tlab$extraformat)
            tlabels[1] <- format(xtics[1],format=tfmt,time.zone=time.zone)
        }
    }
    else tlabels <- F

    # check if there is room in margin, if not silently use outer margin
    if (labels && !outer) {
        mex <- par("mex")
        mar <- par("mar")
        lines.avail <- mar[side] * mex / cex	# in units of cex
        label.space <- (par("mgp")[2] + line + 1) * mex / cex
        if (label.space > lines.avail) {
            outer <- T
            line <- -mar[side] + line
            mtck <- mtck / par("mfrow")[1]
        }
    }

    # Try to make major tic marks noticable
    axis(side,at=(xtics-x0)/scalef,labels=tlabels,tick=tick,tck=mtck,
          cex=cex,xaxt="s",line=line,outer=outer,...)

    if (tick && mtck < 1) {
        # First minor tick
        xfmtic <- xftic - tlab$delta
        xmtics <- seq(from=xfmtic,to=xrange[2],by=tlab$mdelta)
        axis(side,at=(xmtics-x0)/scalef,labels=F,tick=tick,tck=tck,xaxt="s",...)
    }
}
timelabel <- function(side=1,outer=F,line=par("mgp")[1],cex=NULL,xlab=NULL)
{
    if (! exists(".plot.nts.scale",envir=.eoltsEnv)) {
        stop("plot.nts.scale does not exist. Cannot add time label to ordinary plot")
    }
 
    plot.nts.scale = get(".plot.nts.scale",envir=.eoltsEnv)

    if (is.null(cex)) cex <- plot.nts.scale$cex
    if (is.null(xlab)) {
        tlab <- plot.nts.scale$tlab
        xlab <- paste(sep="","Time(",tlab$units,"), tic=",tlab$munits)
    }

    # check if there is room in margin, if not silently use outer margin
    if (!outer) {
        mex <- par("mex")
        mar <- par("mar")
        lines.avail <- mar[side] * mex / cex
        label.space <- (line + 1) * mex / cex
        if (label.space > lines.avail) {
          outer <- T
          line <- -mar[side] + line
        }
    }

    mtext(xlab,side=side,line=line,outer=outer,cex=cex)
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
    if (! exists(".plot.nts.scale",envir=.eoltsEnv))
          sc = list(scale=1.,off=0.)
    else sc = get(".plot.nts.scale",envir=.eoltsEnv)

    tx <- (tx - sc$off) / sc$scale
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

    if (! exists(".plot.nts.scale",envir=.eoltsEnv))
          sc = list(scale=1.,off=0.)
    else sc = get(".plot.nts.scale",envir=.eoltsEnv)

    tx <- (tx - sc$off) / sc$scale
    points(tx,x@data,...)
}
xlabel.nts <- function(deltat)
{
    # Determine nice intervals and a format for labeling an X axis with
    # time labels.
    # 
    #
    # deltat:  Minimum width in seconds of a time label.
    #	   For example, if a plot is 6 inches wide, representing	
    #	   360 seconds, and a character is 1/6 inch wide, then
    #	   each character is 10 seconds wide.  A time label is
    #	   about 8 character, so deltat should be 80.
    # Time label deltas, each about .5 of preceding.
    tdel <- c(
        #  10y
           315576.e3,
        #  365.25d   100d    50d    20d
           315576.e2,864.e4,432.e4,1728.e3,
        #  10d    5d     2d      1d    12h     6h     4h     2h
           864.e3,432.e3,1728.e2,864.e2,432.e2,216.e2,144.e2,72.e2,
        #  1h    30m   20m  10m  5m    2m   1m   30s  20s   10s
           36.e2,18.e2,12.e2,6.e2,3.e2,12.e1,6.e1,3.e1,2.e1,1.e1,
        #  5s   2s  1s   1/2s  1/4s  1/10s
           5.,  2.,  1.,  .5,  .25,  .1, .05, .025, .01, .005,.0025,.001)
         
    #  Delta before first labeled tic mark
    tdelfirst <- c(
        # 10y
           315576.e3,
        # 365.25d   100d:10 50d:10 20d:1
          315576.e2,864.e3,864.e3, 864.e2,
           
        # 10d:1d 5d:1d  2d:1d   1d    12h:6h  6h:1h  4h:1h  2h
          864.e2,864.e2, 864.e2,864.e2,216.e2, 36.e2, 36.e2,36.e2,
           
        # 1h    30m   15m:5 10m:5 5m:1 2m:1m 1m  30s  20s:10 10s:5
          36.e2,18.e2, 3.e2,3.e2,6.e1, 6.e1,6.e1,3.e1,1.e1, 5.e0,
           
        # 5s  2s:1 1s   1/2s  1/4s   1/10s
          5.,  2.,  1.,  .5,  .25,  .1, .05, .025, .01, .005,.0025,.001)
   
    # Number of minor tics between tdels (12 month/year, etc).
    tminor <- c(			#	   1m
      10,12,5,5,4,5,5,4,6,6,6,4,4,6,6,4,5,5,4,6, 6,4,5,5,4,4,5,5,5,5,5,5,5,5,5)

    # Units per minor tic.
    mtunits <- c(
         "YEAR","MONTH","20DAY","10DAY","5DAY","2DAY","1DAY","12HR","4HR","2HR",
         "1HR","1HR","30MIN","10MIN","5MIN","5MIN","2MIN","1MIN","30SEC",
         # 1m
         "10SEC","5SEC","5SEC","2SEC","1SEC",".5SEC",".25SEC",".1SEC",
         ".05SEC",".02SEC",".01SEC",".005SEC",".002SEC",".001SEC",".0005SEC",
         ".0002SEC")

    tunits <- c(
       "YEAR","DATE","DATE","DATE HHMM","HHMM","HHMMSS",
       "HHMMSS","HHMMSS","HHMMSS","HHMMSS")
   
    tformats <- c(
       "%Y",		#  1 YYYY
       "%Y %b %d",      #  2 YYYY MON DAY
       "%b %d",		#  3    MON DAY
       "%b %d %H%M",    #  4    MON DAY HRMM
       "%H%M",		#  5        HHMM
       "%H%M%S",	#  6        HHMMSS
       "%H%M%S",	#  7        HHMMSS
       "%H%M%OS1",	#  8        HHMMSS.M
       "%H%M%OS2",	#  9        HHMMSS.MM
       "%H%M%OS3",	#  10       HHMMSS.MMM
       "%H%M%OS4")	#  11       HHMMSS.MMMM

    # Format of extra label on first major tic mark, plotted below regular label
    extraformats <- c(
       "%Z",		#  1 YYYY
       "%Z",		#  2 YYYY MON DAY
       "%Y %Z",		#  3      MON DAY
       "%Y %Z",		#  4      MON DAY HHMM
       "%Y %b %d %Z",	#  5              HHMM
       "%Y %b %d %Z",	#  6              HHMMSS
       "%Y %b %d %Z",	#  7              HHMMSS
       "%Y %b %d %Z",	#  8              HHMMSS.M
       "%Y %b %d %Z",	#  9              HHMMSS.MM
       "%Y %b %d %Z",	#  10             HHMMSS.MMM
       "%Y %b %d %Z")	#  11             HHMMSS.MMMM

    #  Mapping of delta index to format
    tltype <- c(
                      1,			# 10year            YY
                      1,2,2,2,4,		# 365.25day-10day   YY MON DAY
                      4,4,4,		#  5day-1day        MON DAY
                      4,5,5,		# 12hour-4hour      MON DAY HHMM
                      5,5,5,5,5,5,5,5,	#  2hour-1min       HHMM
                      6,6,6,		#  30sec-10sec      HHMMSS
                      7,7,7,		#   5sec-1sec       HHMMSS
                                8,	#  .5sec            HHMMSS.M
                                9,	#  .25sec           HHMMSS.MM
                                8,	#  .1sec            HHMMSS.M
                                10,10,10,10,	# .05 - .005 sec
                                11,11)	# .0025,.001 sec


    kt <- length(tdel[tdel >= deltat])

    delmaj <- tdel[kt]
    delmin <- delmaj / tminor[kt]
    delfirst <- tdelfirst[kt]
    type <- tltype[kt]
    tformat <- tformats[type]
    tunits <- tunits[type]
    mtunits <- mtunits[kt]


    # Number of digits in fractional seconds. Not needed with %nF format
    # if (type > 7) fsecdig <- type - 7
    # fsecdig <- 0

    return (list(delta=delmaj,mdelta=delmin,fdelta=delfirst,
                 format=tformat,units=tunits,munits=mtunits,
                 extraformat=extraformats[type]))
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
    
    if (! exists(".plot.nts.scale",envir=.eoltsEnv))
        sc <- list(scale=1.,off=0.)
    else sc = get(".plot.nts.scale",envir=.eoltsEnv)

    tx <- (x@positions - sc$off) / sc$scale

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
        if (! exists(".plot.nts.scale",envir=.eoltsEnv))
            sc <- list(scale=1.,off=0.)
        else sc = get(".plot.nts.scale",envir=.eoltsEnv)
        vx <- (as.numeric(v) - sc$off) / sc$scale
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

    if (! exists(".plot.nts.scale",envir=.eoltsEnv)) {
        stop("plot.nts.scale does not exist.")
    }
    plot.nts.scale <- get(".plot.nts.scale",envir=.eoltsEnv)

    times <- utime(locator(n,type=type,...)$x * plot.nts.scale$scale + plot.nts.scale$off)
    cat("done\n")
    times
}

horiz_legend <- function(x,y,legend,col=NULL,lty=NULL,marks=NULL,cex=par("cex"),bty,xaxt="s",yaxt="s")
{
    # this makes a more compact legend than legend()
    uxy <- par("usr")
    parcex <- par("cex")
    # cxy <- par("cxy") * cex / parcex
    cxy <- c(strwidth("X"),strheight("X")) * cex / parcex

    # cat("cex=",cex,",parcex=",parcex,"\n")
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
                      xaxt=xaxt,yaxt=yaxt)
            else
              points(putx(x+ptln.size),puty(y),col=col[i],pch=marks[i],xaxt=xaxt,yaxt=yaxt)
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

label_times <- function(t1,t2, annotate, adj, col=1, year=T, print=T,cex=par("cex"),...)
{
    # function to print start (t1) and end (t2) times of data
    # on the upper left of a plot (or as specified by 'adj')
    # t1 <- start(data)
    # t2 <- end(data)
    t1.list <- as(t1,"list")
    t2.list <- as(t2,"list")
    if (t1.list$mon != t2.list$mon | t1.list$day != t2.list$day |
        t1.list$year != t2.list$year) {
        if (year)
            if (t1.list$year != t2.list$year)
                times <- paste(format(t1, format = "%Y %b %d %02H:%02M",
                        time.zone=unlist(options("time.zone"))), "-",
                    format(t2, format = "%Y %b %d %02H:%02M  %Z",
                        time.zone=unlist(options("time.zone"))))
            else
                times <- paste(format(t1, format = "%Y %b %d %02H:%02M",
                        time.zone=unlist(options("time.zone"))), "-",
                    format(t2, format = "%b %d %02H:%02M  %Z",
                        time.zone=unlist(options("time.zone"))))
            else
                times <- paste(format(t1, format = "%b %d %02H:%02M",
                        time.zone=unlist(options("time.zone"))), "-",
                    format(t2, format = "%b %d %02H:%02M  %Z",
                        time.zone=unlist(options("time.zone"))))
    }
    else {
        if (year)
            times <- paste(format(t1, format = "%Y %b %d, %02H:%02M",
                    time.zone=unlist(options("time.zone"))), "-",
                format(t2, format = "%02H:%02M  %Z",
                    time.zone=unlist(options("time.zone"))))
        else
            times <- paste(format(t1, format = "%b %d, %02H:%02M",
                    time.zone=unlist(options("time.zone"))), "-",
                format(t2, format = "%02H:%02M  %Z",
                    time.zone=unlist(options("time.zone"))))
    }

    if (!missing(annotate))
        times = paste(times, ", ", annotate, sep="")

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

