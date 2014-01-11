# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
#               Copyright (C) by UCAR
# 
setMethod("plot","spectra",
    function(x,y,...)
        if(missing(y)) invisible(plot.spectra(x, ...))
        else {
            dots <- list(...)
            if (hasArg(xlab)) xlab <- dots$xlab
            else xlab <- deparse(substitute(x))
            if (hasArg(ylab)) ylab <- dots$ylab
            else ylab <- deparse(substitute(y))
            invisible(plot(x@data, y@data,xlab=xlab,ylab=ylab, ...))
        }
        )

setMethod("plot","pspectra",
    function(x,y,...)
        if(missing(y)) invisible(plot.spectra(x, ...))
        else {
            dots <- list(...)
            if (hasArg(xlab)) xlab <- dots$xlab
            else xlab <- deparse(substitute(x))
            if (hasArg(ylab)) ylab <- dots$ylab
            else ylab <- deparse(substitute(y))
            invisible(plot(x@data, y@data,xlab=xlab,ylab=ylab, ...))
        }
        )

setMethod("plot","cospectra",
    function(x,y,...)
        if(missing(y)) invisible(plot.spectra(x, ...))
        else {
            dots <- list(...)
            if (hasArg(xlab)) xlab <- dots$xlab
            else xlab <- deparse(substitute(x))
            if (hasArg(ylab)) ylab <- dots$ylab
            else ylab <- deparse(substitute(y))
            invisible(plot(x@data, y@data,xlab=xlab,ylab=ylab, ...))
        }
        )

setMethod("plot","xspectra",
    function(x,y,...)
        if(missing(y)) invisible(plot.spectra(x, ...))
        else {
            dots <- list(...)
            if (hasArg(xlab)) xlab <- dots$xlab
            else xlab <- deparse(substitute(x))
            if (hasArg(ylab)) ylab <- dots$ylab
            else ylab <- deparse(substitute(y))
            invisible(plot(x@data, y@data,xlab=xlab,ylab=ylab, ...))
        }
        )

setMethod("plot","quadspectra",
    function(x,y,...)
        if(missing(y)) invisible(plot.spectra(x, ...))
        else {
            dots <- list(...)
            if (hasArg(xlab)) xlab <- dots$xlab
            else xlab <- deparse(substitute(x))
            if (hasArg(ylab)) ylab <- dots$ylab
            else ylab <- deparse(substitute(y))
            invisible(plot(x@data, y@data,xlab=xlab,ylab=ylab, ...))
        }
        )

plot.spectra <- function(x,log,multfreq=T,falloff,
    grid=F,type="b",xlab,ylab,xlim,ylim,
    xaxt,yaxt,tlwd=par("lwd"),col,logo=T,mgp,
    ...)
{

    # tlwd: trace line width.  Use that option if you want to
    # 	change the trace line width without changing the line width
    #	of the axes.

    # axt.old <- par("xaxt","yaxt")
    # on.exit(par(axt.old))

    args <- list(...)

    class.x <- class(x)
    # cat("class.x=",class.x,"\n")
    if (missing(log)) {
        if (any(class.x == "cospectra") || any(class.x == "xspectra") ||
            any(class.x == "quadspectra")) logarg <- "x"
    else if (any(class.x == "spectra") || any(class.x == "pspectra"))
        logarg <- "xy"
    else logarg <- "x"		# catch all
    }
    else logarg <- log

    if (missing(falloff)) falloff <- logarg == "xy"

    logx <- any(substring(logarg,1:nchar(logarg),1:nchar(logarg)) == "x")
    logy <- any(substring(logarg,1:nchar(logarg),1:nchar(logarg)) == "y")

    if (par("xaxt") != "n") par(xaxt=ifelse(logx,"l","s"))
    if (missing(xaxt)) xaxt <- par("xaxt")
    if (par("yaxt") != "n") par(yaxt=ifelse(logy,"l","s"))
    if (missing(yaxt)) yaxt <- par("yaxt")

    if (missing(mgp)) {
        # hack: if mgp is the default 3,1,0, tighten it up
        # to  c(1.5,0,0). We're adding an extra time label
        # below the X axis (and I think c(3,1,0) is too much space).
        mgp <- par("mgp")
        if (identical(mgp,c(3,1,0))) mgp <- c(1.5,0,0)
    }
    cat("mgp=",paste(mgp,collapse=","),"\n")

    mfg <- par("mfg")

    nplotted <- (mfg[1] - 1) * mfg[4] + mfg[2]
    nplot <- mfg[3] * mfg[4]
    if (nplotted == nplot) nplotted <- 0
    # cat("nplotted=",nplotted," nplot=",nplot," mfg=",mfg,"\n")
    last.plot <- nplotted == nplot - 1

    nc <- ncol(x)
    dns <- dimnames(x)[[2]]

    if (missing(multfreq) & class.x == "phase") {
        multfreq <- F
        units <- unique(x@units)
        if (units[1] == "") x <- x*(180/pi)
        x@units <- rep("deg",ncol(x))
    }
    if (missing(xlab)) xlab <- paste("f (",x@funits,")",sep="")

    units <- unique(x@units)
    if (length(units) > 1) {
        units <- paste(units,collapse=",")
        warning(paste("more than one units to be plotted on one axis:",
                units))
    }

    if (missing(ylab)) {
        # If multiplying by frequency (1/s) then either remove trailing
        # " s" from units or add "/s".
        if (multfreq) {
            sec <- regexpr(" s$",units)
            units <- ifelse(sec < 0,paste(units,"/s"),substring(units,1,sec-1))
        }
        if (nc == 1) {
            if (multfreq) ylab <- paste("f * S(",dns,") (",units,")",sep="")
            else ylab <- paste("S(",dns,") (",units,")",sep="")
        }
        else {
            if (multfreq) ylab <- paste("f * S (",units,")",sep="")
            else ylab <- paste("S (",units,")",sep="")
        }
    }

    # We want to do our own log labels
    if (logx) {
        xaxt <- "n"
        nonposf <- x@frequencies <= 0.
        if (any(nonposf)) x@frequencies[nonposf] <- NA
    }
    if (missing(xlim))
        frange <- range(x@frequencies,na.rm=T)
    else
        frange <- xlim

    if (multfreq) x <- x * x@frequencies

    if (logy) {
        yaxt <- "n"
        nonposy <- !is.na(x@data) & x@data <= 0.
        if (any(nonposy)) x@data[nonposy] <- NA
    }

    if (missing(ylim))
        yrange <- range(x@data,na.rm=T)
    else
        yrange <- ylim

    if (missing(col)) {
        if (nc > 1) col <- 2:(nc+1)
        else col <- par("col")
    }

    # cat("yrange=",yrange,"\n")
    #
    if (any(!is.na(x@data[,1]))) {
        plot(x@frequencies,x@data[,1],log=logarg,type="n",
            xlab=xlab,ylab=ylab,
            xaxt=xaxt,yaxt=yaxt,
            xlim=frange,ylim=yrange,col=1,mgp=mgp,...)
    }
    else {
        if (all(is.na(yrange))) yrange <- c(.001,.01)  # fake it
        ytmp <- yrange[1] - .1 * (yrange[2] - yrange[1])
        ytmp <- rep(ytmp,length(x@frequencies))
        plot(x@frequencies,ytmp,log=logarg,type="n",
            xlab=xlab,ylab=ylab,
            xaxt=xaxt,yaxt=yaxt,
            xlim=frange,ylim=yrange,col=1,err=-1,mgp=mgp,...)
    }

    # colv <- 2:(nc+1)
    legv <- dns
    ncolor <- length(col)
    for (ic in 1:nc) {
        par(new=T)
        if (any(!is.na(x@data[,ic]))) {
            # browser()
            plot(x@frequencies,x@data[,ic],log=logarg,axes=F,xaxt=xaxt,yaxt=yaxt,
                xlab="",ylab="",col=col[(ic-1)%%ncolor+1],
                xlim=frange,ylim=yrange,type=type,lwd=tlwd,...)
        }
    }

    tlab <- paste(format(start(x),format="%Y %b %d %H:%M:%OS"),"-",format(end(x),format="%Y %b %d %H:%M:%OS %Z"))
    mtext(tlab,side=1,line=mgp[1]+1,adj=0.5)

    lim <- par("usr")

    if ((is.numeric(falloff) || (is.logical(falloff) && falloff)) && logx && logy) {
        if (frange[1] < 1 && frange[2] > 1) xm <- 1
        else xm <- (frange[2] + frange[1]) / 2

        fd <- abs(x@frequencies - xm)
        i <- seq(along=x@frequencies)[!is.na(x@frequencies) &
            fd == min(fd,na.rm=T)][1]

        xm <- log10(x@frequencies[i])
        ym <- mean(log10(x@data[i,]),na.rm=T)

        if (is.numeric(falloff)) m <- falloff
        else {
            if (any(class.x == "cospectra")) {
                m <- if(multfreq) -4/3 else -7/3
            }
            else m <- if(multfreq) -2/3 else -5/3
            cat("falloff slope line=",signif(m,digits=3),"\n")
        }
        if (!is.na(ym)) {
            b <- ym - m * xm
            if (!is.na(b) && !is.na(m)) abline(b,m,col=1,lty=2)
        }
    }

    # cat("logarg=",logarg,"lim=",lim,"logx=",logx,"logy=",logy,"xaxt=",xaxt,"yaxt=",yaxt,"\n")
    if (nc > 1) {
        if (is.null(args$cex)) cex <- par("cex")
        else cex <- args$cex
        lx <- lim[1] + strheight("X") * 1.0
        ly <- lim[4] - strwidth("X") * 1.0
        colv <- col
        if (length(colv) < nc) colv <- rep(colv,(nc/length(colv))+1)
        # browser()
        horiz.legend(lx,ly,legv,col=colv,lty=rep(1,length(colv)),bty="n",cex=cex*1.0)
    }

    assign("spectra.multfreq",multfreq,envir=.eoltsEnv);
    # assign("spectra.logx",logx,0);
    # assign("spectra.logy",logy,0);

    # lim <- par("usr")
    # cat("logarg=",logarg,"lim=",lim,"xaxt=",par("xaxt"),"yaxt=",par("yaxt"),"\n")
    plot_spectra_tics(lim[1:2],lim[3:4],grid=grid,logx,logy,col=1,mgp=mgp)
    if (logo && last.plot && exists("logo_stamp",mode="function")) {
        fls <- get("logo_stamp",mode="function")
        fls()
    }

    invisible()
}

setMethod("lines","spectra",
    function(x,y,...)
        if(missing(y)) invisible(lines.spectra(x, ...))
        else invisible(lines(x@data, y@data, ...))
        )

lines.spectra <- function(x,xaxt=par("xaxt"),...)
{
    if (ncol(x) > 1) stop("lines cannot plot more than one column of spectra")

    multfreq <- get("spectra.multfreq",envir=.eoltsEnv)
    if (multfreq) x <- x * x@frequencies

    if (xaxt == "l") {
        nonposf <- x@frequencies <= 0.
        x@frequencies[nonposf] <- NA
    }
    lines(x@frequencies,x@data,xaxt=xaxt,...)
}

setMethod("points","spectra",
    function(x,y,...)
        if(missing(y)) invisible(points.spectra(x, ...))
        else invisible(points(x@data, y@data, ...))
        )

points.spectra <- function(x,xaxt=par("xaxt"),...)
{
    if (ncol(x) > 1) stop("points cannot plot more than one column of spectra")

    multfreq <- get("spectra.multfreq",envir=.eoltsEnv)
    if (multfreq) x <- x * x@frequencies

    if (xaxt == "l") {
        nonposf <- x@frequencies <= 0.
        x@frequencies[nonposf] <- NA
    }
    points(x@frequencies,x@data,xaxt=xaxt,...)
}

plot_spectra_tics <- function(xs,ys,grid,logx,logy,col=1,mgp)
{

    if (logx) {
        l0 <- ceiling(xs[1])
        ln <- floor(xs[2])
        pl <- unique(trunc(pretty(c(l0,ln))))
        pl <- pl[pl>=l0]
        pl <- pl[pl<=ln]
        axis(1,at=10^(l0:ln),labels=F,tck=.03,xaxt="s",yaxt="s",col=col)
        for (i in pl) 
            mtext(substitute(10^i),side=1,line=mgp[2]+0.2,at=10^i,adj=0.5)
        axis(3,at=10^(l0:ln),labels=F,tck=.03,xaxt="s",yaxt="s",col=col)

        if (!missing(grid) && grid) abline(v=10^(l0:ln),lty=2,col=col)

        l0 <- floor(xs[1])
        ln <- ceiling(xs[2])
        ndec <- ln - l0
        mtics <- 10^(l0 + as.vector(outer(log10(2:9),(0:ndec-1),"+")))
        axis(1,at=mtics,labels=F,tck=.01,xaxt="s",yaxt="s",col=col)
        axis(3,at=mtics,labels=F,tck=.01,xaxt="s",yaxt="s",col=col)

    }
    else {
        if (!missing(grid) && grid) {
            xaxp <- par("xaxp")
            abline(v=seq(from=xaxp[1],to=xaxp[2],length=xaxp[3]+1),lty=2,col=col)
        }
    }


    if (!missing(ys)) {
        if (logy) {
            l0 <- ceiling(ys[1])
            ln <- floor(ys[2])
            pl <- unique(trunc(pretty(c(l0,ln))))
            pl <- pl[pl>=l0]
            pl <- pl[pl<=ln]
            #      axis(2,at=10^(l0:ln),labels=10^(l0:ln),tck=.03,srt=90,xaxt="s",yaxt="s",col=col)
            axis(2,at=10^(l0:ln),labels=F,tck=.03,srt=90,xaxt="s",yaxt="s",col=col)
            for (i in pl)
                mtext(substitute(10^i),side=2,line=mgp[2],at=10^i,adj=0.5,srt=90)
            axis(4,at=10^(l0:ln),labels=F,tck=.03,xaxt="s",yaxt="s",col=col)

            if (!missing(grid) && grid) abline(h=10^(l0:ln),lty=2,col=col)

            l0 <- floor(ys[1])
            ln <- ceiling(ys[2])
            ndec <- ln - l0
            mtics <- 10^(l0 + as.vector(outer(log10(2:9),(0:ndec-1),"+")))
            axis(2,at=mtics,labels=F,tck=.01,xaxt="s",yaxt="s",col=col)
            axis(4,at=mtics,labels=F,tck=.01,xaxt="s",yaxt="s",col=col)
        }
        else {
            if (!missing(grid) && grid) {
                yaxp <- par("yaxp")
                abline(h=seq(from=yaxp[1],to=yaxp[2],length=yaxp[3]+1),lty=2,col=col)
            }
            # Zero axis on linear plots
            if (ys[1]*ys[2] < 0) abline(h=0.0,lty=2)
        }
    }
}
