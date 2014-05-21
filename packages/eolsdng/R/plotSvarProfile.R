# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsdng" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

plotSvarProfile <- function(sdngs,xname,yname,type="b",
    xlim=NULL,xlab,xaxt=par("xaxt"),xaxs=par("xaxs"),
    ylim=NULL,ylab,yaxt=par("yaxt"),yaxs=par("yaxs"),
    col=c("black","red","green","blue","purple","cyan",
        "orange","gold","gray","pink","violet","brown","magenta",
        "chocolate","tomato","darkgreen"),
    title="", tlwd=par("lwd"),...)

{
    # plot profiles of one variable from one or more soundings.

    make_plot <- TRUE
    ns <- length(sdngs)

    xlim1 <- c(Inf,-Inf)
    ylim1 <- c(Inf,-Inf)

    for (sname in names(sdngs)) {

        x <- sdngs[[sname]]
        vnames <- colnames(x)

        if (is.na(match(xname,vnames))) stop(paste(xname,"not found in",sname))
        if (is.na(match(yname,vnames))) stop(paste(yname,"not found in",sname))

        xunits <- eolts::units(x[,xname])
        yunits <- eolts::units(x[,yname])

        xdata <- x[,xname]
        ydata <- x[,yname]

        xinfo <- plotLimits(xdata,xlim,FALSE,xaxs)
        xlim1 <- c(min(xlim1[1],xinfo$lim[[1]][1]),max(xlim1[2],xinfo$lim[[1]][2]))

        yinfo <- plotLimits(ydata,ylim,FALSE,yaxs)
        ylim1 <- c(min(ylim1[1],yinfo$lim[[1]][1]),max(ylim1[2],yinfo$lim[[1]][2]))

    }

    if (substr(yname,1,1) == "p") ylim1 <- rev(ylim1)

    is <- 0
    for (sname in names(sdngs)) {

        x <- sdngs[[sname]]
        vnames <- colnames(x)
        is <- is + 1

        if (is.na(match(xname,vnames))) stop(paste(xname,"not found in",sname))
        if (is.na(match(yname,vnames))) stop(paste(yname,"not found in",sname))

        xunits <- eolts::units(x[,xname])
        yunits <- eolts::units(x[,yname])

        xdata <- x@data[,xname]
        ydata <- x@data[,yname]

        if (make_plot) {
            plot(0,0, type="n", axes=TRUE, main=title,col=col[1],
                xlim=xlim1, xaxt=xaxt, xaxs=xaxs, xlab=paste0(xname,"(",xunits,")"),
                ylim=ylim1, yaxt=yaxt, yaxs=yaxs, ylab=paste0(yname,"(",yunits,")"),
                ...)
            grid()
            legend("topright", names(sdngs), col=col[2:(ns+1)],
                lty=rep(1,ns), lwd = 2.5, cex = 0.8, bty="n")
            make_plot <- FALSE
        }
        lines(xdata, ydata, lwd=tlwd, col=col[is+1],type=type)
        points(xdata, ydata, cex=0.5, col=col[is+1],type=type)
    }
    NULL
}

