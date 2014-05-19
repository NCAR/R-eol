# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsdng" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

plotSContour <- function(xs,xname,yname,contour=FALSE)
{
    xs <- interpSounding(xs,xname,yname)

    zmat <- NULL
    ydata <- NULL
    is <- 0

    reverse <- FALSE
    ymin <- Inf
    ymax <- -Inf

    for (sname in names(xs)) {

        x <- xs[[sname]]
        vnames <- colnames(x)
        is <- is + 1

        if (is.na(match(xname,vnames))) stop(paste(xname,"not found in",sname))
        if (is.na(match(yname,vnames))) stop(paste(yname,"not found in",sname))

        xunits <- eolts::units(x[,xname])
        yunits <- eolts::units(x[,yname])

        xdata <- x@data[,xname]
        if (is.null(ydata)) {
            ydata <- x@data[,yname]
            # before calling filled.contour, ydata must be in increasing order
            flipData <- ydata[1] > tail(ydata,1)
            if (flipData) ydata <- rev(ydata)
        }
        else {
            if (!identical(ydata,x@data[,yname])) stop("changing ydata")
        }

        ymin <- min(ymin,ydata[!is.na(xdata)],na.rm=TRUE)
        ymax <- max(ymax,ydata[!is.na(xdata)],na.rm=TRUE)

        # row: one for each sounding
        # column: one for each interpolated observation
        if (is.null(zmat)) zmat <- matrix(NA,nrow=length(xs),ncol=length(ydata))

        zmat[is,] <- if(flipData) { rev(xdata)} else {xdata}
    }

    flipYaxis <- (yunits == "mb") # flip the Y axis

    xdata <- 1:nrow(zmat)
    xlim <- c(1,nrow(zmat))
    ylim <- c(ymin,ymax)
    cat("ylim=",paste(ylim,collapse=","),"\n")

    if (xname =="rh")
        colorfunc <- colorRampPalette(c("red","yellow","green","cyan","blue"))
    else
        colorfunc <- colorRampPalette(c("blue","cyan","green","yellow","red"))

    if (contour) {
        #browser()
        if (flipYaxis) ylim <- rev(ylim)
        filled.contour(x=xdata,y=ydata,z=zmat,
            xlim=xlim, xaxs="r",
            ylim=ylim, yaxs="r",
            color.palette=colorfunc,
            plot.title= {
                title(ylab=paste0(yname,"(",yunits,")"),
                    xlab="sounding")
                abline(v=xdata,lwd=1)
                text(xdata[1],ylim[1],names(xs)[1],cex=0.8,adj=0)
                text(tail(xdata,1),ylim[1],tail(names(xs),1),cex=0.8,adj=1)
            },
            plot.axes={
                axis(1)
                axis(2,las=0)
            },
            key.title=mtext(paste0(xname,"(",xunits,")"),cex=1.1,side=1))
    }
    else {
        # require(lattice)
        xlim <- c(xlim[1] - 0.04 * diff(xlim),xlim[2] + 0.04 * diff(xlim))
        ylim <- c(ylim[1] - 0.04 * diff(ylim),ylim[2] + 0.04 * diff(ylim))
        if (flipYaxis) ylim <- rev(ylim)
        nlevels <- 20
        plot(levelplot(zmat,row.values=xdata,column.values=ydata,
            xlim=xlim, xaxs="r", xlab="sounding",
            ylim=ylim, yaxs="r", ylab=paste0(yname,"(",yunits,")"),
            aspect="fill",
            at=pretty(range(zmat,na.rm=TRUE),n=nlevels),
            col.regions=colorfunc(nlevels),
            panel=function(...) {
                panel.levelplot(...)
                panel.abline(v=xdata,lwd=1)
                panel.text(xdata[1],ylim[1],names(xs)[1],cex=0.8,adj=c(0,0))
                panel.text(tail(xdata,1),ylim[1],tail(names(xs),1),cex=0.8,adj=c(1,0))
                panel.text(xlim[2]+diff(xlim)*0.10,ylim[1]+100,
                    paste0(xname,"(",xunits,")"))
            }
        ))
    }
    invisible(NULL)
}
