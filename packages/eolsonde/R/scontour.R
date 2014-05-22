# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsonde" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

scontour <- function(sdngs, yname, zname, contour=TRUE, ylim=NULL, ynstep=100)
{

    # parameters to control plot:
    #   ylim, ystep  (is ystep necessary?)
    #   zlim (perhaps only clip z?) Is zstep necessary?
    #       Perhaps only to reduce colors
    sdngs <- interpSoundings(sdngs, yname, zname, ylim, ynstep)

    zmat <- NULL
    ydata <- NULL
    is <- 0

    reverse <- FALSE
    ymin <- Inf
    ymax <- -Inf
    slabels <- NULL # sounding labels

    for (sname in names(sdngs)) {

        sdng <- sdngs[[sname]]
        vnames <- colnames(sdng)
        is <- is + 1

        if (is.na(match(zname,vnames))) stop(paste(zname,"not found in",sname))
        if (is.na(match(yname,vnames))) stop(paste(yname,"not found in",sname))

        zunits <- eolts::units(sdng[,zname])
        yunits <- eolts::units(sdng[,yname])

        if (is.null(ydata)) {
            ydata <- sdng@data[,yname]
            # before calling filled.contour, ydata must be in increasing order
            # There should be no NAs in ydata, which is the result of
            # the interpolation, above.
            flipData <- ydata[1] > tail(ydata,1)
            if (flipData) ydata <- rev(ydata)
        }
        else {
            if (!identical(ydata,sdng@data[,yname])) stop("changing ydata")
        }

        zdata <- clip(sdng[,zname])@data[,1]

        ymin <- min(ymin,ydata[!is.na(zdata)],na.rm=TRUE)
        ymax <- max(ymax,ydata[!is.na(zdata)],na.rm=TRUE)

        # row: one for each sounding
        # column: one for each interpolated observation
        if (is.null(zmat)) zmat <- matrix(NA,nrow=length(sdngs),ncol=length(ydata))

        zmat[is,] <- if(flipData) { rev(zdata)} else {zdata}

        # remove trailing portions in name
        for (char in c(".","_","-")) {
            ic <- rev(unlist(gregexpr(char,sname,fixed=TRUE)))
            for (i in ic) {
                if (i > 14) sname <- substr(sname,1,i-1)
            }
        }
        slabels  <- append(slabels,sname)
    }

    flipYaxis <- (yunits == "mb") # flip the Y axis

    xdata <- 1:nrow(zmat)
    xlim <- c(1,nrow(zmat))
    if (is.null(ylim))
        ylim <- c(ymin,ymax)
    cat("ylim=",paste(ylim,collapse=","),"\n")

    if (zname =="rh")
        colorfunc <- colorRampPalette(c("red","yellow","green","cyan","blue"))
    else
        colorfunc <- colorRampPalette(c("blue","cyan","green","yellow","red"))

    nl <- length(slabels)
    nskip <- as.integer(ceiling(nl/20))
    slabx <- seq(from=1,to=nl,by=nskip)
    slabels <- slabels[slabx]

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
                abline(v=slabx,lwd=1,lty=2,col="lightgray")
                text(slabx,par("usr")[3],slabels,cex=0.8,adj=c(0,0),srt=90)
            },
            plot.axes={
                axis(1)
                axis(2,las=0)
            },
            key.title=mtext(paste0(zname,"(",zunits,")"),cex=1.1,side=1))
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
                panel.abline(v=slabx,lwd=1,lty=2,col="lightgray")
                panel.text(slabx,ylim[1],slabels,cex=0.8,adj=c(0,0),srt=90)
                panel.text(xlim[2]+diff(xlim)*0.10,ylim[1]+100,
                    paste0(zname,"(",zunits,")"))
            }
        ))
    }
    invisible(NULL)
}
