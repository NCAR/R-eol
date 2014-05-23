# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsonde" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

scontour <- function(sdngs, yname, zname, contour=TRUE, ylim=NULL, ynstep=100,
    title="")
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
    slabels <- NULL

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

        slabel <- sname
        # remove trailing portions in name
        for (char in c(".","_","-")) {
            ic <- rev(unlist(gregexpr(char,slabel,fixed=TRUE)))
            for (i in ic) {
                if (i > 14) slabel <- substr(slabel,1,i-1)
            }
        }
        # if it looks like sounding name ends in HHMMSS, remove the seconds to save space
        if (grepl("[0-9]{8}[_-][0-9]{6}$",slabel) || grepl("[0-9]{14}$",slabel))
            slabel <- substr(slabel,1,nchar(slabel)-2)

        slabels  <- append(slabels,slabel)
    }

    flipYaxis <- (yunits == "mb") # flip the Y axis

    xdata <- 1:nrow(zmat)
    xlim <- c(1,nrow(zmat))
    if (is.null(ylim))
        ylim <- c(ymin,ymax)
    cat("ylim=",paste(signif(ylim,4),collapse=","),"\n")

    if (substring(zname,1,2) =="RH")
        colorfunc <- colorRampPalette(c("red","yellow","green","cyan","blue"))
    else
        colorfunc <- colorRampPalette(c("blue","cyan","green","yellow","red"))

    nl <- length(slabels)
    nskip <- as.integer(ceiling(nl/30))
    slabx <- seq(from=1,to=nl,by=nskip)
    slabels <- slabels[slabx]
    ncl <- max(sapply(slabels,nchar)) / 2

    if (contour) {
        #browser()
        # width of one of the sounding labels in inches
        slabinch <- strwidth(slabels[1],units="inches")
        # par("mar") are current margin settings in lines
        # par("mai") are current margin settings in inches
        old_mar <- par("mar")
        xlinesperinch <- old_mar[1]/par("mai")[1]

        ncl <- slabinch * xlinesperinch
        cat("xlinesperinch=",xlinesperinch,", ncl=",ncl,"\n")

        new_mar <- c(ncl,old_mar[2:4])
        par(mar=new_mar)
        on.exit(par(mar=old_mar),add=T)

        # mai <- par("mai")
        # new_mai <- c(strwidth(slabels[1],units="inches") + 0.5,mai[2:4])
        # par(mai=new_mai)

        if (flipYaxis) ylim <- rev(ylim)
        filled.contour(x=xdata,y=ydata,z=zmat,
            xlim=xlim, xaxs="r",
            ylim=ylim, yaxs="r",
            color.palette=colorfunc,
            plot.title= {
                title(main=title,ylab=paste0(yname,"(",yunits,")"),
                    xlab="")
                abline(v=slabx,lwd=1,lty=2,col="lightgray")
                # text(slabx,par("usr")[3],slabels,cex=0.8,adj=rep(c(0.5,0),length(slabels)),srt=90)
            },
            plot.axes={
                axis(1,at=slabx,labels=slabels,cex.axis=0.7,hadj=1,padj=0.5,las=3)
                axis(2,las=0)
            },
            key.title=mtext(paste0(zname,"(",zunits,")"),cex=1.1,side=1)
            )
    }
    else {
        # require(lattice)
        lattice.options(panel.error="stop")
        xlim <- c(xlim[1] - 0.04 * diff(xlim),xlim[2] + 0.04 * diff(xlim))
        ylim <- c(ylim[1] - 0.04 * diff(ylim),ylim[2] + 0.04 * diff(ylim))
        if (flipYaxis) ylim <- rev(ylim)
        nlevels <- 20
        plot(
            levelplot(zmat,row.values=xdata,column.values=ydata,
                xlim=xlim, xlab=NULL, xaxt="n",
                ylim=ylim, yaxs="r", ylab=paste0(yname,"(",yunits,")"),
                aspect="fill",
                at=pretty(range(zmat,na.rm=TRUE),n=nlevels),
                col.regions=colorfunc(nlevels),
                lattice.options(layout.heights=list(bottom.padding=
                        list(x=strwidth(slabels[1],units="inches"),units="inch"))),
                scales=list(x=list(at=slabx,labels=slabels,cex=0.7,rot=90,axs="r")),
                # auto.key=list(title=paste0(zname,"(",zunits,")"),cex=1.1,side=1),
                main=title,
                panel=function(...) {
                    panel.levelplot(...)
                    # panel.axis(side="bottom",at=slabx,labels=slabels,outside=TRUE,text.cex=0.6,rot=90)
                    panel.abline(v=slabx,lwd=1,lty=2,col="lightgray")
                    # panel.text(xlim[2]+diff(xlim)*0.10,ylim[1]+100,
                     #    paste0(zname,"(",zunits,")"))
                }
            )
        )
    }
    logo_stamp()
    invisible(NULL)
}
