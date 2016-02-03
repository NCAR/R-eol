# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsonde" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

scontour <- function(sdngs, yname, zname,
    contour=FALSE, ylim=NULL, ynstep=100,
    zlim=NULL, fixedzlim=FALSE, title)
{

    # proc.times:
    #   First 20 MPEX soundings, each 3694 to 4069 rows, 17 columns
    # > scontour(xs[1:20],"Alt_gp","RH")
    # interp times=
    #    user  system elapsed 
    #   4.687   0.100   4.857 
    # accum zdata times
    #    user  system elapsed 
    #   0.649   0.000   0.657 
    # ylim= -261.4,13970 
    # plot times=
    #    user  system elapsed 
    #   0.624   0.023   0.683 
    # This was a version of interpSoundings with no apply(),
    # just for loops.

    if (fixedzlim) {
        if (is.null(zlim)) stop("If fixedzlim is TRUE, zlim must be specified")
    }
    else {
        # if not fixed scales, clip data
        if (!is.null(zlim) && !is.na(zlim[1]) && !is.na(zlim[2])) {
            # on.exit set clipping limits back to what they were
            zclip <- clip(zname)
            on.exit(clip(zname,zclip$clip,zclip$min,zclip$max),add=T)
            clip(zname,TRUE,zlim[1],zlim[2])
        }
    }

    profile <- FALSE
    if (profile)
        p1 <- proc.time()

    sdngs <- interpSoundings(sdngs, yname, zname, ylim, ynstep)

    if (length(sdngs) == 0) {
        warning(paste("No plot from scontour, since interpSoundings of ",
                zname,"over",yname,"returned nothing"))
        return(invisible(NULL))
    }

    if (profile) {
        p2 <- proc.time()
        cat("interp times=",as.character(p2-p1),"\n")
        p1 <- p2
    }

    zmat <- NULL
    yData <- NULL
    is <- 0

    reverse <- FALSE
    ymin <- Inf
    ymax <- -Inf
    zmin <- Inf
    zmax <- -Inf
    slabels <- NULL
    zunits <- "unknown"

    for (sname in names(sdngs)) {

        sdng <- sdngs[[sname]]
        vnames <- colnames(sdng)
        is <- is + 1

        if (is.na(match(zname,vnames))) stop(paste(zname,"not found in",sname))
        if (is.na(match(yname,vnames))) stop(paste(yname,"not found in",sname))

        zunits <- eolts::units(sdng[,zname])
        yunits <- eolts::units(sdng[,yname])

        # make sure thay Y values aren't changing
        if (is.null(yData)) {
            ydata <- sdng@data[,yname]
            # before calling filled.contour, ydata must be in increasing order
            # There should be no NAs in ydata, which is the result of
            # the interpolation, above.
            flipData <- ydata[1] > tail(ydata,1)
            if (flipData) ydata <- rev(ydata)
        }
        else {
            if (flipData) {
                if (!identical(ydata,rev(sdng@data[,yname]))) stop("changing ydata")
            } else {
                if (!identical(ydata,sdng@data[,yname])) stop("changing ydata")
            }
        }

        zdata <- sdng[,zname]
        zok <- !is.na(zdata@data)
        if (!any(zok)) next

        if (fixedzlim) {
            if (!is.na(zlim[1])) zmin <- zlim[1]
            else zmin <- min(zmin,zdata@data,na.rm=TRUE)
            if (!is.na(zlim[2])) zmax <- zlim[2]
            else zmax <- max(zmax,zdata@data,na.rm=TRUE)
        }
        else {
            zdata <- clip(zdata)
            zok <- !is.na(zdata@data)
            if (!any(zok)) next
            zmin <- min(zmin,zdata@data,na.rm=TRUE)
            zmax <- max(zmax,zdata@data,na.rm=TRUE)
        }
        # get data matrix from time-series
        zdata <- zdata@data

        ymin <- min(ymin,sdng@data[zok,yname],na.rm=TRUE)
        ymax <- max(ymax,sdng@data[zok,yname],na.rm=TRUE)

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

    if (is.null(zmat)) {
        warning(paste("No plot from scontour, since interpSoundings of ",
                zname,"over",yname,"returned nothing"))
        return(invisible(NULL))
    }

    if (profile) {
        p2 <- proc.time()
        cat("accum zdata times=",as.character(p2-p1),"\n")
        p1 <- p2
    }

    flipYaxis <- (yunits == "mb") # flip the Y axis

    xdata <- 1:nrow(zmat)
    xlim <- c(1,nrow(zmat))
    if (is.null(ylim))
        ylim <- c(ymin,ymax)
    if (diff(xlim) <= 0) xlim <- c(0.9,1.1)
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

    if (missing(title))
        title <- paste0(zname,"(",zunits,"), ",length(sdngs)," soundings")

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
            key.title=mtext(paste0(zname,"(",zunits,")"),cex=1.1,side=1,
                las=2)
            )
        logo_stamp()
    }
    else {
        # require(lattice)
        lattice.options(panel.error="stop")
        xlim <- c(xlim[1] - 0.04 * diff(xlim),xlim[2] + 0.04 * diff(xlim))
        ylim <- c(ylim[1] - 0.04 * diff(ylim),ylim[2] + 0.04 * diff(ylim))
        if (flipYaxis) ylim <- rev(ylim)
        nlevels <- 20
        # zmat <- as.vector(zmat)
        data <- expand.grid(x=xdata,y=ydata)
        data$z <- as.vector(zmat)

        # as option to levelplot, results in too much bottom padding
        # lattice.options = list(layout.heights=list(bottom.padding=
        #     list(x=strwidth(slabels[1],units="inches"),units="inch"))),

        # as option to levelplot, looks good
        # lattice.options=lattice.options(layout.heights=list(bottom.padding=
        #     list(x=strwidth(slabels[1],units="inches"),units="inch"))),

        # this also works
        # lo <- lattice.options("layout.heights")
        # lo$bottom.padding <- list(x=strwidth(slabels[1],units="inches"),units="inch")
        # then the option to levelplot
        # lattice.options=lo,

        # too much padding, with ncl=7, plus gives warning:
        # In trellis.par.set(layout.heights = list(bottom.padding = ncl)) :
        #   Note: The default device has been opened to honour attempt to
        # modify trellis settings
        # cat("ncl=",ncl,"\n")
        # trellis.par.set(layout.heights=list(bottom.padding=ncl))

        # works, with no warning
        lh <- trellis.par.get("layout.heights")
        xlab.cex <- 0.7
        lh$bottom.padding <- ncl * xlab.cex

        plot <- levelplot(z ~ x * y,data,
            xlim=xlim, xlab=NULL, xaxt="n",
            ylim=ylim, yaxs="r", ylab=paste0(yname,"(",yunits,")"),
            par.settings=lh,
            aspect="fill",
            at=pretty(c(zmin,zmax),n=nlevels/2),
            cuts=nlevels,
            col.regions=colorfunc(nlevels+1),
            scales=list(x=list(at=slabx,labels=slabels,cex=xlab.cex,rot=90,axs="r")),
            main=title,
            panel=function(...) {
                panel.levelplot(...)
                panel.grid(h=-1,v=-1,lty=2,col="lightgray")
                # grid::grid.text(paste0(zname,"(",zunits,")"),
                #     x = xlim[2]+diff(xlim)*0.1, y = ylim[1]-diff(ylim)*0.05,
                #     hjust=0.5)
            },
            page = function(page) {
                # cat("layout.heights=",str(trellis.par.get("layout.heights")),"\n")
                # cat("layout.widths=",str(trellis.par.get("layout.widths")),"\n")
                # grid::grid.text(paste0(zname,"(",zunits,")"),
                #     x = 0.91, y = .15, hjust=0.5,vjust=1,rot=90)
                isfs::trellis_logo()
            }
        )
        plot(plot)
        
        # lattice.options(default.args = list(page = function(n) 
        #             grid::grid.text(date(), x = 0.01, y = 0.01, 
        #                                     default.units = "npc", 
         #                                                        just = c("left", "bottom"))))
        # mtext(paste0(zname,"(",zunits,")"),1,outer=TRUE,adj=1,line=0)
    }
    if (profile) {
        p2 <- proc.time()
        cat("plot times=",as.character(p2-p1),"\n")
        p1 <- p2
    }
    invisible(NULL)
}
