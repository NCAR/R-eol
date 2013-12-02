# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
plotdat.ts <- function(data,data2, select, rfrnc, nsmth, ylim, yaxt, logy=F, 
    type="l", plot.clipped=T, chksum=F, 
    derived=T, method.smth="mean", lwd=1, first.color=1,
    axes=T, browse=F, ...)
{
    # Generic function for plotting time series of dat objects, per twh.
    #   e.g. plotdat.ts("P"), plotdat.ts("Tdome.in","Tcase.in"), etc.
    #
    # data, data2 = string or dat objects for variables to be plotted.
    #               if data2 missing, data is plotted
    #               else if data and data2 have same units and dimensions, 
    #                       data - data2 is plotted
    # rfrnc = reference column of data data used for plotting differences
    #         when data2 missing.
    # nsmth = number of data points for smoothing with running average.
    # ylim = limits for ordinate (y axis).
    # type = "l", "p", or "b" to plot lines, points, or both
    # logy = T uses a logarithmic y axis
    # plot.clipped = T enables plotting of clipped data
    # chksum = T to edit data on the basis of chksum
    # derived = T replaces a derived variable with a basic variable
    # lwd = line width
    # first.color = position in color table used for first line
    # browse = T opens browser at end of function.
    # ... for setting plotting variables
    #
    # Note that, when making multiple plots per page,
    #   plotdat.ts contains logic to optimize the plot margins.
    # plotdat.ts returns plotted data.

    # if (is.null(dev.list())) cmotif()

    old.par <- par(lwd=par("lwd"), cex=par("cex"), xaxs="i", yaxs="i", tck=0.02)
    on.exit(par(old.par))

    # input data 
    if (is.character(data))
        data <- dat(data, derived=derived)
    if (!missing(select))
        data = data[select,]
    dt <- deltat(data)["median"]
    label <- class(data)[1]
    if (label=="dat") {
        label <- unique(dimnames(data)[[2]])
        if (length(label) > 1) label <- unique(words(label,1,1))
    }
    units <- unique(data@units)

    # reorder matrix columns in ascending order of heights
    hts <- heights(dimnames(data)[[2]])
    data <- data[,order(hts)]
    colors <- 1:ncol(data) + (first.color-1)

    # create legend labels
    if (label=="")
        leg.labels <- dimnames(data)[[2]]
    else
        leg.labels <- suffixes(data, 1+nwords(label,"."),"")
    if (!is.null(stns <- stations(data)))
        if (is.null(names(stns)))
            leg.labels <- paste(leg.labels,"stn",stns)
        else
            leg.labels <- paste(leg.labels, names(stns))
    if (length(units) > 1)
        leg.labels <- paste(leg.labels, " (", data@units, ")", sep="")
    if (!missing(rfrnc))
        leg.labels <- paste (leg.labels[-rfrnc], "-", leg.labels[rfrnc]) 

    # clip data for plotting
    clipped <- F
    clip.par <- clip(label)
    if (!is.null(clip.par) && clip.par$clip) {
        clip.min <- !is.na(data@data) & data@data < clip.par$min
        clip.min <- matrix(clip.min, ncol=ncol(data))
        data[clip.min] <- NA_real_
        clip.max <- !is.na(data@data) & data@data > clip.par$max 
        clip.max <- matrix(clip.max, ncol=ncol(data))
        data[clip.max] <- NA_real_
        if (any(clip.min, na.rm=T) | any(clip.max, na.rm=T))
            clipped <- T
    }

    # input data2 for difference plot
    if (!missing(data2)) {
        if (is.character(data2))
            data2 <- dat(data2, derived=derived)
        label2 <- class(data2)
        if (label2=="dat") {
            label2 <- unique(dimnames(data2)[[2]])
            if (length(label2) > 1) label2 <- unique(words(label2,1,1))
        }
        units2 <- unique(data2@units)
        data2 <- data2[,order(hts)]
        if (length(units)*length(units2)==1 && (units==units2 & 
                ncol(data)==ncol(data2) & nrow(data)==nrow(data2)))
            data <- data - data2
        else 
            stop(paste("Data for ",label," and ",label2," are not compatible:  ",
                    "units are ", paste(units, collapse=", "), " and ", 
                    paste(units2, collapse=", "),
                    "; dimensions are ", paste(dim(data), collapse="x"),
                    " and ", paste(dim(data2), collapse="x"), sep=""))

            clip.par <- clip(label2)
            if (!is.null(clip.par) && clip.par$clip) {
                select <- !is.na(data2@data) & data2@data < clip.par$min
                select <- matrix(select, ncol=ncol(data2))
                data[select] <- NA_real_
                if (clipped)
                    clip.max <- clip.max | select
                else
                    clip.max <- select
                select <- !is.na(data2@data) & data2@data > clip.par$max
                select <- matrix(select, ncol=ncol(data2))
                data[select] <- NA_real_
                if (clipped)
                    clip.min <- clip.min | select
                else
                    clip.min <- select
                if (any(clip.min, na.rm=T) | any(clip.max, na.rm=T))
                    clipped <- T
            }
            if (label2=="")
                leg.add <- dimnames(data2)[[2]]
            else
                leg.add <- suffixes(data2, 1+nwords(label,"."),"")
            if (!is.null(stns <- stations(data2)))
                if (is.null(names(stns)))
                    leg.add <- paste(leg.add,"stn",stns)
                else
                    leg.add <- paste(leg.add, names(stns))
            leg.labels <- paste(leg.labels, "-", leg.add)
    }

    # edit data for plotting
    times <- data[,1]
    if (chksum) {
        ok <- dat("chksumOK.ALL_DATA")
        data[is.na(ok) | ok==0] <- NA_real_
    }
    data[is.infinite(data@data)] <- NA_real_
    if (!missing(rfrnc)) {
        data <- data[,-rfrnc] - data[,rfrnc]
        if (clipped) {
            clip.ref <- clip.min[,rfrnc]
            clip.min <- clip.min[,-rfrnc] | clip.max[,rfrnc]
            clip.max <- clip.max[,-rfrnc] | clip.ref
        }
    }
    if (!missing(nsmth) && nsmth>0) 
        data <- average(data,nsmth*dt,dt, method=method.smth, simple=T,
            use.weights=F)

    if (!missing(ylim)) {
        if (plot.clipped) {
            data[data < ylim[1]] <- ylim[1]
            data[data > ylim[2]] <- ylim[2]
        }
        else {
            data[data < ylim[1]] <- NA_real_
            data[data > ylim[2]] <- NA_real_
        }
    }
    else 
        ylim <- range(data, na.rm=T) 

    #  if (all(is.na(data)))
    #    stop("All edited data are NA;\n perhaps clip limits inappropriate")

    if (logy) { 
        logy <- "y"
        data[data <= 0] <- NA_real_
    }
    else logy <- ""

    # create label for y axis
    ylab <- label
    if (!missing(data2) && label2 != "")
        ylab <- paste(ylab, "-", label2)
    if (!is.null(units) && length(units) == 1 && units != "") 
        ylab <- paste(ylab, " (", units, ")", sep="")
    # set up special axis for wind direction
    if (missing(yaxt)) {
        if (class(data)[1]=="Dir" | class(data)[1]=="dir")
            yaxt <- "n"
        else
            yaxt <- "s"
    }

    # plot data
    mfg <- fun.set.mar()
    if (lwd != 1) par(lwd=lwd, cex=1.2, mar=par("mar")+c(0,1,0,0))
    plot.nts(data, type="n", xlab=(mfg[1]==mfg[3]), log=logy, ylab=ylab, 
        yaxt=yaxt, ylim=ylim, axes=axes, ...)

    # add plot details
    if (axes)
        timeaxis(3, labels=(mfg[1]==1), time.zone="GMT")
    usr <- par("usr")
    if (logy=="" && usr[3]*usr[4] < 0)
        abline(h=0)
    if (mfg[1]==mfg[3] & mfg[2]==mfg[4])
        fun_logo_stamp(F)
    DT1 <- 0
    DT2 <- 0
    if (!missing(nsmth))
        DT1 <- nsmth*dt
    if (!is.null(dpar("avg")) && dpar("avg")[1]!=0)
        DT2 <- dpar("avg")[1]
    else if (!is.null(dpar("smooth")) && dpar("smooth")[1]!=0)
        DT2 <- dpar("smooth")[1]
    if ( (DT1>0 | DT2>0) & mfg[3]+mfg[4]==2) {
        mtext(paste(max(DT1,DT2), "sec low-pass filter"), 
            side=3,adj=0,line=0.1,cex=0.8)
    }
    if (class(data)[1]=="Dir" | class(data)[1]=="dir") {
        inc <- 45
        deg <- (par("usr")[3:4] %/% inc)*inc
        axis(2, at=seq(deg[1],deg[2], by=inc), labels=F)
        axis(4, at=seq(deg[1],deg[2], by=inc), labels=F)
        inc <- 90
        deg <- (par("usr")[3:4] %/% inc)*inc
        axis(2, at=seq(deg[1],deg[2], by=inc), tck=0.03)
        axis(4, at=seq(deg[1],deg[2], by=inc), labels=F, tck=0.03)
    }
    else
        axis(4, labels=F)

    ylim <- usr[3:4]
    if (logy=="y")
        ylim <- 10^ylim
    for (i in 1:ncol(data)) {
        if (!all(is.na(data[,i]))) {
            if (type=="l" | type=="b") 
                lines(data[,i], lty=1, col=colors[i])
            if (type=="p" | type=="b") 
                points(data[,i], pch=8, col=colors[i], cex=0.5)
            if (clipped & plot.clipped) {
                # error when clip.min is not a 2d matrix??
                clip.times <- as.vector(clip.min[,i])
                if (any(clip.times, na.rm=T)) {
                    times[clip.times] <- ylim[1]
                    points(times[clip.times,], pch=4, col=colors[i], cex=1)
                }
                clip.times <- as.vector(clip.max[,i])
                if (any(clip.times, na.rm=T)) {
                    times[clip.times] <- ylim[2]
                    points(times[clip.times,], pch=4, col=colors[i], cex=1)
                }
            }
        }
    }

    # add legend
    if (mfg[3]==2) par(cex=0.75)

    xy <- c(usr[1] + 0.01*(usr[2]-usr[1]), usr[3] + 0.96*(usr[4]-usr[3]))

    if (logy=="y") xy[2] <- 10^xy[2]

    if (type=="l")
        legend(xy[1],xy[2], leg.labels, lty=rep(1,ncol(data)), 
            col=colors[1:ncol(data)], bg="transparent", bty="n")
    else
        legend(xy[1],xy[2], leg.labels, pch=rep(16,ncol(data)), 
            col=colors[1:ncol(data)], bg="transparent", bty="n")

    if (browse) {
        cat("Plotted data are in object \"data\"\n  Type \"c\" to exit browser\n")
        browser()
    }
    invisible(data)
}

