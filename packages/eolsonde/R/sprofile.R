# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsonde" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

sprofile <- function(raw=NULL,qc=NULL,title=NULL,type="b",
    xlim=NULL,xlab,xaxt=par("xaxt"),xaxs=par("xaxs"),
    ylim=NULL,ylab,yaxt=par("yaxt"),yaxs=par("yaxs"),
    col=c("black","red","green","blue","purple","cyan",
        "orange","yellow","gray","pink"),tlwd=par("lwd"),
    pcex=0.25,...)
{
    # Plot profiles of one or more variables vs pressure or altitude,
    # one sounding per plot

    # tlwd: trace line width.  Use that option if you want to
    #   change the trace line width without changing the line width
    #   of the axes.

    legtxt <- NULL
    legcol <- NULL
    lautime <- NULL
    yname <- NULL
    yunits <- NULL
    xrnames <- NULL
    xqnames <- NULL
    reverse_yaxis <- FALSE
    if (!is.null(raw)) {
        lautime <- positions(raw)[1]    # launch time

        xrnames <- colnames(raw)

        # discard non-numeric columns
        nm <- sapply(raw@data[1,],function(x){ mode(x)=="numeric"})
        xrnames <- xrnames[nm]
        xunits <- eolts::units(raw)[nm]

        # find y axis data: p, pressure, gp.alt or gps.alt
        ym <- match(xrnames,c("P","Alt_gp","Alt_gps"))
        # use first one
        yname <- tail(xrnames[!is.na(ym)],1)
        yunits <- tail(eolts::units(raw)[!is.na(ym)],1)

        ym <- match(xrnames,c(yname,"sta"))
        xrnames <- xrnames[is.na(ym)]
        xunits <- xunits[is.na(ym)]

        if (!is.null(qc))
            legtxt <- paste0(xrnames,"_raw(",xunits,")")
        else
            legtxt <- paste0(xrnames,"(",xunits,")")
    }

    if (!is.null(qc)) {
        if (is.null(lautime)) lautime <- positions(qc)[1]

        xqnames <- colnames(qc)
        nm <- sapply(raw@data[1,],function(x){ mode(x)=="numeric"})
        xqnames <- xqnames[nm]
        xunits <- eolts::units(qc)[nm]

        # find y axis data: p, pressure, gp.alt or gps.alt
        ym <- match(xqnames,c("P","Alt_gp","Alt_gps"))
        yqname <- tail(xqnames[!is.na(ym)],1)
        yqunits <- tail(eolts::units(qc)[!is.na(ym)],1)

        if (!is.null(yname) && yname != yqname)
            stop(paste("Inconsistent y axis data variables:",yname,yqname))
        else yname <- yqname

        if (!is.null(yunits) && yunits != yqunits)
            stop(paste("Inconsistent y axis data units:",yunits,yqunits))
        else yunits <- yqunits

        ys <- match(xqnames,c(yname,"sta"))

        xqnames <- xqnames[is.na(ys)]
        xunits <- xunits[is.na(ys)]

        if (!is.null(raw))
            legtxt <- c(legtxt,paste0(xqnames,"_qc(",xunits,")"))
        else
            legtxt <- c(legtxt,paste0(xqnames,"_qc(",xunits,")"))
    }

    if (length(yname) == 0)
        stop("No variable for y axis found")

    # reverse axis if Y is pressure
    if (yunits == "mb") reverse_yaxis <- TRUE

    xnames <- c(xrnames,xqnames)

    mci <- length(legtxt) + 1  # max color index needed
    if (mci > length(col)) {
        ndup <- ceiling(length(legtxt) / (length(col)-1))
        legcol <- rep(2:length(col),ndup)[2:mci]
    }
    else legcol <- 2:mci

    # pch=4 is usually an 'X", reserve that for showing clipped points
    legpch <- (1:(mci+1))[-4]

    # Determine number of x scales for the various variables and units
    if (!is.null(xrnames) && !is.null(xqnames)) {
        mx <- is.na(match(xqnames,xrnames))

        if (any(mx))
            stop(paste("QC variables",paste(xqnames[mx],collapse=","),"not found in raw data"))
    }

    if (!is.null(raw))
        xinfo <- plotLimits(raw[,xrnames],xlim,FALSE,xaxs)
    else
        xinfo <- plotLimits(qc[,xqnames],xlim,FALSE,xaxs)

    nscales <- xinfo$nscales
    xscales <- xinfo$scales # 1=bottom,2=top,3=2nd axis on bottom, etc
    xlim <- xinfo$lim

    ntrace <- length(xnames)

    ylab <- paste0(yname,"(",yunits,")")

    if (!is.null(raw) && nrow(raw) == 0) {
        # empty plot, just containing legends
        plot(0,0, type="n", axes=TRUE,
            xlim=xlim, xlab="",
            ylim=ylim, ylab=ylab,
            ...)
        legend("center", legtxt, col=col[legcol], bty="n",lty=rep(1,ntrace),
            lwd=tlwd, cex=1.5,title=title)
        # box()
        return(invisible(NULL))
    }

    mfg <- par("mfg")
    # cat("par(mfg)=",paste(par("mfg"),collapse=","),"\n")

    # If first plot on page, call adjPar
    if (identical(mfg[1:2],mfg[3:4])) {
        # cat("calling adjPar\n")
        adjPar(nxscales=nscales)
    }

    args <- list(...)

    if (xaxt != "n") xaxt <- "s"
    if (yaxt != "n") yaxt <- "s"

    mgp <- par("mgp")
    if (is.null(args$cex)) cex <- par("cex")
    else cex <- args$cex

    old.par <- par(c("mgp"))
    on.exit(par(old.par),add=T)

    tckadj <- par("tck")
    if (is.na(tckadj)) tckadj = -0.01
    if (tckadj < 0) {   # outside ticks, move axis labels 1/2 char width
        mgp[1:2] <- mgp[1:2] + .3
        par(mgp=mgp)
    }

    xaxis_done <- NULL  # how many axes have been labeled?
    yaxis_done <- FALSE # has the vertical axis been done

    itrace <- 0
    for (xname in xnames) {
        itrace <- itrace + 1

        if (itrace > length(xrnames)) {
            xunits <- eolts::units(qc[,xname])[1]
            xdata <- unlist(qc[,xname]@data)
            ydata <- unlist(qc[,yname]@data)
        }
        else {
            xunits <- eolts::units(raw[,xname])[1]
            xdata <- unlist(raw[,xname]@data)
            ydata <- unlist(raw[,yname]@data)
        }

        xnameunits <- paste0(xname,"(",xunits,")")
        dupunits <- xinfo$dupunits[xnameunits]
        xaxis_num <- xscales[xnameunits] # 1:bottom, 2:top, 3:2nd scale on bottom, etc

        # cat("xnameunits=",xnameunits,"dupunits=",dupunits,"\n")

        xlim1 <- xlim
        if (is.list(xlim1)) {
            xlim1 <- xlim[[xname]]
            if (is.null(xlim1)) xlim1 <- xlim[[xnameunits]]
        }
        # cat("xname=",xname,"xlim1=",paste(xlim1,collapse=","),"xaxs=",xaxs,"\n")

        # All data is NAs
        if (any(is.na(xlim1)) || any(is.infinite(xlim1))) xlim1 <- c(-1,1)

        clipped <- F
        cmin <- xlim1[1]
        cmax <- xlim1[2]
        clip.min <- xdata < cmin & !is.na(xdata)
        if (any(clip.min)) {
            xdata[clip.min] <- NA_real_
            clipped <- T
        }
        clip.max <- xdata > cmax & !is.na(xdata)
        if (any(clip.max)) {
            xdata[clip.max] <- NA_real_
            clipped <- T
        }

        # par(new=TRUE)

        side <- line <- xlab.txt <- NULL
        if (!missing(xlab)) xlab.txt <- xlab
        if (do_xaxis <- (xaxt != "n" && is.na(match(xaxis_num,xaxis_done)))) {
            if (nscales == 1) {
                side <- 1 # 1:bottom, 2:left, 3:top, 4:right
                line <- 0
                if (missing(xlab)) xlab.txt <- paste0("(",xunits,")")
            }
            else {
                iplot <- xaxis_num # 1:bottom, 2:top, 3:2nd scale on bottom, etc
                side <- if (iplot %% 2) 1 else 3
                line <- (mgp[1] + 1) * ((iplot-1) %/% 2)
                if (missing(xlab)) {
                  if (dupunits) xlab.txt <- paste(xname," (",xunits,")",sep="")
                  else xlab.txt <- paste("(",xunits,")",sep="")
                }
            }
        }
        # cat("plot, xname=",xname," xlim1=",signif(xlim1,4),"\n")

        # browser()
        # First trace, create scale, box and yaxes labels
        if (!yaxis_done) {
            if (is.null(ylim)) ylim <- range(ydata,na.rm=T)
            if (reverse_yaxis) ylim <- rev(ylim)
            # xaxs="r" (extend by 4%) or xaxs="i" was passed
            # to plotLimits when determining xlim1.
            # So the axes are not extended again by 4% set xaxs="i" here.
            plot(xdata,ydata,type="n",col=1,axes=TRUE,
                xlim=xlim1,xlab="",xaxs="i",xaxt="n",
                ylim=ylim,ylab=ylab,yaxs=yaxs,yaxt=yaxt,
                err=-1,...)
            # axis(2)
            axis(4,labels=FALSE)
            if (nscales == 1) grid()
            yaxis_done <- TRUE
        }
        else {
            # rescale for changing x axis
            if (FALSE) {
                plot(xdata,ydata, type="n",axes=FALSE,
                    xaxt="n",xaxs="i", xlab="", xlim=xlim1,
                    yaxt="n",yaxs="i", ylab="", ylim=ylim,
                    ...)
            }
            else {
                par(usr=c(xlim1,ylim))
            }
        }

        nas <- is.na(xdata)
        if (type =="b" || type == "l" || type == "o")
            lines(xdata[!nas],ydata[!nas],
                col=col[legcol[itrace]],lty=1,lwd=tlwd,err=-1,...)
        if (type =="b" || type == "p" || type == "o")
            points(xdata[!nas],ydata[!nas],
                col=col[legcol[itrace]],pch=legpch[itrace],cex=pcex,err=-1,...)

        if (clipped) {
            # cat("clipped=TRUE\n")
            # usr <- par("usr")
            # clip.min and clip.max are logical vectors
            # TRUE where the data exceeds the min or max
            # plot those points at the corresponding axis
            if (any(as.vector(clp <- clip.min))) {
                # cat(paste(xname,"#clipped min=",sum(clp),",xlim1[1]=",xlim1[1]),"\n")
                xtmp <- xdata[clp]
                ytmp <- ydata[clp]
                xtmp[] <- xlim1[1]
                points(xtmp,ytmp,pch="*",col=col[legcol[itrace]],cex=1.0)
            }
            if (any(as.vector(clp <- clip.max))) {
                # cat(paste(xname,"#clipped max=",sum(clp),",xlim1[2]=",xlim1[2]),"\n")
                xtmp <- xdata[clp]
                ytmp <- ydata[clp]
                xtmp[] <- xlim1[2]
                points(xtmp,ytmp,pch="*",col=col[legcol[itrace]],cex=1.0)
            }
        }

        if (xlim1[1] * xlim1[2] < 0) abline(v=0,lty=2)

        # browser()
        if (do_xaxis) {
            at <- pretty(xlim1)
            if (side == 3) {        # top
                axis(side=side,at=at,line=line,col=1,cex=cex*.8,labels=TRUE)
                mtext(side=side,line=line+mgp[1]*.8,xlab.txt,col=1,cex=cex)
            }
            else if (nscales > 1 || length(xaxis_done) == 0) {
                axis(side=side,at=at,line=line,col=1,cex=cex*.8,labels=TRUE)
                mtext(side=side,line=line+mgp[1]*.8,xlab.txt,col=1,cex=cex)
            }
            if (nscales == 1 || length(xnames) == 1) axis(4,labels=F,at=at)
        }
        xaxis_done <- c(xaxis_done,xaxis_num)

        # cat("par(page)=",par("page"),"\n")
        # cat("par(mfg)=",paste(par("mfg"),collapse=","),"\n")
        NULL
    }       # xname in xnames

    slabel <- title
    if (!is.null(slabel)) {
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
    }

    legend("topright", legtxt, col=col[legcol], bty="n",lty=rep(1,ntrace), lwd=tlwd,
        cex=1.0,title=slabel)

    # cat("before logo_stamp par(mfg)=",paste(par("mfg"),collapse=","),"\n")

    # first plot
    if (identical(par("mfg")[1:2],c(1L,1L))) logo_stamp()

    invisible(NULL) 
}
