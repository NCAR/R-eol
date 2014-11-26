# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

setMethod("plot",signature(x="dat",y="missing"),
    function(x,...) 
    {
        invisible(plot.dat(x, ...))
    }
    )

plot.dat <- function(x,type="l",xlab,xlim,ylab,ylim=NULL,one_scale=FALSE,
    log="",tlwd=par("lwd"),remargin=TRUE,title,logo=TRUE,
    xaxt="s",yaxt="s",yaxs=par("yaxs"),cols,...)
{

    # tlwd: trace line width.  Use that option if you want to
    # 	change the trace line width without changing the line width
    #	of the axes.
    #

    if (missing(xlim)) {
        t1 <- start(x)
        t2 <- end(x)
        xlim <- utime(c(t1,t2))
    }
    else {
        t1 <- utime(xlim[1])
        t2 <- utime(xlim[2])
    }

    dnames <- dimnames(x)[[2]]

    mfg <- par("mfg")
    # cat("mfg1=",paste(mfg,collapse=","),"\n")
    first_plot <- identical(mfg[1:2],mfg[3:4])

    # Must figure out if this next plot will be on the bottom row
    # of figures on this page.  If the user specifies par(mfrow=c(r,c))
    # the figures will be placed by row (column varying most rapidly).
    # If par(mfcol=c(r,c)) they will be placed by column.
    # Before the first figure is drawn, mfg[1:2] are the row and
    # column of the last figure plotted.
    if (first_plot) {
        bottom_row <- mfg[3] == 1   # only one row of figures
    }
    else {
        by_row <- get(".by_row",envir=.isfsEnv)
        if (by_row) {
            bottom_row <- (mfg[1] == mfg[3]) ||
                (mfg[1] == (mfg[3] - 1)) && (mfg[2] == mfg[4])
        }
        else bottom_row <- mfg[1] == (mfg[3] - 1)
    }

    if (first_plot) xaxs <- "i"
    else xaxs <- "d"                # don't rescale

    # unique loses the station names
    plot_stns <- stations(x)
    if (!is.null(plot_stns)) {
        i <- sort(unique(plot_stns))
        if (!is.null(nstns <- names(plot_stns))) {
            names(i) <- nstns[match(i,plot_stns)]
            if (all(names(i) == as.character(i))) names(i) <- NULL
        }
        plot_stns <- i
    }

    dunits <- x@units
    names(dunits) <- dnames

    yinfo <- plotLimits(x,ylim,one_scale,axs=yaxs)
    nscales <- yinfo$nscales

    if (remargin && first_plot) adjPar(nyscales=nscales)

    ylim <- yinfo$lim
    yscales <- yinfo$scales

    # T for variables with different limits but the same units.
    # If T, we want to add the variable name to the Y axis
    ydupunits <- yinfo$dupunits

    # cat("names yscales=",paste(names(yscales),collapse=","),"\n")

    # at this point, ylim is not missing, is non null, and is either
    # a list or vector:
    #	list: named list of limits
    #	vector of length two: fixed limits to apply to all traces

    icol <- 2

    colv <- NULL

    assemble_legend <- TRUE
    plot_legend <- TRUE

    legv <- x@attributes$legend
    if (!is.null(legv)) {
        if (is.logical(legv)) {
            assemble_legend <- plot_legend <- legv
            legv <- NULL
        }
        else {
            legv <- as.character(legv)
            assemble_legend <- FALSE
        }
    }
    all_same_vars <- all(dnames == dnames[1])
    multunits <- length(unique(dunits)) > 1
    if (all_same_vars) {
        # perhaps they have different units
        if (multunits) all_same_vars <- F
    }

    args <- list(...)

    logy <- any(substring(log,1:nchar(log),1:nchar(log)) == "y")

    xaxt_tmp <- "n"
    xlab <- FALSE
    if (bottom_row) {
        xaxt_tmp <- xaxt
        if (xaxt == "n") xlab <- FALSE
        else xlab <- TRUE
    }
    mgp <- par("mgp")
    if (is.null(args$cex)) cex <- par("cex")
    else cex <- args$cex

    old_par <- par(c("mgp","mgp"))
    on.exit(par(old_par),add=TRUE)

    tckadj <- par("tck")
    if (is.na(tckadj)) tckadj <- -0.01
    if (tckadj < 0) {	# outside ticks, move axis labels 1/2 char width
        mgp[1:2] <- mgp[1:2] + .3
        par(mgp=mgp)
    }

    yaxis_done <- NULL
    xaxis_done <- FALSE

    if (missing(cols)) cols <- seq(from=2,length=ncol(x))

    for (colnum in 1:ncol(x)) {
        varname <- dnames[colnum]

        vunits <- dunits[colnum]
        if (is.null(vunits) || length(vunits) == 0) vunits <- "?"

        varnameunits <- paste0(varname,"(",vunits,")")

        dupunits <- ydupunits[varnameunits]

        datacol <- x[c(t1,t2),colnum]
        if (is.null(datacol)) next

        # 1: left hand side, 2: rhs, 3:second axis on lhs, etc
        yaxis_num <- yscales[[varnameunits]]

        ylim1 <- ylim
        if (is.list(ylim1)) {
            ylim1 <- ylim[[varname]]
            if (is.null(ylim1)) ylim1 <- ylim[[varnameunits]]
        }

        wind_dir <- words(varname,1,1,sep=".")
        wind_dir <- wind_dir == "Dir" || wind_dir == "dir"

        # All data is NAs
        if (any(is.na(ylim1)) || any(is.infinite(ylim1))) ylim1 <- c(-1,1)

        clipped <- FALSE
        cmin <- ylim1[1]
        cmax <- ylim1[2]
        clip.min <- datacol < cmin & !is.na(datacol)
        if (any(clip.min)) {
            datacol@data[clip.min@data] <- NA_real_
            clipped <- TRUE
        }
        clip.max <- datacol > cmax & !is.na(datacol)
        if (any(clip.max)) {
            datacol@data[clip.max@data] <- NA_real_
            clipped <- TRUE
        }

        if (colnum > length(cols))  col <- cols[colnum %% length(cols)]
        else col <- cols[colnum]

        # First trace, create scale, box and xaxis label
        if (! xaxis_done) {
            plot.nts(datacol,type="n",col=1, axes=TRUE,
                xaxt=xaxt_tmp, xaxs=xaxs, xlim=xlim,xlab=xlab,
                yaxt="n", yaxs=yaxs, ylim=ylim1,ylab="",
                err=-1,cex=cex,log=log,...)

            # mfg[1:2] have been incremented
            mfg2 <- par("mfg")
            # cat("mfg2=",paste(mfg,collapse=","),"\n")
            top_row <- mfg2[1] == 1
            bottom_row <- mfg2[1] == mfg2[3]
            last_plot <- identical(mfg2[1:2],mfg2[3:4])

            # if by_row, the column number changes between plots
            by_row <- mfg[2] != mfg2[2]
            assign(".by_row",by_row,envir=.isfsEnv)

            if (!bottom_row && xaxt != "n") timeaxis(1,labels=FALSE,tick=TRUE,time.zone=x@time.zone)

            # put GMT across top of top row
            if (xaxt != "n") timeaxis(3,labels=top_row,tick=TRUE,time.zone="GMT")
            xaxis_done <- TRUE
        }

        par(new=TRUE)

        side <- line <- ylab_txt <- NULL
        if (!missing(ylab)) ylab_txt <- ylab
        if (do.yaxis <- (yaxt != "n" && is.na(match(yaxis_num,yaxis_done)))) {
            if (nscales == 1) {
                side <- 2
                line <- 0
                if (missing(ylab)) {
                    if (all_same_vars) ylab_txt <- paste(varname," (",vunits,")",sep="")
                    else ylab_txt <- paste("(",vunits,")",sep="")
                }
            }
            else {
                iplot <- yaxis_num
                side <- if (iplot %% 2) 2 else 4
                line <- (mgp[1] + 1) * ((iplot-1) %/% 2)
                if (missing(ylab)) {
                    if (dupunits) ylab_txt <- paste(varname," (",vunits,")",sep="")
                    else ylab_txt <- paste("(",vunits,")",sep="")
                }
            }
        }

        # plot traces
        cat("plot.nts, varname=",varname," ylim1=",signif(ylim1,4),"\n")
        plot.nts(datacol,xlim=xlim,ylim=ylim1,type=type,xaxs=xaxs,
            col=col,pch=col,lty=1,ylab="",
            axes=FALSE,err=-1,log=log,lwd=tlwd,...)

        usr <- par("usr")	# used in various places below

        if (clipped) {
            # clip.min and clip.max are nts objects of logical values,
            # TRUE where the data exceeds the min or max
            # plot those points at the corresponding axis
            if (any(as.vector(clp <- clip.min[,1]@data))) {
                tmpd <- clip.min[clp,1]
                tmpd[] <- usr[3]
                points(tmpd,pch=4,col=col,cex=cex)
            }
            if (any(as.vector(clp <- clip.max[,1]@data))) {
                tmpd <- clip.max[clp,1]
                tmpd[] <- usr[4]
                points(tmpd,pch=4,col=col,cex=cex)
            }
        }

        # if (nscales == 1) if (usr[3] * usr[4] < 0) abline(h=0)
        if (usr[3] * usr[4] < 0) abline(h=0,lty=2)

        if (do.yaxis) {
            if (logy) {

                l0lab <- ceiling(usr[3])
                lnlab <- floor(usr[4])

                l0tic <- floor(usr[3])
                lntic <- ceiling(usr[4])

                ndec <- lntic - l0tic
                mtics <- l0tic + as.vector(outer(log10(2:9),(0:ndec-1),"+"))
                if (side == 4) {
                    axis(side=side,at=l0lab:lnlab,labels=10^(l0lab:lnlab),
                        xaxt="s",yaxt="s",col=1,cex=cex*.8,srt=-90)
                    axis(side=side,at=mtics,line=line,labels=FALSE,col=1,xaxt="s",yaxt="s")
                    mtext(side=side,line=line+mgp[1]*.8,ylab_txt,col=1,
                        at=mean(usr[3:4]),srt=-90,cex=cex)
                }
                else {
                    axis(side=side,at=l0lab:lnlab,labels=10^(l0lab:lnlab),
                        xaxt="s",yaxt="s",col=1,cex=cex*.8,srt=90)
                    axis(side=side,at=mtics,line=line,labels=FALSE,col=1,xaxt="s",yaxt="s")
                    mtext(side=side,line=line+mgp[1]*.8,ylab_txt,col=1,cex=cex)
                }
                if (nscales == 1 || length(dnames) == 1) axis(4,labels=FALSE,at=l0lab:lnlab)
            }
            else {
                # special wind direction tic marks
                if (wind_dir) {
                    tic <- diff(pretty(ylim1,nint=4))[1]
                    if (((ylim1[2] - ylim1[1]) / tic) < 2) 
                        tic <- diff(pretty(ylim1,nint=5))[1]
                    if (tic > 180) tic <- 180
                    else if (tic > 90) tic <- 90
                    else if (tic > 45) tic <- 45
                    else if (tic > 30) tic <- 30
                    y1 <- ceiling(ylim1[1]/tic)*tic
                    y2 <- floor(ylim1[2]/tic)*tic
                    if (y2 > y1) {
                        at <- seq(from=y1,to=y2,by=tic)
                        ylabels <- as.character(at)
                        ylabels[at < 0] <- as.character(at[at < 0] + 360)
                        ylabels[at >= 360] <- as.character(at[at >= 360] - 360)
                        abline(h=seq(from=y1,to=y2,by=tic),lty=2)
                    }
                    else {
                        at <- pretty(ylim1)
                        ylabels <- TRUE
                    }
                } else {
                    at <- pretty(ylim1)
                    ylabels <- TRUE
                }
                # rotate right hand side labels so they're easier to read
                # (must use at= argument for srt to work)
                # cat("nscales=",nscales,"length(yaxis_done)=",length(yaxis_done),"side=",side,"at=",at,"line=",line,"ylabels=",paste(ylabels,collapse=","),"\n")
                if (side == 4) {    # right
                    axis(side=side,at=at,line=line,col=1,srt=-90,cex=cex*.8,labels=ylabels)
                    mtext(side=side,line=line+mgp[1]*.8,ylab_txt,col=1,
                        at=mean(usr[3:4]),srt=-90,cex=cex)
                }
                else if (nscales > 1 || length(yaxis_done) == 0) {
                    # axis(side=side,line=line,col=1,cex=cex*.8,at=at,labels=ylabels,adj=1)
                    axis(side=side,at=at,line=line,col=1,srt=90,cex=cex*.8,labels=ylabels)
                    mtext(side=side,line=line+mgp[1]*.8,ylab_txt,col=1,cex=cex)
                }
                if (nscales == 1 || length(dnames) == 1) axis(4,labels=FALSE,at=at)
            }
            yaxis_done <- c(yaxis_done,yaxis_num)
        }


        colv <- c(colv,col)

        if (assemble_legend) {
            if (is.null(stns <- stations(datacol)) || length(stns) == 0 || all(is.na(stns))) {
                if (multunits)
                    legv <- c(legv,varnameunits)
                else
                    legv <- c(legv,varname)
            }
            else if (is.null(names(stns)) || all(names(stns) == "") ||
                all(names(stns) == stns)) {
                if (all_same_vars)
                    legv <- c(legv,ifelse(stns==0 | is.na(stns),"",paste("stn",stns,sep="")))
                else {
                    if (multunits)
                        legv <- c(legv,paste(varnameunits,ifelse(stns==0 | is.na(stns),"",paste("stn",stns,sep="")),sep=" "))
                    else
                        legv <- c(legv,paste(varname,ifelse(stns==0 | is.na(stns),"",paste("stn",stns,sep="")),sep=" "))
                }
            }
        else {
            if (all_same_vars)
                legv <- c(legv,paste(names(stns),
                        ifelse(stns==0 | is.na(stns),"",paste("(",stns,")",sep="")),sep=""))
            else {
                if (multunits)
                    legv <- c(legv,paste(varnameunits," ",names(stns),
                            ifelse(stns==0 | is.na(stns),"",paste("(",stns,")",sep="")),sep=""))
                else
                    legv <- c(legv,paste(varname," ",names(stns),
                            ifelse(stns==0 | is.na(stns),"",paste("(",stns,")",sep="")),sep=""))
            }
        }
        }

        icol <- icol + 1
    }
    if (plot_legend && !is.null(legv)) {
        if (length(legv) != length(colv))
            warning(paste("legend and color vectors are not same length: legend=",
                    paste(legv,collapse=",")," cols=",paste(cols,collapse=",")))
        else {
            cxy <- par("cxy") * cex * 1.0
            lx <- usr[1] + cxy[1] * 1.0
            ly <- usr[4] - cxy[2] * .3

            if (type == "l")
                horiz_legend(lx,ly,legv,col=colv,lty=rep(1,length(colv)),
                    bty="n",cex=cex*1.0)
            else
                horiz_legend(lx,ly,legv,col=colv,marks=colv,
                    bty="n", cex=cex*1.0)
        }
    }

    if (!missing(title) && is.character(title)) title_txt <- title
    else title_txt <- x@title

    if (missing(title) || (is.logical(title) && title) || is.character(title)) {
        if (is.null(title_txt) || length(title_txt) == 0) title_txt <- dnames
        plot_dat_title(title_txt,first_plot,last_plot,t1,t2,plot_stns)
    }

    if (logo && last_plot) logo_stamp()
    par(new=FALSE)
    invisible(NULL)
}

plot_dat_title <- function(title="",first_plot,last_plot,
    t1=dpar("start"),t2=dpar("end"),stns=NULL)
{
    this_plot_date <- unlist(as.list(t1)[c("year","mon","day")])
    if (first_plot) {
        plot_dat_title_save <- NULL
        plot_dat_date <- this_plot_date
        plot_dat_stns <- stns
    }
    else {
        plot_dat_date <- NULL
        if (exists(".plot_dat_date",envir=.isfsEnv))
            plot_dat_date <- get(".plot_dat_date",envir=.isfsEnv)
        if (!is.null(plot_dat_date) && any(this_plot_date != plot_dat_date))
            plot_dat_date <- NULL	# if date changes, don't put in title

        plot_dat_stns <- NULL
        if (exists(".plot_dat_stns",envir=.isfsEnv))
            plot_dat_stns <- get(".plot_dat_stns",envir=.isfsEnv)

        if (is.null(plot_dat_stns) != is.null(stns) ||
            (!is.null(stns) && (length(stns) != length(plot_dat_stns) ||
                    any(sort(stns) != sort(plot_dat_stns)))))
            plot_dat_stns <- NULL	# if stns change, don't put in title

        plot_dat_title_save <- NULL
        if (exists(".plot_dat_title_save",envir=.isfsEnv))
            plot_dat_title_save <- get(".plot_dat_title_save",envir=.isfsEnv)
    }


    # If title is anything other than length 1 or "",
    # append title to plot_dat_title_save 
    # That is, append title to plot_dat_title_save if it is a normal
    # string, like "xxxx", or of length > 1

    havetitle <- length(title) != 1 || title != ""
    if (havetitle) {
        if (length(plot_dat_title_save) != 1 || plot_dat_title_save != "")
            plot_dat_title_save <- unique(c(plot_dat_title_save,title))
        else
            plot_dat_title_save <- title
    }

    if (last_plot) {
        stn_str <- ""
        if (!is.null(plot_dat_stns) && length(plot_dat_stns) > 0 &&
            !(length(plot_dat_stns)==1 && plot_dat_stns==0))
            if (is.null(names(plot_dat_stns)))
                stn_str <- paste(stns,collapse=",")
            else
                stn_str <- paste(paste(names(stns),"(",stns,")",sep=""),collapse=",")
            if (nchar(stn_str > 30)) stn_str <- ""

            title <- paste(stn_str,"  ",
                paste(plot_dat_title_save,collapse=","),sep=" ")

            if (!is.null(plot_dat_date)) {
                t1@time.zone <- t2@time.zone <- getOption("time.zone")
                date.text <- format(t1,format="%Y %b %d")
                day1 <- as.list(t1)[["day"]]
                day2 <- as.list(t2)[["day"]]
                if (t2 - t1 > 86400 || day1 != day2) 
                    date.text <- paste(date.text,format(t2,format=" - %b %d"),sep="")
                title <- paste(title,date.text)
            }

            oma <- par("oma")
            mtext(title,outer=TRUE,line=oma[3]-1.5,cex=par("cex")*1.2)
    }

    if (last_plot && !first_plot && exists(".plot_dat_title_save",envir=.isfsEnv))
        remove(".plot_dat_title_save",envir=.isfsEnv)
    else assign(".plot_dat_title_save",plot_dat_title_save,envir=.isfsEnv)

    assign(".plot_dat_date",plot_dat_date,envir=.isfsEnv)
    assign(".plot_dat_stns",plot_dat_stns,envir=.isfsEnv)

    invisible(title)
}

plotLimits <- function(data,lim,one_scale=FALSE,axs="i",namesep=".")
{

    # unique variable names.
    dnames <- dimnames(data)[[2]]
    dunits <- units(data)

    # Sometimes two data colums may have the same name, but different units
    # e.g.:  P as mb and Pa
    dnameunits <- paste0(dnames,"(",dunits,")")
    dnameunits_unique <- unique(dnameunits)

    scales <- rep(1,length(dnameunits_unique))
    names(scales) <- dnameunits_unique

    # TRUE for variables with different limits but the same units,
    # where the Y axis label should have the variable name
    dupunits <- rep(FALSE,length(dnameunits_unique))
    names(dupunits) <- dnameunits_unique

    # Fixed limits for all traces on plot
    if (!is.null(lim) && !is.list(lim))
        return(list(nscales=1,lim=lim,scales=scales))

    if (one_scale) {
        lim <- range(unlist(sapply(unique(dnames),
                    function(n)
                    {
                        lim <- tryCatch(
                            range(clip(data[,dnames==n]),na.rm=TRUE,finite=TRUE),
                                warning=function(e)e)
                        if (is(lim,"warning")) {
                            # can check if (lim$call[1] == call("clip")
                            if (lim$call[1] == call("min")) lim <- c(Inf,-Inf)
                            else lim <- range(clip(data[,dnames==n]),na.rm=TRUE,finite=TRUE)
                        }
                        if (axs == "r") {
                            dl <- lim[2] - lim[1]
                            lim[1] <- lim[1] - 0.04 * dl
                            lim[2] <- lim[2] + 0.04 * dl
                        }
                        l <- list()
                        l[[n]] <- lim
                        l
                    })),na.rm=TRUE,finite=TRUE)
        # if xaxs or yaxs is "r", extend axis by 4% on each end
        return(list(nscales=1,lim=lim,scales=scales))
    }

    # lim is now either NULL or a list

    # first names of variables, i.e. "dir" for "dir.2m"
    # these may have repeated values
    dnames1 <- words(dnames,1,1,sep=namesep)

    udnames <- unique(dnames)
    udnames1 <- unique(dnames1)

    rmtch <- match(names(lim),udnames,nomatch=0)
    rmtch1 <- match(names(lim),udnames1,nomatch=0)
    # reduce lim to those variables that we have
    lim <- lim[rmtch!=0 | rmtch1 != 0]

    # the length of udunits is the first guess at the number of
    # different scales on the yaxis.
    udunits <- unique(dunits)

    # We may need more scales because of the requested lims
    lim_res <- list()

    iscale <- 0
    for (units_str in udunits) {
        # logical vector along dunits of variables that have units equal to
        # unit.str
        umtch <- !is.na(match(dunits,units_str))

        # variable names with units equal to units_str
        dnu <- unique(dnames[umtch])
        dnu1 <- words(dnu,1,1,sep=namesep)
        dnuu <- paste0(dnu,"(",units_str,")")

        # ylmtch: for each variable with units equal to units_str,
        # the index in lim of its limits
        ylmtch <- match(dnu,names(lim),nomatch=0)
        ylmtch1 <- match(dnu1,names(lim),nomatch=0)

        # use first name match if not a complete match
        ylmtch[ylmtch == 0] <- ylmtch1[ylmtch == 0]

        if (any(ylmtch != 0)) {
            lim_res[dnuu[ylmtch!=0]] <- lim[ylmtch]
            iscale <- iscale + 1
            scales[dnuu[ylmtch!=0]] <- iscale

            dupunits[dnuu[ylmtch!=0]] <- (sum(umtch) > 1)

            chk <- FALSE
            # check for more than one limit
            if (length(unique(ylmtch[ylmtch!=0])) > 1) {
                chk <- unlist(lapply(lim[ylmtch][-1],
                        function(x,y) { x[1]!=y[1] || x[2]!=y[2] },
                        lim[ylmtch][[1]]))
            }
            if (any(chk)) {
                if (FALSE) {
                    warning("variables ",paste(dnu[ylmtch!=0],collapse=","),	
                        " have units \"",
                        units_str, "\" but have different limits=",
                        paste(names(lim[ylmtch]),lim[ylmtch],sep="=",collapse=","),"\n")
                }
                ylmtchk <- ylmtch[ylmtch!=0][-1][chk]	# indices into lim

                for (i in ylmtchk) {
                    iscale <- iscale + 1
                    dylmtch <- match(i,ylmtch,nomatch=0)
                    scales[dnuu[dylmtch]] <- iscale
                }
            }
        }
        if (any(ylmtch == 0)) {
            # No values in lim for these variables
            lim <- range(unlist(sapply(dnu[ylmtch==0],
                        function(n)
                        {
                            lim <- tryCatch(
                                range(clip(data[,dnames==n]),na.rm=TRUE,finite=TRUE),
                                    warning=function(e)e)
                            if (is(lim,"warning")) {
                                # can check if (lim$call[1] == call("clip")
                                if (lim$call[1] == call("min")) lim <- c(Inf,-Inf)
                                else lim <- range(clip(data[,dnames==n]),na.rm=TRUE,finite=TRUE)
                            }
                            if (axs == "r") {
                                dl <- lim[2] - lim[1]
                                lim[1] <- lim[1] - 0.04 * dl
                                lim[2] <- lim[2] + 0.04 * dl
                            }
                            l <- list()
                            l[[n]] <- lim
                            l
                        })),na.rm=TRUE,finite=TRUE)


            lim_res[dnuu[ylmtch==0]] <- list(lim)
            iscale <- iscale + 1
            scales[dnuu[ylmtch==0]] <- iscale
        }
    }

    # dn <- names(lim_res)
    lim <- list(nscales=iscale,lim=lim_res,scales=scales,dupunits=dupunits)
    cat("nscales=",iscale," units=",paste("\"",dunits,"\"",sep="",collapse=","),"\n")

    lim
}

logo_stamp <- function(print.motto=TRUE,cex=0.75)
{
    oma <- par("oma")

    mex <- par("mex")

    lineoff <- cex / mex + .1

    if (is.null(platform <- dpar("platform"))) {
        platform <- Sys.getenv("PLATFORM")
        if (nchar(platform) == 0) platform <- "NCAR ISFS"
    }
    string <- paste(platform,format(utime("now"),format="%H:%M %h %d %Y %Z"))
    mtext(string,side=1,line=oma[1]-lineoff,outer=TRUE,adj=1,cex=cex)

    string <- Sys.getenv("PROJECT")

    # print.motto=F will prevent printing of motto
    # print.motto can also be a string variable to replace
    # getOption("motto")
    if( is.logical(print.motto) && print.motto )
        print.motto <- getOption("motto")
    if(!is.null(print.motto) && is.character(print.motto) )
        string <- paste(string,print.motto,sep=": ")

    mtext(string,side=1,line=oma[1]-lineoff,outer=TRUE,adj=0,cex=cex)

}

trellis_logo <- function(print.motto=TRUE,cex=0.75)
{
    # add a logo to a trellis (lattice) plot
    # this function should be called from the
    # page function of a lattice plot
    # xyplot(...,
    #       page=function(page) { trellis_logo() }
    # )
    if (is.null(platform <- dpar("platform"))) {
        platform <- Sys.getenv("PLATFORM")
        if (nchar(platform) == 0) platform <- "NCAR ISFS"
    }
    string <- paste(platform,
        format(utime("now"),format="%H:%M %h %d %Y %Z"))

    grid::grid.text(string,x=1, y=0, just=c(1,0),gp=grid::gpar(cex=cex))

    string <- Sys.getenv("PROJECT")

    # print.motto=F will prevent printing of motto
    # print.motto can also be a string variable to replace
    # getOption("motto")
    if( is.logical(print.motto) && print.motto )
        print.motto <- getOption("motto")
    if(!is.null(print.motto) && is.character(print.motto) )
        string <- paste(string,print.motto,sep=": ")

    grid::grid.text(string,x=0, y=0, just=c(0,0),gp=grid::gpar(cex=cex))
    NULL
}

adjPar <- function(nxscales=1,nyscales=1)
{
    # This function tightens up the plot margins, setting the par options:
    # par,mar and mfrow based on input.
    #
    # nyscales:  number of scales desired on y axis
    #	room for these scales will be set aside using the mar
    #	parameter to par, in an alternating fashion, first on
    #	on the left, then right,left etc.
    #
    # We reduce wasted space in stacked plots (prows > 1) by reducing
    # the margin lines above and below each plot, and use the outer
    # margin lines for the top and bottom axis labels. This works
    # well when all plots have the same time axis, and the
    # time labels do not need to be repeated for each plot.

    # line spacing in cex
    linecex <- 1.3

    # get cex before calling mfrow
    cex <- par("cex")

    # line of axis title, labels, axis
    mgp <- c(linecex + 0.3,0.3,0)

    # lines of margin on each side of plot
    mar <- rep(0,4)

    # separate Y scales, reserve margin space
    lscales <- (nyscales-1) %/% 2 + 1
    rscales <- nyscales %/% 2

    mar[2] <- max((mgp[1] + linecex) * lscales,linecex)
    mar[4] <- max((mgp[1] + linecex) * rscales,linecex)

    bscales <- (nxscales-1) %/% 2 + 1
    tscales <- nxscales %/% 2
    mar[1] <- max((mgp[1] + linecex) * bscales,linecex)
    mar[3] <- max((mgp[1] + linecex) * tscales,linecex)

    # outer margin lines
    oma <- c(linecex,linecex,linecex*2,linecex)

    # oma[1] <- max(5.0 - mar[1],linecex)
    # oma[3] <- max(5.0 - mar[3],linecex)
    # oma[2] <- oma[4] <- linecex

    # warning:  put cex and mex before mar and oma in this par call.
    # Set cex to original value, and mex to cex, so margin lines are
    # measured in cex units
    par(cex=cex,mex=cex,oma=oma,mar=mar,mgp=mgp)
}
