# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

# Plot method for dat objects

setMethod("plot",signature(x="dat",y="missing"),
    function(x,...) 
    {
        invisible(plot_dat(x, ...))
    }
    )

plot_dat <- function(x,type="l",
    xlab,xlim, xaxs="i", xaxt="s",
    ylab,ylim=NULL, yaxs=par("yaxs"), yaxt=par("yaxt"),
    log="", cols, one_scale=FALSE,
    tlwd=par("lwd"),remargin=TRUE,title,logo=TRUE, debug=FALSE, ...)
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

    dnames <- colnames(x)

    nextp <- eolts::next_plot()

    if (nextp$first) xaxs <- "i"
    else xaxs <- "d"                # don't rescale

    # unique looses the station names
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

    yinfo <- plotLimits(x,ylim,one_scale,axs=yaxs,log=log)
    nyscales <- yinfo$nyscales
    yscales <- yinfo$scales
    ylim <- yinfo$lim
    # TRUE for variables with different limits but the same units.
    # If TRUE, we want to add the variable name to the Y axis
    ydupunits <- yinfo$dupunits

    if (remargin && nextp$first) side_margins(nyscales=nyscales)

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

    xlab <- FALSE
    if (nextp$nrow == nextp$nrows) {
        if (xaxt == "n") xlab <- FALSE
        else xlab <- TRUE
    }

    tckadj <- par("tck")
    if (is.na(tckadj)) tckadj <- -0.01
    # if (tckadj < 0) {	# outside ticks, move axis labels 1/2 char width
    #     mgp[1:2] <- mgp[1:2] + .3
    #     par(mgp=mgp)
    # }

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
        if (any(is.na(ylim1)) || any(is.infinite(ylim1))) {
            if (log == "y") ylim1 <- c(1,10)
            else ylim1 <- c(-1,1)
        }

        clipped <- FALSE
        cmin <- ylim1[1]
        cmax <- ylim1[2]

        # Use !is.na(datacol@data) for the second operand here so that
        # it is a simple matrix, not an nts.
        # Then the times are not aligned in the '&' expression, which
        # raises warnings if they are not sorted.
        clip.min <- datacol < cmin & !is.na(datacol@data)
        if (any(clip.min)) {
            datacol@data[clip.min@data] <- NA_real_
            clipped <- TRUE
        }
        clip.max <- datacol > cmax & !is.na(datacol@data)
        if (any(clip.max)) {
            datacol@data[clip.max@data] <- NA_real_
            clipped <- TRUE
        }

        if (colnum > length(cols))  col <- cols[colnum %% length(cols)]
        else col <- cols[colnum]

        # First trace, sets up plot scaling, plots box and xaxis label,
        # no yaxis
        if (! xaxis_done) {
            if (debug) 
                cat("plot_dat, first plot_nts, varname=",varname,
                    ", ylim1=",signif(ylim1,4),
                    ", side=", side, ",
                    ", nyscales=", nyscales, ",
                    ", xaxs=", xaxs,
                    ", yaxs=", yaxs,
                    ", mgp=", paste0(par("mgp"),collapse=","),
                    ", mar=", paste0(par("mar"),collapse=","),
                    ", yaxis_done=", paste0(yaxis_done,collapse=","),"\n")
                
            plot_nts(datacol,type="n",col=1, axes=TRUE, remargin=remargin,
                xlim=xlim,xlab=xlab,
                ylim=ylim1,ylab="",
                xaxt=xaxt, xaxs=xaxs,
                yaxt="n", yaxs=yaxs,
                log=log,...)

            currp <- eolts::current_plot()

            if (debug)
                cat("varname=", varname,
                    ", nrow=", currp$nrow,
                    ", nrows=", currp$nrows, "\n")

            if (xaxt != "n") {
                if (currp$nrow == currp$nrows)
                    timeaxis(1, labels=TRUE, tick=TRUE, xlab=xlab,
                        time.zone=x@time.zone)
                # else
                #     timeaxis(1, labels=FALSE, tick=TRUE,
                #       time.zone=x@time.zone)
                # put GMT across top of top row
                if (currp$nrow == 1)
                    timeaxis(3, labels=TRUE, xlab=FALSE,
                        tick=TRUE,time.zone="GMT", date.too=TRUE)
                else
                    timeaxis(3, labels=FALSE, xlab=FALSE, tick=TRUE)
            }
            xaxis_done <- TRUE
        }

        par(new=TRUE)   # next plotting command should not clean the frame

        # plot traces
        if (debug) 
            cat("plot_dat, plot_nts of traces, varname=",varname," ylim1=",signif(ylim1,4),
                ", side=", side, ",
                ", nyscales=", nyscales, ",
                ", xaxs=", xaxs,
                ", yaxs=", yaxs,
                ", mgp=", paste0(par("mgp"),collapse=","),
                ", mar=", paste0(par("mar"),collapse=","),
                ", yaxis_done=", paste0(yaxis_done,collapse=","),"\n")

        plot_nts(datacol,xlim=xlim,ylim=ylim1,type=type,
            xaxs=xaxs, xaxt="n", yaxs=yaxs, yaxt="n",
            col=col,pch=col,lty=1,ylab="",
            axes=FALSE,log=log,lwd=tlwd,...)

        usr <- par("usr")

        if (clipped) {
            # clip.min and clip.max are nts objects of logical values,
            # TRUE where the data exceeds the min or max
            # plot those points at the corresponding axis
            if (any(as.vector(clp <- clip.min[,1]@data))) {
                tmpd <- clip.min[clp,1]
                tmpd[] <- usr[3]
                points(tmpd,pch=4,col=col)
            }
            if (any(as.vector(clp <- clip.max[,1]@data))) {
                tmpd <- clip.max[clp,1]
                tmpd[] <- usr[4]
                points(tmpd,pch=4,col=col)
            }
        }

        # dotted line at y=0
        if (usr[3] * usr[4] < 0) abline(h=0,lty=2)

        mgp <- par("mgp")

        if (yaxt != "n" && is.na(match(yaxis_num,yaxis_done))) {

            ylab_txt <- NULL

            if (!missing(ylab)) {
                if (is.logical(ylab)) {
                     if (!ylab) ylab_txt <- ""
                }
                else ylab_txt <- ylab
            }
            if (nyscales == 1) {
                side <- 2
                yaxis_line <- 0
                if (is.null(ylab_txt)) {
                    if (all_same_vars)
                        ylab_txt <- paste(varname," (",vunits,")",sep="")
                    else ylab_txt <- paste("(",vunits,")",sep="")
                }
            }
            else {
                iplot <- yaxis_num
                side <- if(iplot %% 2) 2 else 4
                ny_on_side <- (iplot-1) %/% 2
                yaxis_line <- ny_on_side * (mgp[1] + 1.0)
                if (is.null(ylab_txt)) {
                    if (dupunits)
                        ylab_txt <- paste(varname," (",vunits,")",sep="")
                    else ylab_txt <- paste("(",vunits,")",sep="")
                }
            }

            if (logy) {

                l0lab <- ceiling(usr[3])
                lnlab <- floor(usr[4])

                l0tic <- floor(usr[3])
                lntic <- ceiling(usr[4])

                ndec <- lntic - l0tic
                mtics <- l0tic + as.vector(outer(log10(2:9),(0:ndec-1),"+"))

                axis(side=side,line=yaxis_line, at=10^(l0lab:lnlab),
                    labels=10^(l0lab:lnlab),
                    yaxt=yaxt, col=1)
                # minor log ticks
                axis(side=side,line=yaxis_line, at=10^mtics,labels=FALSE,
                    yaxt=yaxt, col=1)
                if (ylab_txt != "")
                    mtext(side=side,line=yaxis_line+mgp[1],
                        text=ylab_txt, col=1,
                        cex=par("cex.axis")*par("cex"))

                # repeat labels on right if one Y scale
                if (nyscales == 1 || length(dnames) == 1) {
                    axis(4, line=yaxis_line, at=10^(l0lab:lnlab),
                        labels=FALSE, yaxt=yaxt, col=1)
                    axis(4, line=yaxis_line, at=10^mtics, labels=FALSE,
                        yaxt=yaxt, col=1)
                }
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
                        at <- NULL
                        ylabels <- TRUE
                    }
                } else {
                    at <- NULL
                    ylabels <- TRUE
                }
                # rotate right hand side labels so they're easier to read
                # cat("nyscales=",nyscales,"length(yaxis_done)=",length(yaxis_done),"side=",side,"at=",at,"ylabline=",ylabline,"ylabels=",paste(ylabels,collapse=","),"\n")
                if (debug)
                    cat("ylab_txt=", ylab_txt, ", yaxis_line=", yaxis_line,
                        ", mgp[1]=", mgp[1],
                        ", ylabels=", paste(ylabels, collapse=","),"\n")

                axis(side=side,at=at,line=yaxis_line,col=1, yaxt=yaxt,
                    labels=ylabels)
                if (ylab_txt != "") mtext(text=ylab_txt, side=side, line=yaxis_line + mgp[1],
                    col=1, cex=par("cex.lab") * par("cex"))

                if (nyscales == 1 || length(dnames) == 1)
                    axis(4,labels=FALSE,at=at, line=yaxis_line)
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
            cxy <- par("cxy") * par("cex.lab") * par("cex")
            lx <- usr[1] + cxy[1] * 1.0
            ly <- usr[4] - cxy[2] * .3

            if (type == "l")
                horiz_legend(lx,ly,legv,col=colv,lty=rep(1,length(colv)),
                    bty="n",cex=par("cex.lab"))
            else
                horiz_legend(lx,ly,legv,col=colv,marks=colv,
                    bty="n", cex=par("cex.lab"))
        }
    }

    if (!missing(title) && is.character(title)) title_txt <- title
    else title_txt <- x@title

    if (missing(title) || (is.logical(title) && title) || is.character(title)) {
        if (is.null(title_txt) || length(title_txt) == 0) title_txt <- dnames
        plot_dat_title(title_txt,nextp$first,currp$last_plot,t1,t2,plot_stns, time.zone=x@time.zone)
    }

    if (logo && currp$last_plot) logo_stamp()
    par(new=FALSE)
    invisible(NULL)
}

plot_dat_title <- function(title="",first_plot,last_plot,
    t1=dpar("start"),t2=dpar("end"),stns=NULL,
    time.zone=getOption("time.zone"))
{
    this_plot_date <- unlist(as.list(t1, time.zone=time.zone)[c("year","mon","day")])
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
                date.text <- format(t1,format="%Y %b %d", time.zone=time.zone)
                day1 <- as.list(t1, time.zone=time.zone)[["day"]]
                day2 <- as.list(t2, time.zone=time.zone)[["day"]]
                if (t2 - t1 > 86400 || day1 != day2) 
                    date.text <- paste(date.text,
                        format(t2,format=" - %b %d", time.zone=time.zone),sep="")
                title <- paste(title,date.text)
            }

            oma <- par("oma")
            # mtext(title,outer=TRUE,line=oma[3]-1.5,
            #     cex=par("cex.main") * par("cex"))
            title(main=title,outer=TRUE,line=oma[3]-1.5)
    }

    if (last_plot && !first_plot && exists(".plot_dat_title_save",envir=.isfsEnv))
        remove(".plot_dat_title_save",envir=.isfsEnv)
    else assign(".plot_dat_title_save",plot_dat_title_save,envir=.isfsEnv)

    assign(".plot_dat_date",plot_dat_date,envir=.isfsEnv)
    assign(".plot_dat_stns",plot_dat_stns,envir=.isfsEnv)

    invisible(title)
}

plotLimits <- function(data,lim,one_scale=FALSE,axs="i",namesep=".",log="")
{

    # unique variable names.
    dnames <- colnames(data)
    dunits <- units(data)

    # Sometimes two data colums may have the same name, but different units
    # e.g.:  P as mb and Pa
    dnameunits <- paste0(dnames,"(",dunits,")")
    dnameunits_unique <- unique(dnameunits)

    # 1: left hand side, 2: rhs, 3:second axis on lhs, etc
    scales <- rep(1,length(dnameunits_unique))
    names(scales) <- dnameunits_unique

    # TRUE for variables with different limits but the same units,
    # where the Y axis label should have the variable name
    dupunits <- rep(FALSE,length(dnameunits_unique))
    names(dupunits) <- dnameunits_unique

    # Fixed limits for all traces on plot
    if (!is.null(lim) && !is.list(lim))
        return(list(nyscales=1,lim=lim,scales=scales))

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
                            if (log == "y") {
                                dl <- log10(lim[2]) - log10(lim[1])
                                lim[1] <- 10^(log10(lim[1]) - 0.04 * dl)
                                lim[2] <- 10^(log10(lim[2]) + 0.04 * dl)
                            }
                            else {
                                dl <- lim[2] - lim[1]
                                lim[1] <- lim[1] - 0.04 * dl
                                lim[2] <- lim[2] + 0.04 * dl
                            }
                        }
                        l <- list()
                        l[[n]] <- lim
                        l
                    })),na.rm=TRUE,finite=TRUE)
        # if xaxs or yaxs is "r", extend axis by 4% on each end
        return(list(nyscales=1,lim=lim,scales=scales))
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
            # 1: left hand side, 2: rhs, 3:second axis on lhs, etc
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
                                if (log == "y") {
                                    dl <- log10(lim[2]) - log10(lim[1])
                                    lim[1] <- 10^(log10(lim[1]) - 0.04 * dl)
                                    lim[2] <- 10^(log10(lim[2]) + 0.04 * dl)
                                }
                                else {
                                    dl <- lim[2] - lim[1]
                                    lim[1] <- lim[1] - 0.04 * dl
                                    lim[2] <- lim[2] + 0.04 * dl
                                }
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
    lim <- list(nyscales=iscale,lim=lim_res,scales=scales,dupunits=dupunits)

    lim
}

logo_stamp <- function(print.motto=TRUE,cex=0.75 * par("cex"))
{
    oma <- par("oma")

    mex <- par("mex")

    lineoff <- cex / mex + .8

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

trellis_logo <- function(print.motto=TRUE,cex=0.75 * par("cex"))
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

side_margins <- function(nyscales=1)
{
    # tighten up left and right plot margins
    #
    # nyscales:  number of scales desired on y axis
    #	room for these scales will be set aside using the mar
    #	parameter to par, in an alternating fashion, first on
    #	on the left, then right,left etc.
    #

    mar <- par("mar")
    mgp <- eolts::tight_mgp()

    # separate Y scales, reserve margin space
    lscales <- (nyscales+1) %/% 2       # left scales
    rscales <- max(nyscales %/% 2, 1)   # right scales

    # add tics
    mar[2] <- (mgp[1] + 1.0) * lscales + 0.5
    mar[4] <- (mgp[1] + 1.0) * rscales + 0.5

    par(mar=mar, mgp=mgp)
}
