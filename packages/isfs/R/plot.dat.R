# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
#               Copyright (C) by UCAR
# 

setMethod("plot",signature(x="dat",y="missing"),
    function(x,...) 
    {
        invisible(plot.dat(x, ...))
    }
)

plot.dat <- function(x,type="l",xlab,xlim,ylab,ylim=NULL,one.scale=F,
        log="",tlwd=par("lwd"),remargin=T,title,logo=T,
	xaxt,yaxt,cols,...)
{

    # tlwd: trace line width.  Use that option if you want to
    # 	change the trace line width without changing the line width
    #	of the axes.
    #

    # Strange, had to use xlimx variable below, if it was xlim,
    # it complained if it was missing, even though we set it here.
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
    nplotted <- (mfg[1] - 1) * mfg[4] + mfg[2]
    nplot <- mfg[3] * mfg[4]
    if (nplotted == nplot) nplotted <- 0
    # cat("nplotted=",nplotted," nplot=",nplot," mfg=",mfg,"\n")

    first.plot <- nplotted == 0
    first.row <- nplotted < mfg[4]
    last.plot <- nplotted == nplot - 1
    bottom.row <- nplotted >= nplot - mfg[4] 

    # unique loses the station names
    plot.stns <- stations(x)
    if (!is.null(plot.stns)) {
        i <- sort(unique(plot.stns))
        if (!is.null(nstns <- names(plot.stns))) {
            names(i) <- nstns[match(i,plot.stns)]
            if (all(names(i) == as.character(i))) names(i) <- NULL
        }
        plot.stns <- i
    }

    dunits <- x@units
    names(dunits) = dnames

    yinfo <- plot.dat.limits(x,ylim,one.scale)
    nscales <- yinfo$nscales

    if (nscales > 1 && remargin) std.par(nscales)

    ylim <- yinfo$ylim
    yscales <- yinfo$yscales

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

    assemble.legend <- T
    plot.legend <- T

    legv <- x@attributes$legend
    if (!is.null(legv)) {
        if (is.logical(legv)) {
            assemble.legend <- plot.legend <- legv
            legv <- NULL
        }
        else {
            legv <- as.character(legv)
            assemble.legend <- F
        }
    }
    all.same.vars <- all(dnames == dnames[1])
    if (all.same.vars) {
        # perhaps they have different units
        multunits = length(unique(dunits)) > 1
        if (multunits) all.same.vars = F
    }

    args <- list(...)

    if (par("xaxt") != "n") par(xaxt="s")
    if (missing(xaxt)) xaxt = par("xaxt")

    logy = any(substring(log,1:nchar(log),1:nchar(log)) == "y")
    if (par("yaxt") != "n") {
        if (logy) par(yaxt="l")
        else par(yaxt="s")
    }
    if (missing(yaxt)) yaxt = par("yaxt")

    xaxt.tmp <- "n"
    xlab <- F
    if (bottom.row) {
        xaxt.tmp <- xaxt
        if (xaxt == "n") xlab <- F
        else xlab <- T
    }
    mgp <- par("mgp")
    if (is.null(args$cex)) cex <- par("cex")
    else cex <- args$cex

    old.par <- par(c("mgp","mgp"))
    on.exit(par(old.par),add=T)

    tckadj <- par("tck")
    if (is.na(tckadj)) tckadj = -0.01
    if (tckadj < 0) {	# outside ticks, move axis labels 1/2 char width
        mgp[1:2] <- mgp[1:2] + .3
        par(mgp=mgp)
    }

    yaxes.done <- NULL
    xaxes.done <- F

    if (missing(cols)) cols = seq(from=2,length=ncol(x))

    for (colnum in 1:ncol(x)) {
        varname <- dnames[colnum]

        vunits <- dunits[colnum]
        if (is.null(vunits) || length(vunits) == 0) vunits <- "?"

        # Check if this varname has multiple units
        umatch = !is.na(match(names(dunits),varname))
        multunits = length(unique(dunits[umatch])) > 1

        varnameunits = paste(varname,"(",vunits,")",sep="")

        dupunits = ydupunits[varnameunits]

        datacol <- x[c(t1,t2),colnum]
        if (is.null(datacol)) next

        # 1: left hand side, 2: rhs, 3:second axis on lhs, etc
        yaxis.num <- yscales[[varnameunits]]

        ylim1 <- ylim
        if (is.list(ylim1)) {
            ylim1 <- ylim[[varname]]
            if (is.null(ylim1)) ylim1 = ylim[[varnameunits]]
        }

        wind.dir <- words(varname,1,1,sep=".")
        wind.dir <- wind.dir == "Dir" || wind.dir == "dir"

        # All data is NAs
        if (any(is.na(ylim1)) || any(is.infinite(ylim1))) ylim1 <- c(-1,1)

        clipped <- F
        cmin <- ylim1[1]
        cmax <- ylim1[2]
        clip.min <- datacol < cmin & !is.na(datacol)
        if (any(clip.min)) {
            datacol@data[clip.min@data] <- NA_real_
            clipped <- T
        }
        clip.max <- datacol > cmax & !is.na(datacol)
        if (any(clip.max)) {
            datacol@data[clip.max@data] <- NA_real_
            clipped <- T
        }

        if (colnum > length(cols))  col = cols[colnum %% length(cols)]
        else col = cols[colnum]

        # First trace, create scale, box and xaxis label
        if (! xaxes.done) {
            plot.nts(datacol,xlim=xlim,type="n",col=1,
              xaxt=xaxt.tmp,yaxt="n",ylim=ylim1,axes=T,ylab="",xlab=xlab,
                      err=-1,cex=cex,log=log,...)
            if (!bottom.row && xaxt != "n") timeaxis(1,labels=F,tick=T,time.zone=x@time.zone)

            # put GMT across top of first row
            if (xaxt != "n") timeaxis(3,labels=first.row,tick=T,time.zone="GMT")
            xaxes.done <- T
        }

        par(new=T)

        side <- line <- ylab.txt <- NULL
        if (!missing(ylab)) ylab.txt <- ylab
        if (do.yaxis <- (yaxt != "n" && is.na(match(yaxis.num,yaxes.done)))) {
            if (nscales == 1) {
                side <- 2
                line <- 0
                if (missing(ylab)) {
                  if (all.same.vars) ylab.txt <- paste(varname," (",vunits,")",sep="")
                  else ylab.txt <- paste("(",vunits,")",sep="")
                }
            }
            else {
                iplot <- yaxis.num
                side <- if (iplot %% 2) 2 else 4
                line <- (mgp[1] + 1) * ((iplot-1) %/% 2)
                if (missing(ylab)) {
                  if (dupunits) ylab.txt <- paste(varname," (",vunits,")",sep="")
                  else ylab.txt <- paste("(",vunits,")",sep="")
                }
            }
        }

        # plot traces
        cat("plot.nts, varname=",varname," ylim1=",signif(ylim1,4),"\n")
        plot.nts(datacol,xlim=xlim,ylim=ylim1,type=type,
              col=col,pch=col,lty=1,ylab="",
              axes=F,err=-1,log=log,lwd=tlwd,...)

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
                  axis(side=side,at=mtics,line=line,labels=F,col=1,xaxt="s",yaxt="s")
                  mtext(side=side,line=line+mgp[1]*.8,ylab.txt,col=1,
                        at=mean(usr[3:4]),srt=-90,cex=cex)
                }
                else {
                  axis(side=side,at=l0lab:lnlab,labels=10^(l0lab:lnlab),
                        xaxt="s",yaxt="s",col=1,cex=cex*.8,srt=90)
                  axis(side=side,at=mtics,line=line,labels=F,col=1,xaxt="s",yaxt="s")
                  mtext(side=side,line=line+mgp[1]*.8,ylab.txt,col=1,cex=cex)
                }
                if (nscales == 1 || length(dnames) == 1) axis(4,labels=F,at=l0lab:lnlab)
            }
            else {
                # special wind direction tic marks
                if (wind.dir) {
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
                        ylabels <- T
                    }
                } else {
                    at <- pretty(ylim1)
                    ylabels <- T
                }
                # rotate right hand side labels so they're easier to read
                # (must use at= argument for srt to work)
                # cat("nscales=",nscales,"length(yaxes.done)=",length(yaxes.done),"side=",side,"at=",at,"line=",line,"ylabels=",paste(ylabels,collapse=","),"\n")
                if (side == 4) {
                    axis(side=side,at=at,line=line,col=1,srt=-90,cex=cex*.8,labels=ylabels)
                    mtext(side=side,line=line+mgp[1]*.8,ylab.txt,col=1,
                          at=mean(usr[3:4]),srt=-90,cex=cex)
                }
                else if (nscales > 1 || length(yaxes.done) == 0) {
                    # axis(side=side,line=line,col=1,cex=cex*.8,at=at,labels=ylabels,adj=1)
                    axis(side=side,at=at,line=line,col=1,srt=90,cex=cex*.8,labels=ylabels)
                    mtext(side=side,line=line+mgp[1]*.8,ylab.txt,col=1,cex=cex)
                }
                if (nscales == 1 || length(dnames) == 1) axis(4,labels=F,at=at)
            }
            yaxes.done <- c(yaxes.done,yaxis.num)
        }


        colv <- c(colv,col)

        if (assemble.legend) {
            if (is.null(stns <- stations(datacol)) || length(stns) == 0 || all(is.na(stns))) {
                if (multunits)
                    legv <- c(legv,varnameunits)
                else
                    legv <- c(legv,varname)
            }
            else if (is.null(names(stns)) || all(names(stns) == "") ||
                all(names(stns) == stns)) {
                if (all.same.vars)
                  legv <- c(legv,ifelse(stns==0 | is.na(stns),"",paste("stn",stns,sep="")))
                else {
                  if (multunits)
                    legv <- c(legv,paste(varnameunits,ifelse(stns==0 | is.na(stns),"",paste("stn",stns,sep="")),sep=" "))
                  else
                    legv <- c(legv,paste(varname,ifelse(stns==0 | is.na(stns),"",paste("stn",stns,sep="")),sep=" "))
                }
            }
            else {
                if (all.same.vars)
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
    if (plot.legend && !is.null(legv)) {
        if (length(legv) != length(colv))
            warning(paste("legend and color vectors are not same length: legend=",
              paste(legv,collapse=",")," cols=",paste(cols,collapse=",")))
        else {
            cxy <- par("cxy") * cex * 1.0
            lx <- usr[1] + cxy[1] * 1.0
            ly <- usr[4] - cxy[2] * .3

            if (type == "l")
              horiz.legend(lx,ly,legv,col=colv,lty=rep(1,length(colv)),
                  bty="n",cex=cex*1.0)
            else
              horiz.legend(lx,ly,legv,col=colv,marks=colv,
                  bty="n", cex=cex*1.0)
        }
    }

    if (!missing(title) && is.character(title)) title.txt <- title
    else title.txt <- x@title

    if (missing(title) || (is.logical(title) && title) || is.character(title)) {
        if (is.null(title.txt) || length(title.txt) == 0) title.txt <- dnames
        plot.dat.title(title.txt,first.plot,last.plot,t1,t2,plot.stns)
    }

    if (logo && last.plot) logo_stamp()
    par(new=F)
    NULL
}

plot.dat.title <- function(title="",first.plot,last.plot,
	t1=dpar("start"),t2=dpar("end"),stns=NULL)
{
    this.plot.date <- unlist(as.list(t1)[c("year","mon","day")])
    if (first.plot) {
        plot.dat.title.save <- NULL
        plot.dat.date <- this.plot.date
        plot.dat.stns <- stns
    }
    else {
        plot.dat.date = NULL
        if (exists(".plot.dat.date",envir=.isfsEnv))
            plot.dat.date = get(".plot.dat.date",envir=.isfsEnv)
        if (!is.null(plot.dat.date) && any(this.plot.date != plot.dat.date))
              plot.dat.date <- NULL	# if date changes, don't put in title

        plot.dat.stns = NULL
        if (exists(".plot.dat.stns",envir=.isfsEnv))
            plot.dat.stns = get(".plot.dat.stns",envir=.isfsEnv)

        if (is.null(plot.dat.stns) != is.null(stns) ||
            (!is.null(stns) && (length(stns) != length(plot.dat.stns) ||
                    any(sort(stns) != sort(plot.dat.stns)))))
              plot.dat.stns <- NULL	# if stns change, don't put in title

        plot.dat.title.save = NULL
        if (exists(".plot.dat.title.save",envir=.isfsEnv))
            plot.dat.title.save = get(".plot.dat.title.save",envir=.isfsEnv)
    }


    # If title is anything other than length 1 or "",
    # append title to plot.dat.title.save 
    # That is, append title to plot.dat.title.save if it is a normal
    # string, like "xxxx", or of length > 1

    havetitle <- length(title) != 1 || title != ""
    if (havetitle) {
        if (length(plot.dat.title.save) != 1 || plot.dat.title.save != "")
            plot.dat.title.save <- unique(c(plot.dat.title.save,title))
        else
            plot.dat.title.save <- title
    }

    if (last.plot) {
        stn.str <- ""
        if (!is.null(plot.dat.stns) && length(plot.dat.stns) > 0 &&
            !(length(plot.dat.stns)==1 && plot.dat.stns==0))
          if (is.null(names(plot.dat.stns)))
              stn.str <- paste(stns,collapse=",")
          else
              stn.str <- paste(paste(names(stns),"(",stns,")",sep=""),collapse=",")
        if (nchar(stn.str > 30)) stn.str <- ""

        title <- paste(stn.str,"  ",
            paste(plot.dat.title.save,collapse=","),sep=" ")

        if (!is.null(plot.dat.date)) {
            t1@time.zone <- unlist(options("time.zone"))
            t2@time.zone <- unlist(options("time.zone"))
            date.text <- format(t1,format="%Y %b %d")
            day1 <- as.list(t1)[["day"]]
            day2 <- as.list(t2)[["day"]]
            if (t2 - t1 > 86400 || day1 != day2) 
              date.text <- paste(date.text,format(t2,format=" - %b %d"),sep="")
            title <- paste(title,date.text)
        }

        oma <- par("oma")
        mtext(title,outer=T,line=oma[3]-1.5,cex=par("cex")*1.2)
    }
    
    if (last.plot && !first.plot && exists(".plot.dat.title.save",envir=.isfsEnv))
        remove(".plot.dat.title.save",envir=.isfsEnv)
    else assign(".plot.dat.title.save",plot.dat.title.save,envir=.isfsEnv)

    assign(".plot.dat.date",plot.dat.date,envir=.isfsEnv)
    assign(".plot.dat.stns",plot.dat.stns,envir=.isfsEnv)

    invisible(title)
}

plot.dat.limits <- function(data,ylim,one.scale=F)
{

    # unique variable names.
    dnames <- dimnames(data)[[2]]
    dunits <- units(data)

    # Sometimes two data colums may have the same name, but different units
    # e.g.:  P as mb and Pa
    dnameunits = paste(dnames,"(",dunits,")",sep="")
    dnameunits.u = unique(dnameunits)

    yscales <- rep(1,length(dnameunits.u))
    names(yscales) <- dnameunits.u

    # T for variables with different limits but the same units,
    # so that user can put the variable name in the Y axis label.
    dupunits <- rep(F,length(dnameunits.u))
    names(dupunits) <- dnameunits.u

    # Fixed limits for all traces on plot
    if (!is.null(ylim) && !is.list(ylim))
          return(list(nscales=1,ylim=ylim,yscales=yscales))

    if (one.scale) {
        ylim <- range(unlist(sapply(unique(dnames),
            function(x,d,dn)
            {
                l<-list()
                l[[x]] <- range(clip(d[,dn==x]),na.rm=T)
                l
            },
            data,dnames)),na.rm=T)
        return(list(nscales=1,ylim=ylim,yscales=yscales))
    }

    # merge default limits on .isfsEnv with ylim. ylim takes precedence
    if (FALSE) {
    if (exists(".ylimits",envir=.isfsEnv)) {
        ylimtmp <- get(".ylimits",envir=.isfsEnv)
        # logical vector of variables with fixed Y limits
        fixed <- sapply(ylimtmp,function(x)
              { ifelse(is.null(x),F,ifelse(is.null(x$fixed),T,x$fixed)) })
        # 
        if (any(fixed)) ylimtmp <- c(lapply(ylimtmp[fixed],function(x){c(x$min,x$max)}),ylim)
        ylimtmp[names(ylim)] <- ylim	# works if ylim is NULL
        ylim <- ylimtmp
    }
    }

    # ylim is now either NULL or a list

    # first names of variables, i.e. "dir" for "dir.2m"
    # these may have repeated values
    dnames1 <- words(dnames,1,1,sep=".")

    udnames <- unique(dnames)
    udnames1 <- unique(dnames1)

    rmtch <- match(names(ylim),udnames,nomatch=0)
    rmtch1 <- match(names(ylim),udnames1,nomatch=0)
    # reduce ylim to those variables that we have
    ylim <- ylim[rmtch!=0 | rmtch1 != 0]

    # the length of udunits is the first guess at the number of
    # different scales on the yaxis.
    udunits <- unique(dunits)

    # We may need more yscales because of the requested ylims
    ylim.res <- list()

    iscale <- 0
    for (units.str in udunits) {
        # logical vector along dunits of variables that have units equal to
        # unit.str
        umtch <- match(dunits,units.str,nomatch=0) != 0

        # variable names with units equal to units.str
        dnames.u <- unique(dnames[umtch])
        dnames1.u <- words(dnames.u,1,1,sep=".")
        dnameunits.u <- paste(dnames.u,"(",units.str,")",sep="")

        # ylmtch: for each variable with units equal to units.str,
        # the index in ylim of its limits
        ylmtch <- match(dnames.u,names(ylim),nomatch=0)
        ylmtch1 <- match(dnames1.u,names(ylim),nomatch=0)

        # use first name match if not a complete match
        ylmtch[ylmtch == 0] <- ylmtch1[ylmtch == 0]

        if (any(ylmtch != 0)) {
            ylim.res[dnameunits.u[ylmtch!=0]] <- ylim[ylmtch]
            iscale <- iscale + 1
            yscales[dnameunits.u[ylmtch!=0]] <- iscale

            chk <- F
            # check for more than one limit
            if (length(unique(ylmtch[ylmtch!=0])) > 1) {
                chk <- unlist(lapply(ylim[ylmtch][-1],
                        function(x,y) {x[1]!=y[1] || x[2]!=y[2]},
                        ylim[ylmtch][[1]]))
            }
            if (any(chk)) {
                warning("variables ",paste(dnames.u[ylmtch!=0],collapse=","),	
                        " have units \"",
                  units.str, "\" but have different ylim=",
                  paste(names(ylim[ylmtch]),ylim[ylmtch],sep="=",collapse=","),"\n")
                ylmtchk <- ylmtch[ylmtch!=0][-1][chk]	# indices into ylim

                dupunits[dnameunits.u[ylmtch!=0]] <- T

                for (i in ylmtchk) {
                  iscale <- iscale + 1
                  dylmtch <- match(i,ylmtch,nomatch=0)
                  yscales[dnameunits.u[dylmtch]] <- iscale
                }
            }
        }
        if (any(ylmtch == 0)) {
            # No values in ylim for these variables
            ylims.for.units <- range(unlist(sapply(dnames.u[ylmtch==0],
                function(x,d,dn)
                {
                    l<-list()
                    l[[x]] <- range(clip(d[,dn==x]),na.rm=T)
                    l
                },
                data,dnames)),na.rm=T)
            ylim.res[dnameunits.u[ylmtch==0]] <- list(ylims.for.units)
            iscale <- iscale + 1
            yscales[dnameunits.u[ylmtch==0]] <- iscale
        }
    }

    #
    dn <- names(ylim.res)
    ylim <- list(nscales=iscale,ylim=ylim.res,yscales=yscales,dupunits=dupunits)
    cat("nscales=",iscale," units=",paste("\"",dunits,"\"",sep="",collapse=","),"\n")

    # This is a grotesque hack!
    # Without it the names of the ylim member are jibberish! Splus bug?
    names(ylim$ylim) <- dn
    ylim
}

logo_stamp <- function(print.motto=T)
{
    oma <- par("oma")
    # Adjust outer margins on left and right side, so that
    # strings placed by mtext are not too close to those edges
    if (oma[2] < 1) oma[2] <- 1
    if (oma[4] < 1) oma[4] <- 1

    old.par <- par(cex=.75,oma=oma)
    on.exit(par(old.par))

    cex <- par("cex")
    mex <- par("mex")

    lineoff <- cex / mex + .1

    if (is.null(platform <- dpar("platform"))) platform <- "NCAR ISFS"
    string <- paste(platform,format(utime("now"),format="%H:%M %h %d %Y %Z"))
    mtext(string,side=1,line=oma[1]-lineoff,outer=T,adj=1)

    string <- Sys.getenv("PROJECT")

    # print.motto=F will prevent printing of motto
    # print.motto can also be a string variable to replace
    # options("motto")
    if( is.logical(print.motto) && print.motto )
      print.motto <- options("motto")[[1]]
    if(!is.null(print.motto) && is.character(print.motto) )
      string <- paste(string,print.motto,sep=": ")

    mtext(string,side=1,line=oma[1]-lineoff,outer=T,adj=0)

}

# old name for backward compatibility
fun.logo.stamp <- logo_stamp

std.par <- function(nyscales=1,prows=NULL,pcols=NULL,rscale=1,lscale=1)
{
    # This function sets the par options: par,mar and mfrow based on input.
    #
    # nyscales:  number of scales desired on y axis
    #	room for these scales will be set aside using the mar
    #	parameter to par, in an alternating fashion, first on
    #	on the left, then right,left etc.
    # prows,pcols:	number of rows and columns of plots,
    #	used in par(mfrow=c(prows,pcols))
    #	This function does the par(mfrow=xxx), because it
    #	then adjusts cex and mex afterwards
    # rscale:	minimum number of scales to provide on the right
    # lscale:	minimum number of scales to provide on the left
    #	Use rscale,lscale if you want to set aside space in a plot
    #	so that the axes line up with another plot.
    #
    # We reduce wasted space in stacked plots (prows > 1) by reducing
    # the margin lines above and below each plot, and use the outer
    # margin lines for the top and bottom axis labels. This works
    # well when all plots have the same time axis, and the
    # time labels do not need to be repeated for each plot.

    mfrows <- par("mfrow")

    # if prows or pcols is NULL, then don't change
    if (is.null(prows)) prows <- mfrows[1]
    if (is.null(pcols)) pcols <- mfrows[2]

    cex <- par("cex")		# get cex before calling mfrow
                                  # par(mfrow=...) changes cex

    if (any(mfrows != c(prows,pcols)))
      par(mfrow=c(prows,pcols))

    # lines of margin on each side of plot
    # default values
    mar <- c(5,4,4,2)+.1	

    # outer margin lines
    oma <- rep(0,4)

    mar[1] <- 0.5
    mar[3] <- 0.5

    oma[1] <- 4.1 - mar[1]
    oma[3] <- 4.1 - mar[3]

    oma[2] <- oma[4] <- 1.1
    mgp <- c(1.2,0.2,0)		# line of axis title, labels, axis

    # separate Y scales, reserve margin space
    lscales <- (nyscales-1) %/% 2 + 1
    rscales <- nyscales %/% 2

    if (lscales < lscale) lscales <- lscale
    if (rscales < rscale) rscales <- rscale

    mar[2] <- (mgp[1] + 1.0) * lscales + 0.1
    mar[4] <- (mgp[1] + 1.0) * rscales + 0.1

    # warning:  put cex and mex before mar and oma in this par call.
    # Set cex to original value, and mex to cex, so margin lines are
    # measured in cex units
    c(par(cex=cex,mex=cex,oma=oma,mar=mar,mgp=mgp),list(mfrow=mfrows))
}
