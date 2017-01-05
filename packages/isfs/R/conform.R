# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

setGeneric("conform",function(x,y)
    standardGeneric("conform"))

setMethod("conform",signature(x="dat",y="dat"),
    function(x,y)
    {

        xcols <- match_columns(y,x,warn="Creating time series of NAs")
        if (any(xbad <- (xcols == 0))) {
            xcols[xbad] <- 1
            x <- x[,xcols]
            x[,xbad] <- NA_real_
            xstns <- stations(x)
            xstns[xbad] <- stations(y)[xbad]
            stations(x) <- xstns

            # what dimnames to we give the new columns?
            # We'll give the NA columns a name made up of the
            # first word of column 1 of x, and the suffixes of y
            # Jeesh this gets complicated, and probably hides too much
            # from the user.

            dx <- dimnames(x)[[2]]
            dy <- dimnames(y)[[2]]
            dx[xbad] <- paste(words(dx[1],1,1),dy[xbad],sep=".")
            dimnames(x)[[2]] <- dx

            # units will also come from column 1
        }
        else x <- x[,xcols]

        x
    }
    )

setMethod("conform",signature(x="numeric",y="dat"),
    function(x,y)
    {
        if ((ncol(y) %% length(x)) == 0) {
            nrep <- ncol(y) %/% length(x)
            x <- dat(nts(matrix(rep(rep(x,nrep),nrow(y)),ncol=ncol(y),byrow=T),
                positions(y),
                names=rep(deparse(substitute(x)),nrep),
                stations=stations(y)))
        }
        else {
            stop(paste("x is not conformable to y. length(x)=",length(x),
                    "dim(y)=",paste(dim(y),collapse=",",sep="")))
        }
        x
    }
    )

setMethod("conform",signature(x="ANY",y="ANY"),
    function(x,y)
    {
        if (is.null(x) || is.null(y)) x
        warning(paste("conform: unrecognized arguments, class(x)=",class(x),", class(y)=",class(y),". Returning x"))
        x
    }
    )

setGeneric("match_columns",function(y,x,warn)
    standardGeneric("match_columns"))

setMethod("match_columns",signature(y="dat",x="dat"),
    function(y,x,warn=FALSE)
    {

        # for each column in y, its corresponding column in x.
        # return 0 for those columns in y that don't
        # correspond to any in x.
        # Match first by station number, then height, then suffix
        ncx <- ncol(x)
        ncy <- ncol(y)

        stnsx <- stations(x)
        if (length(stnsx) != ncx)
            stop("length(stations(x))=",length(stnsx)," is not equal to ncol(x)=",ncx)

        stnsy <- stations(y)
        if (length(stnsy) != ncy)
            stop("length(stations(y))=",length(stnsy)," is not equal to ncol(y)=",ncy)

        dnx <- dimnames(x)[[2]]
        dny <- dimnames(y)[[2]]
        # cat("dnx=",paste(dnx,collapse=","),"\n")
        # cat("dny=",paste(dny,collapse=","),"\n")

        # if ((ncx == 1 && ncy == 1) ||
        # 	(ncx == ncy && all(stnsx == stnsy))) return(1:ncy)

        sfxsx <- words(dnx,2,sep=".")
        sfxsy <- words(dny,2,sep=".")

        htsx <- heights(x)
        # Remove names so that "identical(htsx[ssx], NA_real_)" works
        names(htsx) <- NULL
        htsy <- heights(y)
        names(htsy) <- NULL

        sitesx <- sites(x)
        sitesy <- sites(y)
        # cat("sitesx=",paste(sitesx,collapse=","),"\n")
        # cat("sitesy=",paste(sitesy,collapse=","),"\n")

        # columns to grab from x in order to match them with y
        # 0 means no match
        xcols <- rep(0,ncy)

        for (stn in unique(stnsy)) {
            stnx <- (1:ncx)[!is.na(match(stnsx,stn))]	# x column numbers of stn
            stny <- (1:ncy)[!is.na(match(stnsy,stn))]	# y column numbers of stn

            # check site names
            sitey = sitesy[stny]
            if (all(is.na(match(sitesx[stnx],sitey)))) sitey = ""

            for (sitei in unique(sitey)) {
                # cat("sitei=",sitei,"\n")
                if (identical(sitey,"")) {
                    ssx <- stnx
                    ssy <- stny
                }
                else {
                    # cat("stnx=",paste(stnx,collapse=","),"\n")
                    # cat("stny=",paste(stnx,collapse=","),"\n")
                    ssx <- stnx[!is.na(match(sitesx[stnx],sitei))]	# x column numbers of stn and site
                    ssy <- stny[!is.na(match(sitesy[stny],sitei))]	# y column numbers of stn and site
                    # cat("ssx=",paste(ssx,collapse=","),"\n")
                    # cat("ssy=",paste(ssy,collapse=","),"\n")
                    if (length(ssx) == 0) ssx = stnx
                    if (length(ssy) == 0) ssy = stny

                }
                lssx <- length(ssx)
                lssy <- length(ssy)
                if (lssx == 0) {
                    # no match for stn in x
                    if ((is.logical(warn) && warn) || is.character(warn)) {
                        warning(paste("No data in x (dimnames=",paste(dnx,collapse=","),
                                ", stations=",paste(stnsx,collapse=","),") for station",stn))
                    }
                }
                else {
                    # conform along the heights
                    for (hty in unique(htsy[ssy])) {
                        # cat("hty=",hty,"\n")
                        # hx is vector of x column numbers of hty and stn
                        # hy is vector of y column numbers of hty and stn

                        # If x at this point has one column, with NA height, it matches against anyting
                        if (identical(htsx[ssx], NA_real_)) {
                            # one column, no height information
                            hx = (1:ncx)[ssx]
                        }
                        else {
                            hx <- (1:ncx)[ssx][!is.na(match(htsx[ssx],hty))]
                        }
                        lhx <- length(hx)
                        hy <- (1:ncy)[ssy][!is.na(match(htsy[ssy],hty))]
                        lhy <- length(hy)
                        if (lhx == 0 && !is.na(hty)) {
                            # no exact match for this height at stn
                            # find minimum difference
                            hd = abs(htsx[ssx]-hty)
                            hdm = min(hd,na.rm=T)
                            # cat("height differences=",hd," minimum=",hdm,"\n")
                            if (!is.na(hdm)) {
                                hx <- (1:ncx)[ssx][!is.na(hd) & hd == hdm]
                                lhx <- length(hx)
                                if (length(lhx) > 0 && ((is.logical(warn) && warn) || is.character(warn))) {
                                    warning(paste("conforming",dnx[hx],", (stns=",stnsx[hx],") to",
                                            paste(dny[hy],"(stn=",stnsy[hy],")",sep="",collapse=", "),
                                            "(matching minimum height difference)"))
                                }
                            }
                            else {
                                # all height differences are NA, but hty
                                # is not NA. Therefore all heights of x are NA
                                hx <- (1:ncx)[ssx][is.na(htsx[ssx])]
                                lhx <- length(hx)
                                if (length(lhx) > 0 && ((is.logical(warn) && warn) || is.character(warn))) {
                                    warning(paste("conforming",dnx[hx],", (stns=",stnsx[hx],") to",
                                            paste(dny[hy],"(stn=",stnsy[hy],")",sep="",collapse=", "),
                                            "(matching NA heights in x to non-NA heights in y)"))
                                }
                            }
                        }

                        if (lhx == 1) {
                            # one x column for this station & height, repeat it
                            xcols[hy] <- rep(hx,lhy)
                        }
                        else if (lhx == 0) {
                            if ((is.logical(warn) && warn) || is.character(warn)) {
                                warning(paste("No data in x (dimnames=",paste(dnx,collapse=","),
                                        ", stns=",paste(stnsx,collapse=","),") for station",stn,
                                        "at height",hty))
                            }
                        }
                        else {
                            # if same number of columns with same height at a station,
                            # set default conformation

                            # more than one column of x. Might be something like CHATS:
                            #   x=Vazimuth.1b.9.6m.ha Vazimuth.2b.9.6m.ha Vazimuth.3b.9.6m.ha ...
                            #   y=u.1b.9.6m.ha u.2b.9.6m.ha u.3b.9.6m.ha u.4b.9.6m.ha ...
                            if (lhx == lhy) {
                                # try match of names after first word
                                my = match(words(dny[hy],2),words(dnx[hx],2),nomatch=0)
                                if (any(my == 0)) xcols[hy] <- hx
                                else xcols[hy] = hx[my]
                            }
                            else {
                                msg <- paste("more than one column of",
                                    paste(dnx[hx],"(stns=",stnsx[hx],")",sep="",collapse=", "),
                                    "conforms to",
                                    paste(dny[hy],"(stns=",stnsy[hy],")",sep="",collapse=", "))
                                tx <- hy[xcols[hy] == 0]
                                msg <- paste(msg,"Will repeat",
                                    dnx[hx][1],"(stn=",stnsx[hx][1],")",
                                    length(tx),"times")
                                xcols[tx] <- rep(hx[1],length(tx))
                                if ((is.logical(warn) && warn) || is.character(warn))
                                    warning(msg)
                            }
                        }
                    }
                }
            }
        }
        if (any(xcols == 0)) {
            if (is.character(warn)) warnstr <- warn
            else warnstr <- ""
            tx <- (1:ncy)[xcols == 0]
            msg <- paste("Cannot conform",
                paste(dnx,"(stn=",stnsx,")",sep="",collapse=", "),
                "to",
                paste(dny[tx],"(stn=",stnsy[tx],")",sep="",collapse=", "),
                ".",warnstr)
            if ((is.logical(warn) && warn) || is.character(warn))
                warning(msg)
        }
        # cat(paste("match_columns of",paste(dny,collapse=","),"to",
        #         paste(dnx,collapse=","),"is",
        #         paste(xcols,collapse=",")),"\n")
        # if (any(is.na(xcols))) browser()
        # xcols[is.na(xcols)] = 0
        xcols
    }
    )
