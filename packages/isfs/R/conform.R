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

        xcols <- match_columns(y,x,warn=TRUE)
        if (any(xbad <- (xcols == 0))) {
            warning(paste("columns",paste((1:length(xbad))[xbad], collapse=","),
                    "will be filled with NA"))
            xcols[xbad] <- 1
            # units will be the same as column 1
            x <- x[,xcols]
            x[,xbad] <- NA_real_
            xstns <- stations(x)
            xstns[xbad] <- stations(y)[xbad]
            stations(x) <- xstns
        }
        else {
            x <- x[,xcols]
        }
        cx <- paste0(words(colnames(x),1,1),suffixes(colnames(y)))

        # fill in missing site names from y
        sx <- sites(cx)
        ns <- sx == ""
        if (any(ns)) {
            sx[ns] <- sites(x[,ns])
            sites(cx) <- sx
        }
        colnames(x) <- cx
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

setGeneric("match_columns",function(x,y,warn)
    standardGeneric("match_columns"))

setMethod("match_columns",signature(x="dat",y="dat"),
    function(x,y,warn=FALSE)
    {

        # for each column in x, its corresponding column in y.
        # return 0 for those columns in x that don't
        # correspond to any in y.
        # Match first by station number, then height, then suffix
        ncx <- ncol(x)
        ncy <- ncol(y)

        stnsx <- stations(x)
        if (length(stnsx) != ncx)
            stop("length(stations(x))=",length(stnsx)," is not equal to ncol(x)=",ncx)

        stnsy <- stations(y)
        if (length(stnsy) != ncy)
            stop("length(stations(y))=",length(stnsy)," is not equal to ncol(y)=",ncy)

        dnx <- colnames(x)
        dny <- colnames(y)
        # cat("dny=",paste(dny,collapse=","),"\n")
        # cat("dnx=",paste(dnx,collapse=","),"\n")

        htsx <- heights(x)
        # Remove names so that "identical(htsy[ssy], NA_real_)" works
        names(htsx) <- NULL
        htsy <- heights(y)
        names(htsy) <- NULL

        sitesx <- sites(x)
        sitesy <- sites(y)
        # cat("sitesx=",paste(sitesx,collapse=","),"\n")
        # cat("sitesy=",paste(sitesy,collapse=","),"\n")

        # columns to grab from y in order to match them with x
        # 0 means no match
        ycols <- rep(0,ncx)

        for (stn in unique(stnsx)) {
            stnx <- (1:ncx)[!is.na(match(stnsx,stn))]	# x column numbers of stn
            stny <- (1:ncy)[!is.na(match(stnsy,stn))]	# y column numbers of stn

            # check site names
            sitex <- sitesx[stnx]
            # if (all(is.na(match(sitex, sitesy[stny])))) sitex <- ""

            for (sitei in unique(sitex)) {
                # cat("sitei=",sitei,"\n")
                # cat("stnx=",paste(stnx,collapse=","),"\n")
                # cat("stny=",paste(stny,collapse=","),"\n")
                # x column numbers of stn and site
                ssx <- stnx[!is.na(match(sitesx[stnx],sitei))]

                # y column numbers matching station and site of x
                ssy <- stny[!is.na(match(sitesy[stny],sitei))]
                # cat("ssx=",paste(ssx,collapse=","),"\n")
                # cat("ssy=",paste(ssy,collapse=","),"\n")
                if (length(ssy) == 0 && sitei == "" ) ssy <- stny
                if (length(ssy) == 0) ssy <- stny[sitesy[stny] == ""]

                lssx <- length(ssx)
                lssy <- length(ssy)
                if (lssy == 0) {
                    # no match for stn in y
                    if ((is.logical(warn) && warn) || is.character(warn)) {
                        warning(paste0("No data in colnames=",paste(dny,collapse=","),
                                ", stations=",paste(stnsy,collapse=",")," for station",stn))
                    }
                }
                else {
                    # conform along the heights
                    for (htx in unique(htsx[ssx])) {
                        # cat("htx=",htx,"\n")
                        # hx is vector of x column numbers of htx and stn
                        # hy is vector of y column numbers of htx and stn
                        hx <- (1:ncx)[ssx][!is.na(match(htsx[ssx],htx))]
                        lhx <- length(hx)

                        # If x or y at this point has one column, with NA height,
                        # it matches against anything
                        if (identical(htsy[ssy], NA_real_) || is.na(htx)) {
                            # one column, no height information
                            hy = (1:ncy)[ssy]
                        }
                        else {
                            # matching heights
                            hy <- (1:ncy)[ssy][!is.na(match(htsy[ssy],htx))]
                        }
                        lhy <- length(hy)
                        if (lhy == 0 && !is.na(htx)) {
                            # no exact match for this height at stn
                            # find minimum difference
                            hd <- abs(htsy[ssy]-htx)
                            hdm <- min(hd,na.rm=T)
                            # cat("height differences=",hd," minimum=",hdm,"\n")
                            if (!is.na(hdm)) {
                                hy <- (1:ncy)[ssy][!is.na(hd) & hd == hdm]
                                lhy <- length(hy)
                                if (length(lhy) > 0 && ((is.logical(warn) && warn) || is.character(warn))) {
                                    warning(paste("conforming",dny[hy],", (stns=",stnsy[hy],") to",
                                            paste(dnx[hx],"(stn=",stnsx[hx],")",sep="",collapse=", "),
                                            "(matching minimum height difference)"))
                                }
                            }
                            else {
                                # all height differences are NA, but htx
                                # is not NA. Therefore all heights of y are NA
                                hy <- (1:ncy)[ssy][is.na(htsy[ssy])]
                                lhy <- length(hy)
                                if (length(lhy) > 0 && ((is.logical(warn) && warn) || is.character(warn))) {
                                    warning(paste("conforming",dny[hy],", (stns=",stnsy[hy],") to",
                                            paste(dnx[hx],"(stn=",stnsx[hx],")",sep="",collapse=", "),
                                            "(matching NA heights in y to non-NA heights in x)"))
                                }
                            }
                        }

                        if (lhy == 1) {
                            # one y column for this station & height, repeat it
                            ycols[hx] <- rep(hy,lhx)
                        }
                        else if (lhy == 0) {
                            if ((is.logical(warn) && warn) || is.character(warn)) {
                                warning(paste0("No data in colnames=",paste(dny,collapse=","),
                                        ", stns=",paste(stnsy,collapse=",")," for station",stn,
                                        "at height",htx))
                            }
                        }
                        else {
                            # if same number of columns with same height at a station,
                            # set default conformation

                            # more than one column of y. Might be something like CHATS:
                            #   y=Vazimuth.1b.9.6m.ha Vazimuth.2b.9.6m.ha Vazimuth.3b.9.6m.ha ...
                            #   x=u.1b.9.6m.ha u.2b.9.6m.ha u.3b.9.6m.ha u.4b.9.6m.ha ...
                            if (lhy == lhx) {
                                # try match of names after first word
                                mx = match(words(dnx[hx],2),words(dny[hy],2),nomatch=0)
                                if (any(mx == 0)) ycols[hx] <- hy
                                else ycols[hx] = hy[mx]
                            }
                            else {
                                msg <- paste("more than one column of",
                                    paste(dny[hy],"(stns=",stnsy[hy],")",sep="",collapse=", "),
                                    "conforms to",
                                    paste(dnx[hx],"(stns=",stnsx[hx],")",sep="",collapse=", "))
                                ty <- hx[ycols[hx] == 0]
                                msg <- paste(msg,"Will repeat",
                                    dny[hy][1],"(stn=",stnsy[hy][1],")",
                                    length(ty),"times")
                                ycols[ty] <- rep(hy[1],length(ty))
                                if ((is.logical(warn) && warn) || is.character(warn))
                                    warning(msg)
                            }
                        }
                    }
                }
            }
        }
        if (any(ycols == 0) && ((is.logical(warn) && warn) || is.character(warn))) {
            if (is.character(warn)) warnstr <- warn
            warnstr <- ""
            ty <- (1:ncx)[ycols == 0]
            msg <- paste("Cannot conform",
                paste(dny,"(stn=",stnsy,")",sep="",collapse=", "),
                "to",
                paste(dnx[ty],"(stn=",stnsx[ty],")",sep="",collapse=", "),
                ".",warnstr)
            warning(msg)
        }
        # cat(paste("match_columns of",paste(dnx,collapse=","),"to",
        #         paste(dny,collapse=","),"is",
        #         paste(ycols,collapse=",")),"\n")
        # if (any(is.na(ycols))) browser()
        # ycols[is.na(ycols)] = 0
        ycols
    }
    )
