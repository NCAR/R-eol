# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

#  Description:
#    Create tables of summary QC information
#
# Suggestions:
# html color coding (style sheets may be useful):
#   Tom suggests color coding the means and sd in a column:
#      a different color for maximum, minimum and median
#	So for a given time period, the station with the max value
#	would be shown in red, the minimum value in blue and
# 	the mean in green.  Now: orange=max, green=min, blue=median
#
#  Tom also felt statistics should be done with unclipped data.
#  
#  Provide links to the same variable on the next and previous day.
#  This requires knowing the file names, or the file name pattern
#  and the times of the next and previous tables. In a batch script
#  perhaps passing the names would be good.
#
# 
qctable_list <- function(data=NULL,vars=NULL,t1=dpar("start"),t2=dpar("end"),ntper=4)
{

    # Return a list of summary information for a list of variables.
    # Each element of the list is a matrix containing summary
    # information for a variable with a certain units.
    # If a variable is found with multiple units, then
    # a separate matrix is created for each instance of variable and
    # units value.
    #
    # Each row of a matrix corresponds to a given station.
    # The ntper parameter indicates how many time periods
    # to split the information into. 
    # For example, if t1=00:00 of a day and t2 is 24 hours later,
    # with ntper=4, then the four periods will be 0-6 hours,
    # 6-12, 12-18, 18-24:00.
    # For each period, the mean, std dev, number of missing data values,
    # and the number of clipped values are reported.
    # Since 4 fields are reported for each time period, the number of
    # columns in the matrix will be ntper*4.

    dt <- as.numeric((t2 - t1) / ntper)

    # scenarios:
    # 1. user passes data and vars 
    #   create mapping between dimnames(data) and vars
    #   loop over map
    # 2. user passes data
    #   set vars to unique(dimnames(data))
    #   then as above
    # 3. user passes vars
    #    vars = cvec
    #    data = NULL

    if (is.null(data) && is.null(vars)) stop("both data and args are NULL")

    if (is.null(vars)) vars <- unique(dimnames(data)[[2]])
    else if (!is.null(data)) {
        dns <- dimnames(data)[[2]]

        # for each var, an integer vector of the indices of matching elements in dns
        mx <- lapply(vars,function(dn) {
            (1:length(dns))[match(words(dns,1,nwords(dn,sep=".")),dn,nomatch=0) != 0]
})
        bx <- unlist(lapply(mx,length)) == 0

        if (any(bx))
            warning(paste("no match for variables:",
                    paste(vars[bx],collapse=","),"in dimnames(data)=",
                    paste(dns,collapse=",")))
    }

    qctab <- lapply(vars,
        function(vn) {
            if (is.null(data)) dv <- dat(vn)
            else {
                mx <- !is.na(match(words(colnames(data),1,nwords(vn,sep=".")),vn))
                if (!any(mx)) {
                    # generate fake time series of NA
                    dv <- dat(nts(matrix(rep(NA,2),nrow=2),
                        c(t1,t2), names=vn,units=""))
                }
                else dv <- data[,mx]
	    }

            # discard last row in most recent data. All variables might not be ready
            if (t1 + ntper * dt > utime("now") && nrow(dv) > 1) dv = dv[-nrow(dv),]
            dunits <- units(dv)
            duniqs <- unique(dunits)
            dvdeltat <- deltat(dv)[["trimmed.mean"]]
            # cat("vn=",vn,", colnames(dv)=",paste(colnames(dv),collapse=","),
            #         ", duniqs=",paste(duniqs,collapse=","),", dunits=",
            #          paste(dunits,collapse=","),"\n")
            # if (length(duniqs) > 1) browser()

            # loop over different units of variable
            utab <- lapply(duniqs,
                function(dunit) {
                    dunits <- units(dv)
                    du <- dv[,dunits==dunit]
                    colseq <- order(heights(du))
                    stns <- stations(du)[colseq]
                    vns <- colnames(du)[colseq]

                    # loop over columns, in order of height
                    uctab <- matrix(sapply(colseq,
                            function(nc) {
                                dc <- du[,nc]
                                times <- as.numeric(seq(from=t1,
                                    length=ntper,by=dt))

                                # loop over times
                                as.vector(sapply(times,
                                        function(t1x) {
                                            t1x <- utime(t1x)
                                            t2x <- t1x + dt
                                            # cat("t1x=",t1x," t2=",t2,"\n")
                                            dct <- dc[utime(c(t1x,t2x)),]
                                            if (length(dct) == 0) rep(NA_real_,4)
                                            else {
                                                nas <- sum(is.na(dct))
                                                dct <- clip(dct)
                                                xm <- mean(dct,na.rm=TRUE)
                                                xd <- sqrt(var(dct,na.rm=TRUE))
                                                nclipped <- sum(is.na(dct)) - nas
                                                c(xm,xd,nclipped,nas)
                                            }
                                        }))
                            }),ncol=ntper*4,byrow=TRUE)
                    times <- as.numeric(seq(from=t1,length=ntper,by=dt))
                    if (dt >= 86400) fmt <- "%b %d %H:%M"
                    else fmt <- "%H:%M"

                    cnames <- sapply(times,function(tx) {
                        paste(format(utime(tx),format=fmt),
                            format(utime(tx+dt),format=fmt),sep="-")})

                    dimnames(uctab) <- list(paste(vns,names(stns),sep=" "),
                        paste(sapply(cnames,function(cn) c(cn,"","","")),c("","sd","nclip","NAs"),sep=""))
                    attr(uctab,"clip") <- clip(words(vns[1],1,1,sep="."))
                    attr(uctab,"units") <- dunit
                    uctab
            })
            names(utab) <- paste(vn," (",duniqs,")",sep="")
            utab$deltat <- dvdeltat
            utab
        })
    # browser()
    qctab <- unlist(qctab,recursive=FALSE)
    qctab$period <- dt
    qctab
}

qctable_links_html <- function(dnames,file="",append=append)
{
    if (length(dnames) > 1) {
        html <- paste("<table border=1 bgcolor=\"#eeeeee\"><caption>Clickable Table Of Variables</caption>\n")
        for (i in seq(from=1,to=length(dnames),by=10)) {
            n <- i + 9
            if (n > length(dnames)) n <- length(dnames)
            html <- paste(html,"  <tr>",sep="")
            html <- paste(html,
                paste("<td><a href=\"#",dnames[i:n],"\">",dnames[i:n],"</a>",sep="",collapse=" "),
                "\n",sep="")
        }
        html <- paste(html,"</table>\n",sep="")
        cat(html,file=file,append=append)
    }
    invisible()
}

qctable_nbspformat <- function(x,...)
{
    # Format function which, after appling the regular format() function
    # to a numeric vector, then replaces spaces with &nbsp; so that
    # text aligns well with a monospaced font.  Text formatted with
    # <pre> does not seem to align correctly in tables.
    sapply(format(x,...),function(x) {
        gsub(" ","&nbsp;",x,fixed=TRUE)
    })
}
qctable_nzformat <- function(x,...)
{
    # Format function which, after applying qctable_nbspformat
    # to a numeric vector, then adds span element around each
    # value > 0.
    cx <- qctable_nbspformat(x,...)
    bx <- !is.na(x) & x > 0
    if (any(bx)) cx[bx] <- paste("<span id=\"badgt0\">",cx[bx],"</span>",sep="")
    cx
}


qctable <- function(vars,data=NULL,ntper=4,file="",append=FALSE,title=NULL,
    prev.link="",next.link="")
{

    t1 <- dpar("start")
    t2 <- dpar("end")

    timecapt <-  paste(
        format(t1,format="%Y %b %d %H:%M"),
        format(t2,format="%Y %b %d %H:%M %Z"),sep=" - ")

    # color: #FF8000 is a shade of orange. May be more readable than just "orange"
    if (!is.null(title)) {
        nw = nwords(file,sep="/")
        qctdir = words(file,1,nw-1,sep="/")
        cssfile = paste0(qctdir,"/qctable.css")
        if (!file.exists(cssfile)) {
          css <- paste(
            "table.numdata {font-family: monospace,monospace;}\n",
            "td.nummax {color: #FF8000;}\n",
            "td.nummed {color: green;}\n",
            "td.nummin {color: blue;}\n",
            "span.nummax {color: #FF8000;}\n",
            "span.nummed {color: green;}\n",
            "span.nummin {color: blue;}\n",
            "#badgt0 {color: red;}\n",sep="")
          cat(css,file=cssfile)
        }
        html <- paste(
            "<!doctype html public \"-//w3c//dtd html 4.0 transitional//en\">\n",
            "<html>\n<head>\n",
            "<meta charset=\"UTF-8\">\n",
            "<link rel=\"stylesheet\" type=\"text/css\" rel=\"noopener\" target=\"_blank\" href=\"qctable.css\">\n",
            "<title>",title,"</title>\n</head>\n",
            "<body bgcolor=\"#dddddd\">\n",
            "<a id=\"top\"></a>\n",
            "<center><h2>",title,"</h2></center>\n",sep="")
        cat(html,file=file,append=append)
        append <- file != "" 
    }
    append <- file != "" 

    if (length(vars) > 1) 
        qctable_links_html(vars,file=file,append=append)

    cat("<p>Do a browser refresh to make sure this page is up-to-date.\n",file=file,append=append)

    first <- TRUE
    for (dnx in vars) {

        # cat("dnx=",dnx,"\n")

        # If a variable has more than units value, there will be
        # more than one element in the dmat list:
        # For example:
        # names(dmat)
        # [1] "co2mr (mV*10)"    "co2mr (mmol/m^3)"

        dmatall <- qctable_list(data=data,vars=dnx,t1=t1,t2=t2,ntper=ntper)

        xdeltat <- dmatall$deltat
        period <- dmatall$period
        dnall <- names(dmatall)
        dnall <- dnall[dnall != "deltat" & dnall != "period"]

        if (first) {

            if (round(xdeltat) %%  60 == 0) {
                n <- round(xdeltat/60)
                deltatstr <- paste(n,"minute")
            }
            else deltatstr <- paste(xdeltat,"second")


            html <- paste("<p>Each table cell below contains four values, which are computed from",deltatstr,"means of the indicated variable over the time period shown at the top of the column.\n",
                "<ol><li>mean of",deltatstr,"means</li>\n",
                "<li>sd: standard deviation of",deltatstr,"means</li>\n",
                "<li>nclip: number of",deltatstr,"means that were not within the indicated clip limits</li>\n",
                "<li>NAs: number of",deltatstr,"means that were NA (missing data)</li></ol>\n")
            cat(html,file=file,append=append)
            html <- paste("<p>Color key:\n",
                "<ul><li><span id=\"badgt0\">red</span>: values of nclip or NAs that are greater than zero.</li>\n",
                "<li><span class=\"nummax\">maximum</span>: mean is the maximum of the values in the column</li>\n",
                "<li><span class=\"nummed\">median</span>: mean is the median of the values in the column</li>\n",
                "<li><span class=\"nummin\">minimum</span>: mean is the minimum in the column</li></ul><p>\n")
            cat(html,file=file,append=append)
        }

        first <- FALSE

        for (dn in dnall) {
            # cat("dn=",dn,"\n")

            html <- ""

            dmat <- dmatall[[dn]]
            if (!is.matrix(dmat)) dmat <- matrix(dmat,ncol=4*ntper)

            clip.limits <- attr(dmat,"clip")
            if (!is.null(clip.limits) && clip.limits$clip)
                clip.limits <- paste("(",clip.limits$min,",",clip.limits$max,")",sep="")
            else clip.limits <- "none"

            dunit <- attr(dmat,"units")


            # format each column separately
            # 4 text fields are placed in each table cell:
            #         mean stdev numnas numclipped
            # We want the fields to be aligned down the column of the table,
            # so that is it easy for the eye to see outliers.
            # The format() function formats all numbers passed to it
            # in a consistent way, with the same number of characters
            # in each result, padding with spaces.
            # Previously the results were then rendered with the <pre>
            # html tag. However firefox (and perhaps other browsers)
            # does a bad job of aligning preformatted text in table cells.
            # The text was always at the top of the cell, and no amount of
            # valign="middle" or CSS attributes would fix it.
            # What does work is using a mono-spaced font in the table, and
            # converting all spaces to &nbsp;
            #
            # scientific=6 means only use exponential form when the fixed form
            # is more than 6 digits wide.
            #
            # Initial character matrix
            fdmat <- matrix("",nrow=nrow(dmat),ncol=ncol(dmat),dimnames=list(NULL,dimnames(dmat)[[2]]))

            # Format means. Use drop=FALSE when subsetting dmat. Otherwise if it is a one row
            # matrix (one variable), dmat[,x] will degrade to a vector, and apply will fail
            mc <- seq(from=1,to=ncol(dmat),by=4)
            # pressures between 1005 and 1015 were all being formatted as 1010
            ndig <- 3
            dmean <- abs(mean(dmat[,mc],na.rm=TRUE))
            if (!is.na(dmean) && dmean > 1000 && dmean < 1.e6) ndig <- 4
            fdmat[,mc] <- apply(dmat[,mc,drop=FALSE],2,qctable_nbspformat,digits=ndig,scientific=6)

            # format standard deviations to 2 significant digits
            sdc <- seq(from=2,to=ncol(dmat),by=4)
            fdmat[,sdc] <- apply(dmat[,sdc,drop=FALSE],2,qctable_nbspformat,digits=2,scientific=6)

            # add color to non-zero counts of nclip and NAs

            # fancy expression to create sequence 3,4,7,8,11,12,etc
            # nzc <- rep(3:4,ntper)+rep(seq(from=0,to=ntper-1)*4,rep(2,ntper))

            # Or just exclude means and sd
            nzc <- (1:ncol(dmat))[c(-mc,-sdc)]

            fdmat[,nzc] <- apply(dmat[,nzc,drop=FALSE],2,qctable_nzformat,digits=3,scientific=6)

            if (dn == dnall[1]) html <- paste(html,"<a id=\"",dnx,"\"></a><p>\n",sep="")

            html <- paste(html,"<table class=\"numdata\" border=1 bgcolor=\"#eeeeee\"><caption><b>",dnx,
                "</b>(",dunit,") ",timecapt,
                ", clip.limits=",clip.limits,"</caption>\n",sep="")

            html <- paste(html,"  <tr><th>variable",sep="")

            ix <- seq(from=1,by=4,to=ncol(dmat))

            # compute medians, minimums, maximums of means
            if (nrow(dmat) > 1) {
                meds <- apply(dmat[,ix],2,median,na.rm=TRUE)
                maxs <- apply(dmat[,ix],2,max,na.rm=TRUE)
                mins <- apply(dmat[,ix],2,min,na.rm=TRUE)
            }
            else {
                meds <- rep(NA_real_,length(ix))
                maxs <- rep(NA_real_,length(ix))
                mins <- rep(NA_real_,length(ix))
            }

            # There can be more than one point the same distance from
            # the median.  Example:  in 1 2 3 4,  values 2 and 3 are
            # the same distance from the median
            maxmeds <- meds
            minmeds <- meds

            # if even number of points, find nearest to median
            for (j in ix) {
                if ((sum(!is.na(dmat[,j])) %% 2) == 0) {
                    ic <- (j + 3) / 4
                    if (!is.na(meds[ic])) {
                        diff <- abs(dmat[,j] - meds[ic])
                        mdiff <- min(diff,na.rm=TRUE)
                        nearest <- !is.na(diff) & (diff - mdiff) <= mdiff * 1.e-6
                        maxmeds[ic] <- max(dmat[nearest,j])
                        minmeds[ic] <- min(dmat[nearest,j])
                    }
                }
            }

            dns <- dimnames(dmat)
            html <- paste(html,
                paste("<th>",dns[[2]][ix],"<br>mean sd nclip NAs",sep="",collapse=""),
                "</th>\n",sep="")

            vnames <- dns[[1]]

            for (i in 1:nrow(dmat)) {
                vn <- vnames[i]
                # variable name left justified, data center justified
                html <- paste(html,"  <tr align=center><td align=left>",vn,"</td>",sep="")

                for (j in ix) {
                    entry <- paste(fdmat[i,j:(j+3)],collapse=" ")
                    entry <- gsub("NaN","&nbsp;&nbsp;&nbsp;",entry,fixed=TRUE)
                    entry <- gsub("NA","&nbsp;&nbsp;",entry,fixed=TRUE)
                    ic <- trunc(j/4) + 1
                    if (!is.na(dmat[i,j])) {
                        if (!is.na(mins[ic]) && dmat[i,j] == mins[ic])
                            html <- paste(html,"<td class=\"nummin\"> ",entry," </td>",sep="")
                        else if (!is.na(maxs[ic]) && dmat[i,j] == maxs[ic])
                            html <- paste(html,"<td class=\"nummax\"> ",entry," </td>",sep="")
                        else if (!is.na(minmeds[ic]) &&
                            dmat[i,j] >= minmeds[ic] && dmat[i,j] <= maxmeds[ic]) 
                            html <- paste(html,"<td class=\"nummed\"> ",entry," </td>",sep="")
                        else html <- paste(html,"<td> ",entry," </td>",sep="")
                    }
                    else html <- paste(html,"<td> ",entry," </td>",sep="")
                }
                html <- paste(html,"\n",sep="")
            }
            html <- paste(html,"</table>\n",sep="")
            html <- paste(html,"<table cellpadding=\"5\"><tr>\n",sep="")

            if (length(vars) > 0)
                html <- paste(html,"<td><a href=\"#top\">top</a>&nbsp;&nbsp;\n",sep="")

            if (prev.link != "")
                html <- paste(html,"<td><a href=\"",prev.link,"#",dnx,"\">previous</a>&nbsp;&nbsp;\n",sep="")

            if (next.link != "")
                html <- paste(html,"<td><a href=\"",next.link,"#",dnx,"\">next</a>&nbsp;&nbsp;\n",sep="")

            html <- paste(html,"<td><a href=\".\">other</a>\n",sep="")

            # html <- paste(html,"<td>&nbsp;&nbsp;key:<td class=\"nummax\">maximum<td class=\"nummed\">median<td class=\"nummin\">minimum<td id=\"badgt0\">#clip or #NAs>0<td>created: ",
            #   format(utime("now"),format="%b %d %H:%M, %Y"),"</table>\n",sep="")

            html <- paste(html,"<td>created: ",
                format(utime("now"),format="%b %d %H:%M, %Y"),"</td></table>\n",sep="")

            cat(html,file=file,append=append)
            append <- file != "" 
        }
    }

    if (!is.null(title)) {
        html <- "</body>\n</html>\n"
        cat(html,file=file,append=append)
    }

    invisible()
}
