# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
#               Copyright (C) by UCAR
# 
#  Description:
#    Create tables of summary QC information
#
# Suggestions:
# html color coding (style sheets may be useful):
#   Tom suggests color coding the means and sd in a column:
#      a different color for maximum, minimum and median
#	So for a given time period, the station with the max value
#	would be shown in red, the minimum value in blue and
# 	the mean in green.  Or red=max, green=min, yellow=median
#
#  Tom also felt statistics should be done with unclipped data.
#  
#  Provide links to the same variable on the next and previous day.
#  This requires knowing the file names, or the file name pattern
#  and the times of the next and previous tables. In a batch script
#  perhaps passing the names would be good.
#
# 
qctable.list = function(x=NULL,vars=NULL,t1=dpar("start"),t2=dpar("end"),ntper=4)
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

    dt = as.numeric((t2 - t1) / ntper)

    # loop over variables
    # this needs rework:
    #   if a station 0: (profiles) do a table of variables matching
    #     a prefix - perhaps treat T,RH,U,V,Spd,Dir,u,v,w,tc specially
    #     or look at heights.
    #	i.e. we would like all heights of T in the same table.
    #   for other variables: if looping over unique full dimnames, then
    #	unique units is unnecessary
    #

    # scenarios:
    # 1. user passes x and vars 
    #   create mapping between dimnames(x) and vars
    #   loop over map
    # 2. user passes x
    #   set vars to unique(dimnames(x))
    #   then as above
    # 3. user passes vars
    #    vars = cvec
    #    x = NULL

    if (is.null(x) && is.null(vars)) stop("both x and args are NULL")

    if (is.null(vars)) vars = unique(dimnames(x)[[2]])
    else if (!is.null(x)) {
        dns = dimnames(x)[[2]]

        # for each var, an integer vector of the indices of matching elements in dns
        mx = lapply(vars,function(x,dns) {
            (1:length(dns))[match(words(dns,1,nwords(x,sep=".")),x,nomatch=0) != 0]
},dns)
        bx = unlist(lapply(mx,length)) == 0

        if (any(bx))
            warning(paste("no match for variables:",
                    paste(vars[bx],collapse=","),"in dimnames(x)=",
                    paste(dns,collapse=",")))

        if (F) {
            mx = unique(unlist(mx))

            if (length(dns[-mx]) > 0)
                warning(paste("no match for dimnames(x)=",
                        paste(dns[-mx],collapse=","),"in list of names:",
                        paste(vars,collapse=",")))
        }
    }

    x = lapply(vars,
        function(vn,nframe,t1,dt,ntper) {
            if (is.null(get("x",frame=nframe))) x = dat(vn)
            else {
                vnames = dimnames(get("x",frame=nframe))[[2]]
                x = get("x",frame=nframe)[,match(words(vnames,1,nwords(vn,sep=".")),vn,nomatch=0) !=0 ]
            }
            # discard last row in most recent data. All variables might not be ready
            if (t1 + ntper * dt > utime("now") && nrow(x) > 1) x = x[-nrow(x),]
            dunits = units(x)
            duniqs = unique(dunits)
            xdeltat = deltat(x)[["trimmed.mean"]]
            # cat("v=",v,", dimnames(x)[[2]]=",paste(dimnames(x)[[2]],collapse=","),
            #         ", duniqs=",paste(duniqs,collapse=","),", dunits=",
            #          paste(dunits,collapse=","),"\n")
            # if (length(duniqs) > 1) browser()

            # loop over different units
            x = lapply(duniqs,
                function(dunit,x,t1,dt,ntper) {
                    dunits = units(x)
                    x = x[,dunits==dunit]
                    colseq = order(heights(x))
                    stns = stations(x)[colseq]
                    dns = dimnames(x)[[2]][colseq]

                    # loop over columns, in order of height
                    x = matrix(sapply(colseq,
                            function(nc,x,t1,dt,ntper) {
                                x = x[,nc]
                                times = as.numeric(seq(from=t1,length=ntper,by=dt))

                                # loop over times
                                as.vector(sapply(times,
                                        function(t1,x,dt) {
                                            t1 = utime(t1)
                                            t2 = t1 + dt
                                            # cat("t1=",t1," t2=",t2,"\n")
                                            x = x[utime(c(t1,t2)),]
                                            if (length(x) == 0) rep(NA_real_,4)
                                            else {
                                                nas = sum(is.na(x))
                                                x = clip(x)
                                                xm = mean(x,na.rm=T)
                                                xd = sqrt(var(x,na.method="available"))
                                                nclipped = sum(is.na(x)) - nas
                                                c(xm,xd,nclipped,nas)
                                            }
                                        },
                                        x,dt))
                            },
                            x,t1,dt,ntper),ncol=ntper*4,byrow=T)
                    times = as.numeric(seq(from=t1,length=ntper,by=dt))
                    if (dt >= 86400) fmt = "%b %02d %02H:%02M"
                    else fmt = "%02H:%02M"
                    cnames = sapply(times,function(t1,dt,fmt) {
                        paste(format(utime(t1),format=fmt),
                            format(utime(t1+dt),format=fmt),sep="-")
},dt,fmt)

                    if (T || length(unique(dns)) > 1) {
                        dimnames(x) = list(paste(dns,names(stns),sep=" "),
                            paste(sapply(cnames,function(x) c(x,"","","")),c("","sd","nclip","NAs"),sep=""))
                        attr(x,"clip") = clip(words(dns[1],1,1,sep="."))
                    }
                    else {
                        # If all variables have same name, print just station number.
                        # This has been disabled by the logical test above
                        dimnames(x) = list(stns,paste(sapply(cnames,function(x) c(x,"","","")),c("","sd","nclip","NAs"),sep=""))
                        attr(x,"clip") = clip(dns[1])
                    }
                    attr(x,"units") = dunit
                    x
                },
                x,t1,dt,ntper)
            if (length(unique(vn)) > 1)
                names(x) = paste(words(vn[1],1,1,sep=".")," (",duniqs,")",sep="")
            else
                names(x) = paste(vn[1]," (",duniqs,")",sep="")
            x$deltat = xdeltat
            x
        },
        sys.nframe(),t1,dt,ntper)
    # browser()
    x = unlist(x,recursive=F)
    x$period = dt
    x
}

qctable.links.html = function(dnames,file="",append=append)
{
    if (length(dnames) > 1) {
        html = paste("<table border=1 bgcolor=\"#BBBBBB\"><caption>Clickable Table Of Variables</caption>\n")
        for (i in seq(from=1,to=length(dnames),by=10)) {
            n = i + 9
            if (n > length(dnames)) n = length(dnames)
            html = paste(html,"  <tr>",sep="")
            html = paste(html,
                paste("<td><a href=\"#",dnames[i:n],"\">",dnames[i:n],"</a>",sep="",collapse=" "),
                "\n",sep="")
        }
        html = paste(html,"</table>\n",sep="")
        cat(html,file=file,append=append)
    }
    invisible()
}

qctable.nbspformat = function(x,...)
{
    # Format function which, after appling the regular format() function
    # to a numeric vector, then replaces spaces with &nbsp; so that
    # text aligns well with a monospaced font.  Text formatted with
    # <pre> does not seem to align correctly in tables.
    sapply(format(x,...),function(x) {
        while((n = regexpr(" ",x)) > 0) substring(x,n,n) = "&nbsp;"
        x
                })
}
qctable.nzformat = function(x,...)
{
    # Format function which, after applying qctable.nbspformat
    # to a numeric vector, then adds span element around each
    # value > 0.
    cx = qctable.nbspformat(x,...)
    bx = !is.na(x) & x > 0
    if (any(bx)) cx[bx] = paste("<span id=\"badgt0\">",cx[bx],"</span>",sep="")
    cx
}


qctable.html = function(vars,x=NULL,ntper=4,file="",append=F,title=NULL,
    prev.link="",next.link="")
{

    t1 = dpar("start")
    t2 = dpar("end")

    timecapt =  paste(
        format(t1,format="%Y %b %02d %02H:%02M"),
        format(t2,format="%Y %b %02d %02H:%02M %Z"),sep=" - ")

    # color: #FF8000 is a shade of orange. May be more readable than just "orange"
    if (!is.null(title)) {
        html = paste(
            "<!doctype html public \"-//w3c//dtd html 4.0 transitional//en\">\n",
            "<html>\n<head>\n",
            "<meta charset=\"UTF-8\">\n",
            "<style>\n",
            "<!--\n",
            "table.numdata {font-family: monospace,monospace;}\n",
            "td.nummax {color: #FF8000;}\n",
            "td.nummed {color: green;}\n",
            "td.nummin {color: yellow;}\n",
            "span.nummax {color: #FF8000;}\n",
            "span.nummed {color: green;}\n",
            "span.nummin {color: yellow;}\n",
            "#badgt0 {color: red;}\n",
            "-->\n",
            "</style>\n",
            "<title>",title,"</title>\n</head>\n",
            "<body bgcolor=\"#AAAAAA\">\n",
            "<a id=\"top\"></a>\n",
            "<center><h2>",title,"</h2></center>\n",sep="")

        cat(html,file=file,append=append)
        append = file != "" 
    }
    append = file != "" 

    if (length(vars) > 1) 
        qctable.links.html(vars,file=file,append=append)

    first = T
    for (dnx in vars) {

        # cat("dnx=",dnx,"\n")

        # If a variable has more than units value, there will be
        # more than one element in the dmat list:
        # For example:
        # names(dmat)
        # [1] "co2mr (mV*10)"    "co2mr (mmol/m^3)"

        dmatall = qctable.list(x=x,vars=dnx,t1=t1,t2=t2,ntper=ntper)

        xdeltat = dmatall$deltat
        period = dmatall$period
        dnall = names(dmatall)
        dnall = dnall[dnall != "deltat" & dnall != "period"]

        if (first) {

            if (round(xdeltat) %%  60 == 0) {
                n = round(xdeltat/60)
                deltatstr = paste(n,"minute")
            }
            else deltatstr = paste(xdeltat,"second")


            html = paste("<p>Each table cell below contains four values, which are computed from",deltatstr,"means of the indicated variable over the time period shown at the top of the column.\n",
                "<ol><li>mean of",deltatstr,"means</li>\n",
                "<li>sd: standard deviation of",deltatstr,"means</li>\n",
                "<li>nclip: number of",deltatstr,"means that were not within the indicated clip limits</li>\n",
                "<li>NAs: number of",deltatstr,"means that were NA (missing data)</li></ol>\n")
            cat(html,file=file,append=append)
            html = paste("<p>Color key:\n",
                "<ul><li><span id=\"badgt0\">red</span>: values of nclip or NAs that are greater than zero.</li>\n",
                "<li><span class=\"nummax\">maximum</span>: mean is the maximum of the values in the column</li>\n",
                "<li><span class=\"nummed\">median</span>: mean is the median of the values in the column</li>\n",
                "<li><span class=\"nummin\">minimum</span>: mean is the minimum in the column</li></ul><p>\n")
            cat(html,file=file,append=append)
        }

        first = F

        for (dn in dnall) {
            # cat("dn=",dn,"\n")

            html = ""

            dmat = dmatall[[dn]]
            if (!is.matrix(dmat)) dmat = matrix(dmat,ncol=4*ntper)

            clip.limits = attr(dmat,"clip")
            if (!is.null(clip.limits) && clip.limits$clip)
                clip.limits = paste("(",clip.limits$min,",",clip.limits$max,")",sep="")
            else clip.limits = "none"

            dunit = attr(dmat,"units")


            # format each column separately
            # 4 text fields are placed in each table cell:
            #         mean stdev numnas numclipped
            # We want the fields to be aligned down the column of the table,
            # so that is it easy for the eye to see outliers.
            # The splus format() function formats all numbers passed to it
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
            # scientific=c(-6,6) means only use exponential form for exponents outside that
            # range. Default is c(-4,4), so we're suppressing the exponential form a bit.
            #
            # Initial character matrix
            fdmat = matrix("",nrow=nrow(dmat),ncol=ncol(dmat),dimnames=list(NULL,dimnames(dmat)[[2]]))

            # Format means. Use drop=F when subsetting dmat. Otherwise if it is a one row
            # matrix (one variable), dmat[,x] will degrade to a vector, and apply will fail
            mc = seq(from=1,to=ncol(dmat),by=4)
            # pressures between 1005 and 1015 were all being formatted as 1010
            ndig = 3
            dmean = abs(mean(dmat[,mc],na.rm=T))
            if (!is.na(dmean) && dmean > 1000 && dmean < 1.e6) ndig = 4
            fdmat[,mc] = apply(dmat[,mc,drop=F],2,qctable.nbspformat,digits=ndig,scientific=c(-6,6))

            # format standard deviations to 2 significant digits
            sdc = seq(from=2,to=ncol(dmat),by=4)
            fdmat[,sdc] = apply(dmat[,sdc,drop=F],2,qctable.nbspformat,digits=2,scientific=c(-6,6))

            # add color to non-zero counts of nclip and NAs

            # fancy expression to create sequence 3,4,7,8,11,12,etc
            # nzc = rep(3:4,ntper)+rep(seq(from=0,to=ntper-1)*4,rep(2,ntper))

            # Or just exclude means and sd
            nzc = (1:ncol(dmat))[c(-mc,-sdc)]

            fdmat[,nzc] = apply(dmat[,nzc,drop=F],2,qctable.nzformat,digits=3,scientific=c(-6,6))

            if (dn == dnall[1]) html = paste(html,"<a id=\"",dnx,"\"></a><p>\n",sep="")

            html = paste(html,"<table class=\"numdata\" border=1 bgcolor=\"#BBBBBB\"><caption><b>",dnx,
                "</b>(",dunit,") ",timecapt,
                ", clip.limits=",clip.limits,"</caption>\n",sep="")

            html = paste(html,"  <tr><th>variable",sep="")

            ix = seq(from=1,by=4,to=ncol(dmat))

            # compute medians, minimums, maximums of means
            if (nrow(dmat) > 1) {
                meds = apply(dmat[,ix],2,median,na.rm=T)
                maxs = apply(dmat[,ix],2,max,na.rm=T)
                mins = apply(dmat[,ix],2,min,na.rm=T)
            }
            else {
                meds = rep(NA_real_,length(ix))
                maxs = rep(NA_real_,length(ix))
                mins = rep(NA_real_,length(ix))
            }

            # There can be more than one point the same distance from
            # the median.  Example:  in 1 2 3 4,  values 2 and 3 are
            # the same distance from the median
            maxmeds = meds
            minmeds = meds

            # if even number of points, find nearest to median
            for (j in ix) {
                if ((sum(!is.na(dmat[,j])) %% 2) == 0) {
                    ic = (j + 3) / 4
                    if (!is.na(meds[ic])) {
                        diff = abs(dmat[,j] - meds[ic])
                        mdiff = min(diff,na.rm=T)
                        nearest = !is.na(diff) & (diff - mdiff) <= mdiff * 1.e-6
                        maxmeds[ic] = max(dmat[nearest,j])
                        minmeds[ic] = min(dmat[nearest,j])
                    }
                }
            }

            dns = dimnames(dmat)
            html = paste(html,
                paste("<th>",dns[[2]][ix],"<br>mean sd nclip NAs",sep="",collapse=""),
                "</th>\n",sep="")

            vnames = dns[[1]]

            for (i in 1:nrow(dmat)) {
                vn = vnames[i]
                # variable name left justified, data center justified
                html = paste(html,"  <tr align=center><td align=left>",vn,"</td>",sep="")

                for (j in ix) {
                    entry = paste(fdmat[i,j:(j+3)],collapse=" ")
                    while((n = regexpr("NaN",entry)) > 0) substring(entry,n,n+2) = "&nbsp;&nbsp;&nbsp;"
                    while((n = regexpr("NA",entry)) > 0) substring(entry,n,n+1) = "&nbsp;&nbsp;"
                    ic = trunc(j/4) + 1
                    if (!is.na(dmat[i,j])) {
                        if (!is.na(mins[ic]) && dmat[i,j] == mins[ic])
                            html = paste(html,"<td class=\"nummin\"> ",entry," </td>",sep="")
                        else if (!is.na(maxs[ic]) && dmat[i,j] == maxs[ic])
                            html = paste(html,"<td class=\"nummax\"> ",entry," </td>",sep="")
                        else if (!is.na(minmeds[ic]) &&
                            dmat[i,j] >= minmeds[ic] && dmat[i,j] <= maxmeds[ic]) 
                            html = paste(html,"<td class=\"nummed\"> ",entry," </td>",sep="")
                        else html = paste(html,"<td> ",entry," </td>",sep="")
                    }
                    else html = paste(html,"<td> ",entry," </td>",sep="")
                }
                html = paste(html,"\n",sep="")
            }
            html = paste(html,"</table>\n",sep="")
            html = paste(html,"<table cellpadding=\"5\"><tr>\n",sep="")

            if (length(vars) > 0)
                html = paste(html,"<td><a href=\"#top\">top</a>&nbsp;&nbsp;\n",sep="")

            if (prev.link != "")
                html = paste(html,"<td><a href=\"",prev.link,"#",dnx,"\">previous</a>&nbsp;&nbsp;\n",sep="")

            if (next.link != "")
                html = paste(html,"<td><a href=\"",next.link,"#",dnx,"\">next</a>&nbsp;&nbsp;\n",sep="")

            html = paste(html,"<td><a href=\".\">other</a>\n",sep="")

            # html = paste(html,"<td>&nbsp;&nbsp;key:<td class=\"nummax\">maximum<td class=\"nummed\">median<td class=\"nummin\">minimum<td id=\"badgt0\">#clip or #NAs>0<td>created: ",
            #   format(utime("now"),format="%b %d %H:%M, %Y"),"</table>\n",sep="")

            html = paste(html,"<td>created: ",
                format(utime("now"),format="%b %d %02H:%02M, %Y"),"</td></table>\n",sep="")

            cat(html,file=file,append=append)
            append = file != "" 
        }
    }

    if (!is.null(title)) {
        html = "</body>\n</html>\n"
        cat(html,file=file,append=append)
    }

    invisible()
}
