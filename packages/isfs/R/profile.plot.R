# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
#               Copyright (C) by UCAR
# 
profile.plot = function(x, every=3600, ntraces=6, smooth=deltat(x)[1],
                        xlim=NULL,ylim,ylab="Heights (m)", dx=0.02,
                        hd, ...) 
{
    # dx = incremental offset of profiles as fraction of data range
    #      (also acceptable offset between adjacent profiles
    # hd = displacement height

    old.par = par(oma=c(1.3,0,3.5,0),mar=c(3.3, 3.3, 1.5, 1.0),
                  mgp=c(1.3,0.1,0),tck=0.02)
    on.exit(par(old.par))

    dt = deltat(x)[1]
    outint = dt;
    if ((smooth %% outint) != 0) outint = smooth
    if (smooth > dt) x = average(x,avgperiod=smooth,outinterval=outint)

    vname0 =  paste(unique(words(dimnames(x)[[2]],1,1)),collapse=",")
    vname = vname0
    vunits =  paste(unique(x@units),collapse=",")
    # get common suffix
    vsuffix = unique(sapply(dimnames(x)[[2]],function(x){words(x,nwords(x,sep="."),nwords(x,sep="."))}))
    vstn = unique(names(stations(x)))
    if ((is.null(vstn) || vstn == "") && length(vsuffix) == 1)
      vname = paste(vname,vsuffix,sep=".")

    hts = heights(x)
    if (!missing(hd))
      hts = hts-hd
    hts.order = order(hts,na.last=FALSE)
    x = x[,hts.order]
    hts = hts[hts.order]

    t1 = tspar(x)[1]
    tp1 = (t1 %/% every) * every
    if (tp1 < t1) tp1 = tp1 + every

    # start times of profile plots
    tps = seq(from=tp1, to=tspar(x)[nrow(x)],by=every*ntraces)

    # number of columns of plots
    nc = 2
    if (length(tps) > 4) nc = 3

    # number of rows of plots
    nr = ceiling(length(tps) / nc)
    old.par = c(old.par,par(mfrow=c(nr,nc)))

    t1 = NULL
    t2 = NULL

    xp = array(NA_real_,dim=c(length(tps),ntraces,length(hts)))

    xinfo <- plot.dat.limits(x,xlim,TRUE)

    clipped <- F
    cmin <- xinfo$ylim[1]
    cmax <- xinfo$ylim[2]
    clip.min <- x < cmin & !is.na(x)
    if (any(clip.min)) {
      x@data[clip.min] <- NA_real_
      clipped <- T
    }
    clip.max <- x > cmax & !is.na(x)
    if (any(clip.max)) {
        x@data[clip.max] <- NA_real_
        clipped <- T
    }
    
    # loop over plots to determine the range
    ip = 0		# plot counter
    for (tp in tps) {
        ip = ip + 1	
        it = 0		# trace counter

        te = tp + every*(ntraces - 1)

        # tpx is time of each trace
        for (tpx in seq(from=tp,to=te,by=every)) {
            it = it + 1
            if (is.null(t1)) t1 = utime(tpx)
            t2 = utime(tpx)

            # time difference of each data point from trace time
            td = abs(tspar(x) - tpx)
            tdm = min(td)

            # Don't plot profile if its time is more than 2 deltat's away
            # from the labeled time
            if (tdm < outint * 2.1) {
                i = seq(along=td)[td == tdm][1]
                xp[ip,it,] = as.vector(x[i,]@data)
            }
        }
    }

    type = "b"

    # add offsets to individual profiles to separate them if too close
    # dx is fraction of plot width
    if (!all(is.na(xp[,,]))) {
        dx = dx*max(pretty(xp, 10))
        offset = matrix(0,nrow=length(tps),ncol=ntraces)
        if (dx > 0) {
            # iterate on plots
            for (ip in seq(along=tps)) {
                # find order and differences of mid-points of profiles
                x.med = apply(xp[ip,,], 1, median, na.rm=T) 
                order.m = order(x.med)
                diff.m = diff(x.med[order.m])
                # compute offsets
                select =  !is.na(diff.m) & diff.m < dx
                offset[ip,] = cumsum(c(0,select))
                # set offset of is.na(diff.m) equal to zero
                offset[ip,c(F,is.na(diff.m))] = 0
                # add offsets to profiles
                offset[ip,order.m] = offset[ip,]
                xp[ip,,] = xp[ip,,] + offset[ip,]*dx
            }
        }
        rng = range(xp,na.rm=T)
    }
    else {
        xp[,,] = 0
        type = "n"
        rng = c(-1,1)
    }

    if (length(xlim) == 0) xlim = rng

    if (missing(ylim)) ylim = range(hts,na.rm=T)

    # loop over plots. Each plot has ntraces in it
    ip = 0		# plot counter
    for (tp in tps) {
        ip = ip + 1	

        te = tp + every*(ntraces - 1)
        lgnd = sapply(as.numeric(seq(from=tp,to=te,by=every)),
            function(x){format(utime(x),format="%02H:%02M")})

        xlab = paste(
            vname,"(",vunits,") ",
            format(utime(tp),format="%02H:%02M-"),
            format(utime(te),format="%02H:%02M %Z"),sep="")

        if (all(is.na(xp[ip,1,]))) {
          # all data is NA, just plot a point off scale
          # xp[ip,1,1] = rng[1] - .1 * (rng[2] - rng[1])
            xp[ip,1,1] = rng[1]
            plot(xp[ip,1,1],hts[1]+100,type="n",axes=TRUE,xlab=xlab,ylab=ylab,
              xlim=xlim,ylim=ylim,col=2,...)
        }
        else
            plot(xp[ip,1,],hts,type=type,axes=TRUE,xlab=xlab,ylab=ylab,
              xlim=xlim,ylim=ylim,col=2,...)

        for (i in 2:ntraces) if (!all(is.na(xp[ip,i,])))
            lines(xp[ip,i,],hts,type=type,col=i+1)

        axis(3,labels=FALSE, tick=TRUE)
        axis(4,labels=FALSE, tick=TRUE)

        # add vertical line at x=0
        # if (xy[1]*xy[2] < 0 && vname0 != "T")
        if (xlim[1]*xlim[2] < 0 && vname0 != "T")
            abline(v=0, lty=1)

        xy = par("usr")
        lx = xy[1] + .05 * (xy[2] - xy[1])
        ly = xy[4] - .05 * (xy[4] - xy[3])
        # list profile offsets
        if (dx > 0)
            lgnd = paste(lgnd,"(",offset[ip,],")",sep="")

        horiz.legend(lx,ly, lgnd, col=2:(ntraces+1),bty="n",cex=1.1)
    }

    if (smooth == 1800) ptitle = "1/2 Hour Average"
    else if (smooth == 3600) ptitle = "Hourly Average"
    else if ((smooth %% 60) == 0) ptitle = paste(smooth %/% 60,"Minute Average")
    else ptitle = paste(smooth,"Second Average")
    ptitle = paste(ptitle,vname,"Profiles")
    # add station id to plot title
    stn = unique(names(stations(x)))
    if (!is.null(vstn) && vstn != "")
      ptitle = paste(ptitle,"; station = ",vstn, sep="")

    # add dx to plot title
    if (dx > 0)
      ptitle = paste(ptitle,"; offsets = (n)*", format(signif(dx,3))," ",vunits, sep="")
    mtext(ptitle,side=3,line=2.0,outer=T,cex=1.3)

    mtext(paste(format(t1,format="%Y %b %d %02H:%02M"),
                format(t2,format="%b %d %02H:%02M %Z"),sep="-"),
      side=3,line=0,outer=T,cex=1.2)
    fun.logo.stamp()
    NULL
}
