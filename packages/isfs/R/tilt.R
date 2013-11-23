# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
#               Copyright (C) by UCAR
# 
sonic.tilt.data <- function(uvw=NULL,uvwflag=NULL, flag="ldiag",
    u.off=0, v.off=0, w.off, u.gain=1, v.gain=1, w.gain=1)
{

    # Need u,v in instrument coordinates.
    # Note that, if necessary, dat(c("u","v","w")) rotates data 
    # into instrument coordinates when dpar(coords) == "instrument"
    check.windcoords()
    rcoords <- dpar("coords")
    if (rcoords != "instrument") {
        on.exit(dpar(coords=rcoords),add=T)
        dpar(coords="instrument")
        if (missing(uvw))
            warning("tilts will be computed in instrument coordinates")
        else
            warning("Make sure uvw data is in instrument coordinates")
    }

    if (is.null(uvw)) uvw <- dat(c("u","v","w"))
    if (is.null(uvw)) stop("No u,v,w data available")
    dns <- dimnames(uvw)[[2]]
    dnsw1 <- words(dns,1,1,sep=".")
    if (sum(!is.na(match(dnsw1,c("u","v","w")))) != 3)
        stop(paste("u,v or w missing or duplicated in",
                paste(dns,collapse=",")))

    uvw[,dnsw1=="u"] <- (uvw[,dnsw1=="u"] - u.off) / u.gain
    uvw[,dnsw1=="v"] <- (uvw[,dnsw1=="v"] - v.off) / v.gain
    uvw[,dnsw1=="w"] <- uvw[,dnsw1=="w"] / w.gain

    # get flag data
    if (flag == "uvwflag") {
        if (is.null(uvwflag)) uvwflag <- dat(c("uflag","vflag","wflag"))
        if (!is.null(uvwflag)) {
            fdns <- dimnames(uvwflag)[[2]]
            fdnsw1 <- words(fdns,1,1,sep=".")

            if (sum(!is.na(match(fdnsw1,c("uflag","vflag","wflag")))) != 3)
                stop(paste("uflag,vflag or wflag missing or duplicated in",
                        paste(fdns,collapse=",")))
        }
        else
            stop("uvwflag is missing in data") 
        dimnames(uvwflag)[[2]] <-  c("u","v","w")
    }
    else if (flag == "ldiag") {
        # alternate criteria for flagging 'bad' data
        # need dat(flag) to also be the fraction of 'bad' data
        uvwflag <- dat(flag)
        if (!is.null(uvwflag)) {
            # need uvwflag to be a single column matrix
            if (ncol(uvwflag) != 1)
                stop("uvwflag has more than one column of data")
            if (flag == "samples.sonic") {
                # assume samples.sonic is percent
                if (max(uvwflag,na.rm=T) > 100)
                    stop("samples.sonic is not a percent")
                else
                    uvwflag <- (100 - uvwflag) / 100
            }
            uvwflag <- Cbind(uvwflag,uvwflag,uvwflag)
            dimnames(uvwflag)[[2]] <-  c("u","v","w")
        }
    }
    else if (flag == "weights") {
        max.weight <- median(uvw@weights,na.rm=T) 
        u.flag <- nts(1 - uvw[,dnsw1=="u"]@weights/max.weight,positions(uvw),names="u",units="")
        v.flag <- nts(1 - uvw[,dnsw1=="u"]@weights/max.weight,positions(uvw),names="v",units="")
        w.flag <- nts(1 - uvw[,dnsw1=="u"]@weights/max.weight,positions(uvw),names="w",units="")
        uvwflag <- Cbind(u.flag,v.flag,w.flag)
    }
    else {
        u.flag <- 0*u
        v.flag <- u.flag
        w.flag <- u.flag
        uvwflag <- Cbind(u.flag,v.flag,w.flag)
        dimnames(uvwflag)[[2]] <- c("u","v","w")
    }

    if (ncol(uvwflag) != 3)
        stop(paste("uflag,vflag or wflag missing or duplicated in"))

    # delete NA's
    bad <- apply(uvw,1,function(x)any(is.na(x))) |
        apply(uvwflag,1,function(x)any(is.na(x)))
    if (all(bad))
        stop("No u,v,w, or flag data available")
    else if (any(bad)) {
        uvv <- uvw[!bad,]
        uvvflag <- uvwflag[!bad,]
    }

    list(uvw=uvw,flags=uvwflag)
}

select.sonic.tilt.data  <- function(uvw,flags,
    wmax,spdmax,spdmin,flagmax,elmax,rm.azm,
    fix.gill=FALSE,fix.ir=FALSE,ohats=FALSE,debug=FALSE)
{

    deg <- 180/pi
    dir.sonic <- atan2(uvw[,2],uvw[,1])*deg
    spd.sonic <- sqrt(uvw[,1]^2 + uvw[,2]^2)
    el.sonic <- atan2(uvw[,3],spd.sonic)*deg

    if (debug) browser()

    # edit data with flagmax, spdmin, spdmax & wmax
    if (length(wmax)==1) wmax <- wmax*c(-1,1)		
    if (ohats) spd.sonic <- uvw[,1]

    select.plt <- abs(spd.sonic) < spdmax & uvw[,3] > wmax[1] &
        abs(spd.sonic) > spdmin & uvw[,3] < wmax[2] &
        flags[,1] <= flagmax & flags[,2] <= flagmax & flags[,3] <= flagmax
    # edit data with elmax
    if (!missing(elmax)) {
        if (length(elmax)==1)
            elmax <- elmax*c(-1,1)
        select.el <- elmax[1] < el.sonic & el.sonic < elmax[2]
        select.el <- select.el & !is.na(select.el)
        select.plt <- select.plt & select.el
    }
    select.plt <- select.plt & !is.na(select.plt)

    # edit data with rm.azm
    # have not adjusted fix.gill and fix.ir for length(rm.azm)=2
    if (fix.gill & length(rm.azm)==1) {
        if (rm.azm <= 30)
            select.dir <- dir.sonic <= 150-rm.azm | dir.sonic >= 150+rm.azm
        else
            select.dir <- 150+rm.azm-360 <= dir.sonic & dir.sonic <= 150-rm.azm
    }
    else if (fix.ir & length(rm.azm)==1) {
        if (rm.azm <= 30)
            select.dir = rm.azm-150 <= dir.sonic & dir.sonic <= 180
        else
            select.dir = rm.azm-150 <= dir.sonic & dir.sonic <= 360-(150+rm.azm)
    }
    else {
        if (length(rm.azm) == 1)
            select.dir <- abs(dir.sonic) <= 180-rm.azm
        else
            select.dir = rm.azm[1] < dir.sonic & dir.sonic < rm.azm[2]
    }
    
    select.dir <- select.dir & !is.na(select.dir)

    select.fit <- select.plt & select.dir

    list(fit=select.fit,plot=select.plt)
}

tiltfit <- function(u,v,w, wbar=NA)
    # wbar = mean vertical velocity; if wbar != NA, b[1] is set equal to wbar 
    # and b[c(2,3)] are determined by the least-squares fit
    # Note that I used NA for the default because the missing() 
    # function did not appear to work properly
{
    mx <- !is.na(u) & !is.na(v) & !is.na(w)
    u <- u[mx]
    v <- v[mx]
    w <- w[mx]

    if (is.na(wbar)) {
        xty <- c(sum(w),sum(u*w),sum(v*w))
        xtx <- cbind(c(length(u),sum(u),sum(v)),
            c(sum(u),sum(u*u),sum(u*v)),
            c(sum(v),sum(u*v),sum(v*v)))
        b <- solve(xtx,xty)
    }
    else {
        w <- w - wbar
        xty <- c(sum(u*w),sum(v*w))
        xtx <- cbind(c(sum(u*u),sum(u*v)),
            c(sum(u*v),sum(v*v)))
        b <- c(wbar,solve(xtx,xty))
    }
    b
}
plot.tilt <- function(uvw=NULL,uvwflag=NULL, flag="ldiag", 
    u.off=0, v.off=0, w.off, u.gain=1, v.gain=1, w.gain=1,
    flagmax=.01, spdmax=20, wmax=2, elmax, spdmin=1, rm.azm=45,
    fix.gill=FALSE, fix.ir=FALSE,
    nsmth=0, method.smth="mean",
    ohats=FALSE, ylim, ellim=5, color=1, debug=FALSE)
{

    # uvw is assumed to be a 3 column nts time series
    # with u>0 for wind from sonic.azm and 
    # v>0 for wind from (sonic.azm - 90 deg)
    #
    # if uvw is missing it is read in as dat("u","v","w")
    #
    # uvwflag is assumed to be a 3 column nts time series 
    # [u.flag,v.flag,w.flag]
    # containing the fraction of flagged sonic data.  
    # Sonic data is deleted for flag > flagmax
    # 3/9/11:
    # if is.missing(uvwflag), 'flag' defines the editing statistic to be used
    #   flag = "uvwflag" reads c("uflag","vflag","wflag")
    #   other possibilities are "samples.sonic", "diag", "ldiag" or "none"
    #
    # ellim or ylim clips elevation angle for plotting only
    #
    # u.off, u.gain, v.off, v.gain are used for linear adjustment of winds
    #
    # flagmax, spdmin, spdmax, wmax, elmax & rm.azm are used to edit data
    # Data are smoothed over nsmth data points using indicated method
    #
    # wind data are deleted for winds from rm.azm relative to sonic tower
    # if length(rm.azm)==2, it is the limits on wind direction
    # 
    # fix.gill=T fixes rm.azm for u axis at 30 deg ccw from boom
    # fix.ir=T fixes rm.azm for u axis at 30 deg cw from boom for inverted-
    # rotated sonic)
    #
    # w.off is a user-specified value for b[1]
    # if(missing(w.off)), b[1] is determined by least-squares fit to wind data
    #
    # color is color of fitted curve
    #
    # ohats=T is used if you want to sort data into wind speed bins,
    # using spdmin and spdmax, based on the sonic u component

    if (is.null(uvw) || is.null(uvwflag)) {
        data <- sonic.tilt.data(uvw,uvwflag, flag=flag, u.off, v.off, w.off, u.gain, v.gain, w.gain)
    }
    else
        data <- list(uvw=uvw,flags=uvwflags)

    dsel <- select.sonic.tilt.data(data$uvw,data$flags,
        wmax,spdmax,spdmin,flagmax,elmax,rm.azm,
        fix.gill=fix.gill,fix.ir=fix.ir,ohats=ohats,debug=debug)

    u <- data$uvw[,1]
    v <- data$uvw[,2]
    w <- data$uvw[,3]

    u.flag <- data$flags[,1]
    v.flag <- data$flags[,2]
    w.flag <- data$flags[,3]

    u.fit <- u[dsel$fit,]
    v.fit <- v[dsel$fit,]
    w.fit <- w[dsel$fit,]
    u <- u[dsel$plot,]
    v <- v[dsel$plot,]
    w <- w[dsel$plot,]

    # smooth data if nsmth>0
    if (!missing(nsmth) && nsmth>0) {
        dt <- deltat(u)["median"]
        u.fit <- average(u.fit,nsmth*dt,nsmth*dt, method=method.smth, simple=T)
        v.fit <- average(v.fit,nsmth*dt,nsmth*dt, method=method.smth, simple=T)
        w.fit <- average(w.fit,nsmth*dt,nsmth*dt, method=method.smth, simple=T)
        u <- average(u,nsmth*dt,nsmth*dt, method=method.smth, simple=T)
        v <- average(v,nsmth*dt,nsmth*dt, method=method.smth, simple=T)
        w <- average(w,nsmth*dt,nsmth*dt, method=method.smth, simple=T)
    }

    old.par <- par(xaxs="i", yaxs="i", tck=.02,mgp=c(1.2,0.2,0))
    on.exit(par(old.par))

    deg <- 180 / pi

    # Calculate tilt angles
    if (missing(w.off))
        w.off <- NA
    b <- tiltfit(u.fit, v.fit, w.fit, w.off)

    rms.residual <- sqrt(mean( (w.fit - b[1] - b[2]*u.fit
                - b[3]*v.fit)^2, na.rm=T))
    # Calculate elevation angles:
    ellab <- "elevation (deg)  (offset subtracted)"
    w <- w - b[1]
    el.plt <- atan2(w, sqrt(u^2 + v^2))*deg
    dir.plt <- atan2(v, u)*deg

    # Calculate rms residual of elevation angles
    w.fit <- w.fit - b[1]
    el.fit <- atan2(w.fit, sqrt(u.fit^2 + v.fit^2))*deg
    el.b <- atan2( b[2]*u.fit + b[3]*v.fit,
        sqrt(u.fit^2 + v.fit^2) )*deg
    rms.elresid <- sqrt(mean( (el.fit - el.b)^2, na.rm=T ))

    # clip elevation angle data
    if (!missing(ylim)) ellim = ylim
    el.plt[el.plt >  ellim] <-  ellim - 0.05
    el.plt[el.plt < -ellim] <- -ellim + 0.05
    if (!missing(ylim))
        yrange = c(-1,1)*ylim
    else
        yrange <- range(el.plt,na.rm=T)

    azlab <- "azimuth (deg)"
    azticks <- seq(-180,180,30)

    plot(dir.plt@data/180, el.plt, xlim=c(-1,1), ylim=yrange,
        xaxt="n",type="n", xlab=azlab, ylab=ellab)
    points(dir.plt@data/180, el.plt,cex=0.25) 
    #    dir.fit <- atan2(v.fit, u.fit)*deg
    #    points(dir.fit/180, el.b, pch=3, col=color+1)
    #    points(dir.fit/180, el.fit, cex=0.25,col=color+1) 

    axis(1,at=seq(-180,180,30)/180,labels=azticks,line=0)
    axis(3,at=seq(-180,180,30)/180,labels=FALSE,line=0)
    # axis(2,line=0)
    axis(4,labels=F,line=0)
    if (yrange[1]*yrange[2]<0) {
        abline(h=0)
        axis(1,at=seq(-180,180,30)/180,labels=F,pos=0)
    }
    abline(v=0)

    title(paste(paste(deparse(sys.call()),collapse=""),"from",
            dimnames(u)[[2]],dimnames(v)[[2]],dimnames(w)[[2]]),cex=0.6)
    tfmt <- "%Y %b %d %H:%M"
    title(sub=paste(paste(format(start(u),format=tfmt),
                format(end(u),format=paste(tfmt,"%Z")),sep="-"),
            "; stn=",dpar("stns"),sep=""),cex=0.7)

    # notate dataset()
    if (exists(".projectEnv") && exists("dataset.which",envir=.projectEnv))
        mtext(paste("dataset =",get("dataset.which",envir=.projectEnv)), 2.2, adj=1)

    # Plot fitted curve:
    az <- seq(-180,180,5)
    urot <- cos(az/deg)
    vrot <- sin(az/deg)
    wrot <- b[2]*urot + b[3]*vrot

    el <- atan2(wrot,sqrt(urot^2 + vrot^2))*deg
    select <- el>=yrange[1] & el<=yrange[2]

    if (dpar("length") < 5 * 86400) nlwd <- 1
    else                 nlwd <- 2

    lines(az[select]/180,el[select],lty=2,lwd=nlwd, col=color)
    if (fix.gill & length(rm.azm)==1) {
        if (rm.azm <= 30)
            select.dir <- az <= 150-rm.azm | az >= 150+rm.azm
        else
            select.dir <- 150+rm.azm-360 <= az & az <= 150-rm.azm
    }
    else if (fix.ir & length(rm.azm)==1) {
        if (rm.azm <= 30)
            select.dir = rm.azm-150 <= az & az <= 180
        else
            select.dir = rm.azm-150 <= az & az <= 360-(150+rm.azm)
    }
    else {
        if (length(rm.azm)==1)
            select.dir <- abs(az) <= 180-rm.azm
        else
            select.dir = rm.azm[1] < az & az < rm.azm[2]
    }
    select <- select & select.dir
    el[!select] <- NA
    lines(az/180,el,lwd=nlwd, col=color)

    # Label w/ offset, pitch, roll:
    old.par2 <- par(cex=0.75,usr=c(0,1,0,1))
    old.par <- c(old.par,old.par2)
    roll <- -atan(b[3])
    pitch <- atan(b[2]*cos(roll))
    pr <- format(round(c(pitch,roll)*deg,1))
    offset <- format(round(b[1]*100))
    rms <- format(round(rms.residual*100))
    rms.el <- format(round(rms.elresid, 1))
    text(0.75,0.05,paste("offset=",offset,"cm/s; pitch=",pr[1],
            ", roll=",pr[2],"deg"))
    text(0.75,0.08,paste("rms residual=",rms,"cm/s,", rms.el,"deg"))
    # Gordon's angles
    lean <- round(atan(sqrt(b[2]^2 + b[3]^2))*deg,1)
    leanaz <- round(atan2(-b[3],-b[2])*deg,1)

    bf <- format(round(b,3))
    text(0.25,0.08,paste("b =",bf[1],bf[2],bf[3]))
    text(0.25,0.05,paste("lean=",lean,"deg, leanaz=",leanaz,"deg"))

    fun.logo.stamp(print=F)
    cat(" b =",format(round(b,3)),"rms rsdl =", rms, "cm/s,",
        rms.el,"deg; lean=",lean,"leanaz=",leanaz,"deg\n")
    if (debug)
        browser()
    invisible(b)
}

uvw.concatenate = function(data="uvw")
{
    # reads uvw and uvwflag data from chats configurations with different
    # heights and concatenates them into 3-column (u,v,w) matrices

    # Need u,v,w in instrument coordinates.
    check.windcoords()
    rcoords = dpar("coords")
    if (rcoords != "instrument") {
        on.exit(dpar(coords=rcoords),add=T)
        dpar(coords="instrument")
    }

    if (data=="uvw") {
        uvw <- dat(c("u","v","w"),aslist=F)

        dns <- dimnames(uvw)[[2]]
        dnsw1 <- words(dns,1,1,sep=".")
        u = apply(uvw[,dnsw1=="u"],1, median, na.rm=T)
        v = apply(uvw[,dnsw1=="v"],1, median, na.rm=T)
        w = apply(uvw[,dnsw1=="w"],1, median, na.rm=T)

        uvw[,1]@data = u
        uvw[,2]@data = v
        uvw[,3]@data = w
        output = uvw[,1:3]
        dimnames(output)[[2]] = dns[seq(1, by=length(dnsw1[dnsw1=="u"]), length=3)]
    }
    else if (data=="uvwflag") {
        uvwflag <- dat(c("uflag","vflag","wflag"),aslist=F)

        dns <- dimnames(uvwflag)[[2]]
        dnsw1 <- words(dns,1,1,sep=".")
        uflag = apply(uvwflag[,dnsw1=="uflag"],1, median, na.rm=T)
        vflag = apply(uvwflag[,dnsw1=="vflag"],1, median, na.rm=T)
        wflag = apply(uvwflag[,dnsw1=="wflag"],1, median, na.rm=T)

        uvwflag[,1]@data = uflag
        uvwflag[,2]@data = vflag
        uvwflag[,3]@data = wflag
        output = uvwflag[,1:3]
        dimnames(output)[[2]] = dns[seq(1, by=length(dnsw1[dnsw1=="uflag"]), 
            length=3)]
    }

    invisible(output)
}

setGeneric("tilt",function(x,tiltp) standardGeneric("tilt"))
setMethod("tilt",signature(x="dat",tiltp="dat"),
    function(x,tiltp)
    {

        stns <- unique(stations(x))

        for (stn in stns) {

            tiltps <- select(tiltp,stns=stn)
            xs <- select(x,stns=stn)

            dns <- dimnames(xs)[[2]]
            dnsw1 <- words(dns,1,1,sep=".")

            # figure out what scalar covariances are in xs
            covars <- nwords(dnsw1,sep="'") > 1
            scalars <- NULL
            if (any(covars)) {
                covars <- dnsw1[covars]
                uvars <- words(covars,1,1,sep="'") == "u"
                scalars <- words(covars[uvars],2,2,sep="'")
                scalars <- scalars[is.na(match(scalars,c("u","v","w")))]
            }

            # Duplicate last row of tiltp so that it covers span of xs
            t2 <- positions(xs)[nrow(xs)]
            if (positions(tiltps)[nrow(tiltps)] < t2) {
                tiltps2 <- tiltps[nrow(tiltps),]
                positions(tiltps2) <- t2 + 0.001
                tiltps <- Rbind(tiltps,tiltps2)
            }

            # tilts can be time varying
            for (r in (1:(nrow(tiltps) - 1))) {
                t1 <- positions(tiltps)[r]
                t2 <- positions(tiltps)[r+1] - 0.001
                if (positions(xs)[nrow(xs)] >= t1 && positions(xs)[1] <= t2) {
                    mr <- sonic.tilt.matrix(
                        tiltps@data[r,"lean"],
                        tiltps@data[r,"leanaz"])
                    mr <- earray(t(mr))
                    # browser()

                    sonic <- xs[utime(c(t1,t2)),]
                    nr <- nrow(sonic)

                    dn <- "w"
                    dm <- !is.na(match(dnsw1,dn))
                    if (sum(dm) > 1) stop(paste("multiple w columns:",paste(dns[dm],collapse=","),". Select a sonic"))
                    if (any(dm))
                        sonic[,dm] <- sonic[,dm] - as.numeric(tiltps[r,"woff"])

                    dn <- c("u","v","w")
                    if (any((dm <- match(dn,dnsw1,nomatch=0))==0) && !all(dm==0))
                        stop(paste("columns",paste(dn[dm==0],collapse=","),"not found in x"))

                    if (!all(dm==0)) {

                        # check for multiple matches
                        dc <- !is.na(match(dnsw1,dn))
                        if (length(unique(dnsw1[dc])) != length(dnsw1[dc]))
                            stop(paste("multiple u,v or w columns:",paste(dns[dc],collapse=","),". Select a sonic"))

                        sonic[,dm] <- sonic[,dm] %*% mr
                    }

                    # second moments
                    dn <- c("u'u'","u'v'","u'w'","u'v'","v'v'","v'w'","u'w'","v'w'","w'w'")
                    if (any((dm <- match(dn,dnsw1,nomatch=0))==0) && !all(dm==0))
                        stop(paste("columns",paste(dn[dm==0],collapse=","),"not found in x"))
                    if (!all(dm==0)) {

                        # check for multiple matches
                        dc <- !is.na(match(dnsw1,unique(dn)))
                        if (length(unique(dnsw1[dc])) != length(dnsw1[dc]))
                            stop(paste("multiple u'*',v'*' or w'*' columns:",paste(dns[dc],collapse=","),". Select a sonic"))

                        # earray class follows Einstein product convention: z[i,k] = (sum over j of) (x[i,j] * y[j,k])
                        sonic[,dm] <- earray(aperm(earray(sonic[,dm],c(nr,3,3)) %*% mr,perm=c(1,3,2)) %*% mr,c(nr,9))
                    }

                    if (!is.null(scalars) && (length(scalars) > 0)) {
                        for (i in 1:length(scalars)) {
                            dn <- paste(c("u","v","w"),"'",scalars[i],"'",sep="")
                            if (any((dm <- match(dn,dnsw1,nomatch=0))==0) && !all(dm==0))
                                stop(paste("columns",paste(dn[dm==0],collapse=","),"not found in x"))
                            if (!all(dm==0)) {

                                # check for multiple matches
                                dc <- !is.na(match(dnsw1,dn))
                                if (length(unique(dnsw1[dc])) != length(dnsw1[dc]))
                                    stop(paste("multiple u'*',v'*' or w'*' columns:",paste(dns[dc],collapse=","),". Select a sonic"))

                                sonic[,dm] <- sonic[,dm] %*% mr
                            }
                            T
                        }
                    }
                    xs[utime(c(t1,t2)),] <- sonic
                }
                T
            }
            xsd <- dimnames(xs)[[2]]
            xd <- dimnames(x)[[2]]
            xstns <- stations(x)
            xm <- match(xd,xsd)
            x[,(!is.na(xm) & xstns == stn)] <- xs[,xm[xstns==stn]]
        }
        x
    }
)
