# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

plot_sunangle <- function(browse=F,year=2006,lat=40,lon=-105,
    yhi=90,twilight=T,localhours=T,verbose=FALSE)
{
    if (verbose) {
        cat(paste("Plotting Sun Angles For: lat,lon =",lat,"/",lon,"\n"))
        cat("See \"? plot_sunangle\"\n")
    }

    # nominal hour offset from greenwich
    TZoffset<- trunc(lon/15)

    timezone_orig <- options("time.zone")
    on.exit(options(time.zone=timezone_orig),add=TRUE)

    # http://en.wikipedia.org/wiki/Tz_database
    # The special area of "Etc" is used for some administrative zones, particularly for "Etc/UTC" which
    # represents Coordinated Universal Time. In order to conform with the POSIX style, those zone names
    # beginning with "Etc/GMT" have their sign reversed from what most people expect. In this style,
    # zones west of GMT have a positive sign and those east have a negative sign in their name
    # (e.g "Etc/GMT-14" is 14 hours ahead/east of GMT.)
    # The above also applies to GMT+-N timezones, without the "Etc".
    # Sys.setenv(TZ=paste0("Etc/",timezone))
    if (TZoffset >= 0)
        timezone <- paste0("GMT",-TZoffset)
    else
        timezone <- paste0("GMT+",-TZoffset)
    options(time.zone=paste0("Etc/",timezone))

    #----------------------------------------
    # Graphical Setups
    #----------------------------------------
    old.par <- par(no.readonly=TRUE)
    on.exit(par(old.par))

    mar <- c(5,4,4,3) + 0.1
    par(mfrow=c(1,1),mar=mar)

    # Make sure the axis limits aren't offset by 4%
    #
    par(xaxs="i",yaxs="i")

    #----------------------------------------
    # Get the SOLAR DATA for the given lat/lon
    #----------------------------------------

    # setup the data arrays
    nmonths <- 12
    day <- 21
    times <- seq( from=0, to=24, length=288 )
    index <- c(1:288)

    allelev <- array(0,c(length(times),nmonths))
    allazm <- array(0,c(length(times),nmonths))

    #Establish the plot boundaries

    xgranule <- 20		# ticks every 20-degrees azm.
    ygranule <- 10		# ticks every 10-degrees elev.

    if( twilight ) {
        ylow <- -18
        ysteps <- c(-18,-12,-6,seq(from=0, to=yhi, by=ygranule))
    } else {
        ylow <- 0
        ysteps <- seq(from=ylow, to=yhi, by=ygranule)
    }

    # Primary PLOT:
    # 5-min Data for the months, centered on the 21st
    # This calls a function which is located in the main ASTER
    # tree: fun.sunangle
    #
    #dpar(lat=lat,lon=lon,lenday=1,avg=3600)

    orig_dpar <- dpar(c("start","end","avg","lat","lon"))
    on.exit(dpar(orig_dpar),add=TRUE)

    dpar(lat=lat,lon=lon,lenday=1,avg=300)	# 5-min data for 1 day at...

    xlow <- 0
    xhi <- 360

    #----------------------------------------
    # Grab and Plot June or Dec, check az limits and plot the rest
    # adding labels as we go
    #----------------------------------------
    if (lat > 0)
        tlist <- list(year=year,mon=6,day=21,hour=0,min=0,sec=0,TZ=timezone)
    else
        tlist <- list(year=year,mon=12,day=21,hour=0,min=0,sec=0,TZ=timezone)
    dpar(start=utime(tlist),lenday=1)
    sun <- dat("azel.sun")

    # check for the azimuth value limits for setting plot limits
    # this will occur in june.
    #select <- sun[,1] > ylow
    #xlow <- trunc(min(sun[select,1])/xgranule-.2) * xgranule
    #xhi <- 360-xlow
    #xsteps <- seq(from=xlow, to=xhi, by=xgranule)

    plot(sun[,1], sun[,2], type="l",
        xlim=c(xlow,xhi),ylim=c(ylow,yhi),
        xlab="Sun Azimuth",ylab="Sun Elevation",
        axes=F)
    text(180,max(sun[,2])+1.5,"6/21",cex=.8)

    # Plot the Rest, labeling each beginning with December
    if (lat > 0)
        tlist$mon <- 12
    else
        tlist$mon <- 6

    dpar(start=utime(tlist))
    sun <- dat("azel.sun")
    lines(sun[,1],sun[,2],lty=1)
    text(180,max(sun[,2])+1.5,"12/21",cex=.8)

    for(i in c(1,2,3,4,5)) {
        tlist$mon <- i
        dpar(start=utime(tlist))
        sun <- dat("azel.sun")
        lines(sun[,1],sun[,2],lty=1)
        jdstamp <- paste(i,"/",day,"-",12-i,"/",day,sep="")
        text(180,max(sun[,2])+1.5,jdstamp,cex=.8)
    }

    for(i in c(7,8,9,10,11)) {
        tlist$mon <- i
        dpar(start=utime(tlist))
        sun <- dat("azel.sun")
        lines(sun[,1],sun[,2],lty=2)
    }


    #----------------------------------------
    # Make the axes by hand...
    #----------------------------------------
    xsteps <- seq(from=xlow, to=xhi, by=xgranule)

    axis(1, at = xsteps, tick = T, labels = T)	#  bottom
    axis(3, at = xsteps, tick = F, labels = F)	#  top
    axis(2, at = ysteps, tick = T, labels = T)	#  left
    #  axis(4, tick = T, labels = F)		#  right

    abline(v=xsteps,h=ysteps,lty=2)
    abline(v=xhi,h=yhi,lty=1)
    abline(h=0,lty=1)		# Solid Mark for sunset

    if( twilight ) {		# Dashed Marks for twilights
        xlow <- xlow + (xhi-xlow)/12
        text(xlow,(-6+1.5),"Civil Twilight",cex=.7)
        text(xlow,(-12+1.5),"Nautical Twilight",cex=.7)
        text(xlow,(-18+1.5),"Astronomical Twilight",cex=.7)
    }


    #----------------------------------------
    # Plot Hour Iso-Lines
    #----------------------------------------
    # message("PLEASE WAIT: Getting Hour Iso-Line Data..." )

    # Establish hourly data 
    # Using weeks instead of months to provide a more resolution
    nweeks <- 52
    hours <- c(1:24)		# hours to plot....faster load

    hourelev <- array(0,c(24,nweeks+1))
    hourazm  <- array(0,c(24,nweeks+1))

    tlist <- list(year=year,mon=1,day=1,hour=0,min=0,sec=0)
    ut <- utime(tlist)

    secondsperweek <- 86400*7
    # Won't really result in averaged data. dat.azel.sun uses value of dat("avg")
    # for desired resolution
    dpar(avg=3600,start=utime(tlist))
                                    
    for(i in c(1:nweeks)) {
        dpar(start=ut)			# establish time for 'dat'
        sun <- dat("azel.sun")		# produces a 25x2 array (from 00:00 -00:00 local)
        select <- sun[,2] >= ylow	# clip data below graph
        sun[!select,2] <- ylow
        hourazm[,i] <- sun@data[c(1:24),1]	# grab only 00:00-23:00 local values
        hourelev[,i] <- sun@data[c(1:24),2]
        ut <- ut + secondsperweek		# incr. time for next set
    }

    # Setup so that the hourly lines are drawn from Dec to Jan also
    #
    hourelev[hours,nweeks+1] <- hourelev[hours,1]
    hourazm[ hours,nweeks+1] <- hourazm[ hours,1]

    # Plot Hourly Values
    #
    for(i in seq(from=1,to=24)) {
        #	if(max(hourelev[i,]) < ylow) next
        if(max(hourelev[i,]) < 0) next

        # plot lines are different for 1st 6-mos, vs 2nd...
        lines(hourazm[i,c(53,1:26)],hourelev[i,c(53,1:26)],lty=5,col=2)
        lines(hourazm[i,c(26:52)],hourelev[i,c(26:52)],lty=6,col=3)

        # label the hour above the line

        px <- hourazm[i,26]
        py <- hourelev[i,26] + (yhi-ylow)/20	# move the pen up a bit

        # Use this for labeling the local times
        if( localhours ) {
            jdstamp <- paste(i-1,sep="")	# hour for the calculated offset
        }
        else {
            ix <- i - 1 - TZoffset
            if( ix > 23 ) ix <- ix-24
            if( ix < 0  ) ix <- ix+24
            # Or use this to label the GMT hours
            jdstamp <- paste(ix,"Z",sep="")	# old Z method
        }

        text(px,py,jdstamp,cex=1,col=2)
    }

    if( localhours ) {
        text(360,yhi-5, paste0("gmt",ifelse(TZoffset >= 0,"+",""),TZoffset," hrs, POSIX TZ=",timezone),adj=1)
        text(360,yhi-8, "Add 1 for DayLight Savings Time",adj=1)
    }
    legend("topleft",c("Dec-Jun hours","Jun-Dec hours"),lty=c(5,6),col=c(2:3),cex=.8,bty="n")

    if( browse )	browser()


    #----------------------------------------
    # Now for my title at the top of the page
    #----------------------------------------
    par(fig=c(.1,1.0,.5,1))
    title(paste("Sun Angle Positions for    lat/lon: ",lat,",",lon,"   Year:",year,sep=""),cex=.8)

    invisible(NULL)
}
plot_sunshadow <- function(year=2006,browse=F,lat=40,lon=-105,
    range=4,localhours=T,lowest=0,verbose=FALSE)
{
    if (verbose) {
        cat(paste("Plotting Sun Shadows For: lat,lon =",lat,"/",lon,"\n"))
        cat("See \"? plot_sunshadow\"\n")
    }

    # nominal hour offset from greenwich
    TZoffset<- trunc(lon/15)

    timezone_orig <- options("time.zone")
    on.exit(options(time.zone=timezone_orig),add=TRUE)

    # http://en.wikipedia.org/wiki/Tz_database
    # The special area of "Etc" is used for some administrative zones, particularly for "Etc/UTC" which
    # represents Coordinated Universal Time. In order to conform with the POSIX style, those zone names
    # beginning with "Etc/GMT" have their sign reversed from what most people expect. In this style,
    # zones west of GMT have a positive sign and those east have a negative sign in their name
    # (e.g "Etc/GMT-14" is 14 hours ahead/east of GMT.)
    # The above also applies to GMT+-N timezones, without the "Etc".
    # Sys.setenv(TZ=paste0("Etc/",timezone))
    if (TZoffset >= 0)
        timezone <- paste0("GMT",-TZoffset)
    else
        timezone <- paste0("GMT+",-TZoffset)
    options(time.zone=paste0("Etc/",timezone))

    #----------------------------------------
    # Graphical Setups
    #----------------------------------------
    old.par <- par(no.readonly=TRUE)
    on.exit(par(old.par))

    mar <- c(5,4,4,3) + 0.1
    par(mfrow=c(1,1),mar=mar)

    # Make sure axis limits aren't offset by 4%
    #
    par(xaxs="i",yaxs="i")
    par(pty="s")		# Make plot square, aspect ratio = 1:1

    # Establish the plot boundaries

    range <- trunc(range)
    xlow <- -range
    xhi <- range
    ylow <- xlow
    yhi <- xhi

    xspan <- xhi-xlow
    yspan <- yhi-ylow

    if( range>4 ) granule<-1 else granule<-.5

    xsteps <- seq(from=xlow, to=xhi, by=granule)
    ysteps <- seq(from=ylow, to=yhi, by=granule)


    #----------------------------------------
    # Get the SOLAR DATA for the given lat/lon
    # 
    # Make sure to Transpose to the shadow data
    #	azimuth of the shadow is 180 opposite of the sun position
    #
    #----------------------------------------

    # setup the data arrays

    nmonths <- 12
    day <- 21

    gmt <- seq( from=0, to=24, length=288 )
    index <- c(1:288)

    allelev <- array(0,c(length(gmt),nmonths))
    allazm <- array(0,c(length(gmt),nmonths))
    x <- array(0, c(length(gmt),nmonths))
    y <- array(0, c(length(gmt),nmonths))

    orig_dpar <- dpar(c("start","end","avg","lat","lon"))
    on.exit(dpar(orig_dpar),add=TRUE)

    dpar(lat=lat,lon=lon,lenday=1,avg=300)	# 5-min data for 1 day at...

    # Primary PLOT:
    # 5-min Data for the months, centered on the 21st
    #
    for(i in c(1:6,12)) {
        tlist <- list(year=year,mon=i,day=21,hour=0,min=0,sec=0,TZ=timezone)
        dpar(start=utime(tlist))
        sun <- dat("azel.sun")

        select <- sun[,2] > lowest
        sun[!select,2] <- 0

        # Convert 'elev' to radians
        sun[select,2] <- (sun[select,2] *pi) / 180

        # Convert Sun azimuth to 180 out for the shadow,
        # and then put them in radians
        sun[,1] <- sun[,1] +180		# look for over 360-degrees
        select <- sun[,1] >= 360
        sun[select,1] <- sun[select,1] -360
        sun[,1] <- (sun[,1] * pi) / 180

        allelev[,i] <- 1/tan(sun@data[c(1:288),2])
        allazm[,i] <- sun@data[c(1:288),1]

        # Put these into cartesian coordinates for plotting
        y[,i] <- allelev[,i] * cos(allazm[,i])
        x[,i] <- allelev[,i] * sin(allazm[,i])
    }


    # Primary Data Plot
    #
    # Draw lines for only through June, because they should be the same
    # for Jul-Dec
    # NOTE: sort the list so that we don't get funny lines from the pen
    # sweep.
    # This looks for sorting of the azimuth array for a minimum

    axislabel <- "Shadow Length / Height of Object"

    ix <- sort.list(x[,3])
    plot(x[ix,3],y[ix,3], type="l",
        xlim=c(xlow,xhi),ylim=c(ylow,yhi),
        xlab=axislabel,ylab=axislabel,
        axes=F)

    for(i in c(1,2,4,5,6,12)) {
        #	ix <- sort.list(x[,i])
        lines(x[ix,i],y[ix,i],lty=1)
    }

    # Put on Month labels

    for(i in c(1:5,6,12)) {

        if( i>=1 && i<=5 ) {
            jdstamp <- paste(i,"/21-",12-i,"/21",sep="")
        } else {
            jdstamp <- paste(i,"/21",sep="")
        }

        select <- x[,i]>=xlow
        minx <- min( x[select,i] )
        select <- x[,i]==minx
        ix <- index[select]
        py <- y[ix,i]
        px <- max( xlow+.1, minx-(granule/2) )

        text(px,py,jdstamp,cex=.8,adj=0)
    }

    #----------------------------------------
    # make the axes by hand...
    # 1=bottom, 3=top, 2=left, 4=right
    #----------------------------------------
    axis(1, at = xsteps, tick=T, labels=T)
    #axis(3, at = xsteps, tick=F, labels=F)
    axis(2, at = ysteps, tick=T, labels=T)
    #axis(4, tick = T, labels=F)
    abline(h=yhi,v=xhi)	# solid line on right/top instead

    # lft/rt and top/bot crosshairs
    lines(c(xlow*.85,xhi*.85),c(0,0),lty=3)
    lines(c(0,0),c(ylow*.85,yhi*.85),lty=3)
    #abline(v=0,h=0,lty=3)

    # Add some unit circles onto the plot
    for(i in c(2:range-1)) {
        symbols(0,0,circles=i,add=TRUE,inches=FALSE,lty=2)
        text(0,i,paste(i,"H",sep=""),cex=1,col=2)
    }

    # Add the Directions
    text(0,yhi*.9,"N",cex=1.2,col=2)
    text(0,ylow*.9,"S",cex=1.2,col=2)
    text(xhi*.9,0,"E",cex=1.2,col=2)
    text(xlow*.9,0,"W",cex=1.2,col=2)


    #----------------------------------------
    # Plot Hour Iso-Lines
    # If So Desired
    #----------------------------------------
    # message("PLEASE WAIT: Getting Hour Iso-Line Data..." )

    nweeks <- 52
    midweek <- nweeks/2
    gmt <- c(1:24)		# Reset for hours only....faster load
    hourelev <- array(0,c(24,nweeks+1))
    hourazm <- array(0,c(24,nweeks+1))

    x <- array(0, c(length(gmt),nweeks+1))
    y <- array(0, c(length(gmt),nweeks+1))

    ut <- utime(paste(year,"1 1 00:00"))
    secondsperweek <- 86400*7
    dpar(avg=3600)

    for(i in c(1:nweeks)) {
        dpar(start=ut)
        sun <- dat("azel.sun")	# produces a 25x2 array (from 0Z-0Z)

        select <- sun[,2] >= 0
        sun[!select,2] <- NA

        # Convert 'elev' to radians
        sun[select,2] <- (sun[select,2] *pi) / 180

        # Convert Sun azimuth to 180 out for the shadow,
        # and then put them in radians
        sun[,1] <- sun[,1] +180		# look for over 360-degrees
        select <- sun[,1] >= 360
        sun[select,1] <- sun[select,1] -360
        sun[,1] <- (sun[,1] * pi) / 180

        # grab only 0Z-23Z values to avoid substitution element len. warnings
        hourelev[,i] <- 1/tan(sun@data[c(1:24),2])
        hourazm[,i] <- sun@data[c(1:24),1]

        ut<- ut+secondsperweek
    }

    # Setup so that the hourly lines are drawn from Dec to Jan also
    #
    hourelev[1:24,nweeks+1] <- hourelev[1:24,1]
    hourazm[1:24,nweeks+1] <- hourazm[1:24,1]

    # And put these data into cartesian coordinates for plotting
    y <- hourelev * cos(hourazm)
    x <- hourelev * sin(hourazm)

    ix<- seq(1:length(x[1,]))	# sets up index for plotting based on select

    # Plot Hour lines
    for(i in seq(from=1,to=24,by=1)) {

        # plot lines are different for 1st 6-mos, vs 2nd...
        #	select <- abs(x[i,])<=range & !is.na(x[i,]) & abs(y[i,])<=range & !is.na(y[i,])
        #	lines(x[i,select],y[i,select]-2,lty=5,col=2)

        lines(x[i,c(26:52)],y[i,c(26:52)],lty=5,col=2)
        lines(x[i,c(53,1:26)],y[i,c(53,1:26)],lty=6,col=3)
    }

    # and Hour Label
    for(i in seq(from=2,to=24,by=2)) {


        if( localhours ) {
            jdstamp <- paste(i-1,sep="")	# hour for the calculated offset
        }
        else {
            ix <- i - 1 - TZoffset
            if( ix > 23 ) ix <- ix-24
            if( ix < 0  ) ix <- ix+24
            # Or use this to label the GMT hours
            jdstamp <- paste(ix,"Z",sep="")	# old Z method
        }

        px <- x[i,26]
        py <- y[i,26] - .2
        text(px,py,jdstamp,cex=1,col=2)
    }


    #----------------------------------------
    # title, legend and useful info
    #----------------------------------------
    if( localhours ) {
        text(xlow*.5,ylow*.84, paste0("gmt",ifelse(TZoffset >= 0,"+",""),
                TZoffset," hrs, POSIX TZ=",timezone,"\n Add 1 for Daylight Savings Time",sep=""))
    }
    text(xhi*.5,ylow*.8, "Shadow extends from 'center'
        at cross-hairs to
        date/hour point on plot")

    xspan <- xspan*.2
    xlow <- xhi*.9
    yspan <- yspan*.07
    ylow <- ylow*.7
    legend("topright",c("Dec-Jun","Jun-Dec"),lty=c(5,6),col=c(2,3),cex=.8,bty="n")

    par(fig=c(.1,1.0,.5,1))
    title(paste("Sun Shadow Lengths for    lat/lon: ",lat,",",lon,"   Year:",year,sep=""),cex=.8)
    if( browse )	browser()

    invisible(NULL)
}
