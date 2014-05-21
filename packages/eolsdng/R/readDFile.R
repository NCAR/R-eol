# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsdng" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

readDFile <- function (file,sta_clean=TRUE) 
{
    # if (file.access(file,4) != 0)
    #     stop(paste("Error:",file,"does not exist or is not readable"))

    # grab first two columns, which look like:
    #   AVAPS-T04 COM
    #   AVAPS-D04 ???
    cols <- scan(file=file,what=list(ava="",sta=""),flush=TRUE,quiet=TRUE)

    # AVAPS-Txx COM lines
    com <- grepl("AVAPS-T",cols$ava,fixed=TRUE) & grepl("COM",cols$sta,fixed=TRUE)

    # AVAPS-Dxx lines are data
    drows <- grepl("AVAPS-D",cols$ava,fixed=TRUE)

    # Variable names are in COM lines 1 and 2.
    hdr <- seq(along=com)[com][1]

    # First header line
    h1 <- unlist(read.table(file=file,skip=hdr-1,nrows=1,row.names=NULL,stringsAsFactors=FALSE)[,c(-1,-2)])

    # Second header line
    h2 <- unlist(read.table(file=file,skip=hdr,nrows=1,row.names=NULL,stringsAsFactors=FALSE)[,c(-1,-2)])

    # Third header line, units
    hunits <- unlist(read.table(file=file,skip=hdr+1,nrows=1,row.names=NULL,stringsAsFactors=FALSE)[,c(-1,-2)])

    # For some variables the field in the first header line may be blank, e.g. "Sonde" (sid)
    if (length(h1) < length(h2)) {
        mx <- match("Sonde",h2)
        if (!is.na(mx)) {
            if (mx == 1) h1 <- c("",h1)
            else h1 <- c(h1[1:(mx-1)],"",h1[mx:length(h1)])
        }
    }

    varnames <- paste(h1,h2,sep="")

    # mapping of header strings to returned variable names
    hnames <- list(
        "Sonde"="sid",
        "UTCDate"="utc.date",
        "UTCTime"="utc.time",
        "AirPress"="p",
        "AirTemp"="temp",
        "RelHumid"="rh",
        "WindDir"="wdir",
        "WindSpd"="wspd",
        "VertVeloc"="dz",
        "GPSLongitude"="lon",
        "GPSLatitude"="lat",
        "GeopotenAltitude"="gp.alt",
        "GPSWnd"="wsat",
        "SondeRH1"="rh1",
        "SondeRH2"="rh2",
        "GPSSnd"="ssat",
        "WindError"="werr",
        "GPSAltitude"="gps.alt")

    # character columns
    # sta:  Spw:  p=0 good PTU checksum, p=1 bad PTU checksum
    # sta:  Spw:  w=0 good wind checksum, w=1 bad wind checksum
    strnames <- c("ava","sta")
    utcnames <- c("utc.date","utc.time")

    # names of the data columns in the D file, in order
    dnames <- hnames[varnames]
    names(hunits) <- dnames

    # Check if there are extra columns in the Dfile than the above
    missnames <- sapply(dnames,is.null)
    if (any(missnames)) {
        warning(paste("extra variable names in header:",
                paste('"',names(hnames)[missnames],'"',sep="",collapse=","), "in file",file))
        dnames[missnames] <- varnames[is.na(match(varnames,names(hnames)))]
    }
    dnames <- unlist(dnames)
    
    # columns in D file, in order
    col.names <- c(strnames, dnames)

    # read as character data
    d <- read.table(file=file, colClasses="character",fill=TRUE,
        col.names=col.names,row.names=NULL, stringsAsFactors=FALSE)

    # extract data rows
    d <- d[drows,]

    # convert utc columns to utime
    utc <- utime(paste(d[,"utc.date"],d[,"utc.time"]),
        in.format="%y%m%d%H%M%OS",time.zone="UTC")

    # missing values for numeric columns
    na_vals <- c(sid=0, p=9999, temp=99, rh=999, wdir=999, wspd=999,
        dz=99, lon=999, lat=99, gp.alt=99999,
        wsat=999, rh1=999, rh2=999, ssat=999, werr=99, gps.alt=99999)

    # numeric variables are those not in strnames or utcnames
    numnames <- dnames[is.na(match(dnames,c(strnames,utcnames)))]

    # convert data columns to numeric
    d2 <- lapply(numnames,function(n,x,na_vals)
        {
            # browser()
            x <- type.convert(x[,n])
            naval <- na_vals[n]
            if (!is.null(naval)) x[!is.na(x) & x==naval] <- NA
            x
        },
        x=d,na_vals=na_vals)

    names(d2) <- numnames

    # d <- data.frame(d[,strnames],utc=utc,d2)
    # sta <- d[,"sta"]
    d <- data.frame(d[,strnames],d2)
    units <- rep("",length(colnames(d)))
    mu <- match(names(hunits),colnames(d),nomatch=0)
    units[mu] <- hunits[mu!=0]
    units <- sub("(","",units,fixed=TRUE)
    units <- sub(")","",units,fixed=TRUE)
    d <- dat(nts(d,utc,units=units))

    if (sta_clean) {
        sta <- unlist(d@data[,"sta"])
        ok <- substring(sta,1,1) == "S"
        d <- d[ok,]
        sta <- sta[ok]

        # If first digit of sta is not zero, set PTU values to NA
        ok  <- grepl("S0",sta,fixed=TRUE)
        ptuqc <- c("p","temp","rh","rh1","rh2")
        d@data[!ok,match(ptuqc,colnames(d))] <- NA_real_

        # If second digit of sta is not zero, set wind/gps values to NA
        ok <- grepl("S00",sta,fixed=TRUE) | grepl("S10",sta,fixed=TRUE)
        windqc <- c("wdir","wspd","dz","lon","lat","gp.alt","wsat","ssat","werr","gps.alt")
        d@data[!ok,match(windqc,colnames(d))] <- NA_real_
    }

    # check for unique sid, save it as attribute
    sids <- unique(as.vector(d@data[,"sid"]))
    if (any(is.na(sids))) sids <- sids[!is.na(sids)]
    attr(d,"sid") <- sids
    if (length(sids) > 1)
        warning(paste0("Multiple sonde ids (",paste(sids,collapse=","),
            ") found in ",file))
    d <- d[,is.na(match(colnames(d),"sid"))]
    d
}

