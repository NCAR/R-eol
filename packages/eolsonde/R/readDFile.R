# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsonde" package for the R software environment.
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
        "Sonde"="SID",
        "UTCDate"="UTCDate",
        "UTCTime"="UTCTime",
        "AirPress"="P",
        "AirTemp"="T",
        "RelHumid"="RH",
        "WindDir"="Wdir",
        "WindSpd"="Wspd",
        "VertVeloc"="Vz",
        "GPSLongitude"="lon",
        "GPSLatitude"="lat",
        "GeopotenAltitude"="Alt_gp",
        "GPSWnd"="wsat",
        "SondeRH1"="RH1",
        "SondeRH2"="RH2",
        "GPSSnd"="ssat",
        "WindError"="Werr",
        "GPSAltitude"="Alt_gps")

    # character columns
    # sta:  Spw:  p=0 good PTU checksum, p=1 bad PTU checksum
    # sta:  Spw:  w=0 good wind checksum, w=1 bad wind checksum
    strnames <- c("ava","sta")
    utcnames <- c("UTCDate","UTCTime")

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
        col.names=col.names,row.names=NULL, stringsAsFactors=FALSE,
        check.names=FALSE)

    # extract data rows
    d <- d[drows,]

    # discard times of all zeroes
    badtime <- d[,"UTCDate"] == "000000"
    if (any(badtime))
        d <- d[!badtime,]

    # convert utc columns to utime
    utc <- utime(paste(d[,"UTCDate"],d[,"UTCTime"]),
        in.format="%y%m%d%H%M%OS",time.zone="UTC")

    # missing values for numeric columns
    na_vals <- list(SID=0, P=9999, T=99, RH=999, Wdir=999, Wspd=999,
        "Vz"=99, lon=999, lat=99, Alt_gp=c(99999,9999),
        wsat=999, RH1=999, RH2=999, ssat=999, Werr=99, Alt_gps=99999)

    # numeric variables are those not in strnames or utcnames
    numnames <- dnames[is.na(match(dnames,c(strnames,utcnames)))]

    # convert data columns to numeric
    d2 <- lapply(numnames,function(n,x,na_vals)
        {
            # if (n == "Alt_gp") browser()
            xx <- type.convert(x[,n],as.is=TRUE)
            nas <- na_vals[[n]]
            if (!is.null(nas)) xx[xx %in% nas] <- NA_real_
            xx
        },
        x=d,na_vals=na_vals)

    names(d2) <- numnames

    # d <- data.frame(d[,strnames],utc=utc,d2)
    # sta <- d[,"sta"]
    d <- data.frame(d[,strnames],d2,check.names=FALSE)
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
        ptuqc <- c("P","T","RH","RH1","RH2")
        d@data[!ok,match(ptuqc,colnames(d))] <- NA_real_

        # If second digit of sta is not zero, set wind/gps values to NA
        ok <- grepl("S00",sta,fixed=TRUE) | grepl("S10",sta,fixed=TRUE)
        windqc <- c("Wdir","Wspd","Vz","lon","lat","Alt_gp","wsat","ssat","Werr","Alt_gps")
        d@data[!ok,match(windqc,colnames(d))] <- NA_real_
    }

    # check for unique sid, save it as attribute
    sids <- unique(as.vector(d@data[,"SID"]))
    if (any(is.na(sids))) sids <- sids[!is.na(sids)]
    attr(d,"SID") <- sids
    if (length(sids) > 1)
        warning(paste0("Multiple sonde ids (",paste(sids,collapse=","),
            ") found in ",file))
    d <- d[,is.na(match(colnames(d),"SID"))]

    # put in time order
    di <- order(positions(d))
    if (any(diff(di) < 0))
        d <- d[di,]

    d
}
