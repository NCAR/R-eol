# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsonde" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

readDFile <- function (file, checkStatus=dpar("checkSondeStatus"), sondeRecords=dpar("sondeRecords"))
{
    # if (file.access(file,4) != 0)
    #     stop(paste("Error:",file,"does not exist or is not readable"))

    # grab first two columns, which look like:
    #   AVAPS-T04 COM
    #   AVAPS-D04 ???
    cols <- scan(file=file,what=list(avaps="",type=""),flush=TRUE,quiet=TRUE)

    # AVAPS-T01
    txt <- grepl("AVAPS-T",cols$avaps,fixed=TRUE)

    # AVAPS-Txx COM lines
    com <- txt & grepl("COM",cols$type,fixed=TRUE)

    # AVAPS-Dxx lines are data
    drows <- grepl("AVAPS-D",cols$avaps,fixed=TRUE)

    con <- file(file,open="r")

    header <- ""
    trailer <- ""

    # header is all txt lines before data
    dn <- (1:length(drows))[drows]  # line numbers of data lines
    if (dn[1] > 1)
        header <- readLines(con,n=dn[1]-1)

    # read rest of file. R man page warns against using seek on Windows.
    trailer <- readLines(con,n=-1)
    close(con)

    ntrail <- length(drows) - dn[length(dn)]

    if (ntrail > 0) {
        lt <- length(trailer)
        trailer <- trailer[(lt-ntrail+1):lt]
    }

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
    # type:  Spw:  p=0 good PTU checksum, p=1 bad PTU checksum
    #             w=0 good wind checksum, w=1 bad wind checksum

    strnames <- c("avaps","status")
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
    sdng <- read.table(file=file, colClasses="character",fill=TRUE,
        col.names=col.names,row.names=NULL, stringsAsFactors=FALSE,
        check.names=FALSE)

    # extract data rows
    sdng <- sdng[drows,]

    # discard times of all zeroes
    badtime <- sdng[,"UTCDate"] == "000000"
    if (any(badtime))
        sdng <- sdng[!badtime,]

    # convert utc columns to utime
    utc <- utime(paste(sdng[,"UTCDate"],sdng[,"UTCTime"]),
        in.format="%y%m%d%H%M%OS",time.zone="UTC")

    # missing values for numeric columns
    na_vals <- list(SID=0, P=9999, T=99, RH=999, Wdir=999, Wspd=999,
        "Vz"=99, lon=999, lat=99, Alt_gp=c(99999,9999),
        wsat=999, RH1=999, RH2=999, ssat=999, Werr=99, Alt_gps=99999)

    # numeric variables are those not in strnames or utcnames
    numnames <- dnames[is.na(match(dnames,c(strnames,utcnames)))]

    # convert data columns to numeric
    numdata <- sapply(numnames,function(n)
        {
            # if (n == "Alt_gp") browser()
            x <- type.convert(sdng[,n],as.is=TRUE)
            # From help page for type.convert:
            # Vectors which are entirely missing values are converted to
            # logical, since ‘NA’ is primarily logical.
            if (is.logical(x)) x <- as.numeric(x)
            nas <- na_vals[[n]]
            if (!is.null(nas)) x[x %in% nas] <- NA_real_
            x
        })

    # browser()

    # extract digits NN in AVAPS-DNN, presumably the channel number
    avaps <- as.integer(substring(sdng[,"avaps"],8))

    # type of record, A= from aircraft data system, P=pre-launch, S=sounding
    rectypes <- c("A","P","S")

    # zero-based index into rectypes of the type of each sounding record,
    # matching the first character in sdng[,"status"] with rectypes
    rectype <- match(substring(sdng[,"status"],1,1),rectypes) - 1

    # must have two digits, 0 or 1, after leading character
    # PTU and GPS status
    status <- match(substring(sdng[,"status"],2,3),c("00","01","10","11")) - 1

    rectype[is.na(status)] <- NA

    sdng <- matrix(c(avaps,status + rectype * 10,as.vector(numdata)),
        ncol=ncol(numdata)+2,dimnames=list(NULL,c(strnames,numnames)))

    units <- rep("",length(colnames(sdng)))
    mu <- match(names(hunits),colnames(sdng),nomatch=0)
    units[mu] <- hunits[mu!=0]
    units <- sub("(","",units,fixed=TRUE)
    units <- sub(")","",units,fixed=TRUE)
    sdng <- dat(nts(sdng,utc,units=units))

    if (!is.null(checkStatus) && checkStatus) {
        # zero-based indices into rectype for the types that are to be kept
        matchtype <- 1:length(rectypes) - 1
        if (!is.null(dpar("sondeRecords")) && !is.na(dpar("sondeRecords")))
            matchtype <- match(dpar("sondeRecords"),rectypes) - 1

        ok <- !is.na(rectype) & !is.na(match(rectype,matchtype)) & !is.na(avaps)

        sdng <- sdng[ok,]
        status <- status[ok]

        # If first digit of status is not zero, set PTU values to NA
        badptu  <- status > 1
        ptunames <- c("P","T","RH","RH1","RH2")
        sdng@data[badptu,match(ptunames,colnames(sdng))] <- NA_real_

        # If second digit of status is not zero, set wind and GPS values to NA
        badgps  <- (status %% 2) != 0
        windnames <- c("Wdir","Wspd","Vz","lon","lat","Alt_gp","wsat","ssat","Werr","Alt_gps")
        sdng@data[badgps,match(windnames,colnames(sdng))] <- NA_real_
    }

    # check for unique sid, save it as attribute
    sids <- unique(as.vector(sdng@data[,"SID"]))
    if (any(is.na(sids))) sids <- sids[!is.na(sids)]
    attr(sdng,"SID") <- sids
    if (length(sids) > 1)
        warning(paste0("Multiple sonde ids (",paste(sids,collapse=","),
            ") found in ",file))
    sdng <- sdng[,is.na(match(colnames(sdng),"SID"))]

    # put in time order
    di <- order(positions(sdng))
    if (any(diff(di) < 0))
        sdng <- sdng[di,]

    attr(sdng,"header") <- header
    attr(sdng,"trailer") <- trailer
    sdng
}
