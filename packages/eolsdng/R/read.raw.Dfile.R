# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsdng" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

read.raw.Dfile <-
function (file=stop("'file' must be specified")) 
{
  if (length(system(paste("ls", file), TRUE)) < 1)
    stop(paste("THE FILE", file, "DOES NOT EXIST"))

  #---------------------------------------------------------------
  ## get # of lines in filename
  #---------------------------------------------------------------
  nn     = system(paste("wc -l", file), TRUE)
  nfline = as.numeric(substring(nn, 1, nchar(nn)-nchar(file)))

  #-----------------------------------------------------------------------
  ##  get rid of header and tail (# of header = 6, # of tail = 19)
  ##  read body in as matrix, put all missing values as NA
  #-----------------------------------------------------------------------
  nl = nfline - 25
  na.strs = c("9999.00","99.00","999.00","999.000000","99.000000","99999.00")
  #d = matrix(scan(filename, what = "", skip = 6, nline = nl, na.strings = na.strs), byrow = T, ncol = 20)
  d = read.table(file, skip = 6, nrows = nl, na.strings = na.strs)

  #----------------------------
  ## name variables in the data
  #----------------------------
  ava      = d$V1
  sta      = d$V2
  sid      = d$V3
  utc.date = d$V4
  utc.time = d$V5
  press    = d$V6
  temp     = d$V7
  rhum     = d$V8
  wdir     = d$V9
  wspd     = d$V10
  dz       = d$V11 ## vertical velocity
  lon      = d$V12
  lat      = d$V13
  gp.alt   = d$V14
  wsat     = d$V15 ##
  rh1      = d$V16
  rh2      = d$V17
  ssat     = d$V18 ##
  werr     = d$V19 ## wind error
  gps.alt  = d$V20

  newdata = data.frame(ava, sta, sid, utc.date, utc.time, p=press, temp, rh=rhum, wdir, wspd, dz, lon, lat, gp.alt, wsat, rh1, rh2, ssat, werr, gps.alt)
  return(newdata)
}

readDFile <- function (file=stop("'file' must be specified")) 
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
    hnames <- list("Sonde"="sid",
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
    strnames <- c("ava","sta")
    utcnames <- c("utc.date","utc.time")

    # names of the data columns in the D file, in order
    dnames <- hnames[varnames]

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

    data.frame(d[,strnames],utc=utc,d2)
}

