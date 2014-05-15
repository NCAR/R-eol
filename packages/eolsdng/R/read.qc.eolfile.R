# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsdng" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

read.qc.eolfile <-
function(file=stop("'file' must be specified"), nskip = 14)
{
  #---------------------------------------------------------
  # Read in the data, skip the header, and name the variable
  #---------------------------------------------------------
  if (length(system(paste("ls", file), TRUE)) < 1)
    stop(paste("THE FILE", file, "DOES NOT EXIST"))

  na.strs = c("-999.00", "-999.000000")
  d = read.table(file, skip = nskip, na.strings=na.strs)
  time   = d$V1
  utc.hh = d$V2
  utc.mm = d$V3
  utc.ss = d$V4
  press  = d$V5
  temp   = d$V6
  dewpt  = d$V7
  rhum   = d$V8
  uwind  = d$V9
  vwind  = d$V10
  wspd   = d$V11
  wdir   = d$V12
  dz     = d$V13
  gp.alt = d$V14
  lon    = d$V15
  lat    = d$V16
  gps.alt= d$V17

  newdata = data.frame(time, utc.hh, utc.mm, utc.ss, p = press, temp, dewpt, rh = rhum, u = uwind, v = vwind, wspd, wdir, dz, gp.alt, lon, lat, gps.alt)
  return(newdata)
}

readQCFile <-
function(file=stop("'file' must be specified"))
{
    hdrw1 <- scan(file=file,what=list(w1=""),flush=TRUE,quiet=TRUE,nlines=30)

    # Find line containing launch time
    # UTC Launch Time (y,m,d,h,m,s):             2008, 08, 15, 17:38:56
    lline <- hdrw1$w1 == "UTC"
    lline <- seq(along=lline)[lline][1]

    # Read 10 lines after UTC, just in case there could eventually be more than one
    utchdrs <- scan(file=file,what="",sep="\n",skip=lline-1,quiet=TRUE,nlines=10)

    # Find "UTC Launch Time", extract year,month,day, HH:MM:SS
    mx <- regexec("^UTC Launch Time[^:]+:[[:space:]]*([0-9]+), *([0-9]+), *([0-9]+), *([0-9]+):([0-9]+):([0-9]+)",utchdrs)

    # return of -1 indicates no match. Exclude those lines
    lline <- sapply(mx,function(x)x[1]!=-1)

    launchutc <- utime(paste(unlist(regmatches(utchdrs[lline],mx[lline]))[-1],collapse=" "),in.format="%Y%m%d%H%M%S",time.zone="UTC")

    # find line containing '/'
    lhdr <- hdrw1$w1 == "/"

    # line number starting with /
    lhdr <- seq(along=lhdr)[lhdr][1]

    # read header line containing variable names
    varnames <- unlist(read.table(file=file,skip=lhdr,nrows=1,row.names=NULL,stringsAsFactors=FALSE)[,-(1:4)])

    # mapping of header strings to returned variable names
    hnames <- list(
        Press="p",
        Temp="temp",
        Dewpt="dewpt",
        RH="rh",
        Uwind="u",
        Vwind="v",
        Wspd="wspd",
        Dir="wdir",
        dZ="dz",
        GeoPoAlt="gp.alt",
        Lon="lon",
        Lat="lat",
        GPSAlt="gps.alt")

    # character columns
    utcnames <- c("time","hh","mm","ss")

    # names of the data columns in the file, in order
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
    col.names <- c(utcnames, dnames)


    # read data into numeric values
    d <- read.table(file=file, skip=lhdr+3,
        col.names=col.names,row.names=NULL, na.strings=c("-999.00","-999.000000"))

    sod <- d[,"hh"] * 3600 + d[,"mm"] * 60 + d[,"ss"]
    t0day <- floor(as.numeric(launchutc)/86400) * 86400
    tx <- t0day + sod
    # browser()
    # just in case a day rollover happened between the
    # first record and the launch time
    if (tx[1] > launchutc + 43200) tx[1] <- tx[1] - 86400

    # check for day rollovers:  23:59 to 00:00 will be a backward
    # jump in time by close to 86400 seconds. Assume anything more
    # than 43200 is a rollover
    mx <- diff(tx) < -43200
    if (any(mx)) {
        mx <- seq(along=mx)[mx][1] + 1
        tx[mx:length(tx)] <- tx[mx:length(tx)] + 86400
    }
    data.frame(utc=utime(tx),d[,c("time",dnames)])
}
