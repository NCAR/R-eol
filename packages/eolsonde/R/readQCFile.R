# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsonde" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

readQCFile <- function(file)
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
    varnames <- unlist(read.table(file=file,skip=lhdr,nrows=1,row.names=NULL,stringsAsFactors=FALSE))

    units <- unlist(read.table(file=file,skip=lhdr+1,nrows=1,row.names=NULL,stringsAsFactors=FALSE))

    # mapping of header strings to returned variable names
    hnames <- list(
        Time="toff",
        hh="hh",
        mm="mm",
        ss="ss",
        Press="P",
        Temp="T",
        Dewpt="Dewpt",
        RH="RH",
        Uwind="U",
        Vwind="V",
        Wspd="Wspd",
        Dir="Wdir",
        dZ="dZ/dt",
        GeoPoAlt="Alt_gp",
        Lon="lon",
        Lat="lat",
        GPSAlt="Alt_gps")

    # time columns
    utcnames <- units[2:4]
    varnames[2:4] <- utcnames

    # map the names of the data columns in the file
    dnames <- hnames[varnames]
    # Check if there are extra columns in the QC file than the above
    missnames <- sapply(dnames,is.null)
    if (any(missnames)) {
        warning(paste("extra variable names in header:",
                paste('"',names(hnames)[missnames],'"',sep="",collapse=","), "in file",file))
        dnames[missnames] <- varnames[is.na(match(varnames,names(hnames)))]
    }
    dnames <- unlist(dnames)

    # read data into numeric values
    d <- read.table(file=file, skip=lhdr+3,
        col.names=dnames,row.names=NULL, na.strings=c("-999.00","-999.000000"),
        check.names=FALSE)

    sod <- d[,utcnames[1]] * 3600 + d[,utcnames[2]] * 60 + d[,utcnames[3]]
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

    # non-time variables.
    dnames <- dnames[-(2:4)]
    units <- units[-(2:4)]
    
    dat(nts(d[,dnames],utime(tx),units=units))
}
