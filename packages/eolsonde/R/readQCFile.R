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
    hdrw1 <- scan(file=file, what=list(w1=""), flush=TRUE, quiet=TRUE, nlines=50,
        blank.lines.skip=FALSE, fill=TRUE)

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

    # look for line starting with "----"
    lhdr <- seq(along=hdrw1$w1)[regexec("^----",hdrw1$w1) > 0]
    if (length(lhdr) != 1) {
        warning(paste("Cannot find ---- line in",file))
        return(NULL)
    }

    # save all of header, assume 3 lines after line wth '/'
    header <- scan(file=file,what="",sep='\n',nlines=lhdr,quiet=TRUE)

    # read header line containing variable names
    varnames <- unlist(read.table(file=file,skip=lhdr-3,nrows=1,row.names=NULL,stringsAsFactors=FALSE))

    units <- unlist(read.table(file=file,skip=lhdr-2,nrows=1,row.names=NULL,stringsAsFactors=FALSE))

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
        dZ="Vz",
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
    sdng <- read.table(file=file, skip=lhdr,
        col.names=dnames,row.names=NULL, na.strings=c("-999.00","-999.00000","-999.000000"),
        check.names=FALSE)

    sod <- sdng[,utcnames[1]] * 3600 + sdng[,utcnames[2]] * 60 + sdng[,utcnames[3]]
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

    sdng <- as.matrix(sdng[,dnames])

    if (FALSE) {
        # if any columns are logical, convert to numeric
        ldata <- sapply(dnames,function(n){is.logical(sdng[1,n])})

        if (any(ldata)) {
            sapply((1:length(ldata))[ldata],function(i)
                {
                    sdng[,i] <<- as.numeric(sdng[,i])
                    NULL
                }
            )
            NULL
        }
    }
    
    sdng <- dat(nts(sdng,utime(tx),units=units))

    # put in time order
    di <- order(positions(sdng))
    if (any(diff(di) < 0))
        sdng <- sdng[di,]
    attr(sdng,"header") <- header
    sdng
}

writeQCFile <- function(sdng,file)
{
    if (missing(file)) stop("file argument must be specified")

    if (is.null(attr(sdng,"header"))) stop("sdng has no header attribute")

    if (grepl(".gz$",file))
        con <- gzfile(file,open="w")
    else if (grepl(".bz2$",file))
        con <- bzfile(file,open="w")
    else
        con <- file(file,open="w")

    writeLines(attr(sdng,"header"),con)

    for (i in 1:nrow(sdng)) {
        d <- sdng@data[i,]
        d[is.na(d)] <- -999
        names(d) <- colnames(sdng)
        hms <- format(positions(sdng)[i],format="%H %M %OS2",time.zone="UTC")

        line <- sprintf("%7.2f %s %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %8.2f %9.2f %11.6f %12.6f %8.2f",
            d["toff"],hms,d["P"],d["T"],d["Dewpt"],d["RH"],
            d["U"],d["V"],d["Wspd"],d["Wdir"],d["Vz"],
            d["Alt_gp"],d["lon"],d["lat"],d["Alt_gps"])
        writeLines(line,con,sep="\n")
    }
    close(con)
}
