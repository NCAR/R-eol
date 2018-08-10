# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

find_datasets <- function(path=NULL, pattern="^netcdf", verbose=TRUE)
{

    if (is.null(path)) {
        for (root in c("ISFS","ISFF")) {
            rpath <- Sys.getenv(root,unset=NA)
            if (is.na(rpath)) next
            for (plat in c("ISFS","ISFF","")) {
                path <- file.path(rpath,"projects",Sys.getenv("PROJECT"),plat)
                if (file.exists(path)) break
            }
        }
        if (!file.exists(path))
            warning(paste0("directory $ISF[SF]/projects/$PROJECT/ISF[SF]=",path," does not exist"))
    } else if (!file.exists(path))
            warning(paste0("directory ",path," does not exist"))

    if (verbose) cat("Searching for netcdf datasets in",path,"\n")

    dirslev1 <- list.files(path,pattern,include.dirs=TRUE)

    if (length(dirslev1) == 0)
        warning(paste0("no directories found on",path," matching pattern: \"",pattern,"\""))

    dsets <- list()

    for (dirlev1 in dirslev1) {

        # cat("dirlev1=",dirlev1,"\n")
        lev1path <- file.path(path,dirlev1)
        dirslev2 <- list.dirs(path=lev1path,full.names=FALSE)

        for (dirlev2 in dirslev2) {

            # cat("dirlev2=",dirlev2,"\n")

            # file.path adds a trailing slash if second arg is ""
            if (dirlev2 == "") lev2path <- lev1path
            else lev2path <- file.path(lev1path,dirlev2)

            ncdf <- list.files(lev2path,"\\.nc$")
            if (length(ncdf) == 0) ncdf <- list.files(lev2path,"\\.cdf$")

            if (length(ncdf) > 0) {

                # first try to match 4 digit year, looking like 20xx or 1[3-9]xx
                # Don't try 10xx, 11xx, or 12xx since that is most likely
                # month and day.
                ncpat <- sub("(20[0-9][0-9])|(1[3-9][0-9][0-9])","%Y",ncdf)

                # If not all file names match, try a 2 digit year starting with 8 or 9
                if (!all(grepl("%Y",ncpat,fixed=TRUE))) {
                    ncpat <- sub("[8-9][0-9]","%y",ncdf)

                    # If not all file names match, try any leading 2 digit year
                    if (!all(grepl("%y",ncpat,fixed=TRUE)))
                        ncpat <- sub("[0-9][0-9]","%y",ncdf)
                }

                ncpat <- sub("(%[yY][^01]*)[01][0-9]","\\1%m",ncpat)
                ncpat <- sub("(%m[^0-3]*)[0-3][0-9]","\\1%d",ncpat)
                ncpat <- sub("(%d[^0-2]*)[0-2][0-9]","\\1%H",ncpat)
                ncpat <- sub("(%H[^0-5]*)[0-5][0-9]","\\1%M",ncpat)
                ncpat <- sub("(%M[^0-5]*)[0-5][0-9]","\\1%S",ncpat)

                ncpat <- unique(ncpat)

                if (length(ncpat) > 1) {
                    warning(paste("In directory",dirlev2,
                            "cannot determine a unique file name pattern from file names:",
                            paste(ncpat,collapse=", ")))
                    t1 <- utime(0)
                }
                else {
                    t1 <- utime(ncdf[1],in.format=ncpat,time.zone="UTC")
                }

                # parse at most 10 files
                ix <- seq(from=1,to=length(ncdf),by=ceiling(length(ncdf)/10))

                # times are not important when reading global attributes
                con <- netcdf(dir=lev2path,file=sort(ncdf)[ix], start=t1,end=t1)
                # read global attributes
                attrs <- readnc(con)
                close(con)

                dname <- NULL
                dirlev12 <- file.path(dirlev1,dirlev2)
                if ("dataset" %in% names(attrs) && nchar(attrs$dataset) > 0) {
                    dname <- attrs$dataset
                    if (verbose) cat(paste0("Found dataset=",dname," in global attributes of files in ",dirlev12,"\n"))
                }
                if (is.null(dname)) {
                    # cat("dirlev12=",dirlev12,",pattern=",pattern,"\n")
                    dname <- sub(pattern,"",dirlev12)
                    if (nchar(dname) == 0 || dname == "/") dname <- catpath
                    # cat("dname=",dname,"\n")

                    dname <- sub("^_+","",dname)    # remove leading underscores
                    dname <- sub("^/+","",dname)    # remove leading slashes
                    dname <- sub("/$","",dname)    # remove trailing slashes
                    if (verbose) cat(paste0("Created dataset=",dname," from path ",dirlev12,"\n"))
                }
                if (dname %in% names(dsets)) {
                    warning(paste0("Duplicate datasets called ", dname,
                            " in ", paste(dirslev1,collapse=",")))
                }

                # cat("dname=",dname,"\n")

                desc <- ""
                if ("dataset_description" %in% names(attrs))
                    desc <- attrs$dataset_description

                anames <- c("h2o_flux_corrected", "co2_flux_corrected", "gsoil_philip_corrected",
                    "file_length_seconds","lambdasoil_fat_corrected")

                int_attrs <- attrs[anames]
                ix <- sapply(int_attrs,is.null)
                # for those attributes not found, explicitly set them to NULL
                int_attrs <- c(int_attrs[!ix],sapply(anames[ix],function(x)NULL))

                calpaths <- ""
                if ("calibration_file_path" %in% names(attrs))
                    calpaths <- gsub(",version=[^:]+","",attrs$calibration_file_path)

                datacoords <- "instrument"

                # check wind3d_horiz_coordinates to see if it contains "geo"
                if ("wind3d_horiz_coordinates" %in% names(attrs))
                    datacoords <- attrs$wind3d_horiz_coordinates
                else if (grepl("geo",dname,ignore.case=TRUE)) datacoords <- "geo"

                if (datacoords == "geographic") datacoords <- "geo"

                # if ("calfile_version" %in% names(attrs))
                # browser()
                
                dsets[[dname]] <- c(list(enable=TRUE,desc=desc,
                    calpath=calpaths, ncd=lev2path,ncf=ncpat,datacoords=datacoords, start=t1),
                    int_attrs)
            }
        }
    }
    assign(".datasets",dsets,envir=.isfsEnv)
    invisible(dsets)
}

datasets <- function(all=FALSE,warn=TRUE)
{

    if (!exists(".datasets",envir=.isfsEnv)) {
        if (warn) warning(".datasets not found on .isfsEnv. Do find_datasets() to create the list")
        dsets <- list()
    }
    else dsets <- get(".datasets",envir=.isfsEnv)

    # discard datasets that are not enabled.
    if (!all && length(dsets) < 0) 
        dsets <- dsets[sapply(dsets,function(x) { ifelse (is.null(x$enable),FALSE,x$enable) })]

    dsets
}

dataset <- function(which,verbose=FALSE)
{
    
    # select one from a collection of datasets.
    # The list of available datasets can be passed as the datasets argument.
    # If that is NULL, an object ".datasets" is used.
    # An entry in the datasets list should have these elements:
    #   desc    character string describing the dataset
    #   ncf     character string containing the NetCDF file name format, like isfs_%Y%m%d.nc
    #   ncd     directory path of NetCDF file names
    #   lenfile length in seconds of the NetCDF files
    #   datacoords  Wind coordinates of data: "geo", "instrument"

    dsets <- isfs::datasets()

    if (missing(which)) {
        if(exists("current_dataset",envir=.isfsEnv))
            current <- get("current_dataset",.isfsEnv)
        else current <- "unknown"

        if (verbose) {
            cat(paste("************************************************\n",
                'Choose one of the following by number or name: dataset(1), dataset("name"):\n',
                paste(
                    sapply(1:length(dsets),function(i) sprintf("%2d",i)),
                    sapply(names(dsets),function(s) sprintf("%-12s",paste("\"",s,"\"",sep=""))),
                    sapply(dsets,function(s) sprintf("%s",s$desc)),
                    sep=" ",collapse="\n"),"\n",
                "************************************************\n",
                "current dataset is \"",current,"\"\n",sep=""))
        }
        return(dsets[current])
    }

    if (is.list(which)) {
        if (length(which) != 0) stop("which should be character or a named list of length 1")
        which <- names(which)
    }
    else if (!is.character(which)) which <- names(dsets)[which]
    dset <- dsets[[which]]
    if (is.null(dset)) stop(paste("dataset",which,"not found"))

    # browser()

    if (!is.null(dset$lenfile)) dpar(lenfile=dset$lenfile)
    dpar(datacoords=dset$datacoords)

    if (!is.null(dset$start) && is.null(dpar("start")))
        dpar(start=dset$start,lenday=1)

    ncf <- Sys.setenv(NETCDF_FILE=dset$ncf)
    ncd <- Sys.setenv(NETCDF_DIR=dset$ncd)

    Sys.setenv(DATASET=which)

    anames <- c("h2o_flux_corrected", "co2_flux_corrected", "gsoil_philip_corrected",
            "lambdasoil_fat_corrected")
    for (a in anames) {
        if (a %in% names(dset)) dpar(dset[a])
        else dpar(sapply(a,function(x)NULL))
    }

    a <- "file_length_seconds"
    if (a %in% names(dset)) dpar(lenfile=dset[[a]])

    # Execute the function f if it exists. Obsolete.
    if (!is.null(dset[["f"]])) dset$f()

    if (ncf != dset$ncf || ncd != dset$ncd) clear_cache()

    assign("current_dataset",which,envir=.isfsEnv)

    if (verbose) {
        cat(paste("************************************************\n"))
        cat(paste("current dataset is \"", get("current_dataset",envir=.isfsEnv),"\"\n",sep=""))

        cat(paste0(
            "NETCDF_FILE=",Sys.getenv("NETCDF_FILE"),"\n",
            "NETCDF_DIR=",Sys.getenv("NETCDF_DIR"),"\n",
            "dpar(\"datacoords\")=\"", dpar("datacoords"),"\" (coordinates of netcdf data)\n",
            ifelse(is.null(dset$calpath),"",paste0("calpath=",dset$calpath,"\n")),
            ifelse(is.null(dset$wind3d_horiz_rotation),"",paste0("wind3d_horiz_rotation=",dset$wind3d_horiz_rotation,"\n")),
            ifelse(is.null(dset$wind3d_tilt_correction),"",paste0("wind3d_tilt_correction=",dset$wind3d_tilt_correction,"\n")),
            ifelse(is.null(dpar("lenfile")),"",paste0("dpar(\"lenfile\")=",dpar("lenfile")," sec\n"))))
        cat(paste("************************************************\n"))
    }

    # return a list of length one, so that it has a name
    invisible(dsets[which])
}
