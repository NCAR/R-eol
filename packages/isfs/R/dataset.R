# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

find_datasets <- function(
    path=file.path(Sys.getenv("ISFF"),"projects",Sys.getenv("PROJECT"),"ISFF"),
    pattern="^netcdf")
{

    # remove duplicate .projectEnv from .GlobalEnv
    if (exists(".projectEnv",where=1,inherits=FALSE) && exists(".projectEnv",where=2,inherits=TRUE))
        rm(".projectEnv",pos=1,inherits=FALSE)

    if (!exists(".projectEnv"))
        stop(".projectEnv not found. Should be located in project .RData")

    if (!file.exists(path))
        warning(paste0("directory $ISFF/projects/$PROJECT/ISFF=",path," does not exist"))

    ncds <- list.files(path,pattern,include.dirs=TRUE)

    if (length(ncds) == 0)
        warning(paste0("no directories found on",path," matching pattern: \"",pattern,"\""))

    datasets <- list()

    for (ncd in ncds) {

        ncpath <- file.path(path,ncd)
        ncdf <- list.files(ncpath,"\\.nc$")
        if (length(ncdf) == 0) ncdf <- list.files(ncpath,"\\.cdf$")

        if (length(ncdf) > 0) {

            ncpat <- sub("(_?)[12][9012][0-9][0-9]","\\1%Y",ncdf)

            # If couldn't match a 4 digit year, try 2 digits,
            # assume only in the 80 or 90's
            if (all(ncpat == ncdf))  
                ncpat <- sub("(_?)[89][0-9]","\\1%y",ncdf)

            ncpat <- sub("(%[yY][^01]*)[01][0-9]","\\1%m",ncpat)
            ncpat <- sub("(%m[^0-3]*)[0-3][0-9]","\\1%d",ncpat)
            ncpat <- sub("(%d[^0-2]*)[0-2][0-9]","\\1%H",ncpat)
            ncpat <- sub("(%H[^0-5]*)[0-5][0-9]","\\1%M",ncpat)
            ncpat <- sub("(%M[^0-5]*)[0-5][0-9]","\\1%S",ncpat)

            ncpat <- unique(ncpat)
            if (length(ncpat) > 1)
                warning(paste("Cannot determine a unique file name pattern from file names:",
                    paste(ncpat,collapse=", ")))

            ix <- seq(from=1,to=length(ncdf),by=ceiling(length(ncdf)/10))

            con <- netcdf(dir=ncpath,file=ncdf[ix])
            # read global attributes
            attrs <- readnc(con)
            close(con)

            dname <- sub(pattern,"",ncd)    # remove netcdf
            dname <- sub("^_+","",dname)    # remove leading underscores
            dname <- sub("^\\.+","",dname)  # remove periods (why?)
            if (nchar(dname) == 0) dname <- "default"

            if ("dataset" %in% names(attrs)) {
                if (attrs$dataset != dname) warning(paste0("files in directory ",
                        ncd," have global attribute \"dataset\" of \"",attrs$dataset,
                        "\", which does not match the directory suffix. Setting datasetname to \"",attrs$dataset,"\""))
                dname <- attrs$dataset
            }

            desc <- ""
            if ("dataset_description" %in% names(attrs))
                desc <- attrs$dataset_description

            anames <- c("h2o_flux_corrected", "co2_flux_corrected", "gsoil_philip_corrected")

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

            datasets[[dname]] <- c(list(enable=TRUE,desc=desc,
                calpath=calpaths, ncd=ncpath,ncf=ncpat,datacoords=datacoords),
                int_attrs)
        }
    }
    assign(".datasets",datasets,envir=.projectEnv)
    datasets
}

dataset <- function(which,verbose=F,datasets=NULL)
{
    
    # select one from a collection of datasets.
    # The list of available datasets can be passed as the datasets argument.
    # If that is NULL, an object ".datasets" is used.
    # An entry in the datasets list should have these elements:
    #   enabled  TRUE/FALSE
    #   desc    character string describing the dataset
    #   ncf     character string containing the NetCDF file name format, like isfs_%Y%m%d.nc
    #   ncd     directory path of NetCDF file names
    #   lenfile length in seconds of the NetCDF files
    #   datacoords  Wind coordinates of data: "geo", "instrument"
    #   qcdir   directory of cal files, which is copied to the environment variable QC_DIR
    #   sonicdir directory of sonic anemometer cal files, which is copied to the
    #           environment variable SONIC_DIR

    # remove duplicate .projectEnv from .GlobalEnv
    if (exists(".projectEnv",where=1,inherits=FALSE) && exists(".projectEnv",where=2,inherits=TRUE))
        rm(".projectEnv",pos=1,inherits=FALSE)

    if (!exists(".projectEnv"))
        stop(".projectEnv not found. Should be located in project .RData")

    if (is.null(datasets)) {
        if (exists(".datasets",envir=.projectEnv))
            datasets <- get(".datasets",envir=.projectEnv)
        else if (exists(".datasets"))
            datasets <- get(".datasets")
    }

    current <- "unknown"
    if(exists("dataset.which",envir=.projectEnv))
        current <- get("dataset.which",.projectEnv)

    # discard datasets that are not enabled.
    datasets <- datasets[sapply(datasets,function(x) { ifelse (is.null(x$enable),FALSE,x$enable) })]

    if (missing(which)) {
        if (verbose) {
            cat(paste("************************************************\n",
                'Choose one of the following by number or name: dataset(1), dataset("name"):\n',
                paste(
                    sapply(1:length(datasets),function(i) sprintf("%2d",i)),
                    sapply(names(datasets),function(s) sprintf("%-12s",paste("\"",s,"\"",sep=""))),
                    sapply(datasets,function(s) sprintf("%s",s$desc)),
                    sep=" ",collapse="\n"),"\n",
                "************************************************\n",
                "current dataset is \"",current,"\"\n",sep=""))
        }
        return(current)
    }

    if (!is.character(which)) which <- names(datasets)[which]
    dset <- datasets[[which]]
    if (is.null(dset)) stop(paste("dataset",which,"not found"))

    # browser()

    if (!is.null(dset$lenfile)) dpar(lenfile=dset$lenfile)
    dpar(datacoords=dset$datacoords)

    if (!is.null(dset$qcdir))
        Sys.setenv(QC_DIR=dset$qcdir)

    if (!is.null(dset$sonicdir))
        Sys.setenv(SONIC_DIR=dset$sonicdir)

    ncf <- Sys.setenv(NETCDF_FILE=dset$ncf)
    ncd <- Sys.setenv(NETCDF_DIR=dset$ncd)

    Sys.setenv(DATASET=which)

    anames <- c("h2o_flux_corrected", "co2_flux_corrected", "gsoil_philip_corrected")
    for (a in anames) {
        if (a %in% names(dset)) dpar(dset[a])
        else dpar(sapply(a,function(x)NULL))
    }

    if (!is.null(dset[["f"]])) dset$f()
    if (ncf != dset$ncf || ncd != dset$ncd) clear_cache()

    assign("dataset.which",which,envir=.projectEnv)

    if (verbose) {
        cat(paste("************************************************\n"))
        cat(paste("current dataset is \"", get("dataset.which",envir=.projectEnv),"\"\n",sep=""))

        cat(paste0("NETCDF_FILE=",Sys.getenv("NETCDF_FILE"),"\n",
            "NETCDF_DIR=",Sys.getenv("NETCDF_DIR"),"\n",
            "dpar(\"datacoords\")=\"", dpar("datacoords"),"\" (coordinates of netcdf data)\n",
            ifelse(is.null(dset$calpath),"",paste0("calpath=",dset$calpath,"\n")),
            ifelse(is.null(dset$qcdir),"",paste0("QC_DIR=",dset$qcdir,"\n")),
            ifelse(is.null(dset$sonicdir),"",paste0("SONIC_DIR=",dset$sonicdir,"\n")),
            ifelse(is.null(dset$wind3d_horiz_rotation),"",paste0("wind3d_horiz_rotation=",dset$wind3d_horiz_rotation,"\n")),
            ifelse(is.null(dset$wind3d_tilt_correction),"",paste0("wind3d_tilt_correction=",dset$wind3d_tilt_correction,"\n")),
            ifelse(is.null(dpar("lenfile")),"",paste0("dpar(\"lenfile\")=",dpar("lenfile")," sec\n"))))
        cat(paste("************************************************\n"))
    }

    which
}
