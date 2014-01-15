# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:

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
    #   f       an arbitrary function which can perform any extra settings
    if (is.null(datasets)) {
        if (!exists(".datasets")) stop(".datasets object for project not found")
        datasets <- .datasets
    }
    current <- "unknown"
    if(exists("dataset.which",envir=.projectEnv)) current <- get("dataset.which",envir=.projectEnv)

    # discard datasets that are not enabled.
    datasets <- datasets[sapply(datasets,function(x) { x$enable })]

    if (missing(which)) {
        if (verbose) {
            cat(paste("************************************************\n",
                'Choose one of the following by number or name: dataset(1), dataset("name"):\n',
                paste(
                    sapply(1:length(datasets),function(i) sprintf("%2d",i)),
                    sapply(names(datasets),function(s) sprintf("%-12s",paste("\"",s,"\"",sep=""))),
                    sapply(datasets,function(s) sprintf("%s",s$desc)),
                    sep=" ",collapse="\n"),"\n",
                "************************************************\n",sep=""),"\n")
        }
        return(current)
    }

    if (!is.character(which)) which <- names(datasets)[which]
    dset <- datasets[[which]]
    if (is.null(dset)) stop(paste("dataset",which,"not found"))

    # browser()

    dpar(datacoords=dset$datacoords,lenfile=dset$lenfile)
    Sys.setenv(QC_DIR=dset$qcdir)
    Sys.setenv(SONIC_DIR=dset$sonicdir)

    ncf <- Sys.setenv(NETCDF_FILE=dset$ncf)
    ncd <- Sys.setenv(NETCDF_DIR=dset$ncd)

    dset$f()
    if (ncf != dset$ncf || ncd != dset$ncd) clear.cache()

    assign("dataset.which",which,envir=.projectEnv)

    if (verbose) {
        cat(paste("************************************************\n"))
        cat(paste("current dataset is \"", get("dataset.which",envir=.projectEnv),sep="","\"\n"))

        cat(paste("netcdf file=",Sys.getenv("NETCDF_FILE"),"\n",
                "netcdf dir=",Sys.getenv("NETCDF_DIR"),"\n",
                "QC_DIR=",Sys.getenv("QC_DIR"), "\n",
                "SONIC_DIR=",Sys.getenv("SONIC_DIR"),"\n",
                "dpar(\"datacoords\")=\"", dpar("datacoords"),"\" (coordinates of netcdf data)\n", sep=""))
        cat(paste("************************************************\n"))
    }

    which
}

sync <- function(pos)
{
    # There may be an .projectEnv environment object in the
    # $ISFF/projects/$PROJECT/ISFF/R/.RData. Save it before detaching/re-attaching
    projectEnv <- NULL
    sl <- search()
    if (missing(pos)) {
        projdata <- paste(Sys.getenv("ISFF"),"projects",Sys.getenv("PROJECT"),
            "ISFF/R",sep="/")
        pos <- grepl(projdata,sl)
        if (!any(pos)) stop(paste("pos is missing and can't find",projdata))
        pos <- base::seq(along=pos)[pos][1]
        if (exists(".projectEnv",where=pos)) projectEnv <- get(".projectEnv",pos=pos)
    }

    rd <- sl[pos]

    if (grepl(".RData",rd)) {
        rd <- sub("^file:","",rd)
        detach(pos=pos)
        attach(rd,pos=pos)
        if (!is.null(projectEnv)) assign(".projectEnv",projectEnv,pos=pos)
    }
    else stop(paste("attached object at position",pos,"is not an .RData:",rd))

    invisible(rd)
}
