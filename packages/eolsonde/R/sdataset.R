# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsonde" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

sdataset <- function(dataset=NULL,
    path=file.path(Sys.getenv("SONDE_ROOT"),"projects",Sys.getenv("PROJECT"),Sys.getenv("PLATFORM"),"data"))
{

    if (!exists(".projectEnv"))
        .projectEnv <<- new.env(parent=emptyenv())

    if (is.null(dataset)) {
        datasets <- list.dirs(path,full.names=FALSE,recursive=FALSE)

        if (length(datasets) < 1) stop(paste("No directories found in",path))
        if (length(datasets) > 1) {
            ip <- character(0)
            while(length(ip) == 0) {
                cat("Choose a dataset by number (0 to quit):\n")
                cat(paste0(paste(1:length(datasets),datasets,sep=": "),"\n",collapse=""))
                ip <- as.integer(readLines(n=1))
                if (is.na(ip) || ip == 0) return(invisible(NULL))
                if (ip > length(datasets)) cat("Invalid entry\n")
            }
            dataset <- datasets[ip]
        }
        else dataset <- datasets
    }

    Sys.setenv(DATASET=dataset)

    dataset
}
