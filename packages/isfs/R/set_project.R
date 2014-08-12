# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

set_project <- function(root="ISFF",project=NULL)
{
    curproj <- Sys.getenv("PROJECT",unset=NA)

    rootpath <- Sys.getenv(root,unset=NA)
    if (is.na(rootpath)) stop(paste(root,"environment variable not found"))

    projpath <- file.path(rootpath,"projects")
    projects <- list.dirs(projpath,recursive=FALSE,full.names=FALSE)

    if (is.null(project)) {

        if (length(projects) < 1) stop(paste("No directories found in",projpath))
        if (length(projects) > 1) {
            ip <- character(0)
            while(length(ip) == 0) {
                cat("Choose a project by number (0 to quit):\n")
                cat(paste0(paste(1:length(projects),projects,sep=": "),"\n",collapse=""))
                ip <- as.integer(readLines(n=1))
                if (is.na(ip) || ip == 0) return()
                if (ip > length(projects)) cat("Invalid entry\n")
            }
            project <- projects[ip]
        }
        else project <- projects
    }
    
    projdata <- file.path(projpath,project,"ISFF","R",".RData")
    if (!file.exists(projdata)) {
        projdata <- file.path(projpath,project,"R",".RData")
    }

    curpos <- NULL
    if (!is.na(curproj)) {
        sl <- search()
        curdata <- file.path(projpath,curproj,"ISFF","R",".RData")
        pos <- grepl(curdata,sl)
        if (!any(pos)) {
            curdata <- file.path(projpath,curproj,"R",".RData")
            pos <- grepl(curdata,sl)
        }
        if (any(pos)) curpos <- base::seq(along=pos)[pos][1]
    }

    Sys.setenv(PROJECT=project)
    if (file.exists(projdata)) {
        if (curproj == project && !is.null(curpos)) sync(pos=curpos)
        else {
            if (!is.null(curpos)) {
                cat("detaching",curdata,"pos=",curpos,"\n")
                detach(pos=curpos)
            }
            else curpos <- 2L
            cat("attaching",projdata,"\n")
            attach(projdata,pos=curpos)

            if (existsFunction("project_init")) {
                cat("doing: project_init()\n")
                project_init()
            }
            else if (existsFunction("project.init")) {
                cat("doing: project.init()\n")
                project.init()
            }
        }
    }
    else if (!is.null(curpos)) {
        cat("detaching",curdata,"pos=",curpos,"\n")
        detach(pos=curpos)
    }
    project
}
