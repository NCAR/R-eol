# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsonde" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

set_sproject <- function(root="SONDE_ROOT",project=NULL,platform=NULL)
{
    curproj <- Sys.getenv("PROJECT",unset=NA)
    curplat <- Sys.getenv("PLATFORM",unset=NA)

    rootpath <- Sys.getenv(root,unset=NA)
    if (is.na(rootpath)) stop(paste(root,"environment variable not found"))

    projspath <- file.path(rootpath,"projects")
    if (is.null(project)) {
        projects <- list.dirs(projspath,recursive=FALSE,full.names=FALSE)

        hidden <- grepl("^\\.",projects)
        if (any(hidden)) projects <- projects[!hidden]

        if (length(projects) < 1) stop(paste("No directories found in",projspath))
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
    projpath <- file.path(projspath,project)

    if (is.null(platform)) {
        platforms <- list.dirs(projpath,recursive=FALSE,full.names=FALSE)
        hidden <- grepl("^\\.",platforms)
        if (any(hidden)) platforms<- platforms[!hidden]

        if (length(platforms) < 1) stop(paste("No directories found in",projpath))
        if (length(platforms) > 1) {
            ip <- character(0)
            while(length(ip) == 0) {
                cat("Choose a platform by number (0 to quit):\n")
                cat(paste0(paste(1:length(platforms),platforms,sep=": "),"\n",collapse=""))
                ip <- as.integer(readLines(n=1))
                if (is.na(ip) || ip == 0) return()
                if (ip > length(platforms)) cat("Invalid entry\n")
            }
            platform <- platforms[ip]
        }
        else platform <- platforms
    }

    Sys.setenv(PROJECT=project)
    Sys.setenv(PLATFORM=platform)
    projdata <- file.path(projpath,platform,"R",".RData")

    curpos <- NULL
    if (!is.na(curproj) && !is.na(curplat)) {
        sl <- search()
        curdata <- file.path(projspath,curproj,curplat,"R",".RData")
        pos <- grepl(curdata,sl)
        if (any(pos)) curpos <- base::seq(along=pos)[pos][1]
    }

    if (file.exists(projdata)) {
        if (curproj == project && curplat == platform && !is.null(curpos)) sync(pos=curpos)
        else {
            if (!is.null(curpos)) {
                cat("detaching",curdata,", pos=",curpos,"\n")
                detach(pos=curpos)
            }
            else curpos <- 2L
            cat("attaching",projdata,", pos=",curpos,"\n")
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
    c(project=project,platform=platform)
}
