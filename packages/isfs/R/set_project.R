# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

set_project <- function(root="ISFF",project=NULL,platform="ISFF")
{
    curproj <- Sys.getenv("PROJECT",unset=NA)

    rootpath <- Sys.getenv(root,unset=NA)
    if (is.na(rootpath)) stop(paste(root,"environment variable not found"))

    projpath <- file.path(rootpath,"projects")
    
    if (is.null(project)) {
        projects <- list.dirs(projpath,recursive=FALSE,full.names=FALSE)

        hidden <- grepl("^\\.",projects)
        if (any(hidden)) projects <- projects[!hidden]
        if ("CVS" %in% projects)
            projects <- projects[-match("CVS",projects)]

        lp <- length(projects)
        if (lp < 1) stop(paste("No directories found in",projpath))
        if (lp > 1) {
            ip <- character(0)
            while(length(ip) == 0) {
                cat("Choose a project by number (0 to quit):\n")
                # add number, use format so they're all the same length
                strs <- format(paste(1:lp,projects,sep=": "))
                # 3 to a line
                strs <- paste0(unlist(sapply(seq(from=1,to=lp,by=3),
                    function(i){
                        ix <- i:(min(i+2,lp))
                        paste0(paste0(strs[ix],collapse=" "),"\n")
                    })),collapse="")
                cat(strs)
                ip <- as.integer(readLines(n=1))
                if (is.na(ip) || ip == 0) return(invisible(NULL))
                if (ip > length(projects)) cat("Invalid entry\n")
            }
            project <- projects[ip]
        }
        else project <- projects
    }
    
    projdata <- file.path(projpath,project,platform,"R",".RData")
    if (!file.exists(projdata)) {
        projdata <- file.path(projpath,project,"R",".RData")
    }

    curpos <- NULL
    if (!is.na(curproj)) {
        sl <- search()
        curdata <- file.path(projpath,curproj,platform,"R",".RData")
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
    project
}
