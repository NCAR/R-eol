# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

sync <- function(pos,root=c("ISFS","ISFF"))
{
    # detach/attach $ISF[SF]/projects/$PROJECT/ISF[SF]/R/.RData so that the
    # user's R session has the latest copy.


    sl <- search()
    if (missing(pos)) {
        for (rt in root) {
            rootpath <- Sys.getenv(rt,unset=NA)
            if (!is.na(rootpath)) break
        }
        if (is.na(rootpath)) stop(paste(root[1],"environment variable not found"))

        for (plat in c(root,"")) {
            projdata <- file.path(rootpath,"projects",Sys.getenv("PROJECT"), plat,"R")
            pos <- grepl(projdata,sl)
            if (any(pos)) break
        }
        if (!any(pos)) stop(paste("pos is missing and can't find",projdata))
        pos <- base::seq(along=pos)[pos][1]
    }

    rd <- sl[pos]

    if (grepl(".RData",rd)) {
        rd <- sub("^file:","",rd)
        detach(pos=pos)
        attach(rd,pos=pos)
    }
    else stop(paste("attached object at position",pos,"is not an .RData:",rd))

    invisible(rd)
}

# Two S+ functions that don't exist in R:
# SPO 11/2019
#
# Similar to sync above, this version of synchronize will synchronize the first path starting with 
# "file" -- usually a project's R directory -- if pos is not specified (exactly what sync does)
#
synchronize = function(pos) {
    requireNamespace("stringr")
    if (missing(pos)) {
       all = search()
       wtype = substring(all,1,stringr::str_locate(all,":")[,1]-1)
       pos = match("file",wtype)
    }
    if (!is.na(pos)) {
       what = search()[pos]
       wtype = substring(what,1,stringr::str_locate(what,":")[1]-1)
       wstr = substring(what,stringr::str_locate(what,":")[1]+1,nchar(what))
       detach(pos=pos)
       # One is not supposed to use library() or require() within packages.
       # So library(wstr, character.only=TRUE) was replaced with requireNamespace(wstr).
       # May not have the same functionality...
       if (wtype=="package") requireNamespace(wstr)
       if (wtype=="file") attach(wstr,pos=pos)
    }
    invisible(NULL)
}

#
# This locates the search paths containing an object with the specified name
#
whereis = function(what) {
    all = search()
    out = NULL
    for (i in 1:length(all)) {
    if (any(objects(pos=i)==what)) out = c(out,i)
    }
    all[out]
}
