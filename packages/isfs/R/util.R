# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
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
