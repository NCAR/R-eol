# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

lookup <- function(x,verbose=T)
{
    if (mode(x) != "character") stop("No lookup method defined for object")

    if (length(x) > 1) return(unlist(sapply(x,lookup,verbose=verbose)))

    toc <- variables()
    nv <- length(toc)
    if (nv == 0) {
        warning("No variables found for times in netcdf files")
        return(NULL)
    }

    nw <- nwords(x,sep=".")
    prefixes <- words(toc,rep(1,nv),rep(nw,nv),sep=".")

    xm <- match(prefixes,x,nomatch=0) != 0

    toc <- toc[xm]
    if (verbose)
      cat("names matching",x,":",paste(toc,collapse="  "),"\n")

    # more than one match, use secondary search criteria
    if (length(toc) > 1) {
        names(toc) <- toc
        toc <- lapply(toc,create.keys)
        keys <- c("sonic","h2o","temp")
        ikey <- 0
        while (length(toc) > 1 && ikey < length(keys)) {
          ikey <- ikey + 1
          key <- keys[ikey]
          xm <- lapply(toc,lookup.key,key,dpar(key))
          toc <- toc[unlist(xm)]
          if (verbose)
              cat("names matching",paste("dpar(",key,"=",
                  if(is.null(dpar(key))) "NULL" else dpar(key),"):",
                  sep="",collapse=""),
                  paste(names(toc),collapse="  "),"\n")
          }
        # At this point toc is a list of lists
        if (length(toc) > 0) {
            toc <- names(toc)
            names(toc) <- rep(x,length(toc))
        }
    }
    if (length(toc) == 0) warning(paste(x,"not found in lookup()"))
    toc
}

lookup.key <- function(x,key,value)
{
    return(if (is.null(x[[key]]) || is.null(value)) T else x[[key]] == value)
}

create.keys <- function(x)
{
    prefix <- words(x,sep=".",1,1)

    vars <- components(x)
    w1vars <- words(vars,sep=".",1,1)

    # look for sonic name
    sonic <- NULL
    if (any(is <- match(w1vars,c("u","v","w","tc"),nomatch=0) != 0))
          sonic <- words(vars,sep=".",2,2)[is][1]
    else if (any(is <- match(w1vars,c("uflag","vflag","wflag","tcflag"),nomatch=0) != 0))
          sonic <- words(vars,sep=".",2,2)[is][1]

    # look for water vapor instrument name
    h2o <- NULL
    if (any(is <- match(c("h2o"),w1vars,nomatch=0) != 0))
          h2o <- words(vars,sep=".",2,2)[is][1]

    temp <- NULL
    if (any(is <- match(c("t"),w1vars,nomatch=0) != 0))
          temp <- words(vars,sep=".",2,2)[is][1]

    list(prefix=prefix,variables=vars,sonic=sonic,h2o=h2o,temp=temp)
}

components <- function(xnam)
{
    # input                       output
    # "u"                         "u"
    # "u'w'"                      c("u","w")
    # "u'u'w'"                    c("u","u","w")
    # "u'u'.4m"                   c("u.4m","u.4m")
    # "u'h2o'.(ati,csi).4m"       c("u.ati.4m","h2o.csi.4m")
    # "u'h2o'.(,csi).4m"          c("u.4m","h2o.csi.4m")
    # "x'y'z'.(a,,c).4m"          c("x.a.4m","y.4m","z.c.4m")
    #
    # Also supports old colon notation:
    # "u:w"                       c("u","w")
    # "u:u:w"                     c("u","u","w")


    if (length(xnam) > 1) return(lapply(xnam,components))

    head <- words(xnam,sep=".",1,1)

    nvars <- nwords(head,sep="'")
    vars <- words(head,sep="'",1:nvars,1:nvars)
    if (vars[nvars] == "") vars <- vars[-nvars]

    mid <- words(xnam,sep="(",2,2)
    if (mid != "") {
        tail <- words(words(mid,sep=")",2,2),sep=".",2)

        mid <- words(mid,sep=")",1,1)


        nmid <- nwords(mid,sep=",")
        mid <- words(mid,sep=",",1:nmid,1:nmid)
    }
    else {
        nw <- nwords(xnam,sep=".")
        tail <- words(xnam,sep=".",2,nw)
    }
    addsep <- function(x,char=".")
          { if (is.null(x) || x=="") "" else paste(char,x,sep="") }
    mid <- sapply(mid,addsep,".")
    tail <- sapply(tail,addsep,".")

    paste(vars,mid,tail,sep="")
}
