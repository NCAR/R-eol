#
#               Copyright (C) by UCAR
# 

.eoltsEnv = new.env(parent=emptyenv())

cache.name <- function(what)
{
    paste(".",what,".cache",sep="")
}

clear.cache <- function()
{
    cached = objects(pattern="^\\..*\\.cache$",envir=.eoltsEnv,all.names=TRUE)
    # browser()
    if (length(cached) > 0)
        remove(list=cached,envir=.eoltsEnv)
}

cache.object <- function(what,val)
{
    nm = cache.name(what)
    assign(nm,val,envir=.eoltsEnv);
}

get.cache.object <- function(what)
{
    if (check.cache(what))
        get(cache.name(what),envir=.eoltsEnv)
    else NULL
}

rm.cache.object <- function(what)
{
    if (check.cache(what))
        remove(list=cache.name(what),envir=.eoltsEnv)
}

check.cache <- function(what)
{
    nm = cache.name(what)
    exists(nm,envir=.eoltsEnv)
}
