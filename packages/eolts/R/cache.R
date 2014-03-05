# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
#               Copyright (C) by UCAR
# 

# Empty environment in the eolts namespace for adding session
# cache variables. This is not in the global environment, and so is
# not saved on exit. Currently it is not exported, and so
# isn't in the package:eolts environment.
# .getNamespace("eolts")
# ls(all=TRUE,.getNamespace("eolts"))
# get(".eoltsEnv",envir=.getNamespace("eolts"))
# <environment: 0x54fec50>
# eolts:::.eoltsEnv
# <environment: 0x54fec50>
.eoltsEnv <- new.env(parent=emptyenv())

cache.name <- function(what)
{
    paste(".",what,".cache",sep="")
}

clear.cache <- function()
{
    cached <- objects(pattern="^\\..*\\.cache$",envir=.eoltsEnv,all.names=TRUE)
    # browser()
    if (length(cached) > 0)
        remove(list=cached,envir=.eoltsEnv)
}

cache.object <- function(what,val)
{
    nm <- cache.name(what)
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
    nm <- cache.name(what)
    exists(nm,envir=.eoltsEnv)
}
