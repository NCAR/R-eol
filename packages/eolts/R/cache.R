# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolts" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

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

cache_name <- function(what)
{
    paste(".",what,".cache",sep="")
}

clear_cache <- function()
{
    cached <- objects(pattern="^\\..*\\.cache$",envir=.eoltsEnv,all.names=TRUE)
    # browser()
    if (length(cached) > 0)
        remove(list=cached,envir=.eoltsEnv)
}

cache_object <- function(what,val)
{
    nm <- cache_name(what)
    assign(nm,val,envir=.eoltsEnv);
}

get_cache_object <- function(what)
{
    if (check_cache(what))
        get(cache_name(what),envir=.eoltsEnv)
    else NULL
}

rm_cache_object <- function(what)
{
    if (check_cache(what))
        remove(list=cache_name(what),envir=.eoltsEnv)
}

check_cache <- function(what)
{
    nm <- cache_name(what)
    exists(nm,envir=.eoltsEnv)
}
