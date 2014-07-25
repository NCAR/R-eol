# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolts" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

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
