# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

nwords <- function(s,sep=".")
{
    if (is.null(s)) return(0)
    sapply(strsplit(s,split=sep,fixed=TRUE),length)
}

words <- function(x,first=1,last=1000000,sep=".")
{

    if (is.null(x)) return(NULL)
    if (length(first) == 1) first = rep(first,length(x))
    if (length(last) == 1) last = rep(last,length(x))
    sapply(1:length(first),
       function(i,x,first,last,sep)
       {
           xi = i
           if (xi > length(x)) xi = ((i-1) %% length(x)) + 1
           x = unlist(strsplit(x[xi],split=sep,fixed=TRUE))
           if (last[i] > length(x)) last[i] = length(x)
           if (last[i] < first[i]) ""
           else paste(x[first[i]:last[i]],collapse=sep)
       },x,first,last,sep)
}
