# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

# Set function called from eolts::dpar to be used when any of the parameters
# listed in .dparDefaults are set.
setDpar <- function(parm,vals)
{
    if (length(parm) != 1) stop("parm must be of length 1")
    pname <- names(parm)
    pval <- parm[[1]]

    vals[[pname]] <- pval
    vals
}

# dpar parameters supported by this isfs package. See isfs::onAttach(), where these
# are appended to the list that is used by eolts::dpar().
.dparDefaults <- list(
    accelgrav=list(type="numeric",setf=setDpar,default=9.81,flushCache=FALSE),
    # for simple averages, one value, average interval in seconds
    # for non-simple averages, two values, non-simple interval, simple interval
    avg=list(type="numeric",setf=setDpar,default=NULL,flushCache=TRUE),
    co2_flux_corrected=list(type="logical",setf=setDpar,default=NULL,flushCache=FALSE),
    coords=list(type="character",setf=setDpar,default=NA,flushCache=FALSE),
    datacoords=list(type="character",setf=setDpar,default=NA,flushCache=FALSE),
    gsoil_philip_corrected=list(type="logical",setf=setDpar,default=NULL,flushCache=FALSE),
    h2o_flux_corrected=list(type="logical",setf=setDpar,default=NULL,flushCache=FALSE),
    hts=list(type="numeric",setf=setDpar,default=NULL,flushCache=TRUE),
    lat=list(type="numeric",setf=setDpar,default=NA,flushCache=FALSE),
    lon=list(type="numeric",setf=setDpar,default=NA,flushCache=FALSE),
    platform=list(type="character",setf=setDpar,default="",flushCache=FALSE),
    pyrgeometer.B=list(type="numeric",setf=setDpar,default=NA,flushCache=FALSE),
    pyrgeometer.Swcor=list(type="numeric",setf=setDpar,default=NA,flushCache=FALSE),
    RH.ice=list(type="logical",setf=setDpar,default=NA,flushCache=FALSE),
    robust=list(type="logical",setf=setDpar,default=FALSE,flushCache=TRUE),
    sites=list(type="character",setf=setDpar,default=NULL,flushCache=TRUE),
    # smoothing period, in seconds
    smooth=list(type="numeric",setf=setDpar,default=NULL,flushCache=TRUE),
    sfxs=list(type="character",setf=setDpar,default=NULL,flushCache=TRUE),
    stns=list(type="integer",setf=setDpar,default=NULL,flushCache=TRUE),
    vonKarman=list(type="numeric",setf=setDpar,default=0.4,flushCache=FALSE)
)
