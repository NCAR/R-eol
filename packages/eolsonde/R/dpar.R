# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsonde" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

# Set function called from eolsonde::dpar to be used when any of the parameters
# listed in .dparParams are set.
setDpar <- function(parm,vals)
{
    if (length(parm) != 1) stop("parm must be of length 1")
    pname <- names(parm)
    pval <- parm[[1]]

    vals[[pname]] <- pval
    vals
}

# dpar parameters supported by this eolsonde package. See eolsonde::onAttach(), where these
# are appended to the list that is used by eolsonde::dpar().
.dparParams <- list(
    checkSondeStatus=list(type="logical",setf=setDpar,default=TRUE,flushCache=FALSE),
    # In AVAPS D files, these values in the STA column will be set to their zero-based index in dpar("sondeStatus")
    # First character can be A=from aircraft data system, P=pre-launch, S=sounding
    sondeRecords=list(type="character",setf=setDpar,default="S",flushCache=FALSE)
)
