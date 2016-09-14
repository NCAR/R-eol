# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

dat.fastH2OSep <- function(what,derived=TRUE,...)
{
    # separation, in meters between the fast water vapor sensor and sonic anemometer w axis

    datvar <- datVar() # requested variable name, x of dat.x

    default.val <- 0.3	# 30 centimeters

    warning(paste("project dat.fastH2OSep not found for fasth2o<->sonic separation. dat.fastH2OSep returning", default.val,"meters"))

    x <- dat(sub(datvar,"h2o",what,fixed=TRUE),derived=F)
    if (is.null(x)) return(x)

    if (nrow(x) > 1) {
      tz <- c(tspar(x)[1],tspar(x)[nrow(x)])
      z <- matrix(rep(default.val,ncol(x)*2),nrow=2,byrow=T)
    }
    else {
      tz <- tspar(x)[1]
      z <- matrix(rep(default.val,ncol(x)),nrow=1,byrow=T)
    }
    dat(nts(z,tz,
        names=sub("h2o","fastH2OSep",colnames(x),fixed=TRUE),
        units=rep("m",ncol(x)),
        stations=stations(x)))
}

