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

  default.val <- 0.3	# 30 centimeters

  warning(paste("project dat.fastH2OSep not found for fasth2o<->sonic separation. dat.fastH2OSep returning", default.val,"meters"))

  x <- dat(expand("h2o",what),derived=F)
  if (is.null(x)) return(x)
  sfxs <- suffixes(x,2)

  if (nrow(x) > 1) {
    tz <- c(tspar(x)[1],tspar(x)[nrow(x)])
    z <- matrix(rep(default.val,ncol(x)*2),nrow=2,byrow=T)
  }
  else {
    tz <- tspar(x)[1]
    z <- matrix(rep(default.val,ncol(x)),nrow=1,byrow=T)
  }
  dat(nts(z,tz,
      names=paste("fastH2OSep",sfxs,sep=""),
      units=rep("m",ncol(x)),
      stations=stations(x)))
}

