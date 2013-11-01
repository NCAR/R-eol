# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
#               Copyright (C) by UCAR
# 
#  Description:
#    dat routines returning various variables concerning licor 7500 hygrometers
#
dat.licor7500Sep <- function(what,...) {
    # separation, in meters between licor 7500 and sonic anemometer w axis

    default.val <- 0.3	# 30 centimeters

    warning(paste("project dat.licor7500Sep not found for licor7500<->sonic separation. dat.licor7500Sep returning", default.val,"meters"))

    x = dat(expand("h2o",what),derived=F)
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
            paste("licor7500Sep",sfxs,sep=""),
            rep("m",ncol(x)),
            stations=stations(x)))
}

