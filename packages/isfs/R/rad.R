# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

#  Description: dat functions for derived radiation variables.
#

calcRlw <- function(Rpile,Tcase,Tdome=NULL,B=4,swcor=0,Rsw=NULL)
{
    # Long wave calculation for either in or out.
    # Temperatures in degK

    if (is.null(Rpile)) return(NULL)

    SB <- 5.67e-8	   # Stefan-Boltzmann constant, W/m^2-degK^4

    if (!is.null(Tcase)) {
        if (is.null(Tdome) || all(is.na(Tdome))) Rpile <- Rpile + SB*Tcase^4
        else {
            # If there is not a Tdome variable for every Tcase,
            # then create one equal to Tcase, which will make the
            # Tcase^4-Tdome^4 correction term zero.
            # Kipp and Zonen pygeometers don't have a Tdome.
            #
            Tdome = conform(Tdome,Tcase)
            misstd = apply(Tdome@data,2,function(x){all(is.na(x))})
            if (any(misstd)) Tdome[,misstd] = Tcase[,misstd]

            # B can be a simple vector, one for each column of Tcase or Tdome
            Bvec <- !inherits(B,"nts") &&
                (length(B) == ncol(Tcase) || (length(B) == 1))
            if (Bvec) {
                Rpile <- Rpile + SB *
                    nts(t(((1 + B) * t(Tcase^4) - B * t(Tdome^4))),Tcase@positions)
                class(Rpile) <- "dat"
            }
            else {
                good.B = apply(B,2,function(x)any(!is.na(x)))
                if (any(good.B))
                    Rpile[,good.B] <- Rpile[,good.B] + SB * ((1 + B[,good.B]) * Tcase[,good.B]^4 - B[,good.B] * Tdome[,good.B]^4)
                if (any(!good.B))
                    Rpile[,!good.B] <- Rpile[,!good.B] + SB * Tcase[,!good.B]^4
            }
        }
    }

    if (any(!is.na(swcor) & swcor != 0) && !is.null(Rsw) && length(Rsw) > 0 && any(!is.na(Rsw))) {
        # Short wave correction
        # Warn if Rsw looks negative.
        which <- suffixes(Rsw,2,leadch="")[1]
        if (mean(Rsw,na.rm=T) < -5)
            warning(paste("mean Rsw.",which,
                    " is negative. Shortwave correction to Rlw.",which," may be wrong",sep=""))
        Rpile <- Rpile - swcor * Rsw
    }

    colnames(Rpile) <- paste("Rlw",suffixes(Rpile,2),sep="")
    Rpile@units <- rep("W/m^2",ncol(Rpile))
    Rpile
}

dat.Rlw.in <- function(what,derived=TRUE,B=dpar("pyrgeometer.B"),
    swcor=dpar("pyrgeometer.swcor"),...)
{
    dat.Rlw.either(what=what,B=B,swcor=swcor,...)
}

dat.Rlw.out <- function(what,derived=TRUE,B=dpar("pyrgeometer.B"),
    swcor=dpar("pyrgeometer.swcor"),...)
{
    dat.Rlw.either(what=what,B=B,swcor=swcor,...)
}

dat.Rlw.either <- function(what="Rlw.out",B=dpar("pyrgeometer.B"),
    swcor=dpar("pyrgeometer.swcor"),...)

{
    robust <- dpar("robust")
    Rpile.name <- sub("Rlw","Rpile",what,fixed=TRUE)
    Tcase.name <- sub("Rlw","Tcase",what,fixed=TRUE)
    Tdome.name <- sub("Rlw","Tdome",what,fixed=TRUE)
    Bpyrg.name <- sub("Rlw","Bpyrg",what,fixed=TRUE)
    Rsw.name <- sub("Rlw","Rsw",what,fixed=TRUE)
    Swcor.name <- sub("Rlw","Swcor",what,fixed=TRUE)

    tcase <- dat(Tcase.name,...)
    if (is.null(tcase)) {
        Tcase.name <- sub("(\\.in)|(\\.out)","",Tcase.name)
        tcase <- dat(Tcase.name,...)
    }
    if (!is.null(tcase)) {
        degC <- apply(tcase@data,2,mean,trim=0.1,na.rm=T) < 150
        degC[is.na(degC)] <- F
        if (any(degC)) tcase[,degC] <- tcase[,degC] + 273.15
    }

    # If robust, don't read in Tdome.  The "Reading ... Tdome" message
    # misleads the user who assumes Tdome is being used.
    #

    x = dat(Rpile.name,...)
    if (inherits(tcase,"dat")) tcase = conform(tcase,x)

    if (is.null(robust) || robust) x <- calcRlw(x,tcase)
    else {
        nwarn <- getOption("warn")
        options(warn=-1)
        tdome <- dat(Tdome.name,...)
        options(warn=nwarn)

        if (!is.null(tdome)) {
            degC <- apply(tdome@data,2,mean,trim=0.1,na.rm=T) < 150
            degC[is.na(degC)] <- F
            if (any(degC)) tdome[,degC] <- tdome[,degC] + 273.15

            if (is.null(B)) {
                B <- dat(Bpyrg.name)
                if (inherits(B,"dat") && !is.null(tcase)) {
                    B <- conform(B,tcase)
                    B <- approx(B,xout=tspar(tcase),method="constant",f=0,rule=2)
                }
            }
        }

        # Short wave correction
        sw <- NULL
        if (is.null(swcor)) {

            options(warn=-1)
            swcor <- dat(Swcor.name,...)
            options(warn=nwarn)

            if (any(!is.na(swcor) & swcor != 0) && inherits(swcor,"dat")) {
                sw <- dat(Rsw.name,...)
                while (is.null(sw) && nwords(Rsw.name,sep=".") > 2) {
                    new.name <- words(Rsw.name,1,nwords(Rsw.name)-1)
                    warning(paste(Rsw.name,"not found for stations",
                            paste(dpar("stns"),collapse=",")," Will try to find",new.name))
                    Rsw.name <- new.name
                    sw <- dat(Rsw.name,...)
                }
                if (is.null(sw) || length(sw) == 0) {
                    warning(paste(Rsw.name,"not found for stations",
                            paste(dpar("stns"),collapse=",")," Cannot apply short wave correction to",what))
                    swcor <- 0
                }
                else {
                    swcor <- conform(swcor,sw)
                    swcor <- approx(swcor,xout=tspar(sw),method="constant",f=0,rule=2)
                }
            }
        }
        if (is.null(sw) && !identical(swcor,0.0)) sw <- dat(Rsw.name)

        if (inherits(tdome,"dat")) tdome = conform(tdome,x)
        if (inherits(B,"dat")) B = conform(B,x)
        if (inherits(swcor,"dat")) swcor = conform(swcor,x)
        if (inherits(sw,"dat")) sw = conform(sw,x)
        x <- calcRlw(x,tcase,tdome,B=B,swcor=swcor,Rsw=sw)
    }

    # Check if Rlw is available without being derived.
    if (any(words(variables(),1,nwords(what,sep=".")) == what)) {
        x2 = dat(what,derived=F,...)
        x <- Cbind(x,x2)
    }
    # browser()
    x
}

# Default Bpyrg value
dat.Bpyrg <- function(what,derived=TRUE,...) {
    warning(paste("project dat function for",what,"does not exist, setting Bpyrg=4.0"))
    4.0
}

# Default Swcor value
dat.Swcor <- function(what,derived=TRUE,...) {
    warning(paste("project dat function for",what,"does not exist, setting swcor=0.0"))
    0.0
}

dat.Rlw <- function(what,derived=TRUE,...) 
{
    datvar <- datVar() # name of variable to be created
    Rlwin <- dat(sub(datvar,"Rlw.in",what,fixed=TRUE),...)
    Rlwout <- dat(sub(datvar,"Rlw.out",what,fixed=TRUE),...)

    Rlw <- Cbind(Rlwin,Rlwout)

    if (any(words(variables(),1,2) == "Rlw.net")) {
        Rlwnet <- dat(paste("Rlw.net",which,sep=""),...,derived=F)
        Rlw <- Cbind(Rlw,Rlwnet)
    }
    Rlw
}

dat.Rsum <- function(what,derived=TRUE,...)
{
    datvar <- datVar() # name of variable to be created
    # sign convention: positive Rsum and Rnet is
    # a downward energy flux
    Rsw.in = dat(sub(datvar,"Rsw.in",what,fixed=TRUE),...) 
    Rsw.in[Rsw.in < 0] = 0
    Rsw.out = dat(sub(datvar,"Rsw.out",what,fixed=TRUE),...)
    Rsw.out[Rsw.out < 0] = 0
    x <- Rsw.in - conform(Rsw.out,Rsw.in) +
        conform(dat(sub(datvar,"Rlw.in",what,fixed=TRUE),...),Rsw.in) -
        conform(dat(sub(datvar,"Rlw.out",what,fixed=TRUE),...),Rsw.in)

    if (is.null(x)) return(x)
    colnames(x) <- paste("Rsum",suffixes(x,3),sep="")
    # x@units <- rep("W/m^2",ncol(x))
    x
}

dat.Rsw.net <- function(what,derived=TRUE,...)
{
    datvar <- datVar() # name of variable to be created
    Rswinname <- sub(datvar,"Rlw.in",what,fixed=TRUE)
    Rsw.in = dat(Rswinname,...) 
    Rsw.in[Rsw.in < 0] = 0
    Rsw.out = dat(sub(datvar,"Rsw.out",what,fixed=TRUE),...)
    Rsw.out[Rsw.out < 0] = 0
    x <- Rsw.in - conform(Rsw.out,Rsw.in)

    if (is.null(x)) return(x)
    colnames(x) <- sub(Rswinname,datvar,colnames(x),fixed=TRUE)
    # x@units <- rep("W/m^2",ncol(x))
    x
}

dat.Rlw.net <- function(what,derived=TRUE,...) 
{
    datvar <- datVar() # name of variable to be created
    Rlwinname <- sub(datvar,"Rlw.in",what,fixed=TRUE)
    Rlwin <- dat(Rlwinname,...)
    Rlwout <- dat(sub(datvar,"Rlw.out",what,fixed=TRUE),...)
    x <- Rlwin - conform(Rlwout,Rlwin)
    if (is.null(x)) return(x)
    colnames(x) <- sub(Rlwinname,datvar,colnames(x),fixed=TRUE)
    # x@units <- rep("W/m^2",ncol(x))
    x

}

dat.albedo <- function(what,derived=TRUE,...)
{
    datvar <- datVar() # name of variable to be created

    Rsw.in <- dat(sub(datvar,"Rsw.in",what,fixed=TRUE),...)
    Rsw.out <- dat(sub(datvar,"Rsw.out",what,fixed=TRUE),...)
    x <- Rsw.out/conform(Rsw.in,Rsw.out)
    x[Rsw.in < 1 | Rsw.out < 1] <- NA_real_

    colnames(x) <- paste("albedo",suffixes(x,3),sep="")
    x@units <- rep("",ncol(x))
    x
}
dat.Tsfc <- function(what,derived=TRUE,emissivity=dpar("emissivity.sfc"),...)
{
    datvar <- datVar() # name of variable to be created
    if (is.null(emissivity))
        emissivity <- 0.98
    SB <- 5.67e-8	   # Stefan-Boltzmann constant, W/m^2-degK^4

    tsfc = NULL

    # If user asks for Tsfc.pyrg, don't read Tsfc variables
    # from netcdf file. Then set it back to Tsfc.
    if (words(what,1,2,sep=".") != "Tsfc.pyrg") {
        vars = lookup(what,verbose=F)
        if (length(vars) > 0) tsfc = dat(what,...,derived=F)
    }
    else what = paste("Tsfc",suffixes(what,3),sep="")

    vars = lookup(sub(datvar,"Rlw.out",what,fixed=TRUE),verbose=F)
    rvars = lookup(sub(datvar,"Rpile.out",what,fixed=TRUE),verbose=F)
    if (length(vars) > 0 || length(rvars) > 0) {
        rlwin = dat(sub(datvar,"Rlw.in",what,fixed=TRUE),...)
        rlwout = dat(sub(datvar,"Rlw.out",what,fixed=TRUE),...)

        # conform rlw to whichever has fewest columns.
        if (ncol(rlwin) < ncol(rlwout)) rlwout = conform(rlwout,rlwin)
        else rlwin = conform(rlwin,rlwout)

        x <- ( (rlwout - (1-emissivity)* rlwin) / (emissivity*SB) )^0.25 - 273.1
        colnames(x) <- paste("Tsfc.pyrg",suffixes(x,3),sep="")
        x@units <- rep("degC",ncol(x))
        x <- Cbind(tsfc,x)
    }
    else x = tsfc
    x
}
dat.Tsky <- function(what,derived=TRUE,...)
{
    datvar <- datVar() # name of variable to be created

    SB <- 5.67e-8	   # Stefan-Boltzmann constant, W/m^2-degK^4

    x <- (dat(sub(datvar,"Rlw.in",what,fixed=TRUE),...) / SB) ^ 0.25 - 273.15

    colnames(x) <- paste("Tsky",suffixes(x,3),sep="")
    x@units <- rep("degC",ncol(x))
    x
}
