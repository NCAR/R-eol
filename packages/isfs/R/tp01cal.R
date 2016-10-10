# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.


dat.Lambdasoil <- function(what,derived=TRUE,tsec=180,rc=0.005,rh=0.001,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    if (any(words(variables(),1,1) == "Lambdasoil")) {
        lambda <- dat(what,derived=FALSE,...)
        if (is.null(lambda)) return(NULL)

        fat_done <- dpar("lambdasoil_fat_corrected")
        fat_done <- !is.null(fat_done) && as.logical(fat_done)

    } else {
        lambda <- calc_Lambdasoil(datvar,what,...)
        fat_done  <- FALSE
    }
    # Compute F(a*t), where a is the soil diffusivity.
    if (!fat_done) {
        lambda <- lambda * conform(
            dat(sub(datvar,"Fat_tp01",what,fixed=TRUE),tsec=tsec,rc=rc,rh=rh,...),lambda)
    }
    lambda
}

calc_Lambdasoil <- function(datvar="Lambasoil",what="Lambdasoil",...)
{
    # Function to calculate Lambdasoil from Tau, Vheat and Vpile values from a
    # Huksefulx TP01 soil thermal properties probe.
    #
    # A TP01 interfaced through a Wisard sensor reports a derived Lambdasoil.
    # This function can be used to double check that computation.
    # 
    # This uses a list to map site name or character station number
    # to TP01 serial numbers. This list should be given the name
    # "tp01_serial_numbers" and assigned in the .isfsEnv,
    # typically in the project_init function:
    #
    # CHATS:
    #   assign("tp01_serial_numbers",list(vt=200239),envir=.isfsEnv)
    # HVAMS03:
    #   assign("tp01_serial_numbers",list(
    #     "5"=200234, "9"=200235, "8"=200236, "1"=200238,
    #     "2"=200239, "3"=200240, "4"=200241, "6"=200242, "7"=200243
    #     ), envir=.isfsEnv)
    # TREX:
    #   assign("tp01_serial_numbers",list(
    #       "1"=200235, "2"=200240, "3"=200243
    #       ), envir=.isfsEnv)
    # CuPIDO:
    #   assign("tp01_serial_numbers",list(
    #       "3"=200235, "5"=200236, "10"=200240, "2"=200243
    #       ), envir=.isfsEnv)
    # METCRAX:
    #   assign("tp01_serial_numbers",list(
    #       rim=200234, el=200235, wl=200236, flr=200239, wu=200240,
    #       sw=200241, eu=200242),
    #       ), envir=.isfsEnv)
    #
    # The calibrations for all TP01 probes (by serial number):
    # (Note (9/13/2010): With Wisard, should never need this code, but decided
    # to add probes 589--592 for completeness anyway.)
    #
    #   s/n     Re      E.lambda(e-6)  L.tp
    tp01 <- matrix(c(
        200234,     14.7,     97.9,  0.060,
        200235,     14.8,    104.6,  0.060,
        200236,     14.8,    104.9,  0.060,
        200238,     15.4,    154.0,  0.060,
        200239,     16.2,    175.8,  0.060,
        200240,     15.0,    160.8,  0.060,
        200241,     16.2,    141.5,  0.060,
        200242,     15.9,    152.4,  0.060,
        200243,     15.6,    160.5,  0.060,
        200589,     16.1,    158.0,  0.060,
        200590,     15.4,    136.0,  0.060,
        200591,     16.7,    140.0,  0.060,
        200592,     15.5,    162.0,  0.060),
        ncol=4,byrow=T,
        dimnames=list(NULL,c("SN","Re","E.lambda","L")))


    if (any(words(variables(),1,1) == "Vpile_max")) {
        wisard_mode <- FALSE
        # Campbell logger reporting Tau63, Vpile and Vheat.
        # statsproc computes the max values in 5 minutes.

        tau <- dat(sub(datvar,"Tau63_max",what,fixed=TRUE),...)
        if (is.null(tau)) return(NULL)
        tau[tau<0] <- NA_real_

        pile <- dat(sub(datvar,"Vpile_max",what,fixed=TRUE),...)
        mv <- units(pile) == "mV"
        if (any(mv)) {
            pile[,mv] <- pile[,mv] * 0.001
        }
        heat <- dat(sub(datvar,"Vheat_max",what,fixed=TRUE),...)
    } else if (any(words(variables(),1,2) == "Vpile.on")) {
        # Wisard sensor reports Tau63, Vheat, Vpile.on, Vpile.off
        # Compute thermopile response as Vpile.on - Vpile.off
        wisard_mode <- TRUE

        tau <- dat(sub(datvar,"Tau63",what,fixed=TRUE),...)
        if (is.null(tau)) return(NULL)
        tau[tau<0] <- NA_real_

        pile_on <- dat(sub(datvar,"Vpile.on",what,fixed=TRUE),...)
        pile_off <- dat(sub(datvar,"Vpile.off",what,fixed=TRUE),...)
        uv <- units(pile_on) == "uV"
        if (any(uv)) {
            pile_on[,uv] <- pile_on[,uv] * 0.000001
            pile_off[,uv] <- pile_off[,uv] * 0.000001
        }
        heat <- dat(sub(datvar,"Vheat",what,fixed=TRUE),...)
    }
    else return(NULL)

    # browser()

    sites <- sites(tau)	
    stns <- stations(tau)
    #
    # match recorded values by either stations or suffixes
    #
    tp01sns <- get("tp01_serial_numbers",envir=.isfsEnv)

    if (is.list(tp01sns)) {
        sites[stns > 0] <- as.character(stns[stns > 0])
        ix <- match(sites,names(tp01sns),nomatch=0)
        sns <- rep(0,length(sites))
        sns[ix] <- unlist(tp01sns[sites])  # serial numbers
        # row numbers in tp01 for the sensors in tau
        ix <- match(sns,tp01[,"SN"],nomatch=0)
    }
    cat("TP01 serial numbers=",sns,"\n")

    if (any(ix == 0)) {
        tau <- tau[,ix!=0]
        heat <- heat[,ix!=0]
        if (wisard_mode) {
            pile_on <- pile_on[,ix!=0]
            pile_off <- pile_off[,ix!=0]
        } else {
            pile <- pile[,ix!=0]
        }
    }

    Re.tp <- tp01[ix,"Re"]
    E.lambda <- tp01[ix,"E.lambda"] * 1e-6
    L.tp <- tp01[ix,"L"]

    lambda <- tau * NA_real_

    sapply(1:ncol(tau),function(ic) {

        lx <- nrow(tau)
        if (wisard_mode) {
            ig <- 1:lx
            heatm <- heat@data[,ic]
            pilev <- pile_on@data[,ic] - pile_off@data[,ic]
        }
        else {
            ig <- (1:lx)[!is.na(tau@data[,ic])]
            lig <- length(ig)
            if (lig == 0) return()

            igm1 <- ig - 1
            if (igm1[1] <1 ) igm1[1] <- ig[1]

            igp2 <- ig + 2
            if (any(igp2 > lx)) igp2[igp2 > lx] <- ig[lig]

            # Max heater voltage
            heatm <- pmax(heat@data[igm1,ic], heat@data[ig,ic],na.rm=T)

            # Max change in thermopile voltage
            pilev <- pmax(pile@data[igm1,ic], pile@data[ig,ic],na.rm=T)
            pilev <- pilev - as.vector(pile[igp2,ic])
            # browser()
        }

        Q <- (heatm^2) / (Re.tp[ic] * L.tp[ic])
        lambda[ig,ic] <<- E.lambda[ic] * Q / pilev
        NULL
    })
    
    colnames(lambda) <- paste(rep("Lambdasoil",ncol(lambda)),
        suffixes(lambda),sep="")
    lambda@units <- rep("W/(m K)",ncol(lambda))
    lambda[lambda<=0] <- NA_real_
    lambda[is.infinite(lambda)] <- NA_real_
    lambda
}

dat.Cvsoil <- function(what, derived=TRUE, cache=FALSE,...)
{
    datvar <- datVar() # requested variable name, x of dat.x
    #
    # Below 2 lines should allow this code to be used for both Wisard
    # (Lambdasoil reported by motes) and pre-Wisard deployments
    # (lambdasoil derived from above)
    #
    vars <- words(variables(),1,1)
    lambda <- dat(sub(datvar,"Lambdasoil",what,fixed=TRUE),...)
    if (is.null(lambda)) return(NULL)
    Cv <- lambda / conform(dat("asoil",...),lambda)
    sufs <- suffixes(Cv)
    colnames(Cv) <- paste(rep("Cvsoil",ncol(Cv)),sufs,sep="")
    Cv@units <- rep("J/(m3 K)",ncol(Cv))
    Cv
}
dat.asoil <- function(what,derived=TRUE,cache=FALSE,...)
{
    # TP01 manual suggests limits of 0.05e-6 and 1.0e-6 for
    # diffusivity.  The user should apply those limits
    # themselves via the clip() function.
    #

    # These 2 values implied from the TP01 manual for agar gel
    a.ref <- 0.14e-6
    dt.ref <- 19
    #
    # Below 2 lines should allow this code to be used for both Wisard (Tau63) and 
    # pre-Wisard (max) deployments
    #
    vars <- words(variables(),1,1)
    if (any(vars=="Lambdasoil")) tau <- dat("Tau63") else tau <- dat("Tau63_max")
    if (is.null(tau)) return(NULL)

    a <- a.ref * dt.ref / tau
    a[is.infinite(a)] <- NA_real_
    a[a<=0] <- NA_real_

    sufs <- suffixes(tau)
    colnames(a) <- paste(rep("asoil",ncol(a)),sufs,sep="")
    a@units <- rep("m2/s",ncol(a))
    a
}

dat.Fat_tp01 <- function(what,derived=TRUE,tsec=180,rc=0.005,rh=0.001,...)
{
    # Compute dimensionless F(at) for Hukseflux TP01 from
    # soil heat diffusivity
    # tsec: time in seconds of heating cycle
    # rh: distance of hot junctions of the thermopile from the heating
    #   wire in meters
    # rc: distance of cold junctions of the thermopile from the heating
    #   wire in meters

    datvar <- datVar() # requested variable name, x of dat.x

    x <- dat(sub(datvar,"asoil",what,fixed=TRUE),...)
    x <- x * tsec
    ok <- !is.na(x) & (x >= (rc^2 - rh^2))
    # first two terms
    x <- 1 - 1 / (8 * log(rc/rh)) *
        ((rc^2 - rh^2)/x - (rc^4 - rh^4)/(16 * x * x))
    if (any(!ok)) x[!ok] <- 1
    x@units <- rep("",ncol(x))
    x
}

