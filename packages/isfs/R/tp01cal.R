# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# Generic [hopefully!] version which knows what to do given a mapping
# of probe to location (provided by tp01.stns in project.init.q):
#
#tp01.stns = c(5,9,8,1,2,3,4,6,7) # HVAMS03
#tp01.stns = c(0,1,0,0,0,2,0,0,3) # TREX
#tp01.stns = c(0,3,5,0,0,10,0,0,2) # CuPIDO
#tp01.stns = c(".rim",".el",".wl",0,".flr",".wu",".sw",".eu",0) # METCRAX [suffixes]
#
# This is my implementation of a routine to obtain TP01 data.
# It:
# - grabs Vheat and Vpile
# - computes (thermal diffusivity), lambda (thermal conductivity), and Cp (heat capacity)
#   (needs sensor calibration values)
# - creates an dat-object
#

dat.lambdasoil = function(what,derived=TRUE,cache=F,...)
{
    #
    # The calibrations for all TP01 probes (by serial number):
    # (Note (9/13/2010): With Wisard, should never need this code, but decided
    # to add probes 589--592 for completeness anyway.)
    #
    #       s/n     Re      E.lambda (e-6)  L.tp
    tp01 = matrix(c(
            200234,  14.7,     97.9,  0.060,
            200235,  14.8,    104.6,  0.060,
            200236,  14.8,    104.9,  0.060,
            200238,  15.4,    154.0,  0.060,
            200239,  16.2,    175.8,  0.060,
            200240,  15.0,    160.8,  0.060,
            200241,  16.2,    141.5,  0.060,
            200242,  15.9,    152.4,  0.060,
            200243,  15.6,    160.5,  0.060,
            200589,	 16.1,    158.0,  0.060,
            200590,	 15.4,    136.0,  0.060,
            200591,	 16.7,    140.0,  0.060,
            200592,	 15.5,    162.0,  0.060),
        ncol=4,byrow=T)
    dimnames(tp01)[[2]] = c("SN","Re","E.lambda","L")

    tau = dat("Tau63_max")
    if (is.null(tau)) return(NULL)
    tau[tau<0] = NA_real_
    pile = dat("Vpile_max")
    heat = dat("Vheat_max")
    stns = stations(tau)
    nstns = length(stns)
    sufs = suffixes(tau)	
    #
    # match recorded values by either stations or suffixes
    #
    if (all(stns==0)) ix = match(sufs,tp01.stns)
    else ix = match(stns,tp01.stns)

    Re.tp = tp01[ix,"Re"]
    E.lambda = tp01[ix,"E.lambda"]*1e-6
    L.tp = tp01[ix,"L"]

    lambda = pile*NA_real_

    for (i in 1:nstns) {
        nt = nrow(tau[,i])
        ig = (1:nt)[!is.na(tau[,i])]
        if (sum(ig)!=0) {
            igm1 = ig-1
            if (igm1[1]<1) igm1[1] = ig[1]
            igp2 = ig+2
            ng = length(ig)
            if (any(igp2>nt)) igp2[igp2>nt] = ig[ng]

            il = as.logical(heat@data[igm1,i]>heat@data[ig,i])
            il[is.na(il)] = F
            heatm = as.vector(heat[ig,i])
            heatm[il] = heat[igm1,i][il]

            il = as.logical(pile@data[igm1,i]>pile@data[ig,i])
            il[is.na(il)] = F
            pilem = as.vector(pile[ig,i])
            pilem[il] = pile[igm1,i][il]
            pilem = pilem - as.vector(pile[igp2,i])

            Q = (heatm^2)/(Re.tp[i]*L.tp[i])
            lambda[ig,i] = E.lambda[i]*Q/(pilem*0.001)
        }
    }
    dimnames(lambda)[[2]] =
    paste(rep("lambdasoil",ncol(lambda)),sufs,sep="")
    lambda@units = rep("W/(m K)",nstns)
    lambda[lambda<=0] = NA_real_
    lambda[is.infinite(lambda)] = NA_real_
    lambda
}

dat.Cvsoil = function(what, cache=F,...) {
    #
    # Below 2 lines should allow this code to be used for both Wisard
    # (Lambdasoil reported by motes) and pre-Wisard deployments
    # (lambdasoil derived from above)
    #
    vars = words(variables(),1,1)
    if (any(vars=="Lambdasoil")) lambda = dat("Lambdasoil") else lambda = dat("lambdasoil")
    if (is.null(lambda)) return(NULL)
    Cv = lambda/dat("asoil")
    sufs = suffixes(Cv)
    dimnames(Cv)[[2]] = paste(rep("Cvsoil",ncol(Cv)),sufs,sep="")
    Cv@units = rep("J/(m3 K)",ncol(Cv))
    Cv
}
dat.asoil = function(what,derived=TRUE,cache=F,...)
{
    # TP01 manual suggests limits of 0.05e-6 and 1.0e-6 for
    # diffusivity.  The user should apply those limits
    # themselves via the clip() function.
    #

    # These 2 values implied from the TP01 manual for agar gel
    a.ref = 0.14e-6
    dt.ref = 19
    #
    # Below 2 lines should allow this code to be used for both Wisard (Tau63) and 
    # pre-Wisard (max) deployments
    #
    vars = words(variables(),1,1)
    if (any(vars=="Lambdasoil")) tau = dat("Tau63") else tau = dat("Tau63_max")
    if (is.null(tau)) return(NULL)

    a = a.ref * dt.ref / tau
    a[is.infinite(a)] = NA_real_
    a[a<=0] = NA_real_

    sufs = suffixes(tau)
    dimnames(a)[[2]] = paste(rep("asoil",ncol(a)),sufs,sep="")
    a@units = rep("m2/s",ncol(a))
    a
}

Fat.tp01 = function(a,tsec=180,rc=0.005,rh=0.001)
{
    # Compute dimensionless F(at) for Hukseflux TP01
    # a: soil heat diffusivity
    # tsec: time in seconds of heating cycle
    # rh: distance of hot junctions of the thermopile from the heating wire in meters
    # rc: distance of cold junctions of the thermopile from the heating wire in meters
    at = a * tsec
    ok = !is.na(at) & (at >= (rc^2 - rh^2))
    # first two terms
    fat = 1 - 1 / (8 * log(rc/rh)) * ((rc^2 - rh^2)/at - (rc^4 - rh^4)/(16 * at * at))
    if (any(!ok)) fat[!ok] = 1
    fat
}

dat.Lambdasoil = function(what,derived=TRUE,...)
{
    # Compute F(a*t), where a is the soil diffusivity.
    x = dat(what,derived=FALSE,...)
    if (is.null(x)) return(NULL)
    asoil = dat(expand("asoil",what),...)
    x * conform(Fat.tp01(asoil),x)
}
