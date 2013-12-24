# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
#               Copyright (C) by UCAR
# 
dat.rho.soil = function(x,...)
{
    # Use an average bulk density for this site (using 1.4)
    istns <- dpar("stns")
    ns <- length(istns)
    x <- rep(1.4,ns)
    class(x) <- "density"
    names(x) <- rep("rho.soil",ns)
    attr(x,"dunits") <- rep("g/cm3",ns)
    attr(x,"stations") <- istns
    x
}

dat.Csoil = function(what,...)
{
    # Csoil = soil heat capacity
    #
    # Csoil is calculated from 
    #       The bulk density (gm/cm^3) of dry soil  = rho.soil
    #       The mineral particle density (gm/cm^3)  = min.dens
    #       Mineral volumetric fraction             = min.vol.frac
    #                                               = blk.dens/min.dens
    #       Organic volumetric fraction             = org.vol.frac
    #       Gravimetric water fraction              = h2o.soil
    #       Water volumetric fraction = h2o.soil * rho.soil / water.dens
    #                                 = h2o.soil * rho.soil
    #
    # The soil parameters blk.dens, min.dens, org.vol.frac must be 
    # specified here for each site.  Gravmoist varies with soil moisture
    # and is obtained from a table containing a value for each jday.

    # The values for the soil parameters at the Sabetha STORMFEST92 site
    # were derived from estimates of the soil propertites provided by
    #    Jay Ham, KSU. 
    #                    Bulk density = 1.45 g cm-3
    #                    Mineral particle density = 2.65 g cm-3
    #                    Organic material density = 1.00 g cm-3
    #                    Water density            = 1.00 g cm-3
    #                    Volumetric mineral fraction = 0.55
    #                    Volumetric organic fraction = 0.01
    #                    Volumetric water fraction   = 1.45/1.00 * rho.soil
    # 02/02: This code had assumed that h2o was gravimetric, from early ASTER
    #        deployments.  Now don't to multiply by rho.soil, since volumetric.
    # Also, I assume that 1.9e6 is rho*Cp for minerals, 2.5e6 is rho*Cp for
    # organics, and 4.2e6 is rho*Cp for water in J/(m3 C).  THIS SHOULD BE CHECKED.
    # Finally, I'm also changing the organic fraction to 5% -- more typical??
    # ...SPO
    min.vol.frac <- 0.55
    org.vol.frac <- 0.05
    h2o <- dat(expand("Qsoil",what))

    # Check units. Expect vfract or %vol
    hunits <- h2o@units
    unkunits <- is.na(match(hunits,c("%vol","vfract","vol%")))

    if (any(unkunits)) warning(paste("Unknown units for Qsoil: ",
            paste(unique(hunits[unkunits]),collapse=",")))

    # convert %vol to fraction
    ip = (hunits == "%vol")|(hunits == "vol%")
    if (any(ip))
        h2o[,ip] <- h2o[,ip] / 100

    #  rho <- dat("rho.soil")
    x <- ( 1.9*min.vol.frac + 2.5*org.vol.frac + 4.2*h2o )*1e6
    dimnames(x) <- list(NULL,expand("Csoil",h2o))
    x@units <- rep("J/(m3 C)",ncol(x))
    x
}

dat.Ssoil = function(what,sum=T,dfill=F,doderiv=F,...)
{
    # Ssoil is the time-rate of change of the soil
    # heat storage per square meter, for each layer
    # where the soil temperature was measured.
    #
    # Ssoil = Cvsoil * dTsoil/dt * dz
    #
    # Cvsoil is the volumetric heat capacity (J/(m^3 degK)),
    # dTsoil/dt is the time derivative of the soil temperature
    # and dz is the depth of the layer.
    #
    # Assuming the net horizontal heat flow is negligible,
    #   Ssoil = G1 - G2
    # where G1 is the upward heat flux into the bottom
    # of the layer, and G2 is the heat flux out of
    # the top of the layer.

    # If sum == T, the sum the Ssoil values is returned.

    if (!doderiv && !is.na(match("dTsoil_dt",words(variables(),1,1)))) {
        tx <- dat(expand("dTsoil_dt",what))
    }
    else {
        tx <- dat(expand("Tsoil",what))
        doderiv = T
    }

    res = NULL

    for (stn in unique(stations(tx))) {
        txs = select(tx,stns=stn)
        for (st in unique(sites(txs))) {

            txss = select(txs,sites=st)
            if (is.null(txss)) next
            # heights function returns negative values, in meters,
            # for soil depths
            depths <- -heights(txss)

            # re-arrange columns by increasing depth
            txss = txss[,sort.list(depths)]
            depths = sort(depths)

            if (any(is.na(txss))) {
                # interpolate if missing values
                if (dfill) {
                    dns = dimnames(txss)
                    txss@data = matrix(apply(txss@data,1,function(x,d){
                            gx = !is.na(x)
                            sgx = sum(gx)
                            # when approx rule=2, values outside of the non-NA depths will be the extreme values,
                            # not extrapolated.
                            if (sgx > 1 && sgx < length(d)) approx(d[gx],x[gx],xout=d,method="linear",rule=2)$y
                            else x
            },d=depths),ncol=length(depths),dimnames=dns,byrow=T)
                }
                else if (FALSE) {
                    # interpolation over a grid of time and depth. Probably won't work to
                    # fill in missing data.
                    # From documentation of the interp function:
                    # The spreads of x and y must be within four orders of magnitude of each
                    #     other for interp to work.
                    # So, scale the time values from -0.5 to 0.5
                    tp = as.numeric(tspar(txss))
                    tp = (tp - (tp[1] + tp[length(tp)]) / 2 ) / (tp[length(tp)] - tp[1])
                    txx@data = interp(x=tp,y=depths,z=txss@data,xo=tp,yo=depths,ncp=2,extrap=T)$z
                }
            }

            if (doderiv) {
                txss = d.by.dt(txss)
                # ......smooth d.by.dt(Tsoil) by center averaging over 35 minutes
                if (is.null(dpar("avg"))) txss = average(txss,35*60,5*60)
            }

            # determine depth of Gsoil heat flux plate
            g = select(dat(expand("Gsoil",what),derived=F),stns=stn,sites=st)
            depthg = NULL
            if (!is.null(g)) depthg = unique(-heights(g))

            if (is.null(depthg) || length(depthg) == 0) {
                if (is.null(g)) warning(paste("Gsoil not found at station=",stn,", site=\"",st,"\". Assuming 5cm as bottom limit of Tsoil",sep=""))
                else warning(paste("Gsoil depth unknown at station=",stn,", site=\"",st,"\". Assuming 5cm",sep=""))
                depthg = 0.05 # default depth in meters
            }
            if (length(depthg) > 1) {
                warning(paste("Multiple Gsoil depths at station=",stn,", site=\"",st,"\". Assuming 5cm",sep=""))
                depthg = 0.05 # default depth in meters
            }

            if (length(depths) > 1) {
                # middle values between the depths
                dmid = apply(matrix(c(depths[-length(depths)],depths[-1]),ncol=2),1,function(x){sum(x)/length(x)})

                # dz of soil that each transducer measures
                # This value will be 1.25 cm (0.0125m) for all levels of the
                # Wisard 4-prong temperature probe with transducers at depths of
                # 0.6, 1.9, 3.1, 4.4 cm
                # This also assumes the top probe measures up to the surface, depth=0, and the
                # bottom probe measures down to the depth of the soil flux plate, Gsoil.
                dz = diff(c(0,dmid,depthg))
            }
            else dz = rep(depthg,ncol(txss))

            # multiply dT/dt by dz, then sum

            # a missing (NA) temperature will result in no addition to Gsfc from that depth.
            if (sum) {
                dns = expand("dT_by_dt_dz",g)
                txss@data = matrix(apply(txss@data,1,function(x,dz){sum(x*dz,na.rm=T)},dz=dz),
                    byrow=T,ncol=length(dns),dimnames=list(NULL,dns))
                stations(txss) = stn
            }
            else {
                dns = expand("dT_by_dt_dz",txss)
                txss@data = matrix(apply(txss@data,1,function(x,dz){x*dz},dz=dz),
                    byrow=T,ncol=length(dns),dimnames=list(NULL,dns))
            }

            # get soil heat capacity (J/(m^3 degK))
            vars = words(variables(),1,1)
            if (any(vars=="Lambdasoil") || any(vars=="lambdasoil"))
                Cs = dat(expand("Cvsoil",what))
            else
                Cs = dat(expand("Csoil",what))
            Cs = select(Cs,stns=stn,sites=st)

            # Units:   J/(m^3 degK) * d(degK)/d(sec) * m =  W/m^2
            x = Cs * txss
            if (sum) dimnames(x) = list(NULL,expand("Ssoil",Cs))
            else dimnames(x) = list(NULL,expand("Ssoil",txss))

            x@units = rep("W/m^2",ncol(x))

            if (is.null(res)) res = x
            else res = Cbind(res,x)
            NULL
        }
    }
    res
}

dat.Ssoilz = function(what,dfill=F,fit=3,sum=T,...)
{
    # Like Ssoil, Ssoilz is the is the time-rate of change
    # of the soil heat storage per square meter. But it is
    # calculated from the second derivative of the temperature w.r.t. z=depth.
    #
    # Ssoilz = Lambdasoil * d2Tsoil/dz2 * dz
    #
    # dfill: Whether to fill in missing temperatures with interpolated values.
    #   Extrapolation is not done, instead, missing temperatures at the min and
    #   max depths will be replaced with the temperature at the nearest depth.
    #
    # fit: order of polynomial fit of temperature w.r.t z (soil depth)
    #    The maximum value of fit is 3 for temperatures measured at 4 depths
    # sum: whether to sum all the storage terms
    #   If fit==2,sum=F, the flux values at all depths will be equal, since the
    #   second derivative of a second order polynomial is a constant.


    if ((fit < 2 && fit != 0) || fit > 3) stop("order of polynomial fit must be 2 or 3")
    tx <- dat(expand("Tsoil",what))

    res = NULL

    for (stn in unique(stations(tx))) {
        txs = select(tx,stns=stn)
        for (st in unique(sites(txs))) {

            txss = select(txs,sites=st)
            # heights function returns negative values, in meters,
            # for soil depths
            depths <- -heights(txss)

            # re-arrange columns by increasing depth
            dseq = sort.list(depths)
            txss = txss[,dseq]
            depths = depths[dseq]

            g = select(dat(expand("Gsoil",what),derived=F),stns=stn,sites=st)
            depthg = NULL
            if (!is.null(g)) depthg = unique(-heights(g))

            if (is.null(depthg) || length(depthg) == 0) {
                if (is.null(g)) warning(paste("Gsoil not found at station=",stn,", site=\"",st,"\". Assuming 5cm as bottom limit of Tsoil",sep=""))
                else warning(paste("Gsoil depth unknown at station=",stn,", site=\"",st,"\". Assuming 5cm",sep=""))
                depthg = 0.05 # default depth in meters
            }
            if (length(depthg) > 1) {
                warning(paste("Multiple Gsoil depths at station=",stn,", site=\"",st,"\". Assuming 5cm",sep=""))
                depthg = 0.05 # default depth in meters
            }

            if (any(is.na(txss))) {
                # interpolate if missing values
                if (dfill) {
                    dns = dimnames(txss)
                    txss@data = matrix(apply(txss@data,1,function(x,d){
                            gx = !is.na(x)
                            sgx = sum(gx)
                            # when approx rule=2, values outside of the non-NA depths will be the extreme values,
                            # not extrapolated.
                            if (sgx > 1 && sgx < length(d)) approx(d[gx],x[gx],xout=d,method="linear",rule=2)$y
                            else x
            },d=depths),ncol=length(depths),dimnames=dns,byrow=T)
                }
            }

            if (fit == 0) {
                cat("depths=",paste(depths,collapse=", "),"\n")
                dz = diff(depths)
                cat("dz=",paste(dz,collapse=", "),"\n")
                rdepths = depths[c(-1,-length(depths))]
                cat("result depths=",paste(rdepths,collapse=", "),"\n")

                if (sum)
                    dns = "d2Tsoil_by_dz2_dz"
                else
                    dns = paste("d2Tsoil_by_dz2_dz.",rdepths*100,"cm",sep="")
                if (st != "") dns = paste(dns,st,sep=".")

                txss@data = matrix(apply(txss@data,1,function(y,z,dosum) {
                        # double difference
                        y = diff(diff(y)/diff(z))
                        # We don't divide by diff(z) a second time above because we're returning d2Tsoil/dz^2 * dz
                        if (dosum)
                            sum(y,na.rm=T)
                        else
                            y
            },z=depths,dosum=sum),ncol=length(dns),byrow=T,dimnames=list(NULL,dns))
            }
            else {
                if (length(depths) > 1) {
                    # middle values between the depths
                    dmid = apply(matrix(c(depths[-length(depths)],depths[-1]),ncol=2),1,function(x){sum(x)/length(x)})

                    # dz of soil that each transducer measures
                    # This value will be 1.25 cm (0.0125m) for all levels of the
                    # Wisard 4-prong temperature probe with transducers at depths of
                    # 0.6, 1.9, 3.1, 4.4 cm
                    # This also assumes the top probe measures up to the surface, depth=0, and the
                    # bottom probe measures down to the depth of the soil flux plate, Gsoil.
                    dz = diff(c(0,dmid,depthg))
                }
                else dz = depthg

                cat("depths=",paste(depths,collapse=", "),"\n")
                cat("dz=",paste(dz,collapse=", "),"\n")

                if (sum)
                    dns = "d2Tsoil_by_dz2_dz"
                else
                    dns = expand("d2Tsoil_by_dz2_dz",txss)
                if (st != "") dns = paste(dns,st,sep=".")

                if (fit == 2) {
                    txss@data = matrix(apply(txss@data,1,function(y,z,dz,dosum) {
                            # if (sum(!is.na(y)) < 4) return(y)
                            if (sum(!is.na(y)) < 3) {
                                if(dosum) return(NA_real_)
                                else return(rep(NA_real_,length(z)))
                            }
                            # 2nd order fit
                            pfit = lm(y ~ z^2 + z,data=list(y=y,z=z))
                            cs = coef(pfit)
                            # re-create points using fit
                            # sapply(z,function(z,cs){z^2*cs[2] + z*cs[3] + cs[1]},cs=cs)
                            if (dosum) sum(dz * 2 * cs[2])
                            else dz * 2 * cs[2]
            },z=depths,dz=dz,dosum=sum),ncol=length(dns),byrow=T,dimnames=list(NULL,dns))
                }
                else {
                    txss@data = matrix(apply(txss@data,1,function(y,z,dz,dosum) {
                            # if (sum(!is.na(y)) < 4) return(y)
                            if (sum(!is.na(y)) < length(z)) {
                                if(dosum) return(NA_real_)
                                else return(rep(NA_real_,length(z)))
                            }
                            # 3rd order fit
                            pfit = lm(y ~ z^3 + z^2 + z,data=list(y=y,z=z))
                            cs = coef(pfit)
                            # sapply(z,function(z,cs){z^3*cs[2] + z^2*cs[3] + z*cs[4] + cs[1]},cs=cs)
                            if (dosum) sum(dz * (6 * cs[2] * z + 2 * cs[3]))
                            else dz * (6 * cs[2] * z + 2 * cs[3])
            },z=depths,dz=dz,dosum=sum),ncol=length(dns),byrow=T,dimnames=list(NULL,dns))
                }
            }

            stations(txss) = rep(stn,ncol(txss))

            # Units:   degK/m^2 * m =  degK/m
            units(txss) = rep("degK/m",ncol(txss))

            # get soil heat conductivity (W/(m degK))
            lambda = dat(expand("Lambdasoil",what))
            lambda = select(lambda,stns=stn,sites=st)

            # Units:   W/(m degK) * degK/m =  W/m^2
            x = lambda * txss
            dimnames(x) = list(NULL,expand("Ssoilz",txss))

            x@units = rep("W/m^2",ncol(x))

            if (is.null(res)) res = x
            else res = Cbind(res,x)
            NULL
        }
    }
    res
}

dat.Gsoilz <- function(what,fit=0,dfill=F,...)
{
    # soil heat flux calculated from
    # lambda * dTsoil/dz
    # fit: order of polynomial fit of temperature w.r.t z (soil depth)
    # The maximum value of fit is 3 for temperatures measured at 4 depths

    tx <- dat(expand("Tsoil",what))

    res = NULL

    for (stn in unique(stations(tx))) {
        txs = select(tx,stns=stn)
        for (st in unique(sites(txs))) {

            txss = select(txs,sites=st)
            # heights function returns negative values, in meters,
            # for soil depths
            depths <- -heights(txss)

            # re-arrange columns by increasing depth
            dseq = sort.list(depths)
            txss = txss[,dseq]
            depths = depths[dseq]

            g = select(dat(expand("Gsoil",what),derived=F),stns=stn,sites=st)
            depthg = NULL
            if (!is.null(g)) depthg = unique(-heights(g))

            if (is.null(depthg) || length(depthg) == 0) {
                if (is.null(g)) warning(paste("Gsoil not found at station=",stn,", site=\"",st,"\". Assuming 5cm as bottom limit of Tsoil",sep=""))
                else warning(paste("Gsoil depth unknown at station=",stn,", site=\"",st,"\". Assuming 5cm",sep=""))
                depthg = 0.05 # default depth in meters
            }
            if (length(depthg) > 1) {
                warning(paste("Multiple Gsoil depths at station=",stn,", site=\"",st,"\". Assuming 5cm",sep=""))
                depthg = 0.05 # default depth in meters
            }

            if (any(is.na(txss))) {
                # interpolate if missing values
                if (dfill) {
                    dns = dimnames(txss)
                    txss@data = matrix(apply(txss@data,1,function(x,d){
                            gx = !is.na(x)
                            sgx = sum(gx)
                            # when approx rule=2, values outside of the non-NA depths will be the extreme values,
                            # not extrapolated.
                            if (sgx > 1 && sgx < length(d)) approx(d[gx],x[gx],xout=d,method="linear",rule=2)$y
                            else x
            },d=depths),ncol=length(depths),dimnames=dns,byrow=T)
                }
            }

            if (fit == 0) {
                if (length(depths) > 1) {
                    # middle values between the depths
                    rdepths = apply(matrix(c(depths[-length(depths)],depths[-1]),ncol=2),1,function(x){sum(x)/length(x)})
                }
                else rdepths = c(depths)

                cat("result depths=",paste(rdepths,collapse=", "),"\n")

                dns = paste("dTsoil_by_dz.",rdepths*100,"cm",sep="")
                if (st != "") dns = paste(dns,st,sep=".")
                txss@data = matrix(apply(txss@data,1,function(y,dz) {
                        diff(y)/dz},dz=diff(depths)),
                    ncol=length(dns),byrow=T,dimnames=list(NULL,dns))
            }
            else {
                if (length(depths) > 1) {
                    # middle values between the depths
                    dmid = apply(matrix(c(depths[-length(depths)],depths[-1]),ncol=2),1,function(x){sum(x)/length(x)})
                    rdepths = c(0,dmid,depthg)
                }
                else rdepths = c(0,depths)

                cat("result depths=",paste(rdepths,collapse=", "),"\n")

                dns = paste("dTsoil_by_dz.",rdepths*100,"cm",sep="")
                if (st != "") dns = paste(dns,st,sep=".")

                if (fit == 2) {
                    txss@data = matrix(apply(txss@data,1,function(y,z,ndepths) {
                            if (sum(!is.na(y)) < length(z)) return(rep(NA_real_,length(ndepths)))
                            # 2nd order fit
                            pfit = lm(y ~ z^2 + z,data=list(y=y,z=z))
                            cs = coef(pfit)
                            # re-create points using fit
                            # sapply(z,function(z,cs){z^2*cs[2] + z*cs[3] + cs[1]},cs=cs)
                            # first derivative of the fit wrt z, evaluated at ndepths
                            (2 * cs[2] * ndepths + cs[3])
                        },z=depths,ndepths=rdepths),ncol=length(dns),byrow=T,dimnames=list(NULL,dns))
                }
                else {
                    txss@data = matrix(apply(txss@data,1,function(y,z,ndepths) {
                            if (sum(!is.na(y)) < length(z)) return(rep(NA_real_,length(ndepths)))
                            # 3rd order fit
                            pfit = lm(y ~ z^3 + z^2 + z,data=list(y=y,z=z))
                            cs = coef(pfit)
                            # sapply(z,function(z,cs){z^3*cs[2] + z^2*cs[3] + z*cs[4] + cs[1]},cs=cs)
                            # first derivative of the fit wrt z, evaluated at ndepths
                            (3 * cs[2] * ndepths^2 + 2 * cs[3] * ndepths + cs[4])
                        },z=depths,ndepths=rdepths),ncol=length(dns),byrow=T,dimnames=list(NULL,dns))
                }
            }
            stations(txss) = rep(stn,length(dns))

            # Units:   degK/m
            units(txss) = rep("degK/m",ncol(txss))

            # get soil heat conductivity (W/(m degK))
            lambda = dat(expand("Lambdasoil",what))
            lambda = select(lambda,stns=stn,sites=st)

            # Units:   W/(m degK) * degK/m =  W/m^2
            x = lambda * txss
            dimnames(x) = list(NULL,expand("Gsoilz",txss))

            x@units = rep("W/m^2",ncol(x))

            if (is.null(res)) res = x
            else res = Cbind(res,x)
            NULL
        }
    }
    res
}

dat.Gsfc <- function(what,...)
{
    # sign convention for Gsoil and Gsfc:
    # positive is upward energy flux
    G <- dat(expand("Gsoil",what))
    S <- conform(dat(expand("Ssoil",what)),G)
    x <- G - S
    dimnames(x) <- list(NULL,expand("Gsfc",S))
    x
}

philip = function(lambda,Tp=3.93,Dp=38.56,lp=1.22)
{
    # Philip correction for heat flux plates
    #
    # Tp is thickness of heat flux plate, Dp is the diameter.
    # lp is the conductivity of the plate, in W/m/K
    #
    # Default values above are for a REBS HFT3.
    #
    # lambda is the conductivity of the soil
    #
    # Gm is the flux measured by the flux plate
    # G is the corrected flux:
    #
    # Gm/G = 1 / ( 1 - 1.92 * T/D * (1 - lambda/lp))

    1 / ( 1 - 1.92 * Tp / Dp * (1 - lambda / lp))
}

dat.Gsoil = function(what,lc=0.906,Tp=3.93,Dp=38.56,lp=1.22,derived=TRUE,...)
{
    G = dat(what,derived=FALSE,...)

    if (!is.null(dpar("robust")) && !dpar("robust")) {
        # If we know the conductivity of the calibration medium,
        # then apply the inverse of the Philip correction at that
        # conductivity.
        if (!missing(lc) && !is.na(lc)) {
            if (is(lc,"nts")) lc = conform(lc,G)
            G = G * philip(lc,Tp=Tp,Dp=Dp,lp=lp)
        }

        # then apply the Philip correction with the measured soil conductivity.
        lambda = dat(expand("Lambdasoil",what),...)
        lambda = conform(lambda,G)
        G = G / philip(lambda,Tp=Tp,Dp=Dp,lp=lp)
    }
    G
}
