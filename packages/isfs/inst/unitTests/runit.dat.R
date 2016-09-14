# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolts" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

setup_test <- function(pkg="isfs")
{
    options(time.zone="US/Mountain")
    dpar(start="2016 sep 11 19:17:30",lenmin=50)

    find_datasets(path=system.file("unitTests", package=pkg),pattern="netcdf")

    load(file.path(system.file("unitTests", package=pkg),
            "RData","Vazimuth.RData"), envir=.GlobalEnv)

}

test.density <- function()
{
    if (length(datasets(warn=FALSE)) == 0) setup_test()

    dataset("noqc_instrument")

    stns <- 2:3
    dpar(stns=stns)

    x <- dat("rhoAir")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    x <- dat("rhoDry")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    return()
}

test.dimensionless <- function()
{

    if (length(datasets(warn=FALSE)) == 0) setup_test()

    dataset("noqc_instrument")
    dpar(coords="geo")

    stns <- 2:3
    dpar(stns=stns)

    x <- dat("Cd")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    # Commented, requires Tsfc
    # x <- dat("Ct")
    # checkEquals(ncol(x),length(stns))
    # checkEquals(nrow(x),11)

    for (var in c("sigma_w/u*","sigma_u/u*","sigma_v/u*","sigma_tc/tc*",
        "us'tc'","sigma_dir","r_uw","r_wtc","r_utc","uw_tilt_err")) {
        x <- dat(var)
        checkEquals(ncol(x),length(stns))
        checkEquals(nrow(x),11)
    }

    if (FALSE) {
        # These require 3rd moments
        for (var in c("S_u","S_w","S_tc","S_h2o")) {
            x <- dat(var)
            checkEquals(ncol(x),length(stns))
            checkEquals(nrow(x),11)
        }
    }

    return()
}

test.fastH2OSep <- function()
{
    if (length(datasets(warn=FALSE)) == 0) setup_test()

    stns <- 2:3
    dpar(stns=stns)

    x <- dat("fastH2OSep")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),2)

    return()
}

test.fluxes <- function()
{
    if (length(datasets(warn=FALSE)) == 0) setup_test()

    stns <- 2:3
    dpar(stns=stns)

    cat("dat(\"u't'\")","\n")
    x <- dat("u't'")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    cat("dat(\"v't'\")","\n")
    x <- dat("v't'")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    cat("dat(\"w't'\")","\n")
    x <- dat("w't'")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    cat("dat(\"u'tnew'\")","\n")
    x <- dat("u'tnew'")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    cat("dat(\"v'tnew'\")","\n")
    x <- dat("v'tnew'")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    cat("dat(\"w'tnew'\")","\n")
    x <- dat("w'tnew'")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    cat("dat(\"u'mr'\")","\n")
    x <- dat("u'mr'")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    cat("dat(\"v'mr'\")","\n")
    x <- dat("v'mr'")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    cat("dat(\"w'mr'\")","\n")
    x <- dat("w'mr'")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    cat("dat(\"u'h2o'\")","\n")
    x <- dat("u'h2o'")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    cat("dat(\"v'h2o'\")","\n")
    x <- dat("v'h2o'")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    cat("dat(\"w'h2o'\")","\n")
    x <- dat("w'h2o'")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    cat("dat(\"H\")","\n")
    x <- dat("H")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    cat("dat(\"H.dry\")","\n")
    x <- dat("H.dry")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    cat("dat(\"LE\")","\n")
    x <- dat("LE")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    cat("dat(\"Lv\")","\n")
    x <- dat("Lv")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    cat("dat(\"BR\")","\n")
    x <- dat("BR")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    cat("dat(\"Scorr\")","\n")
    x <- dat("Scorr")
    if (is(x,"dat")) {
        checkEquals(ncol(x),length(stns))
        checkEquals(nrow(x),11)
    }

    cat("dat(\"TKE\")","\n")
    x <- dat("TKE")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    cat("dat(\"w'co2'\")","\n")
    x <- dat("w'co2'")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    return()
}

test.krypton <- function()
{
    return()
}

test.licor <- function()
{
    if (length(datasets(warn=FALSE)) == 0) setup_test()

    stns <- 2:3
    dpar(stns=stns)

    cat("dat(\"licor7500Sep\")","\n")
    x <- dat("licor7500Sep")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),2)

    return()
}

test.scaling <- function()
{

    if (length(datasets(warn=FALSE)) == 0) setup_test()

    dataset("noqc_instrument")
    dpar(coords="geo")

    stns <- 2:3
    dpar(stns=stns)

    cat("dat(\"L\")","\n")
    x <- dat("L")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    cat("dat(\"u*\")","\n")
    x <- dat("u*")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    cat("dat(\"z0\")","\n")
    x <- dat("z0")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    cat("dat(\"z0raw\")","\n")
    x <- dat("z0raw")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    cat("dat(\"Draw\")","\n")
    x <- dat("Draw")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),11)

    cat("dat(\"heightSonic\")","\n")
    x <- dat("heightSonic")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),2)

    cat("dat(\"heightProp\")","\n")
    x <- dat("heightProp")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),2)

    cat("dat(\"D\")","\n")
    x <- dat("D")
    checkEquals(ncol(x),length(stns))
    checkEquals(nrow(x),2)

    return()
}

test.soil <- function()
{

    if (length(datasets(warn=FALSE)) == 0) setup_test()

    dpar(stns=0)

    cat("dat(\"Csoil\")","\n")
    x <- dat("Csoil")
    cat("ncol(Csoil)=",ncol(x),"\n")
    cat("colnames(Csoil)=",colnames(x),"\n")
    checkEquals(ncol(x),2)
    checkEquals(nrow(x),11)

    cat("dat(\"Ssoil\")","\n")
    x <- dat("Ssoil")
    cat("ncol(Ssoil)=",ncol(x),"\n")
    cat("colnames(Ssoil)=",colnames(x),"\n")
    checkEquals(ncol(x),2)
    checkEquals(nrow(x),11)

    cat("dat(\"Ssoilz\")","\n")
    x <- dat("Ssoilz")
    cat("ncol(Ssoilz)=",ncol(x),"\n")
    cat("colnames(Ssoilz)=",colnames(x),"\n")
    checkEquals(ncol(x),2)
    checkEquals(nrow(x),11)

    cat("dat(\"Gsoilz\")","\n")
    x <- dat("Gsoilz")
    cat("ncol(Gsoilz)=",ncol(x),"\n")
    cat("colnames(Gsoilz)=",colnames(x),"\n")
    checkEquals(ncol(x),6)
    checkEquals(nrow(x),11)

    cat("dat(\"Gsfc\")","\n")
    x <- dat("Gsfc")
    cat("ncol(Gsfc)=",ncol(x),"\n")
    checkEquals(ncol(x),2)
    checkEquals(nrow(x),11)

    cat("dat(\"Gsoil\")","\n")
    x <- dat("Gsoil")
    cat("ncol(Gsoil)=",ncol(x),"\n")
    checkEquals(ncol(x),2)
    checkEquals(nrow(x),11)

    return()
}

test.rad <- function()
{

    if (length(datasets(warn=FALSE)) == 0) setup_test()

    dataset("noqc_instrument")

    stns <- 0
    dpar(stns=stns)

    x <- dat("Rsw")
    cat("colnames(Rsw)=",colnames(x),"\n")
    checkEquals(ncol(x),8)
    checkEquals(nrow(x),11)

    x <- dat("Rlw")
    checkEquals(ncol(x),8)
    checkEquals(nrow(x),11)

    x <- dat("Rlw.net")
    checkEquals(ncol(x),4)
    checkEquals(nrow(x),11)

    x <- dat("Rsw.net")
    checkEquals(ncol(x),4)
    checkEquals(nrow(x),11)

    x <- dat("Rsum")
    checkEquals(ncol(x),4)
    checkEquals(nrow(x),11)

    x <- dat("albedo")
    checkEquals(ncol(x),4)
    checkEquals(nrow(x),11)

    x <- dat("Tsfc")
    checkEquals(ncol(x),4)
    checkEquals(nrow(x),11)

    x <- dat("Tsky")
    checkEquals(ncol(x),4)
    checkEquals(nrow(x),11)

    return()
}

test.tp01 <- function()
{

    if (length(datasets(warn=FALSE)) == 0) setup_test()

    dpar(stns=0)

    cat("dat(\"Lambdasoil\")","\n")
    x <- dat("Lambdasoil")
    cat("ncol(Lambdasoil)=",ncol(x),"\n")
    cat("colnames(Lambdasoil)=",colnames(x),"\n")
    checkEquals(ncol(x),2)
    checkEquals(nrow(x),11)

    cat("dat(\"Cvsoil\")","\n")
    x <- dat("Cvsoil")
    cat("ncol(Cvsoil)=",ncol(x),"\n")
    cat("colnames(Cvsoil)=",colnames(x),"\n")
    checkEquals(ncol(x),2)
    checkEquals(nrow(x),11)

    cat("dat(\"asoil\")","\n")
    x <- dat("asoil")
    cat("ncol(asoil)=",ncol(x),"\n")
    cat("colnames(asoil)=",colnames(x),"\n")
    checkEquals(ncol(x),2)
    checkEquals(nrow(x),11)

    cat("dat(\"Fat_tp01\")","\n")
    x <- dat("Fat_tp01")
    cat("ncol(Fat_tp01)=",ncol(x),"\n")
    cat("colnames(Fat_tp01)=",colnames(x),"\n")
    checkEquals(ncol(x),2)
    checkEquals(nrow(x),11)

    return()
}

test.water <- function()
{

    if (length(datasets(warn=FALSE)) == 0) setup_test()

    dataset("noqc_instrument")

    stns <- 2:3
    dpar(stns=stns)

    cat("dat(\"satvp\")","\n")
    x <- dat("satvp")
    cat("ncol(satvp)=",ncol(x),"\n")
    cat("colnames(satvp)=",colnames(x),"\n")
    checkEquals(ncol(x),2)
    checkEquals(nrow(x),11)

    cat("dat(\"RH.ice\")","\n")
    x <- dat("RH.ice")
    cat("ncol(RH.ice)=",ncol(x),"\n")
    cat("colnames(RH.ice)=",colnames(x),"\n")
    checkEquals(ncol(x),2)
    checkEquals(nrow(x),11)

    cat("dat(\"H2O\")","\n")
    x <- dat("H2O")
    cat("ncol(H2O)=",ncol(x),"\n")
    cat("colnames(H2O)=",colnames(x),"\n")
    checkEquals(ncol(x),2)
    checkEquals(nrow(x),11)

    cat("dat(\"MR\")","\n")
    x <- dat("MR")
    cat("ncol(MR)=",ncol(x),"\n")
    cat("colnames(MR)=",colnames(x),"\n")
    checkEquals(ncol(x),2)
    checkEquals(nrow(x),11)

    cat("dat(\"mr\")","\n")
    x <- dat("mr")
    cat("ncol(mr)=",ncol(x),"\n")
    cat("colnames(mr)=",colnames(x),"\n")
    checkEquals(ncol(x),2)
    checkEquals(nrow(x),11)

    cat("dat(\"Q\")","\n")
    x <- dat("Q")
    cat("ncol(Q)=",ncol(x),"\n")
    cat("colnames(Q)=",colnames(x),"\n")
    checkEquals(ncol(x),2)
    checkEquals(nrow(x),11)

    cat("dat(\"Tc\")","\n")
    x <- dat("Tc")
    cat("ncol(Tc)=",ncol(x),"\n")
    cat("colnames(Tc)=",colnames(x),"\n")
    checkEquals(ncol(x),2)
    checkEquals(nrow(x),11)

    cat("dat(\"Tdew\")","\n")
    x <- dat("Tdew")
    cat("ncol(Tdew)=",ncol(x),"\n")
    cat("colnames(Tdew)=",colnames(x),"\n")
    checkEquals(ncol(x),2)
    checkEquals(nrow(x),11)

    cat("dat(\"evap\")","\n")
    x <- dat("evap")
    cat("ncol(evap)=",ncol(x),"\n")
    cat("colnames(evap)=",colnames(x),"\n")
    checkEquals(ncol(x),2)
    checkEquals(nrow(x),11)

    cat("dat(\"h2o\")","\n")
    x <- dat("h2o")
    cat("ncol(h2o)=",ncol(x),"\n")
    cat("colnames(h2o)=",colnames(x),"\n")
    checkEquals(ncol(x),2)
    checkEquals(nrow(x),11)

    return()
}

test.winds <- function()
{

    if (length(datasets(warn=FALSE)) == 0) setup_test()

    dataset("noqc_instrument")

    stns <- 2:3
    dpar(stns=stns)

    dpar(coords="instrument")

    u <- dat("u")
    checkEquals(ncol(u),length(stns))
    checkEquals(nrow(u),11)

    uu <- dat("u",derived=FALSE)
    checkEquals(uu,u)

    vaz <- dat("Vazimuth")
    checkEquals(ncol(vaz),length(stns))

    dpar(coords="geo")
    u <- dat("u")
    checkEquals(ncol(u),length(stns))

    x <- dat("spd")
    checkEquals(ncol(x),length(stns))

    x <- dat("dir")
    checkEquals(ncol(x),length(stns))

    x <- dat("Spd")
    checkEquals(ncol(x),length(stns))

    x <- dat("Dir")
    checkEquals(ncol(x),length(stns))

    stns <- 0
    dpar(stns=stns)

    x <- dat("spd")
    checkEquals(ncol(x),5)

    x <- dat("dir")
    checkEquals(ncol(x),5)

    return()
}

