# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsdng" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

readin.data <-
function (sounding, varname) 
{
    if (varname=="Temperature")           data = sounding$temp
    if (varname=="Dewpoint Temperature")  data = sounding$dewpt
    if (varname=="Relative Humidity")     data = sounding$rh
    if (varname=="Wind Speed")            data = sounding$wspd
    if (varname=="Wind Direction")        data = sounding$wdir
    if (varname=="dZ/dt")                 data = sounding$dz
    if (varname=="Latitude")              data = sounding$lat
    if (varname=="Longitude")             data = sounding$lon
    if (varname=="Geopotential Height")   data = sounding$gp.alt

    return(data)
}

