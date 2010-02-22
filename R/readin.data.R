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

