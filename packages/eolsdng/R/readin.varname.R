readin.varname <-
function (varname)
{
  var = list()
  varname = tolower(varname)
  if (varname=="t" | varname=="temp" | varname=="temperature") {
    var$name = "Temperature"
    var$unit = "C"
  }
  else if (varname=="dpt" | varname=="dewpt" | varname=="dewpoint temperature") {
    var$name = "Dewpoint Temperature"
    var$unit = "C"
  }
  else if (varname=="rh" | varname=="rhum" | varname=="relative humidity") {
    var$name = "Relative Humidity"
    var$unit = "%"
  }
  else if (varname=="w" | varname=="wspd" | varname=="wind speed") {
    var$name = "Wind Speed"
    var$unit = "m/s"
  }
  else if (varname=="wdir" | varname=="wind direction") {
    var$name = "Wind Direction"
    var$unit = "deg"
  }
  else if (varname=="dz" | varname=="dz/dt" | varname=="vvel" | varname=="vertical velocity") {
    var$name = "dZ/dt"
    var$unit = "m/s"
  }
  else if (varname=="lat" | varname=="latitude") {
    var$name = "Latitude"
    var$unit = "deg"
  }
  else if (varname=="lon" | varname=="longitude") {
    var$name = "Longitude"
    var$unit = "deg"
  }
  else if (varname=="ht" | varname=="height" | varname=="gph" | varname=="geoheight" | varname=="geopotential height") {
    var$name = "Geopotential Height"
    var$unit = "m"
  }
  else
    stop(paste(varname, "IS NOT SUPPORTTED BY PACKAGE !!!"))

  return(var)
}
