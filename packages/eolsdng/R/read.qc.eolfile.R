read.qc.eolfile <-
function(file=stop("'file' must be specified"), nskip = 14)
{
  #---------------------------------------------------------
  # Read in the data, skip the header, and name the variable
  #---------------------------------------------------------
  if (length(system(paste("ls", file), TRUE)) < 1)
    stop(paste("THE FILE", file, "DOES NOT EXIST"))

  na.strs = c("-999.00", "-999.000000")
  d = read.table(file, skip = nskip, na.strings=na.strs)
  time   = d$V1
  utc.hh = d$V2
  utc.mm = d$V3
  utc.ss = d$V4
  press  = d$V5
  temp   = d$V6
  dewpt  = d$V7
  rhum   = d$V8
  uwind  = d$V9
  vwind  = d$V10
  wspd   = d$V11
  wdir   = d$V12
  dz     = d$V13
  gp.alt = d$V14
  lon    = d$V15
  lat    = d$V16
  gps.alt= d$V17

  newdata = data.frame(time, utc.hh, utc.mm, utc.ss, p = press, temp, dewpt, rh = rhum, u = uwind, v = vwind, wspd, wdir, dz, gp.alt, lon, lat, gps.alt)
  return(newdata)
}

