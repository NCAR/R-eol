sounding.verticalinterp <-
function(ftype="qc", fdir=NULL, varname="temperature", YTYPE="height", sonderange=NA)
{
  varname = tolower(varname)
  ftype = tolower(ftype)
  YTYPE = tolower(YTYPE)

  if (is.null(fdir))
    stop(paste("PLEASE SPECIFY THE LOCATION OF", ftype, "SOUNDING DATA !!!"))
  fs = system(paste("cd ", fdir, ";ls D*", sep=""), TRUE)
  if (length(sonderange) == 1) {
    if (!is.na(sonderange))
      stop("sonderange IS A INTEGRAL VECTOR OF LENGTH 2, INDICATING THE START/END NUMBER OF SOUNDINGS TO BE INTERPOLATED RESPECTIVELY !!!")
  }
  if (length(sonderange) > 2) 
    stop("sonderange IS A INTEGRAL VECTOR OF LENGTH 2, INDICATING THE START/END NUMBER OF SOUNDINGS TO BE INTERPOLATED RESPECTIVELY !!!")
  if (length(sonderange) == 2) {
    if (sonderange[1] > sonderange[2])
      stop("sonderange IS A INTEGRAL VECTOR OF LENGTH 2, INDICATING THE START/END NUMBER OF SOUNDINGS TO BE INTERPOLATED RESPECTIVELY !!!")
    fs = fs[sonderange[1]:sonderange[2]]
  }
  nf = length(fs)

  if (YTYPE == "pressure")	yout = seq(0,1200,5)
  if (YTYPE == "height")	yout = seq(0,30000,75)
  ny = length(yout)
  ## return value each row is a sounding record
  ## first row would be pressure or height range
  xout = matrix(NA, nrow=nf, ncol=ny)
 
  for(i in 1:nf)
  {
    print(paste(i, fs[i]))
    f = paste(fdir, fs[i], sep="/")
    if (ftype == "raw") {
      sounding = read.raw.Dfile(f)
      #-- get only valid data --#
      id  = (sounding[, 2] == "S00" | sounding[, 2] == "S01")
      sounding = sounding[id, ]
    }
    if (ftype == "qc")
      sounding = read.qc.eolfile(f, nskip=14)
    if (varname=="t" | varname=="temp" | varname=="temperature")
      data = sounding$temp
    if (varname=="rh" | varname=="rhum" | varname=="relativehumidity")
      data = sounding$rh
    if (varname=="w" | varname=="wspd" | varname=="windspeed") {
      varname = "wspd"
      data = sounding$wspd
    }
    if (varname=="dz" | varname=="dz/dt" | varname=="vvel" | varname=="verticalvelocity") {
      varname = "dz/dt"
      if (ftype == "qc")	data = sounding$dz
      if (ftype == "raw")	data = sounding$vvel
    }
    if (YTYPE == "pressure")
      y = sounding$p
    if (YTYPE == "height")
      y = sounding$gp.alt
    if ((varname=="wspd" | varname=="dz/dt") & ftype == "raw")
      ind = (!is.na(y) & !is.na(data) & sounding[,2] != "S01")
    else
      ind = (!is.na(y) & !is.na(data))
    yin = y[ind]
    xin = data[ind]
    ndt = length(yin)
    if (ndt < 2) {
      print (paste("THERE IS NO", varname, "DATA OR", YTYPE, "INFORMATION !"))
      next
    }

    ###--- interpolate at vertical levels(pressure:5mb; height:75m) ---###
    ybeg = yin[1]
    yend = yin[length(yin)]
    ######## see file AF-C130/D-files/D20080815_232648_P.3
    if (ybeg<yend & max(yin)>yend) {
      id   = match(max(yin), yin)
      yin  = yin[1:id]
      xin  = xin[1:id]
      yend = yin[length(yin)]
    }
    ## find interpolation range 
    ## upsonde:height from top to bottom getting bigger
    ##         pressure from top to bottom getting smaller 
    ## dropsode&driftsonde:height getting smaller
    ##                     pressure from top to bottom getting bigger 
    iy2 = ny
    if (ybeg < yend) {
      for (k1 in 1:ny) {
        if (yout[k1] > ybeg) {
          iy1 = k1
          break
        }
      }
      for (k2 in 1:ny) {
        if (yout[k2] > yend) {
          iy2 = k2 - 1
          break
        }
      }
    }
    if (ybeg > yend) {
      for (k1 in 1:ny) {
        if (yout[k1] > yend) {
          iy1 = k1
          break
        }
      }
      for (k2 in 1:ny) {
        if (yout[k2] > ybeg) {
          iy2 = k2 - 1
          break
        }
      }
    }
    ypoints = yout[iy1:iy2]
    if (YTYPE == "pressure") {
      yin = log(yin)
      ypoints = log(ypoints)
    }
    xpoints = approx(yin, xin, ypoints)$y
    for (ik in 1:length(xpoints))
      xout[i,iy1+ik-1] = xpoints[ik]
  }
  out = rbind(yout, xout)
  ## not missing
  ind = (!is.na(out[2:nf,]))
  ## not whole level missing
  indx = FALSE
  for (k in 1:ny)	indx[k] = any(ind[ ,k])
  out = out[ ,indx]
  return(out)
}

