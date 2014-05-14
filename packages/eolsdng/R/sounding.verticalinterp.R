sounding.verticalinterp <-
function(ftype="qc", fdir=NULL, varname="temperature", YTYPE="height", sonderange=NA)
{
  var = readin.varname(varname)
  varname = var$name
  ftype = tolower(ftype)
  YTYPE = tolower(YTYPE)
  if (YTYPE!="pressure" & YTYPE!="height")
    stop("Y-axis CAN ONLY BE pressure OR height !!!")
  if ((varname=="Geopotential Height") & YTYPE!="pressure")
    stop("Y-axis HAS TO BE pressure TO INTERPOLATE height !!!")

  fs = check.files(fdir)
  check.sonderange(sonderange, length(fs))
  if (length(sonderange) == 2)
    fs = fs[sonderange[1]:sonderange[2]]
  yyyy = substring(fs, 2, 5)
  mm   = substring(fs, 6, 7)
  dd   = substring(fs, 8, 9)
  hh   = substring(fs,11,12)
  min  = substring(fs,13,14)
  lautime = paste("D", yyyy, mm, dd, hh, min, sep = "")
  nf = length(fs)

  if (YTYPE == "pressure")	yout = seq(0,1200,5)
  if (YTYPE == "height")	yout = seq(0,30000,75)
  ny = length(yout)
  ## return value each row is a sounding record
  ## first row would be pressure or height range
  ## first column would be sounding time information
  xout = matrix(NA, nrow=nf, ncol=ny)
 
  for(i in 1:nf)
  {
    print(paste(i, fs[i]))
    f = paste(fdir, fs[i], sep="/")
    sounding = readin.file(ftype, f)
    if (ftype == "raw") {
      #-- get only valid data --#
      id  = (sounding[, 2] == "S00" | sounding[, 2] == "S01")
      sounding = sounding[id, ]
    }
    data = readin.data(sounding=sounding, varname=varname)

    if (YTYPE == "pressure")
      y = sounding$p
    if (YTYPE == "height")
      y = sounding$gp.alt
    if ((varname=="Wind Speed" | varname=="Wind Direction" | varname=="dZ/dt") & ftype == "raw")
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
  ## not missing
  ind = (!is.na(xout))
  ## not whole level missing
  indx = FALSE
  for (k in 1:ny)	indx[k] = any(ind[ ,k])
  out = xout[ ,indx]
  colnames(out) = yout[indx]
  rownames(out) = lautime
  return(out)
}
