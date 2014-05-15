# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsdng" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

get.sounding.allLines <-
function(ftype="qc", fdir=NULL, varname="temperature", YTYPE="pressure", projsonde=NULL, sonderange=NA, legendxy=c(-30,0), xrange=c(-30,105), yrange=c(1000,0))
{
  var = readin.varname(varname)
  varname = var$name
  unit = var$unit
  fs = check.files(fdir)
  YTYPE = check.YTYPE(YTYPE)
  print(paste("Please make sure ONLY",toupper(ftype),"data under", fdir, "!!!"))
  
  check.sonderange(sonderange, length(fs))
  if (length(sonderange) == 2)
    fs = fs[sonderange[1]:sonderange[2]]
  nf = length(fs)

  yyyy = substring(fs, 2, 5)
  mm   = substring(fs, 6, 7)
  dd   = substring(fs, 8, 9)
  hh   = substring(fs,11,12)
  min  = substring(fs,13,14)
  lautime = paste("D", yyyy, mm, dd, hh, min, sep = "")

  system("rm -f allLines.ps")
  postscript(file="allLines.ps", paper="letter", horizontal=TRUE)
  mycols = c("black","red","green","blue","purple","cyan","orange","gold","gray","pink","violet","brown","magenta","chocolate","tomato","darkgreen")
  if (nf > length(mycols))
    mycolors = rep(mycols, round(nf/length(mycols)+0.5, 0))
  else
    mycolors = mycols

  if (YTYPE == "pressure") {
    if (is.null(yrange))	yrange = c(1000, 0)
    ystr = "Pressure (hPa)"
  }
  if (YTYPE == "height") {
    if (is.null(yrange))	yrange = c(0, 10)
    ystr = "Geopotential Height (km)"
  }
  xstr = paste(varname, "(", unit, ")")
  plot(0,0, type="n", ylim=yrange, xlim=xrange, ylab=ystr, xlab=xstr, axes=TRUE, ask=TRUE, main=projsonde)
  grid(lwd=2)
  legend(legendxy[1],legendxy[2], lautime, col=mycolors[1:nf], lty=rep(1,nf), lwd = 2.5, cex = 0.8, bty="n")

  for(i in 1:nf)
  { 
    print(paste(i, fs[i]))
    f = paste(fdir, fs[i], sep="/")
    sounding = readin.file(ftype,f)
    if (ftype == "raw") {
      #-- get only valid data --#
      id  = (sounding[, "sta"] == "S00" | sounding[, "sta"] == "S01")
      sounding = sounding[id, ]
    }
    if (YTYPE == "pressure")
      y = sounding[,"p"]
    if (YTYPE == "height")
      y = sounding[,"gp.alt"]

    data = readin.data(sounding=sounding, varname=varname)
    if ((varname=="Wind Speed" | varname=="Wind Direction" | varname=="dZ/dt") & ftype == "raw")
      ind = (!is.na(y) & !is.na(data) & sounding[,"sta"] != "S01")
    else
      ind = (!is.na(y) & !is.na(data))
    yin = y[ind]
    xin = data[ind]
    ndt = length(yin)
    if (ndt < 2) {
      print (paste("THERE IS NO", varname, "DATA OR", YTYPE, "INFORMATION in file", f, "!!!"))
      next
    }

    lines(xin, yin, lwd=1, col=mycolors[i], type="l")
    points(xin, yin, cex=0.5, col=mycolors[i])
  }
  box()
  dev.off()
  print("PLOTS HAVE BEEN PRINTED TO file 'allLines.ps' IN THE WORKING DIRECTORY !")
}

