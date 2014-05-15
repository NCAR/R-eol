# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsdng" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

get.sounding.profiles <-
function(raw.plot=TRUE, raw.dir=NULL, qc.plot=FALSE, qc.dir=NULL, Tprof=TRUE, RHprof=TRUE, WSPDprof=TRUE, dZprof=TRUE, YTYPE="pressure", projsonde=NULL, sonderange=NA, YAXISrange=NULL)
{
  YTYPE = tolower(YTYPE)
  if (YTYPE != "pressure" & YTYPE != "height")
    stop("YTYPE CAN ONLY BE pressure OR height !!!!")

  if (raw.plot) {
    rawfs = check.files(raw.dir)
    check.sonderange(sonderange, length(rawfs))
    if (length(sonderange) == 2)
      rawfs = rawfs[sonderange[1]:sonderange[2]]
    nrawf = length(rawfs)
    fs = rawfs
  }
  if (qc.plot) {
    qcfs = check.files(qc.dir)
    if (!raw.plot) {
      check.sonderange(sonderange, length(qcfs))
      if (length(sonderange) == 2)
        qcfs = qcfs[sonderange[1]:sonderange[2]]
      fs = qcfs
    }
    nqcf = length(qcfs)
  }
  offcex = 0
  flagcol = 5
  if (raw.plot & qc.plot) {
    if(nrawf < nqcf)
      print("The number of raw files SHOULD be lager or equal to QCed files!")
    offcex = 0.7
    flagcol = 0
  }

  yyyy = substring(fs, 2, 5)
  mm   = substring(fs, 6, 7)
  dd   = substring(fs, 8, 9)
  hh   = substring(fs,11,12)
  min  = substring(fs,13,14)
  sec  = substring(fs,15,16)
  lautime = paste("D", yyyy, mm, dd, hh, min, sec, sep = "")

  system("rm -f profiles.ps")
  postscript(file="profiles.ps", paper="letter", horizontal=TRUE)
  mycolors = c("black","red","green","blue","purple","cyan","orange","yellow","gray","pink")
  par(mfrow = c(2,4), mar = c(3,4,3,1), pty="m")

  ######-- legend information --######
  if (raw.plot) {
    rleg = ""
    rcol = 1
    if (Tprof) {
      rleg = c(rleg, "raw-T+60")
      rcol = c(rcol, mycolors[2])
    }
    if (RHprof) {
      rleg = c(rleg, "raw-RH")
      rcol = c(rcol, mycolors[3])
    }
    if (WSPDprof) {
      rleg = c(rleg, "raw-Wspd")
      rcol = c(rcol, 4)
    }
    if (dZprof) {
      rleg = c(rleg, "raw-dZ/dt")
      rcol = c(rcol, mycolors[5])
    }
  }
 
  if (qc.plot) { 
    qleg = "" 
    qcol = 5 
    flagcol = 0 
    if (!raw.plot)	flagcol = 5
    if (Tprof) {
      qleg = c(qleg, "qc-T+60")
      qcol = c(qcol, mycolors[7-flagcol])
    }
    if (RHprof) {
      qleg = c(qleg, "qc-RH")
      qcol = c(qcol, mycolors[8-flagcol])
    }
    if (WSPDprof) {
      qleg = c(qleg, "qc-Wspd")
      qcol = c(qcol, mycolors[9-flagcol])
    }
    if (dZprof) {
      qleg = c(qleg, "qc-dz/dt")
      qcol = c(qcol, mycolors[10-flagcol])
    }
  }

  if (raw.plot & !qc.plot) {
    leg = rleg[2:length(rleg)]
    col = rcol[2:length(rcol)]
  }
  if (!raw.plot & qc.plot) {
    leg = qleg[2:length(qleg)]
    col = qcol[2:length(qcol)]
  }
  if (raw.plot & qc.plot) {
    leg = c(rleg[2:length(rleg)], qleg[2:length(qleg)])
    col = c(rcol[2:length(rcol)], qcol[2:length(qcol)])
  }
  nline = length(leg)
  
  if (YTYPE == "pressure") {
    yrge = c(1000, 0)
    ystr = "Pressure (hPa)"
    ly   = 200
  }
  if (YTYPE == "height") {
    yrge = c(0, 10)
    ystr = "GPS Altitude (km)"
    ly   = 8
  }
  plot(0,0, type="n", ylim=yrge, xlim=c(-30,105), ylab=ystr, xlab="", axes=TRUE, ask=TRUE)
  #grid(col=1)
  if (!is.null(projsonde))
    legend(-30,yrge[2], projsonde, bty = "n", cex = 1.7)
  legend(-5,ly, leg, col=col, lty=rep(1,nline), lwd = 2.5, cex = 1.5)
  box()
 ################

  nf = length(fs)
  for(i in 1:nf)
  {
    print(paste(i, lautime[i]))
    if (raw.plot) {
      pflight = NA
      hflight = NA
      if (qc.plot) {
        if (!any(substring(qcfs,6,16)==substring(rawfs[i],6,16))) {
          print(paste(lautime[i], "ONLY RAW DATA, NO QC DATA!!!"))
          next
        }
        aqcf = qcfs[substring(qcfs,6,16)==substring(rawfs[i],6,16)]
        aqcf = paste(qc.dir, aqcf, sep = "/")
      }
      arawf =  paste(raw.dir, rawfs[i], sep="/")
      raw.sounding = readin.file("raw",arawf)
      if (nrow(raw.sounding) <= 1)
        next
      #-- get flight level --#
      pflight = raw.sounding[raw.sounding[,"sta"] == "A11", "press"]
      if (length(pflight)<1)
        pflight = raw.sounding[raw.sounding[,"sta"] == "A00", "press"]
      hflight = raw.sounding[raw.sounding[,"sta"] == "A11", "gps.alt"]/1000
      if (length(hflight)<1)
        hflight = raw.sounding[raw.sounding[,"sta"] == "A00", "gps.alt"]/1000
      #-- get only valid data --#
      id  = (raw.sounding[, "sta"] == "S00" | raw.sounding[,"sta"] == "S01")
      raw.sounding = raw.sounding[id, ]
      if (nrow(raw.sounding) <= 1) {
        print(paste(lautime[i], "has NO valid data!!"))
        next
      }
      raw.ps  = raw.sounding[,"p"]
      raw.ght = raw.sounding[,"gps.alt"]/1000
      raw.t   = raw.sounding[,"temp"]
      raw.w   = raw.sounding[,"wspd"]
      raw.rh  = raw.sounding[,"rh"]
      raw.dz  = raw.sounding[,"dz"]

      if (YTYPE == "height") {
        if (length(raw.ght[!is.na(raw.ght)]) < 1) {
          print(paste(lautime[i], "has NO height information!"))
          next
        }
        rind.t  = (!is.na(raw.ght) & !is.na(raw.t))
        rind.w  = (!is.na(raw.ght) & !is.na(raw.w) & raw.sounding[,"sta"] != "S01")
        rind.rh = (!is.na(raw.ght) & !is.na(raw.rh))
        rind.dz = (!is.na(raw.ght) & !is.na(raw.dz) & raw.sounding[,"sta"] != "S01")
        ###-- Y axis as gps height --###
        rawY.t  = raw.ght[rind.t]
        rawY.w  = raw.ght[rind.w]
        rawY.rh = raw.ght[rind.rh]
        rawY.dz = raw.ght[rind.dz]
        ystr    = "GPS Altitude (km)"
        yrange  = range(raw.ght, na.rm=TRUE)
      }
      if (YTYPE == "pressure") {
        if (length(raw.ps[!is.na(raw.ps)]) < 1) {
          print(paste(lautime[i], "has NO pressure information!"))
          next
        }
        rind.t = (!is.na(raw.ps) & !is.na(raw.t))
        rind.w = (!is.na(raw.ps) & !is.na(raw.w) & raw.sounding[,"sta"] != "S01")
        rind.rh = (!is.na(raw.ps) & !is.na(raw.rh))
        rind.dz = (!is.na(raw.ps) & !is.na(raw.dz) & raw.sounding[,"sta"] != "S01")
        ###-- Y axis as pressure --###
        rawY.t  = raw.ps[rind.t]
        rawY.w  = raw.ps[rind.w]
        rawY.rh = raw.ps[rind.rh]
        rawY.dz = raw.ps[rind.dz]
        ystr    = "Pressure (hPa)"
        yrange  = rev(range(raw.ps, na.rm=TRUE))
      }
      rawX.t  = raw.t[rind.t]
      rawX.w  = raw.w[rind.w]
      rawX.rh = raw.rh[rind.rh]
      rawX.dz = raw.dz[rind.dz]
    }

    if (qc.plot) {
      if (!raw.plot) aqcf =  paste(qc.dir, qcfs[i], sep="/")
      qc.sounding = readin.file("qc",aqcf)
      qc.ps  = qc.sounding[,"p"]
      qc.ght = qc.sounding[,"gps.alt"]/1000
      qc.t   = qc.sounding[,"temp"]
      qc.w   = qc.sounding[,"wspd"]
      qc.rh  = qc.sounding[,"rh"]
      qc.dz  = qc.sounding[,"dz"]

      if (YTYPE == "height") {
        if (length(qc.ght[!is.na(qc.ght)]) < 1) {
          print(paste(lautime[i], "has NO height information!"))
          next
        }
        qind.t  = (!is.na(qc.ght) & !is.na(qc.t))
        qind.w  = (!is.na(qc.ght) & !is.na(qc.w))
        qind.rh = (!is.na(qc.ght) & !is.na(qc.rh))
        qind.dz = (!is.na(qc.ght) & !is.na(qc.dz))
        ###-- Y axis as gps height --###
        qcY.t  = qc.ght[qind.t]
        qcY.w  = qc.ght[qind.w]
        qcY.rh = qc.ght[qind.rh]
        qcY.dz = qc.ght[qind.dz]
        ystr   = "GPS Altitude (km)"
        if(!raw.plot)   yrange  = range(qc.ght, na.rm=TRUE)
      }
      if (YTYPE == "pressure") {
        if (length(qc.ps[!is.na(qc.ps)]) < 1) {
          print(paste(lautime[i], "has NO pressure information!"))
          next
        }
        qind.t = (!is.na(qc.ps) & !is.na(qc.t))
        qind.w = (!is.na(qc.ps) & !is.na(qc.w))
        qind.rh = (!is.na(qc.ps) & !is.na(qc.rh))
        qind.dz = (!is.na(qc.ps) & !is.na(qc.dz))
        ###-- Y axis as pressure --###
        qcY.t  = qc.ps[qind.t]
        qcY.w  = qc.ps[qind.w]
        qcY.rh = qc.ps[qind.rh]
        qcY.dz = qc.ps[qind.dz]
        ystr   = "Pressure (hPa)"
        if(!raw.plot)   yrange  = rev(range(qc.ps, na.rm=TRUE))
      }
      qcX.t  = qc.t[qind.t]
      qcX.w  = qc.w[qind.w]
      qcX.dz = qc.dz[qind.dz]
      qcX.rh = qc.rh[qind.rh]
    }

    ##allow user to customize the range of y-axis, instead of using range to set it automatically.
    if (!is.null(YAXISrange))  yrange = YAXISrange

    ##########-- Plot sounding profiles --##########
    plot(0,0, type="n", ylim=yrange, xlim=c(-30,105), ylab=ystr, xlab="", axes=TRUE, ask=TRUE)
    grid(col=1)
    title(lautime[i], cex=.8)

    if (raw.plot) {
      if (YTYPE == "pressure" & length(pflight)>0)
        points(100, pflight, pch = 2, col = 4, cex = 1.5)
      if (YTYPE == "height" & length(hflight)>0)
        points(100, hflight, pch = 2, col = 4, cex = 1.5)
      if (Tprof) {
        lines(rawX.t+60, rawY.t, lwd=1, col=mycolors[2], type="l")
        points(rawX.t+60, rawY.t, cex=1, col=mycolors[2])
      }
      if (RHprof) {
        lines(rawX.rh, rawY.rh, lwd=1, col=mycolors[3])
        points(rawX.rh, rawY.rh,  cex=1, col=mycolors[3])
      }
      if (WSPDprof) {
        lines(rawX.w, rawY.w, lwd=1,  col=mycolors[4])
        points(rawX.w, rawY.w, cex=1, col=mycolors[4])
      }
      if (dZprof) {
        lines(rawX.dz, rawY.dz, lwd=1,  col=mycolors[5])
        points(rawX.dz, rawY.dz,  cex=1, col=mycolors[5])
       }
    } 

    if (qc.plot) { 
      flagcol = 0 
      if (!raw.plot)	flagcol = 5
      if (Tprof) {
        lines(qcX.t+60, qcY.t, lwd = 1, col=mycolors[7-flagcol], type="l")
        points(qcX.t+60, qcY.t, cex=1-offcex, col=mycolors[7-flagcol])
      }
      if (RHprof) {
        lines(qcX.rh, qcY.rh, lwd=1, col=mycolors[8-flagcol])
        points(qcX.rh, qcY.rh,  cex=1-offcex, col=mycolors[8-flagcol])
      }
      if (WSPDprof) {
        lines(qcX.w, qcY.w, lwd=1,  col=mycolors[9-flagcol])
        points(qcX.w, qcY.w,  cex=1-offcex, col=mycolors[9-flagcol])
      }
      if (dZprof) {
        lines(qcX.dz, qcY.dz, lwd=1, col=mycolors[10-flagcol])
        points(qcX.dz, qcY.dz,  cex=1-offcex, col=mycolors[10-flagcol])
      }
    }
    box()
  }
  dev.off()
  print("PLOTS HAVE BEEN PRINTED TO file 'profiles.ps' IN THE WORKING DIRECTORY !")
}

