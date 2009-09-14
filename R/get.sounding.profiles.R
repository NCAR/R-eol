get.sounding.profiles <-
function(raw.plot=TRUE, raw.dir=NULL, qc.plot=FALSE, qc.dir=NULL, Tprof=TRUE, RHprof=TRUE, WSPDprof=TRUE, dZprof=TRUE, YTYPE="pressure")
{
  if (YTYPE != "pressure" & YTYPE != "height")
    stop("Ytype CAN ONLY BE pressure OR height !!!!")

  if (raw.plot) {
    if (is.null(raw.dir))
      stop("PLEASE SPECIFY THE LOCATION OF RAW SOUNDING FILES!!!")
    rawfs = system(paste("cd ", raw.dir, "; ls D*", sep="/"), TRUE)
    nrawf = length(rawfs)
    if(nrawf == 0)
      stop(paste("There is NO raw sounding files under directory", raw.dir))
    fs = rawfs
  }
  if (qc.plot) {
    if (is.null(qc.dir))
      stop("PLEASE SPECIFY THE LOCATION OF QCED SOUNDING FILES!!!")
    qcfs = system(paste("cd ", qc.dir, "; ls D*eol", sep = "/"), TRUE)
    nqcf = length(qcfs)
    if(nqcf == 0)
      stop(paste("There is NO QCed eol-files under directory", qc.dir))
    if (!raw.plot)	fs = qcfs
  }
  offcex = 0
  flagcol = 5
  if (raw.plot & qc.plot) {
    if(nrawf < nqcf)
      stop("The number of raw files MUST be lager or equal to QCed files!")
    offcex = 0.7
    flagcol = 0
  }

  yyyy = substring(fs, 2, 5)
  mm   = substring(fs, 6, 7)
  dd   = substring(fs, 8, 9)
  hh   = substring(fs,11,12)
  min  = substring(fs,13,14)
  lautime = paste("D", yyyy, mm, dd, hh, min, sep = "")

  postscript(file="profile.ps", paper="letter")
  mycolors = c("black","red","green","blue","purple","cyan","orange","yellow","gray","pink")
  par(mfrow = c(2,4), mar = c(3,4,3,1), pty="m")
  nf = length(fs)
  for(i in 1:nf)
  {
    pflight=NA
    if (raw.plot) {
      if (qc.plot) {
        if (!any(substring(qcfs,6,16)==substring(rawfs[i],6,16))) {
          print(paste(lautime[i], "ONLY RAW DATA, NO QC DATA!!!"))
          next
        }
        aqcf = qcfs[substring(qcfs,6,16)==substring(rawfs[i],6,16)]
        aqcf = paste(qc.dir, aqcf, sep = "/")
      }
      arawf =  paste(raw.dir, rawfs[i], sep="/")
      raw.sounding = read.raw.Dfile(arawf)
      #-- get flight level --#
      pflight = raw.sounding[raw.sounding[, 2] == "A11", 6]
      if (length(pflight)<1)
        pflight = raw.sounding[raw.sounding[, 2] == "A00", 6]
      hflight = raw.sounding[raw.sounding[, 2] == "A11", 20]/1000
      if (length(hflight)<1)
        hflight = raw.sounding[raw.sounding[, 2] == "A00", 20]/1000
      #-- get only valid data --#
      id  = (raw.sounding[, 2] == "S00" | raw.sounding[, 2] == "S01")
      raw.sounding = raw.sounding[id, ]
      raw.ps  = raw.sounding$p
      raw.gph = raw.sounding$gps.alt/1000
      raw.t   = raw.sounding$temp
      raw.w   = raw.sounding$wspd
      raw.rh  = raw.sounding$rh
      raw.dz  = raw.sounding$vvel

      if (YTYPE == "height") {
        rind.t  = (!is.na(raw.gph) & !is.na(raw.t))
        rind.w  = (!is.na(raw.gph) & !is.na(raw.w) & raw.sounding[, 2] != "S01")
        rind.rh = (!is.na(raw.gph) & !is.na(raw.rh))
        rind.dz = (!is.na(raw.gph) & !is.na(raw.dz))
        ###-- Y axis as geopotential height --###
        rawY.t  = raw.gph[rind.t]
        rawY.w  = raw.gph[rind.w]
        rawY.rh = raw.gph[rind.rh]
        rawY.dz = raw.gph[rind.dz]
        ystr    = "Height (km)"
        yrange  = range(raw.gph, na.rm=TRUE)
      }
      if (YTYPE == "pressure") {
        rind.t = (!is.na(raw.ps) & !is.na(raw.t))
        rind.w = (!is.na(raw.ps) & !is.na(raw.w) & raw.sounding[, 2] != "S01")
        rind.rh = (!is.na(raw.ps) & !is.na(raw.rh))
        rind.dz = (!is.na(raw.ps) & !is.na(raw.dz))
        ###-- Y axis as pressure --###
        rawY.t  = raw.ps[rind.t]
        rawY.w  = raw.ps[rind.w]
        rawY.rh = raw.ps[rind.rh]
        rawY.dz = raw.ps[rind.dz]
        ystr    = "Pressure (hpa)"
        yrange  = rev(range(raw.ps, na.rm=TRUE))
      }
      rawX.t  = raw.t[rind.t]
      rawX.w  = raw.w[rind.w]
      rawX.rh = raw.rh[rind.rh]
      rawX.dz = raw.dz[rind.dz]
    }

    if (qc.plot) {
      if (!raw.plot) aqcf =  paste(qc.dir, qcfs[i], sep="/")
      qc.sounding = read.qc.eolfile(aqcf, nskip=14)
      qc.ps  = qc.sounding$p
      qc.gph = qc.sounding$gps.alt/1000
      qc.t   = qc.sounding$temp
      qc.w   = qc.sounding$wspd
      qc.rh  = qc.sounding$rh
      qc.dz  = qc.sounding$dz

      if (YTYPE == "height") {
        qind.t  = (!is.na(qc.gph) & !is.na(qc.t))
        qind.w  = (!is.na(qc.gph) & !is.na(qc.w))
        qind.rh = (!is.na(qc.gph) & !is.na(qc.rh))
        qind.dz = (!is.na(qc.gph) & !is.na(qc.dz))
        ###-- Y axis as geopotential height --###
        qcY.t  = qc.gph[qind.t]
        qcY.w  = qc.gph[qind.w]
        qcY.rh = qc.gph[qind.rh]
        qcY.dz = qc.gph[qind.dz]
        ystr   = "Height (km)"
        if(!raw.plot)   yrange  = range(qc.gph, na.rm=TRUE)
      }
      if (YTYPE == "pressure") {
        qind.t = (!is.na(qc.ps) & !is.na(qc.t))
        qind.w = (!is.na(qc.ps) & !is.na(qc.w))
        qind.rh = (!is.na(qc.ps) & !is.na(qc.rh))
        qind.dz = (!is.na(qc.ps) & !is.na(qc.dz))
        ###-- Y axis as pressure --###
        qcY.t  = qc.ps[qind.t]
        qcY.w  = qc.ps[qind.w]
        qcY.rh = qc.ps[qind.rh]
        qcY.dz = qc.ps[qind.dz]
        ystr   = "Pressure (hpa)"
        if(!raw.plot)   yrange  = rev(range(qc.ps, na.rm=TRUE))
      }
      qcX.t  = qc.t[qind.t]
      qcX.w  = qc.w[qind.w]
      qcX.dz = qc.dz[qind.dz]
      qcX.rh = qc.rh[qind.rh]
    }

    print(paste(i, lautime[i]))

    ##-------- Plot sounding profiles -------###
    plot(0,0, type="n", ylim=yrange, xlim=c(-30,105), ylab=ystr, xlab="", axes=TRUE, ask=TRUE)
    grid(col=1)
    title(lautime[i], cex=.8)

    if (raw.plot) {
      if (YTYPE == "pressure" & length(pflight)>0)
        points(100, pflight, pch = 2, col = 4, cex = 1.5)
      if (YTYPE == "height" & length(hflight)>0)
        points(100, hflight, pch = 2, col = 4, cex = 1.5)
      rleg = ""
      rcol = 1
      if (Tprof) {
        rleg = c(rleg, "raw-T+60")
        rcol = c(rcol, mycolors[2])
        lines(rawX.t+60, rawY.t, lwd=1, col=mycolors[2], type="l")
        points(rawX.t+60, rawY.t, cex=1, col=mycolors[2])
      }
      if (RHprof) {
        rleg = c(rleg, "raw-RH")
        rcol = c(rcol, mycolors[3])
        lines(rawX.rh, rawY.rh, lwd=1, col=mycolors[3])
        points(rawX.rh, rawY.rh,  cex=1, col=mycolors[3])
      }
      if (WSPDprof) {
        rleg = c(rleg, "raw-Wspd")
        rcol = c(rcol, 4)
        lines(rawX.w, rawY.w, lwd=1,  col=mycolors[4])
        points(rawX.w, rawY.w, cex=1, col=mycolors[4])
      }
      if (dZprof) {
        qleg = c(rleg, "raw-dz/dt")
        qcol = c(rcol, mycolors[5])
        lines(rawX.dz, rawY.dz, lwd=1,  col=1)
        points(rawX.dz, rawY.dz,  cex=1, col=mycolors[5])
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
        lines(qcX.t+60, qcY.t, lwd = 1, col=mycolors[7-flagcol], type="l")
        points(qcX.t+60, qcY.t, cex=1-offcex, col=mycolors[7-flagcol])
      }
      if (RHprof) {
        qleg = c(qleg, "qc-RH")
        qcol = c(qcol, mycolors[8-flagcol])
        lines(qcX.rh, qcY.rh, lwd=1, col=mycolors[8-flagcol])
        points(qcX.rh, qcY.rh,  cex=1-offcex, col=mycolors[8-flagcol])
      }
      if (WSPDprof) {
        qleg = c(qleg, "qc-Wspd")
        qcol = c(qcol, mycolors[9-flagcol])
        lines(qcX.w, qcY.w, lwd=1,  col=mycolors[9-flagcol])
        points(qcX.w, qcY.w,  cex=1-offcex, col=mycolors[9-flagcol])
      }
      if (dZprof) {
        qleg = c(qleg, "qc-dz/dt")
        qcol = c(qcol, mycolors[10-flagcol])
        lines(qcX.dz, qcY.dz, lwd=1,  col=1)
        points(qcX.dz, qcY.dz,  cex=1-offcex, col=mycolors[10-flagcol])
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
    if(i == 1)  legend(-21,yrange[2], leg, col=col, lty=rep(1,nline))
    box()
  }
  dev.off()
}

