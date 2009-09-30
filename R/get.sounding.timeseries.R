get.sounding.timeseries <-
function(ftype="qc", fdir=NULL, varname="temperature", YTYPE="height", sonderange=NA, projsonde=NULL, flightlines=NA, flightnames=NULL, plotcontour=FALSE)
{
  varname = tolower(varname)
  ftype = tolower(ftype)
  YTYPE = tolower(YTYPE)
  if (is.null(fdir))
    stop(paste("PLEASE SPECIFY THE LOCATION OF", ftype, "SOUNDING DATA !!!"))
  if (YTYPE != "pressure" & YTYPE != "height")
    stop("YTYPE CAN ONLY BE pressure OR height !!!!")

  msg = "sonderange IS A INTEGRAL VECTOR OF LENGTH 2, INDICATING THE START/END NUMBER OF SOUNDINGS TO BE INTERPOLATED RESPECTIVELY !!! ALL SOUNDING DATA WILL BE PLOTTED AT THIS POINT !!!"
  fs = system(paste("cd ", fdir, ";ls D*", sep=""), TRUE)
  if (length(sonderange) == 1) {
    if (!is.na(sonderange)) {
      print(msg)
      sonderange = NA
    }
  }
  if (length(sonderange) > 2) {
    print(msg)
    sonderange = NA
  }
  if (length(sonderange) == 2) 
    fs = fs[min(sonderange):max(sonderange)]
  yyyy = substring(fs, 2, 5)
  mm   = substring(fs, 6, 7)
  dd   = substring(fs, 8, 9)
  hh   = substring(fs,11,12)
  min  = substring(fs,13,14)
  lautime = paste("D", yyyy, mm, dd, hh, min, sep = "")

  if (length(flightlines)== 1) {
    if (is.na(flightlines)) 
      flightnames = NULL 
    else if (flightlines > length(fs)) 
      print(paste("flightlines", flightlines, "SHOULD NOT BE LARGER THAN TOTAL # OF SOUNDINGS", length(fs)))
  }
  else {
    if (any(flightlines > length(fs)))
      print(paste("flightlines", max(flightlines), "SHOULD NOT BE LARGER THAN TOTAL # OF SOUNDINGS", length(fs)))
  }
  if (!is.null(flightnames)) {
    if (length(flightlines) != length(flightnames)-1)
      print("flightnames SHOULD BE ONE LENGTH LONGER THAN flightlines !!!")
    xfn = c(5, flightlines+5)
  }
  else	xfn = 0

  print(paste("Please make sure ONLY",toupper(ftype),"data under", fdir, "!!!"))
  tmp  = sounding.verticalinterp(ftype,fdir,varname,YTYPE,sonderange)
  ydat = tmp[1, ]
  data = tmp[2:nrow(tmp), ]
  if (YTYPE == "pressure") {
    yrange = rev(range(ydat))
    ylab = "Pressure (hpa)"
    ###--- y axis for flight names
    yfn = yrange[2] + 100
  }
  if (YTYPE == "height") {
    yrange = range(ydat)/1000
    ylab = "Geopotential Altitude (km)"
    ###--- y axis for flight names
    yfn = yrange[2] - 1
  }
  mtitle = projsonde

  colst = 0.1
  if (varname=="t" | varname=="temp" | varname=="temperature") {
    mtitle = paste(mtitle, "TEMPERATURE","C",sep="    ")
    ###-- temperature level range --###
    st = round(min(data,na.rm=TRUE)/10-0.5, 0)*10
    if (min(data,na.rm=TRUE)-st > 5)	st = st + 5
    ed = round(max(data,na.rm=TRUE)/10+0.5, 0)*10
    if (ed-max(data,na.rm=TRUE) > 5)	ed = ed - 5
    levels = seq(st,ed,5)
    colst = 0
  }
  else if (varname=="rh" | varname=="rhum" | varname=="relativehumidity") {
    mtitle = paste(mtitle, "RELATIVE HUMIDITY","%",sep = "    ")
    levels = seq(0,105,5)
  }
  else if (varname=="w" | varname=="wspd" | varname=="windspeed") {
    mtitle = paste(mtitle, "WIND SPEED","m/s",sep = "    ")
    levels =  seq(0,20,1)
  }
  else if (varname=="dz" | varname=="dz/dt" | varname=="vvel" | varname=="verticalvelocity") {
    kunit = "m/s"
    levels = seq(0,20,1)
    ## for upsonde
    if (min(data) < 0) {
      data = data*(-1)
      kunit = "-m/s"
    }
    mtitle = paste(mtitle, "dZ/dt",kunit,sep="    ")
  }
  else {
    stop(paste("THERE IS NO OPTION OF",varname,"FOR timeseries PLOT. CURRENTLY ONLY temperature, rh, windspeed and dz/dt are available"))
  }
  nlvl = length(levels)

  print(paste("min=",min(data,na.rm=TRUE),"max=",max(data,na.rm=TRUE)))
  postscript(file="timeseries.ps",paper="letter")
  ##-------- Plot sounding timeseris -------###
  ## nrow(data) equals to nf (# of soundings)
  x = seq(0,nrow(data),length.out=nrow(data)) 
  y = seq(min(ydat),max(ydat),length.out=ncol(data))
  if (YTYPE == "height")	y = y/1000
  if (nrow(data) <= 60) {
    xlist = list(at=x,rot=90,labels=lautime,axs="i")
    a = x
    alabels = lautime
    alas = 2
  }
  else {
    if (nrow(data) <= 300)	aby = 10
    else	aby = 50
    a = c(0, x[seq(aby, length(x), by=aby)])
    alabels = c(1, round(x[seq(aby, length(x), by=aby)]/10, 0)*10)
    if (a[length(a)] < nrow(data)) {
      a[length(a)] = nrow(data)
      alabels[length(a)] = nrow(data)
    }
    xlist = list(at=a,labels=alabels)
    alas = 1
  }
  print("PLOT HAS BEEN PRINTED TO file 'timeseries.ps' IN THE WORKING DIRECTORY !")
  if (plotcontour) {
    par(mar=c(6.3,4,4,2))
    filled.contour(x=x,y=y,data,ylim=yrange,levels=levels,
          plot.title = title(main=mtitle,ylab=ylab,xlab="",
                             abline(v=x[flightlines],lwd=3),
                            #grid(nx=nrow(data)-1,ny=NA,col=1),
                             text(xfn,yfn,flightnames,cex=1.5)),
          plot.axes = {axis(1,at=a,labels=alabels,las=alas,cex.axis=0.8)
                       axis(2)},
          col=rainbow((nlvl-1),start=colst))
  }
  else {
    offset = (x[2]-x[1])/2
    print(levelplot(data,row.values=x,column.values=y,ylim=yrange,
               scales=list(tck=1,tick.number=c(8,20),x=xlist),
               xlab=NULL,ylab=ylab,main=paste(projsonde,mtitle),
               at=levels,col.regions=rainbow((nlvl-1),start=colst),
               aspect="fill",
               panel=function(...) {
                              panel.levelplot(...)
                              panel.abline(v=x[flightlines]-offset,lwd=3)
                              panel.text(xfn,yfn,flightnames,cex=1.5)
                     }))
  }
  dev.off()
}

