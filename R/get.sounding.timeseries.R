get.sounding.timeseries <-
function(ftype="qc", fdir=NULL, varname="temperature", YTYPE="height", sonderange=NA, projsonde=NULL, flightlines=NA, flightnames=NULL, plotcontour=FALSE)
{
  var = readin.varname(varname)
  varname = var$name
  unit = var$unit
  ftype = tolower(ftype)
  YTYPE = check.YTYPE(YTYPE)

  print(paste("Please make sure ONLY",toupper(ftype),"data under", fdir, "!!!"))
  data = sounding.verticalinterp(ftype,fdir,varname,YTYPE,sonderange)
  nf   = nrow(data)
  lautime = rownames(data)
  ydat = as.numeric(colnames(data))

  if (length(flightlines)== 1) {
    if (is.na(flightlines)) 
      flightnames = NULL 
    else if (flightlines > nf) 
      print(paste("flightlines", flightlines, "SHOULD NOT BE LARGER THAN TOTAL # OF SOUNDINGS", nf))
  }
  else {
    if (any(flightlines > nf))
      print(paste("flightlines", max(flightlines), "SHOULD NOT BE LARGER THAN TOTAL # OF SOUNDINGS", nf))
  }
  if (!is.null(flightnames)) {
    if (length(flightlines) != length(flightnames)-1)
      print("flightnames SHOULD BE ONE LENGTH LONGER THAN flightlines !!!")
    xfn = c(5, flightlines+5)
  }
  else	xfn = 0

  if (YTYPE == "pressure") {
    yrange = rev(range(ydat))
    ylab = "Pressure (hpa)"
    ###--- y axis for flight names
    yfn = yrange[2] + 100
  }
  if (YTYPE == "height") {
    yrange = range(ydat)/1000
    ylab = "Geopotential Height (km)"
    ###--- y axis for flight names
    yfn = yrange[2] - 1
  }

  colst = 0.1
  if (varname=="Relative Humidity")
    levels = seq(0,105,5)
  else {
    ###-- level range --###
    st = round(min(data,na.rm=TRUE)/10-0.5, 0)*10
    if (min(data,na.rm=TRUE)-st > 5)	st = st + 5
    ed = round(max(data,na.rm=TRUE)/10+0.5, 0)*10
    if (ed-max(data,na.rm=TRUE) > 5)	ed = ed - 5
    levels = seq(st,ed,5)

    if (varname=="Temperature")
      colst = 0
    if (varname=="dZ/dt") {
      if (max(data,na.rm=TRUE) < 0) {
        data = data*(-1)
        unit = "-m/s"
      }
    }
  }
  mtitle = paste(projsonde, varname, unit, sep = "    ")
  nlvl = length(levels)

  print(paste("min=",min(data,na.rm=TRUE),"max=",max(data,na.rm=TRUE)))
  system("rm -f timeseries.ps")
  postscript(file="timeseries.ps", paper="letter", horizontal=TRUE)
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
    require(lattice)
    offset = (x[2]-x[1])/2
    print(levelplot(data,row.values=x,column.values=y,ylim=yrange,
               scales=list(tck=1,tick.number=c(8,20),x=xlist),
               xlab=NULL,ylab=ylab,main=mtitle,aspect="fill",
               at=levels,col.regions=rainbow((nlvl-1),start=colst),
               panel=function(...) {
                              panel.levelplot(...)
                              panel.abline(v=x[flightlines]-offset,lwd=3)
                              panel.text(xfn,yfn,flightnames,cex=1.5)
                     }))
  }
  dev.off()
  print("PLOT HAS BEEN PRINTED TO file 'timeseries.ps' IN THE WORKING DIRECTORY !")
}
