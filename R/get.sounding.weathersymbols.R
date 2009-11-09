get.sounding.weathersymbols <-
function(fdir=NULL, pslevel=500, projsonde=NULL, sonderange=NA, addmap=FALSE, tsize=1)
{
  if (is.null(fdir))
    stop(paste("PLEASE SPECIFY THE LOCATION OF QCed SOUNDING DATA !!"))
  print(paste("Please make sure ONLY QCed data under", fdir, "!!!"))

  tmp = sounding.verticalinterp("qc",fdir,"temp","pressure",sonderange)
  temp = round(tmp[, colnames(tmp) == as.character(pslevel)],1)
  lautime = rownames(tmp)
  nf = nrow(tmp)
  tmp = sounding.verticalinterp("qc",fdir,"dewpt","pressure",sonderange)
  dewpt = round(tmp[, colnames(tmp) == as.character(pslevel)],1)
  tmp = sounding.verticalinterp("qc",fdir,"ht","pressure",sonderange)
  ht = round(tmp[, colnames(tmp) == as.character(pslevel)],0)
  tmp = sounding.verticalinterp("qc",fdir,"wdir","pressure",sonderange)
  wdir = tmp[, colnames(tmp) == as.character(pslevel)]
  tmp = sounding.verticalinterp("qc",fdir,"wspd","pressure",sonderange)
  wspd = tmp[, colnames(tmp) == as.character(pslevel)]
  tmp = sounding.verticalinterp("qc",fdir,"lat","pressure",sonderange)
  lat = tmp[, colnames(tmp) == as.character(pslevel)]
  tmp = sounding.verticalinterp("qc",fdir,"lon","pressure",sonderange)
  lon = tmp[, colnames(tmp) == as.character(pslevel)]

  print(cbind(lat, lon, temp, dewpt, wspd, ht))
  system("rm -f weathersymbols.ps")
  postscript("weathersymbols.ps",paper="letter",horizontal=TRUE)
  mtitle = paste(projsonde, "  ", pslevel, "hPa", sep = "")
  x1 = min(lon,na.rm=TRUE) - 1
  x2 = max(lon,na.rm=TRUE) + 1
  y1 = min(lat,na.rm=TRUE) - 1
  y2 = max(lat,na.rm=TRUE) + 1
  if(addmap) {
    database = "world2"
    if (any(lon < 0))	database = "world"
    map(database, xlim=c(x1,x2), ylim=c(y1,y2), col="grey")
    map.axes()
  }
  else
    plot(0, 0, xlim=c(x1,x2), ylim=range(lat,na.rm=TRUE), main=mtitle, xlab="longitude (deg)", ylab="latitude (deg)")
  station.symbol(lon,lat,direction=wdir,speed=wspd,temp=temp,dewpt=dewpt,press=ht,cex=tsize)
  grid(lwd=2.5)
  box()
  dev.off()
}

