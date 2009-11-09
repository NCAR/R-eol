read.raw.Dfile <-
function (file=stop("'file' must be specified")) 
{
  if (length(system(paste("ls", file), TRUE)) < 1)
    stop(paste("THE FILE", file, "DOES NOT EXIST"))

  #---------------------------------------------------------------
  ## get # of lines in filename
  #---------------------------------------------------------------
  nn     = system(paste("wc -l", file), TRUE)
  nfline = as.numeric(substring(nn, 1, nchar(nn)-nchar(file)))

  #-----------------------------------------------------------------------
  ##  get rid of header and tail (# of header = 6, # of tail = 19)
  ##  read body in as matrix, put all missing values as NA
  #-----------------------------------------------------------------------
  nl = nfline - 25
  na.strs = c("9999.00","99.00","999.00","999.000000","99.000000","99999.00")
  #d = matrix(scan(filename, what = "", skip = 6, nline = nl, na.strings = na.strs), byrow = T, ncol = 20)
  d = read.table(file, skip = 6, nrows = nl, na.strings = na.strs)

  #----------------------------
  ## name variables in the data
  #----------------------------
  ava      = d$V1
  sta      = d$V2
  sid      = d$V3
  utc.date = d$V4
  utc.time = d$V5
  press    = d$V6
  temp     = d$V7
  rhum     = d$V8
  wdir     = d$V9
  wspd     = d$V10
  vvel     = d$V11 ## vertical velocity
  lon      = d$V12
  lat      = d$V13
  gp.alt   = d$V14
  wsat     = d$V15 ##
  rh1      = d$V16
  rh2      = d$V17
  ssat     = d$V18 ##
  werr     = d$V19 ## wind error
  gps.alt  = d$V20

  newdata = data.frame(ava, sta, sid, utc.date, utc.time, p=press, temp, rh=rhum, wdir, wspd, vvel, lon, lat, gp.alt, wsat, rh1, rh2, ssat, werr, gps.alt)
  return(newdata)
}

