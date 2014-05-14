\name{sounding.verticalinterp}
\alias{sounding.verticalinterp}
\title{Do Interpolation on Vertical Levels for Sounding Data}
\description{
 Read in EOL raw sounding data or QCed sounding data. Then interpolate data of selected variable to a seqential pressure levels or altitude levels.
}
\usage{
 sounding.verticalinterp(ftype="qc",fdir=NULL,
   varname="temperature",YTYPE="height",
   sonderange=NA)
}
\arguments{
 \item{ftype}{A character string giving files type, currently only takes "qc"(QCed sounding data files) and "raw"(raw sounding data files). Default is "qc".}
 \item{fdir}{Sounding data files location. Has to be consistant with ftype, and only wanted D* files in the directory.}
 \item{varname}{A variable name which will be plotted as timeseries. Currently takes "temperature"("t","temp"), "dewpointtemperature"("dpt","dewpt"), "relativehumidity"("rh","rhum"), "windspeed"("w","wspd"), "winddirection"("wdir"), "dz/dt"("dz","verticalvelocity","vvel"), "latitude"("lat"), "longitude"("lon"), and "height"("ht") only when YTYPE is "pressure". Defaultly temperature will be interpolated.}
 \item{YTYPE}{Default is "height", sounding variable will be interpolated on geopotential altitude levels. If being assigned as "pressure", sounding variable will be interpolated on pressure levels. The allowed values are "height" and "pressure"}
 \item{sonderange}{A numeric vector of length 2, indicating the beginning and ending number of soundings respectively. Only soundings in the range will be processed if sonderange is assigned. It will be ignored if inputs are not valid.}
}
\value{
 A matrix with interpolated sounding data of selected variable.
}
\details{
 This function does vertical interpolations to sounding data of selected variable, the variable will be interpolated to every 75m (YTYPE="height") or every 5hPa (YTYPE="pressure"). A data matrix will be returned with sounding launch time (Dyearmmddhrmn) as row names and vertical levels (pressure or geopotential altitudes) as column names.
 
 A range of soundings may be picked for processing. 

 A warning message might be given if the sounding does not rise/drop monotonically: "In approx(yin, xin, ypoints) : collapsing to unique 'x' values". It will more likely be seen when processing raw data or upsonde data.
}
\author{Liangying Zhang \email{lzhang@ucar.edu}}
\seealso{
 \code{\link[EOLsounding]{read.raw.Dfile}},
 \code{\link[EOLsounding]{read.qc.eolfile}},
 \code{\link[EOLsounding]{get.sounding.timeseries}}.
}
\examples{
qc.dir <- system.file("DropsondeData", "qc.eol", package="EOLsounding")
sounding.verticalinterp(fdir=qc.dir)
}
\keyword{file}
