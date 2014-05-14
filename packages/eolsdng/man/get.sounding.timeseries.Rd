\name{get.sounding.timeseries}
\alias{get.sounding.timeseries}
\title{Plot EOL Sounding Timeseries}
\description{
 Plot sounding timeseries for EOL raw sounding data and QCed sounding data. The plots will be saved to file named "timeseries.ps" under working directory.
}
\usage{
 get.sounding.timeseries(ftype="qc",fdir=NULL,
   varname="temperature",YTYPE="height",
   sonderange=NA,projsonde=NULL,
   flightlines=NA,flightnames=NULL,
   plotcontour=FALSE,levelInterval=5)
}
\arguments{
 \item{ftype}{A character string giving files type, currently only takes "qc"(QCed sounding data files) and "raw"(raw sounding data files). Default is "qc".}
 \item{fdir}{Sounding data files location. Please make sure only wanted D* files in the directory.}
 \item{varname}{A variable name which will be plotted as timeseries. Currently takes "temperature"("t","temp"), "relativehumidity"("rh","rhum"), "windspeed"("w","wspd"), or "dz/dt"("dz","verticalvelocity","vvel"). Defaultly temperature will be plotted.}
 \item{YTYPE}{The only allowed values are "height" and "pressure". Default is "height", sounding timeseries will be plotted vs geopotential height. If assigned as "pressure", sounding timeseries will be plotted vs pressure.}
 \item{sonderange}{A numeric vector of length 2, indicating the beginning and ending number of soundings respectively. Only soundings in the range will be processed/plotted if sonderange is assigned. It will be ignored if inputs are not valid.}
 \item{projsonde}{A character string of project name and/or sounding type to be added to plot title.}
 \item{flightlines}{A numeric vector of sounding numbers when lines wanted to identify soundings from different flights. Should be the numbers of soundings starting a new flight. Default is missing.}
 \item{flightnames}{A vector of character strings showing flight names. Valid only when flightlines is not missing. The vector's length should be one more longer than that of flightlines. Data will be repeatedly used if short of length. The remained values will be cutoff if data assigned more than needed.}
 \item{plotcontour}{Logical:contour plot is preferred when set to TRUE. Otherwise, a level plot (color filling) will be drawn.}
 \item{levelInterval}{Integer:Level interval for color keys. Default is 5.}
}
\value{
 A timeseries plot of selected variable will be printed to file "timeseries.ps" in working directory.
}
\details{
 This function plots sounding timeseries. Users may choose which variable to be plotted. Y axis can be chosen as height(geopotential altitude) or pressure. Lines and names can also be added to the plot to identify soundings from different flights. A range of soundings may be picked for processing.

 Level plot (default,color filling) and contour plot are available to choose. The plot will be automatically printed to a file in working directory named "timeseries.ps".

 The X labels will show as Dyearmndyhrmm when the number of total soundings is not larger than 40, otherwise the number of soundings will be displayed.

 A warning message might be given if the sounding does not rise/drop monotonically: "In approx(yin, xin, ypoints) : collapsing to unique 'x' values". It will more likely be seen when processing raw data or upsonde data.
}
\author{Liangying Zhang \email{lzhang@ucar.edu}}
\seealso{
 \code{\link[EOLsounding]{sounding.verticalinterp}},
 \code{\link[EOLsounding]{get.sounding.profiles}}.
}
\examples{
qc.dir <- system.file("DropsondeData", "qc.eol", package="EOLsounding")
## print to file "timeseries.ps"
get.sounding.timeseries(fdir=qc.dir,
                   flightlines=c(5,9),flightnames=c("F01","F02","F03"))
}
\keyword{aplot}
