\name{get.sounding.allLines}
\alias{get.sounding.allLines}
\title{Plot EOL Sounding Profiles for one variable with multi soundings}
\description{
 Plot sounding profiles for one variable with multi soundings from EOL raw sounding data or QCed sounding data. The plots will be saved to file named "allLines.ps" under working directory.
}
\usage{
 get.sounding.allLines(ftype="qc",fdir=NULL,
   varname="temperature",YTYPE="pressure",
   projsonde=NULL,sonderange=NA,
   legendxy=c(-30,0),xrange=c(-30,105),yrange=c(1000,0))
}
\arguments{
 \item{ftype}{A character string giving files type, currently only takes "qc"(QCed sounding data files) and "raw"(raw sounding data files). Default is "qc".}
 \item{fdir}{Sounding data files location. Please make sure only wanted D* files in the directory.}
 \item{varname}{Variable name. Default is temperature.}
 \item{YTYPE}{The only allowed values are "height" and "pressure". Default is "pressure". If assigned as "height", sounding profiles will be plotted vs geopotential altitude.}
 \item{projsonde}{A character string of project name and/or sounding type to be added to plot title.}
 \item{sonderange}{A numeric vector of length 2, indicating the beginning and ending number of soundings respectively. Only soundings in the range will be processed/plotted if sonderange is assigned. It will be ignored if inputs are not valid.}
 \item{legendxy}{A numeric vector of length 2, indicating x and y coordinates for legend, if NA (missing), then legend will not be shown.}
 \item{xrange}{A vector of length 2, define range for x-axis.}
 \item{yrange}{Range for y-axis.}
}
\value{
 A series of profiles of selected variable will be lumped together and printed to file "allLines.ps" in working directory.
}
\details{
 This function plots sounding profiles. Users may choose which variable to be plotted. Y axis can be chosen as height(geopotential altitude) or pressure. A range of soundings may be picked for processing. Users may choose range for x-axis and y-axis, locate legend as well.
}
\author{Liangying Zhang \email{lzhang@ucar.edu}}
\seealso{
 \code{\link[EOLsounding]{get.sounding.profiles}}.
}
\examples{
qc.dir <- system.file("DropsondeData", "qc.eol", package="EOLsounding")
## print to file "allLines.ps"
get.sounding.allLines(fdir=qc.dir,varname="rh",xrange=c(0,100))
}
\keyword{aplot}
