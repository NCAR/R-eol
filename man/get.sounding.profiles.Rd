\name{get.sounding.profiles}
\alias{get.sounding.profiles}
\title{Plot EOL sounding profiles}
\description{
 Plot sounding profiles for EOL raw sounding data and QCed sounding data.
}
\usage{
 get.sounding.profiles(raw.plot=TRUE,raw.dir,qc.plot=FALSE,
   qc.dir,Tprof=TRUE,RHprof=TRUE,WSPDprof=TRUE,YTYPE="pressure")
}
\arguments{
 \item{raw.plot}{A logic value, whether or not to plot profiles from raw data. Default is TRUE.}
 \item{raw.dir}{A string indicating directory where raw sounding data files are.}
 \item{qc.plot}{A logic value, whether or not to plot profiles from QCed data. Default is FALSE.}
 \item{qc.dir}{A string indicating directory where QCed sounding data files are.}
 \item{Tprof}{A logic value, whether or not to plot temperature profiles. Default is TRUE.}
 \item{RHprof}{A logic value, whether or not to plot relative humidity profiles. Default is TRUE.}
 \item{WSPDprof}{A logic value, whether or not to plot wind speed profiles. Default is TRUE.}
 \item{YTYPE}{Default is "pressure", sounding profiles will plot against pressures. If assign as "height", sounding profiles will plot against GPS altitude.}
}
\value{
 The profiles will be plotted on 2x4 panels.
}
\details{
 This function plots sounding profiles. Users may choose whether to put raw and
 QCed data together, whether to plot temperature, relative humidity and wind speed
 on same plot or seperately. Y axis can be choose as pressure or GPS height.
}
\author{Liangying Zhang \email{lzhang@ucar.edu}}
\seealso{
 \code{\link[EOLsounding]{read.raw.Dfile}}.
 \code{\link[EOLsounding]{read.qc.eolfile}}.
}
\examples{
raw.dir <- "/scr/sci2/lzhang/TPARC/AF-C130/D-files"
qc.plot <- TRUE
qc.dir <- "/scr/sci2/lzhang/TPARC/AF-C130/qc.eol"
## print to file
postscript(file="profile.ps")
get.sounding.profiles(raw.plot=TRUE,raw.dir=raw.dir,
                      qc.plot=TRUE,qc.dir=qc.dir)
dev.off()
}
\keyword{aplot}
