\name{get.sounding.profiles}
\alias{get.sounding.profiles}
\title{Plot EOL Sounding Profiles}
\description{
 Plot sounding profiles for EOL raw sounding data and/or QCed sounding data. 
 The plots will be saved to file named "profiles.ps" under working directory.
}
\usage{
 get.sounding.profiles(raw.plot=TRUE,raw.dir=NULL,qc.plot=FALSE,
   qc.dir=NULL,Tprof=TRUE,RHprof=TRUE,WSPDprof=TRUE,dZprof=TRUE,
   YTYPE="pressure",projsonde=NULL)
}
\arguments{
 \item{raw.plot}{Logical:if true to plot profiles from raw data. Default is TRUE.}
 \item{raw.dir}{A character string giving where raw sounding data files are.}
 \item{qc.plot}{Logical:if true to plot profiles from QCed data. Default is FALSE.}
 \item{qc.dir}{A character string showing where QCed sounding data files are.}
 \item{Tprof}{Logical:if true to plot temperature profiles. Default is TRUE.}
 \item{RHprof}{Logical:if true to plot relative humidity profiles. Default is TRUE.}
 \item{WSPDprof}{Logical:if true to plot wind speed profiles. Default is TRUE.}
 \item{dZprof}{Logical:if true to plot profiles of sounding falling(rising) rate. Default is TRUE.}
 \item{YTYPE}{Default is "pressure", sounding profiles will be plotted against pressures. If assign as "height", sounding profiles will be plotted against GPS altitude.}
 \item{projsonde}{A character string to input project name and/or sounding type, which will be plotted as legends in first plot served as an information panel.}
}
\value{
 The profiles will be plotted on 2x4 panels and printed to file "profiles.ps" in working directory.
}
\details{
 This function plots sounding profiles. Users may choose whether to put raw and QCed data together, whether to plot temperature, relative humidity and wind speed on same plot or seperately. Y axis can be chosen as pressure or height(GPS altitude). The first panel is blank with only legends, users may input project name, sounding type and/or other information to show here.

 When raw sounding is plotted, only valid data will be processed, meaning S00 and/or S01 for T and RH, and only S00 for Wspd and dz/dt.

 If both raw plot and qc plot are chosen, but only raw data is available, the plot will be skipped.
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
## will print to file "profile.ps"
get.sounding.profiles(raw.plot=TRUE,raw.dir=raw.dir,
                      qc.plot=TRUE,qc.dir=qc.dir)
}
\keyword{aplot}
