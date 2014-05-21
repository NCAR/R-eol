\name{get.sounding.histograms}
\alias{get.sounding.histograms}
\title{Plot Histograms for EOL Sounding Data}
\description{
 Plot sounding histograms for EOL QCed sounding data.
 The plots will be saved to file named "histograms.ps" under working directory.
}
\usage{
 get.sounding.histograms(fdir=NULL,projsonde=NULL,
   Thist=TRUE,RHhist=TRUE,Phist=TRUE,WSPDhist=TRUE,WDIRhist=TRUE)
}
\arguments{
 \item{fdir}{QCed sounding data files location.}
 \item{projsonde}{A character string to input project name and/or sounding type, which will be shown as part of plot title.}
 \item{Thist}{Logical:if true(default) to plot histograms of temperature.}
 \item{RHhist}{Logical:if true(default) to plot histograms of relative humidity.}
 \item{Phist}{Logical:if true(default) to plot histograms of pressure.} 
 \item{WSPDhist}{Logical:if true(default)to plot histograms of wind speed.}
 \item{WDIRhist}{Logical:if true(default) to plot histograms of wind direction(from 0 to 360 degree).}
}
\value{
 The histograms will be printed to file "histograms.ps" in working directory.
}
\details{
 This function plots sounding histograms. Users may choose whether to plot temperature, relative humidity, pressure, wind speed or wind direction. The histograms will be plotted on a nX1 panel, depends on how many variables user choose.

 Histograms will only work for QCed data.
}
\author{Liangying Zhang \email{lzhang@ucar.edu}}
\seealso{
 \code{\link[EOLsounding]{read.qc.eolfile}}.
}
\examples{
    \dontrun{
        fdir <- system.file("DropsondeData", "qc.eol", package="EOLsounding")
        ## will print to file "histograms.ps"
        get.sounding.histograms(fdir=fdir, projsonde="TPARC Dropsonde")
    }
}
\keyword{aplot}
