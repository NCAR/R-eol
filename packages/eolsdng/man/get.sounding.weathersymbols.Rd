\name{get.sounding.weathersymbols}
\alias{get.sounding.weathersymbols}
\title{Plot Sounding Weather Symbols}
\description{
 Plot sounding weather annotations at the given coordinates, by interpolating sounding data to the given pressure level. The plot will be saved to file named "weathersymbols.ps" under working directory.
}

\usage{
 get.sounding.weathersymbols(fdir=NULL, pslevel=500,
         projsonde=NULL, sonderange=NA, addmap=FALSE, tsize=1)
}

\arguments{
  \item{fdir}{Sounding data files location. Only QCed data will be processed.}
  \item{pslevel}{Which pressure level to plot? Default is 500 hPa.}
  \item{projsonde}{A character string of project name and/or sounding type or any string to be added to plot title.}
  \item{sonderange}{A numeric vector of length 2, indicating the beginning and ending number of soundings respectively. Only soundings in the range will be processed/plotted if sonderange is assigned. It will be ignored if inputs are not valid.}
  \item{addmap}{If TRUE, a map will be added as background. Error message will be given when the plot region does not have map data (e.g. over ocean).}
  \item{tsize}{Size for wind symbol, like par parameter cex.}
}

\value{
 Weather symbols will be plotted to file "weathersymbols.ps" in working directory.
}

\details{
 Interpolate QCed EOL sounding data at given pressure level, then plot weather symbols at given coordinates (includes wind, temperature, dew point temperature, geopotential height). A map will be added if addmap is TRUE.
}

\author{Liangying Zhang \email{lzhang@ucar.edu}}

\seealso{
 \code{\link{station.symbol}},
 \code{\link{sounding.verticalinterp}}.
}

\examples{
fdir <- system.file("DropsondeData", "qc.eol", package="EOLsounding")
## print to file "weathersymbols.ps"
get.sounding.weathersymbols(fdir=fdir, projsonde="TPARC Dropsonde", tsize=1.3)
}

\keyword{ aplot }
