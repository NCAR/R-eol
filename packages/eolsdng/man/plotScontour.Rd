\name{plotScontour}
\alias{plotScontour}
\title{Plot Contours of EOL Sounding Profiles}
\description{
 Plot profile contours of a variable from a list of soundings, where the X axis is the sounding index, and Y is pressure or altitude. The plot is made on the current graphics device. If a device is not active in an interactive session, the default graphics device will be started.
}
\usage{
plotScontour(sdngs, zname, yname, contour = FALSE)
}
\arguments{
  \item{sdngs}{
      \code{sdngs} A list of soundings, as returned by \code{\link{readSoundings}}
}
  \item{zname}{
      \code{zname} Name of variable to be contoured.
}
  \item{yname}{
      \code{yname} Name of variable to use for the Y axis, typically "p", "gps.alt", or "gp.alt".
}
  \item{contour}{
\code{contour} When TRUE, a contour plot is generated. Otherwise, a level plot (color filling) will be drawn.
}
}
\details{
}
\value{
none
}
\references{
}
\author{
Gordon Maclean
}
\note{
}
\seealso{
\code{\link{readSoundings}}
\code{\link{interpSoundings}}
}
\examples{
    \dontrun{
Sys.setenv(PROJECT="DEEPWAVE")
Sys.setenv(PLATFORM="AVAPS")
Sys.setenv(DATASET="Dfiles-RF01")
xs = readSoundings()
plotScontour(xs,"rh","p")
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ time series }
