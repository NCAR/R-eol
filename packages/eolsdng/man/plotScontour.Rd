\name{plotScontour}
\alias{plotScontour}
\title{Plot Contours of EOL Sounding Profiles}
\description{
 Plot profile contours of a variable from a list of soundings, where the X axis is the sounding index, and Y is pressure or altitude.
}
\usage{
plotScontour(sdngs, yname, zname, contour = TRUE)
}
\arguments{
  \item{sdngs}{
      \code{sdngs} A list of soundings, as returned by \code{\link{readSoundings}}
}
  \item{yname}{
      \code{yname} Name of variable to use for the Y axis, typically "p", "gps.alt", or "gp.alt".
}
  \item{zname}{
      \code{zname} Name of variable to be contoured.
}
  \item{contour}{
\code{contour} When TRUE, a contour plot is generated. Otherwise, a level plot (color filling) will be drawn.
}
}
\details{
}
\value{
None.  A plot is made on the current graphics device. If a device is not active in an interactive session, the default graphics device will be started.
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
# Read sample D files provided in the eolsdng R package
dir = system.file(package="eolsdng","unitTests","data","D-file")

# start time for search of soundings
t1 = utime("2008 8 15 17:20",time.zone="UTC")
# end time
t2 = t1 + 60 * 20   # add 20 minutes

xs = readSoundings(dir=dir,start=t1,end=t2)

# Plot contours of temp vs X=sounding index, Y=pressure.
plotScontour(xs,"p","temp")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sounding }
