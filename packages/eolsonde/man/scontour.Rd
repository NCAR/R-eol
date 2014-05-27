\name{scontour}
\alias{scontour}
\title{Plot Contours of EOL Sounding Profiles}
\description{
 Plot profile contours of a variable from a list of soundings, where the X axis is the sounding index, and Y is pressure or altitude.
}
\usage{
scontour(sdngs, yname, zname, contour = FALSE, ylim=NULL, ynstep=100, title)
}
\arguments{
  \item{sdngs}{
      A list of soundings, as returned by \code{\link{readSoundings}}
  }
  \item{yname}{
      Name of variable to use for the Y axis, typically "P", "Alt_gps", or "Alt_gp".
  }
  \item{zname}{
      Name of variable to be contoured. Values of this variable will be clipped before being interpolated onto the X-Y grid, where X is the sounding index.
  }
  \item{contour}{
        When TRUE, a contour plot is generated. Otherwise, a level plot (color filling) will be drawn.
  }
  \item{ylim}{
      Limits on yaxis. If not specified, the limits will be determined from the data, after clipping.
  }
  \item{ynstep}{
      Number of steps in Y to interpolate the Z variable.
  }
  \item{title}{
      Title for the plot. If missing, the name of the Y variable plus the number of sounding will be used for the title.
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
# Read sample D files provided in the eolsonde R package
dir = system.file(package="eolsonde","unitTests","data","D-file")

# start time for search of soundings
t1 = utime("2008 8 15 17:20",time.zone="UTC")
# end time
t2 = t1 + 60 * 20   # add 20 minutes

xs = readSoundings(dir=dir,start=t1,end=t2)

# Plot contours of Z=temperature vs X=sounding index, Y=pressure.
scontour(xs,"P","T")

# Contour of Z=vertical velocity (Vz) vs X=sounding index, Y=geopotential altitude
scontour(xs,"Alt_gp","Vz")

# Clip some outliers of Alt_g, re-plot.
clip("Alt_gp",TRUE,0,15000)
scontour(xs,"Alt_gp","Vz")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sounding }
