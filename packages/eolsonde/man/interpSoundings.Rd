\name{interpSoundings}
\alias{interpSoundings}
\title{Do Interpolation on Vertical Levels for Sounding Data}
\description{
Interpolate one variable, Y, in a sounding to a sequence of another variable, X, using the \code{approx} function.
}
\usage{
interpSoundings(sdngs, xname, ynames, xlim=NULL, nstep=100)
}
\arguments{
  \item{sdngs}{
A list of soundings, as returned by \code{\link{readSoundings}}, or an individual sounding.
}
  \item{xname}{
Name of X (independent) variable of the interpolation, passed as the x argument to \code{\link{approx}}.
  }
  \item{ynames}{
Name of Y variable to be interpolated, passed as the y argument to \code{\link{approx}}.
  }
  \item{xlim}{
Limits of interpolation over X.
  }
  \item{nstep}{
Number of X values to interpolate over.
  }
}
\details{
In each of the soundings, the units of the X variable is checked for consistency. interpSoundings will stop with an error if the units of x change between soundings.

If the units of X are millibars, "mb", indicating it is a pressure, the interpolation is done over \code{log(X)}.

If xlim is not NULL, the interpolation is done over \code{Xout = seq(from=xlim[1],to=xlim[2],length=nstep)}. Xout is reversed if the X variable in the sounding is decreasing in time.  xlim should be in the same units as X, i.e. not \code{log(X)} if X is a pressure.

If xlim is NULL, then the clipped limits (see \code{\link{isfs::clip}}) of the xname variable are used for \code{Xout = seq(from=min(X),to=max(X),length=nstep)}, which is reversed if X is decreasing in time.

Before \code{approx} is called, the Y time series is aligned, via linear interpolation, to the times of non-NA values of X, using \code{\link{isfs::align}}, with a match tolerance of 10 times the mean time delta of the X time series. The interpolation of Y is not done for times in X where there is not a point in Y within the match tolerance.

After the time interpolation is done, then the interpolation of Y over Xout is done: \code{approx(X,Y,Xout,method="linear",rule=1,ties=mean}
}
\value{
A list of sounding time series where each sounding has nstep rows, where the xname colunm will be Xout, the yname column will have the results of \code{approx(X,Y,Xout,...)} over the sounding, and the times are an equally spaced sequence from the time of the first non-NA value in X to the time of the last non-NA value.
}
\references{
}
\author{
Gordon Maclean
}
\note{
}

\seealso{
\code{\link{readSoundings}}, \code{\link{scontour}}, \code{\link{approx}}
}
\examples{
    \dontrun{
        xs = readSoundings()
        sprofile(xs)
        # Interpolate four variables over pressure
        xs = interpSoundings(xs,"P",c("T","RH","Wspd","Wdir"))
        sprofile(xs)

        # Interpolate over geopotential altitude
        xs = interpSoundings(xs,"Alt_gp",c("T","RH","Wspd","Wdir"))

        # Clip some outliers of Alt_gp
        clip("Alt_gp",TRUE,0,15000)
        xs = interpSoundings(xs,"Alt_gp",c("T","RH","Wspd","Wdir"))
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ soundings }
