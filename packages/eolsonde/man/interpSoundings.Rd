\name{interpSoundings}
\alias{interpSoundings}
\title{Do Interpolation on Vertical Levels for Sounding Data}
\description{
Interpolate all columns of a list of soundings.
}
\usage{
interpSoundings(sdngs, xname, ynames, xlim=NULL, nstep=100)
}
\arguments{
  \item{sdngs}{
A list of soundings, as returned by \code{\link{readSoundings}}, or an individual sounding.
}
  \item{xname}{
Name of variable to be interpolated over, whose values will be the x argument passed to \code{\link{approx}}.
  }
  \item{ynames}{
Name of variable to be interpolated, whose values will be the y argument passed to \code{\link{approx}}.
  }
  \item{xlim}{
Limits of interpolation over X.
  }
  \item{nstep}{
Number of X values to interpolate over.
  }
}
\details{
In each of the soundings, the units of the x variable is checked for consistency. interpSoundings will stop with an error if the units of x change between soundings.

If xlim is not NULL, the interpolation is done over \code{seq(from=xlim[1],to=xlim[2],length=nstep)}.

If xlim is NULL, and the units of x are "mb" (millibars) then the interpolation is done over \code{xout=seq(5,1200,length=nstep)}. Otherwise the x variable is assumed to be an altitude in meters, and the interpolation is done over \code{xout=seq(0,30000,length=nstep)}.
}
\value{
A list of sounding time series, like \code{readSoundings}, but containing only the xname and ynames variables.
}
\references{
}
\author{
Gordon Maclean
}
\note{
}

\seealso{
\code{\link{readSoundings}}, \code{\link{approx}}
}
\examples{
    \dontrun{
        xs = readSoundings()
        sprofile(xs)
        xs = interpSoundings(xs,"p",c("temp","rh","wspd","wdir"))
        sprofile(xs)
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ soundings }
