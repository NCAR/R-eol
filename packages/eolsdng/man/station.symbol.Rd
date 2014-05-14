\name{station.symbol}
\alias{station.symbol}
\title{ Adds meteorological station annotations to a plot. }
\description{ Adds meteorological station annotations at the given
              coordinates. The annotation includes speed and direction of the
              wind, temperature, pressure(height when at upper air), dewpoint
              ...  to a given plot. }

\usage{
station.symbol(cx, cy, direction=0, speed=NA,
               fill=rep(0,length(cx)), temp=NA, press=NA,
               dewpt=NA, circle=TRUE, cex=1)
}

\arguments{
  \item{cx}{x coordinates for annotations.}
  \item{cy}{y coordinates for annotations. Must have same length as cx.}
  \item{direction}{Wind direction. From 0 to 360 degree. Must have same length as cx/cy.}
  \item{speed}{Wind speed. Must have same length as cx/cy.}
  \item{fill}{Fill for visibility: 0 = clear skies, 1 = 25\% obscured, 2 = 50\% obscured, 3 = 75\% obscured, 4 = no visibility.
              Must have same length as cx/cy.}
  \item{temp}{Temperature values to plot symbol (must have same length as cx/cy).
              If NA then no temperature value will be annotated.}
  \item{press}{Pressure values to plot symbol (must have same length as cx/cy).
              If NA then no pressure value will be annotated.}
  \item{dewpt}{Dewpoint Temperature values to plot symbol (must have same length as cx/cy).
               If NA then no dewpoint value will be annotated.}
  \item{circle}{If TRUE, will plot the usual station symbols with a circle at its base.
                If FALSE, only wind symbols will be annotated.}
  \item{cex}{Usual plotting parameter. }
}

\value{ Adds to a plot. }

\author{ Doug Nychka, Eric Gilleland, Liangying Zhang }

\seealso{
 \code{\link{get.sounding.weathersymbols}}.
}

\examples{
   plot(0:1, 0:1, type="n")
   station.symbol(0.5, 0.5, direction=75, speed=30, fill=3,
                  temp=31, press=987, dewpt=26)
}

\keyword{ aplot }
