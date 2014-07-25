\name{plot_sunangle}
\alias{plot_sunangle}
\title{
Make a summary plot of sun elevation vs azimuth at a given latitude, longitude for a given year.
}
\description{
Make a summary plot of sun elevation vs azimuth at a given latitude, longitude for a given year.
}
\usage{
plot_sunangle(browse = F, year = 2006, lat = 40, lon = -105, yhi = 90, twilight = T, localhours = T, verbose=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{browse}{
}
  \item{year}{
4 digit year
}
  \item{lat}{
Latitude in degrees, negative for south latitudes.
}
  \item{lon}{
Longitude in degrees, negative for west.
}
  \item{yhi}{
Upper plot limit for azimuth.
}
  \item{twilight}{
Shows values for civil,nautical and astronomical twilight below the horizon.
}
  \item{localhours}{
Use local time, or if FALSE, GMT.
}
  \item{verbose}{
Print a usage message.
}
}
\details{
}
\value{
}
\references{
}
\author{
John Militzer
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
}
\examples{
# Boulder
plot_sunangle(year=2014,lat=40,lon=-105)

# Hokitika NZ (southern hemisphere plots still need work)
plot_sunangle(year=2014,lat=-42.7,lon=171)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ }
