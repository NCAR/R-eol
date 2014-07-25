\name{plot_sunshadow}
\alias{plot_sunshadow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot shadow lengths over a year at a given latitude and longitude.
}
\description{
Plot shadow lengths over a year at a given latitude and longitude.
}
\usage{
plot_sunshadow(year = 2006, browse = F, lat = 40, lon = -105, range = 4, localhours = T, lowest = 0, verbose=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{year}{
4 digit year
}
  \item{lat}{
Latitude in degrees, negative for south latitudes.
}
  \item{lon}{
Longitude in degrees, negative for west.
}
  \item{range}{
Mazimum shadow length.
}
  \item{localhours}{
Plot hours in local time
}
  \item{lowest}{
Minimum sun angle.
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
plot_sunshadow(year=2014,lat=40,lon=-105)

# Hokitika NZ (southern hemisphere plots still need work)
plot_sunshadow(year=2014,lat=-42.7,lon=171)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ }
