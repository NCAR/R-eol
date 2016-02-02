\name{geoid_height}
\alias{geoid_height}
\title{
Return geoid height in meters at a point.
}
\description{
This function uses the \href{http://geographiclib.sourceforge.net/}{GeographicLib} C++ library and associated geoid datasets to compute the height of the geoid at a given latitude and longitude.

From the GeographicLib web site: \dQuote{The geoid height, N, can be used to convert a height above the ellipsoid, h, to the corresponding height above the geoid (roughly the height above mean sea level), H, using the relations: \eqn{h = N + H} and \eqn{H = -N + h}}.
}
\usage{
geoid_height(lat, lon, name = "egm96-5", cubic = TRUE)
}
\arguments{
  \item{lat}{
Vector of latitudes in fractional degrees north, from -90 to 90.
}
  \item{lon}{
Vector of longitudes in fractional degrees east, from -540 to 540.
}
  \item{name}{
Name of the geoid data file, such as "egm96-5". See the table below for possible names.
}
  \item{cubic}{
Interpolation method; FALSE means bilinear, TRUE (the default) means cubic.
}
}
\details{
The GeographicLib software, including shareable libraries and data files, must be installed on the system running R.

If you are building the eolsonde package from source, the C++ header files must also be installed, typically on /usr/include, or /usr/local/include.

On RedHat Linux systems, the GeographicLib RPM packages can be installed with yum:
\samp{
    sudo yum install GeographicLib GeographicLib-devel
}

Multiple geoid datasets of different geoid models and resolutions are available from GeographicLib, with the following names:
\tabular{lllll}{
  name        \tab geoid   \tab grid  \tab  tar.bz2 \tab disk \cr
              \tab         \tab resolution \tab  size,MB    \tab size,MB \cr
  egm84-30    \tab EGM84   \tab 30'   \tab   0.5    \tab  0.6 \cr
  egm84-15    \tab EGM84   \tab 15'   \tab   1.5    \tab  2.1 \cr
  egm96-15    \tab EGM96   \tab 15'   \tab   1.5    \tab  2.1 \cr
  egm96-5     \tab EGM96   \tab  5'   \tab    11    \tab   19 \cr
  egm2008-5   \tab EGM2008 \tab  5'   \tab    11    \tab   19 \cr
  egm2008-2_5 \tab EGM2008 \tab  2.5' \tab    35    \tab   75 \cr
  egm2008-1   \tab EGM2008 \tab  1'   \tab   170    \tab  470 \cr
}

On Linux systems, the geoid data are stored in .pgm files on \emph{/usr/share/GeographicLib/geoids}.
The  \emph{geographiclib-get-geoids} command (which is part of the GeographicLib RPM),
can then be used to download and install the geoid data files:

\samp{
    geographiclib-get-geoids -h
}

\samp{
    sudo geographiclib-get-geoids egm2008-5
}

On Macs, the geoid files should be downloaded from \href{http://geographiclib.sourceforge.net/}{GeographicLib} and un-tar'd to \emph{/usr/local/share/GeographicLib/geoids}. 
}
\value{
Vector containing the heights in meters of the geoid at the given points.
}
\references{
\url{http://geographiclib.sourceforge.net/}
}
\author{
Gordon Maclean
}
\note{
}

\seealso{
}
\examples{
geoid_height(0,0)
## [1] 17.163
geoid_height(0,0,"egm2008-1")
## [1] 17.226
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ geoid }
