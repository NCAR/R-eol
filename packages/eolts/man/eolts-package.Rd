\name{eolts-package}
\alias{eolts-package}
\alias{eolts}
\docType{package}
\title{
Support for reading, writing, analysis and plotting of time series and spectra
}
\description{
This package provides R functions to read, analyse, plot and write time series data, and to generate and plot spectra from time series. Support includes reading time series from NetCDF files, and writing time series to a NetCDF RPC server. Developed at the National Center for Atmospheric Research, Earth Observing Laboratory (NCAR EOL), sponsored by the National Science Foundation.
}
\details{
\tabular{ll}{
Package: \tab eolts\cr
License: \tab BSD_2_clause + file LICENSE\cr
URL: \tab http://github.com/ncareol/R-eol.git\cr
Authors@R: \tab person("Gordon","Maclean",role=c("aut","cre"),
            email="maclean@ucar.edu")\cr
Depends: \tab R (>= 3.0), methods, stats, splusTimeDate, graphics, gWidgets2\cr
Imports: \tab splusTimeSeries\cr
Suggests: \tab RUnit\cr
}
%% promptPackage generates a "Index:" with a preformatted section at this point.
%% The preformatted macro screws things up, causing the paragraph breaks below to be lost
%% in html output. According to the "Writing R Extensions", preformatted shouldn't be nested
%% within other macros.
The eolts package defines three primary S4 classes, \code{"\linkS4class{utime}"}, \code{"\linkS4class{nts}"} and \code{"\linkS4class{spectra}"}, as well as a class \code{"\linkS4class{netcdf}"} for accessing time series data from NetCDF files.

\code{"\linkS4class{utime}"} is a class for handling time. Do \code{class? utime} for information on the class, or \code{? utime} for help on the \code{\link{utime}} function.

An object of class \code{"\linkS4class{nts}"} is a matrix or data.frame, with a column for each variable and a row for each time observation. An \code{\link{nts}} has a \code{\link{positions}} slot, a vector of \code{\link{utime}}s, containing the times of the observations, one for each row in the matrix or data.frame.

\code{\link{spectra}} is an S4 base class for several types of spectra, consisting of a \code{matrix} of data, with a slot for an associated vector of \code{\link{frequencies}}.

\code{\link{fftw}} is object for Fast Fourier Transforms, based on FFTW from \url{www.fftw.org}, represented as a type of \code{\link{spectra}}. An \code{fftw} can be quickly generated from an \code{\link{nts}}.

Other \code{spectra} are generated from \code{fftw}s: \code{\link{pspectra}} (power spectra), \code{\link{xspectra}} (cross spectra), \code{\link{cospectra}}, \code{\link{quadspectra}} (quadrature spectra), \code{\link{phase}} and \code{\link{coherence}}.

A \code{\link{plot}} method has been defined for \code{"\linkS4class{nts}"} providing time labels on the X axis. \code{plot} is also available for \code{\link{spectra}}.

\code{"\linkS4class{netcdf}"} provides support for reading time series from NetCDF files into nts objects.

\code{\link{dpar}} sets global session values of use in accessing and processing data, such as the desired start and end \code{\link{utime}} when reading data from NetCDF files.

When the eolts package is attached to an R session, the following values are set in the global session \code{\link{options}}:
\itemize{
    \item \code{time.in.format=c( "\%Y\%m\%d\%H:\%M:\%OS", "\%Y\%m\%d\%H:\%M", "\%Y\%m\%d\%H\%M\%OS", "\%Y \%b\%d\%H:\%M:\%OS", "\%Y \%b\%d\%H:\%M", "\%Y \%b\%d\%H\%M\%OS")}
    \item \code{time.out.format = "\%Y \%m \%d \%H:\%M:\%OS3 \%Z"}
    \item \code{time.zone = Sys.timezone()}
}
\code{options("time.in.format")} provides default values for the \code{in.format} argument to \code{\link{utime}}, which tries each in succession when parsing a date and time from a character string.  With the above values for \code{in.format} any of the following time strings can be parsed with the \code{\link{utime}} function:
\itemize{
    \item "2014 6 23 14:57:27.23"
    \item "2014 6 23 14:57"
    \item "2014 6 23 145727.23"
    \item "2014 Jun 23 14:57:27.23"
    \item "2014 Jun 23 14:57"
    \item "2014 Jun 23 145727.23"
}
}
\author{
Gordon Maclean\cr
Maintainer: Gordon Maclean <maclean@ucar.edu>
}
\references{
}
\keyword{
package
ts
}
\seealso{
\tabular{ll}{
?command \tab link\cr
\code{class? utime} \tab \code{\linkS4class{utime}}\cr
\code{? utime} \tab \code{\link{utime}}\cr
\code{class? nts} \tab \code{\linkS4class{nts}}\cr
\code{? nts} \tab \code{\link{nts}}\cr
\code{class? netcdf} \tab \code{\linkS4class{netcdf}}\cr
\code{? netcdf} \tab \code{\link{netcdf}}\cr
\code{? dpar} \tab \code{\link{dpar}}\cr
\code{? isfs} \tab \code{\link[isfs:isfs-package]{isfs}}\cr
}
}
\examples{

}
