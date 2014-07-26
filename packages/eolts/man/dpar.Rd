\name{dpar}
\alias{dpar}
\alias{dpar_eolts}
\title{Set/get global session parameters for accessing and processing data}
\description{
Set or get global parameters for accessing and processing data, in similar manner to \code{\link{par}} for graphics parameters, or session options with \code{\link{options}}.
}
\usage{
dpar(..., save_cache = FALSE)
}
\arguments{
  \item{\dots}{
To set one or more parameters, pass named arguments in the form \code{name = value} or a list containing named elements. To fetch the value one or more parameters, pass their names in a character vector. If no argument is passed, the current list of options is returned.
}
  \item{save_cache}{
Do not flush the data cache when changing a parameter which affects the reading of data from NetCDF. Generally leave it as the default.
}
}
\details{
The list of dpar parameters that is supported by the eolts package is shown below. The list can be extended by other packages, for example \code{\link[isfs:isfs-package]{isfs}}.
}
\value{
If one or more parameters were changed in the call to \code{dpar}, via arguments of the form \code{name = value}, or with a list argument, then a list is returned containing the previous value of each parameter that was set, by name.

If parameters were queried via a character vector of parameter names, the current values are returned. If one parmeter is queried, a single value is returned. If the character vector of names was longer than one, a list is returned, containing the parameter values, by name.

If no arguments were passed, then a list is returned, containing all current values, by name.
}
\section{Data Parameters}{
    \describe{
        \item{\code{start}}{start time to be passed to data access commands, such as \code{\link{netcdf}}. This can be character, or any type compatible with \code{\link{utime}}.}
        \item{\code{end}}{end time to be passed to data access commands, such as \code{\link{netcdf}}. This can be character, or any type compatible with \code{\link{utime}}. Setting \code{end} results in \code{lensec} being set to \code{(end - start)}.}
        \item{\code{lenday}, \code{lenhr}, \code{lenmin} or \code{lensec}}{Length of the time series period for accessing data. Setting any time length also sets the other lengths, and causes the \code{end} time to be set to \code{start + lensec}}
        \item{\code{lenfile}}{Length of data files in seconds. If \code{lenfile} is non-zero, the data file names are expected to represent times which are integral multiples of \code{lenfile} seconds from Jan 1, 1970. If \code{lenfile} is zero, the file names are not restricted.}
        \item{\code{ncverbose}}{Verbosity level when reading NetCDF variables: 0=quiet, 1=show variable names and times, 2=show vars, filenames, start, count indices.}
    }
}
\references{
}
\author{
Gordon Maclean
}
\note{
}

\seealso{
\tabular{ll}{
?command \tab link\cr
\code{class? utime} \tab \code{\linkS4class{utime}}\cr
\code{? utime} \tab \code{\link{utime}}\cr
\code{? netcdf} \tab \code{\link{netcdf}}\cr
\code{? dpar_isfs} \tab \code{\link{dpar_isfs}}\cr
}
}
\examples{
    old.dpar = dpar(start="2014 jun 19 13:00",lenhr=5)
    on.exit(dpar(old.dpar),add=TRUE)
}
\keyword{ ts }
