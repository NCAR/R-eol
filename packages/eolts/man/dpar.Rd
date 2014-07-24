\name{dpar}
\alias{dpar}
\title{Set/get global session parameters for accessing and processing data}
}
\description{
\code{dpar} Sets or gets global parameters for accessing and processing data, in similar manner to \code{\link{par}} for graphics parameters, or session options with \code{\link{options}}.
}
\usage{
dpar(..., save_cache = F)
}
\arguments{
  \item{\dots}{
To set one or more parameters, pass named arguments in the form \code{tag = value}, from the list below, or a list containing named elements. To fetch the value one or more parameters, pass their names in a character vector. If no argument is passed, the current list of options is returned.
}
  \item{save_cache}{
}
}
\details{
}
\value{
If one or more parameters were changed in the call to \code{dpar}, via arguments of the form \code{tag = value}, or with a list argument, then a list is returned containing the previous value of each parameter that was set, by name.

If parameters were queried via a character vector of parameter names, the current values are returned. If one parmeter is queried, a single value is returned. If the character vector of names was longer than one, a list is returned, containing the parameter values, by name.

eIf no arguments were passed, then a list is returned, containing all current values, by name.
}
\section{Data Parameters}{
    \describe{
        \item{\code{start}}{start time to be passed to data access commands, such as \code{\link{netcdf}} or used internally by \code{\link{dat}}. This can be character, or any type compatible with \code{\link{utime}}.}
        \item{\code{end}}{end time to be passed to data access commands, such as \code{\link{netcdf}} or used internally by \code{\link{dat}}. This can be character, or any type compatible with \code{\link{utime}}. Setting \code{end} results in \code{lensec} being set to \code{(end - start)}.}
        \item{\code{lenday}}{Number of days in the data period. Specifying a data period time length is an alternative to specifying an \code{end} time. Setting \code{lenday} causes the \code{end} time to be set to \code{start + lenday * 86400}, where 86400 is the number of seconds in a day. This value cannot be read. Query \code{lensec} return read the time length in seconds.}
        \item{\code{lenhr}}{Number of hours in the data period. As with \code{lenday}, the \code{end} time is set to \code{start + lenhr * 3600}, and it cannot be queried.}
        \item{\code{lenmin}}{Number of minutes in the data period. As with \code{lenday}, the \code{end} time is set to \code{start + lenhr * 60}, and it cannot be queried.}
        \item{\code{lensec}}{Number of seconds in the data period. As with \code{lenday}, the \code{end} time is set to \code{start + lenhr * 60}.}
        \item{\code{lenfile}}{Length of data files in seconds. If \code{lenfile} is non-zero, the data file names are expected to represent times which are integral multiples of \code{lenfile} seconds from Jan 1, 1970. If \code{lenfile} is zero, the file names are not restricted.}
        \item{\code{robust}}{Logical value used by ISFS data processing. When robust=TRUE, simple algorithms are used in derivations by the various \code{\link{dat}} functions. When robust=FALSE, more complicated algorithm, which may be less robust in terms of run-time errors, are used.}
        \item{\code{datacoords}}{Character value, either "geo" or "instrument" indicating the current coordinates of the 3D wind vectors.}
        \item{\code{coords}}{Character value, either "geo" or "instrument" indicating what 3D wind coordinates the user wants. If \code{coords} is not equal to \code{datacoords}, then the \code{dat.u} and \code{dat.v} functions will apply rotations to U and V, using the current value of \code{Vazimuth}.}

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
    \code{\link{netcdf}}, \code{\link{utime}}
}
\examples{
    dpar(start="2014 jun 19 13:00",lenhr=5)

    old.dpar = dpar(robust=FALSE)
    on.exit(dpar(old.dpar),add=TRUE)
}
\keyword{ ts }
