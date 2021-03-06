\name{netcdf-class}
\Rdversion{1.1}
\docType{class}
\alias{netcdf-class}
\alias{is.open}
\alias{readnc}
\alias{readts}
\alias{stations}
\alias{variables}
\alias{writets}

\title{Class \code{"netcdf"}}
\description{
A connection to a group of NetCDF files
}
\section{Objects from the Class}{
The usual way to create a \code{\linkS4class{netcdf}} object is with the \code{\link{netcdf}} function.  A lower-level alternative is \code{new("netcdf", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{file}:}{Object of class \code{"character"} Format of the NetCDF file names, often containing time field descriptors such as "\%Y", supported by \code{\link{utime}} and \code{\link{strptime}} }
    \item{\code{dir}:}{Object of class \code{"character"} Directory path to NetCDF files }
    \item{\code{start}:}{Object of class \code{"utime"} Start of time period for reading data }
    \item{\code{end}:}{Object of class \code{"utime"} End of time period for reading data }
    \item{\code{lenfile}:}{Object of class \code{"integer"} File length, in seconds or 0 for variable length }
    \item{\code{timeNames}:}{Object of class \code{"character"} Names for time variable in NetCDF files }
    \item{\code{server}:}{Object of class \code{"character"} If writing to NetCDF files with nc_server RPC protocol, the hostname of the server  }
    \item{\code{interval}:}{Object of class \code{"numeric"} Desired time variable delta-t in seconds when writing  files with nc_server  }
    \item{\code{cdlfile}:}{Object of class \code{"character"} Name of NetCDL file to be used for new NetCDF file initialization }
    \item{\code{cppPtr}:}{Object of class \code{"raw"} Pointer to underlying C+++ object }
  }
}
\section{Methods}{
  \describe{
    \item{close}{\code{signature(con = "netcdf")}: ... }
    \item{format}{\code{signature(x = "netcdf")}: ... }
    \item{is.open}{\code{signature(con = "netcdf")}: ... }
    \item{readnc}{\code{signature(con = "netcdf", variables = "character", start = "integer", count = "integer")}: ... }
    \item{readnc}{\code{signature(con = "netcdf", variables = "character", start = "missing", count = "missing")}: ... }
    \item{readnc}{\code{signature(con = "netcdf", variables = "list", start = "integer", count = "integer")}: ... }
    \item{readnc}{\code{signature(con = "netcdf", variables = "list", start = "missing", count = "missing")}: ... }
    \item{readts}{\code{signature(con = "netcdf", variables = "character", start = "missing", end = "missing")}: ... }
    \item{readts}{\code{signature(con = "netcdf", variables = "character", start = "utime", end = "utime")}: ... }
    \item{readts}{\code{signature(con = "netcdf", variables = "list", start = "missing", end = "missing")}: ... }
    \item{readts}{\code{signature(con = "netcdf", variables = "list", start = "utime", end = "utime")}: ... }
    \item{show}{\code{signature(object = "netcdf")}: ... }
    \item{stations}{\code{signature(x = "netcdf")}: ... }
    \item{variables}{\code{signature(con = "netcdf")}: ... }
    \item{writets}{\code{signature(con = "netcdf", x = "missing")}: ... }
    \item{writets}{\code{signature(con = "netcdf", x = "nts")}: ... }
	 }
}
\references{
\url{http://www.unidata.ucar.edu/software/netcdf}
}
\author{
Gordon Maclean
}
\note{
}

\seealso{
\tabular{ll}{
?command \tab link\cr
\code{? netcdf} \tab \code{\link{netcdf}}\cr
\code{class? nts} \tab \code{\linkS4class{nts}}\cr
\code{? nts} \tab \code{\link{nts}}\cr
\code{class? utime} \tab \code{\linkS4class{utime}}\cr
\code{? utime} \tab \code{\link{utime}}\cr
\code{? dpar} \tab \code{\link{dpar}}\cr
}
}
\examples{
}
\keyword{classes ts}
