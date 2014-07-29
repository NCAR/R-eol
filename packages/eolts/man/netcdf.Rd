\name{netcdf}
\alias{netcdf}
\title{
Create a connection to a set of NetCDF files.
}
\description{
Create a connection to a set of NetCDF files.
}
\usage{
netcdf(file = Sys.getenv("NETCDF_FILE"), dir = Sys.getenv("NETCDF_DIR"), start = dpar("start"), end = dpar("end"), lenfile = dpar("lenfile"), timeNames = c("time", "Time"), server = Sys.getenv("NETCDF_SERVER"), interval = 300, cdlfile = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
Character vector, containing either the format of the NetCDF file names or actual file names. A file name format contains time field descriptors such as "\%Y", supported by \code{\link{utime}} and \code{\link{strptime}}
}
  \item{dir}{
Character vector of length 1, the path name of the directory containing the NetCDF files.
}
  \item{start}{
A \code{\linkS4class{utime}} object, containing the beginning of the time period to be read.
}
  \item{end}{
A \code{\linkS4class{utime}} object, containing the end of the time period to be read.
%%     ~~Describe \code{end} here~~
}
  \item{lenfile}{
Numeric value, the length of the files in seconds. This is optional.
}
  \item{timeNames}{
Character vector of names of the time variable in the NetCDF files.
}
  \item{server}{
Hostname of a NetCDF RPC server running the nc_server process. Only necessary if using \code{\link{writets}} to write data to NetCDF files.
}
  \item{interval}{
Time variable interval, in seconds. Only necessary if using \code{\link{writets}} to write data to NetCDF files.
}
  \item{cdlfile}{
Character string containing the name of a NetCDL file to be used when initializing new NetCDF files. Again, this is only necessary if using \code{\link{writets}} to write data to NetCDF files.
}
}
\details{
}
\value{
An object of class \code{\linkS4class{netcdf}}, a connection to a set of NetCDF files, which can then be used in calls to \code{\link{readts}}, \code{\link{writets}}, and \code{\link{close}}.
}
\references{
\url{http://www.unidata.ucar.edu/software/netcdf}
}
\author{
Gordon Maclean
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\tabular{ll}{
?command \tab link\cr
\code{class? netcdf} \tab \code{\linkS4class{netcdf}}\cr
\code{class? nts} \tab \code{\linkS4class{nts}}\cr
\code{? nts} \tab \code{\link{nts}}\cr
\code{class? utime} \tab \code{\linkS4class{utime}}\cr
\code{? utime} \tab \code{\link{utime}}\cr
\code{? dpar} \tab \code{\link{dpar}}\cr
\code{? eolts} \tab \code{\link[eolts:eolts-package]{eolts}}\cr
}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
