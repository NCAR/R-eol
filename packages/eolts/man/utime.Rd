\name{utime}
\alias{utime}
\title{
Function to create a utime object.
}
\description{
Creates an object of \code{"\linkS4class{utime}"}.
}
\usage{
utime(val = as.numeric(Sys.time()), in.format = getOption("time.in.format"), time.zone = getOption("time.zone"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{val}{
Often a character string, which is converted to a \code{"\linkS4class{utime}"}, using \code{\link{strptime}} with the given in.format and time.zone. If \code{val} is numeric, it represents the number of non-leap seconds since Jan 1, 1970 00:00 UTC.  \code{val} can also be a POSIXct, splusTimeDate::timeDate, utime or a list, with a year, mon, day, hour,min and sec member.
}
  \item{in.format}{
Vector of one or more character strings describing the input time format, see \code{\link{strptime}}.  If \code{val} is character, the \code{strptime} function is used for parsing, using each element of \code{in.format} until success.
}
  \item{time.zone}{
Character string describing the time zone. See \code{\link{OlsonNames}}.
}
}
\details{
When the \code{eolts} package is attached to an R session, the following values are set in \code{options}:
\itemize{
    \item \code{time.in.format=c( "\%Y\%m\%d\%H:\%M:\%OS", "\%Y\%m\%d\%H:\%M", "\%Y\%m\%d\%H\%M\%OS", "\%Y \%b\%d\%H:\%M:\%OS", "\%Y \%b\%d\%H:\%M", "\%Y \%b\%d\%H\%M\%OS")}
    \item \code{time.out.format = "\%Y \%m \%d \%H:\%M:\%OS3 \%Z"}
    \item \code{time.zone = Sys.timezone()}
}
\code{options("time.in.format")} provides default values for the \code{in.format} argument to \code{\link{utime}}, which will try each in succession when parsing a date and time from a character string.  With the above values for \code{time.in.format} any of the following time strings can be parsed with the \code{\link{utime}} function:
\itemize{
    \item "2014 6 23 14:57:27.23"
    \item "2014 6 23 14:57"
    \item "2014 6 23 145727.23"
    \item "2014 Jun 23 14:57:27.23"
    \item "2014 Jun 23 14:57"
    \item "2014 Jun 23 145727.23"
}
}
\value{
An object of \code{"\linkS4class{utime}"}. If the \code{val} string could not be parsed successfully with any \code{in.format} the result will be NA.
}
\references{
\url{http://en.wikipedia.org/wiki/Unix_time}
}
\author{
Gordon Maclean
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
class?utime: \code{"\linkS4class{utime}"}
}
\examples{
t2 = utime()    # current time

# Parse time using formats in getOption("time.in.format"),
# and time zone in getOption("time.zone")
t1 = utime("2014 jan 13 01:32:32.717")

cat("time difference is:",t2-t1,"seconds\n")

options(time.zone="US/Mountain")
# loop over days in Feb 2013
for (tx in seq(from=utime("2013 feb 1 00:00"), to=utime("2013 mar 1 00:00")-1, by=86400)) {
    tx = utime(tx)  # must convert from numeric to utime.
    cat("day is",format(tx,format="\%d"),"\n")
    \dontrun{
    x = getData(tx,tx+86400)
    }
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
