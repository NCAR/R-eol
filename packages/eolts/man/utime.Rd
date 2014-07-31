\name{utime}
\alias{utime}
\title{
Function to create a utime object.
}
\description{
Creates an object of class \code{"\linkS4class{utime}"}.
}
\usage{
utime(val = as.numeric(Sys.time()), in.format = getOption("time.in.format"), time.zone = getOption("time.zone"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{val}{
Often a character string, which is converted to a \code{"\linkS4class{utime}"}, using \code{\link{strptime}} with \code{in.format} and \code{time.zone}. If \code{val} is numeric, it should represent the number of non-leap seconds since Jan 1, 1970 00:00 UTC and is converted directly to a \code{\linkS4class{utime}} without requiring a format or time zone.  \code{val} can also be a POSIXct, splusTimeDate::timeDate, utime or a list, with a year, mon, day, hour,min and sec member.
}
  \item{in.format}{
Vector of one or more character strings describing the input time format, see \code{\link{strptime}}.  If \code{val} is character, the \code{strptime} function is used to parse it, using each element of \code{in.format} until success.
}
  \item{time.zone}{
Character string name of a time zone. See \code{\link{timezones}}.
}
}
\details{
When the \code{eolts} package is attached to an R session, the following values are set in \code{options}:
\itemize{
    \item \code{time.in.format = c( "\%Y\%m\%d\%H:\%M:\%OS", "\%Y\%m\%d\%H:\%M", "\%Y\%m\%d\%H\%M\%OS", "\%Y \%b\%d\%H:\%M:\%OS", "\%Y \%b\%d\%H:\%M", "\%Y \%b\%d\%H\%M\%OS")}
    \item \code{time.out.format = "\%Y \%m \%d \%H:\%M:\%OS3 \%Z"}
    \item \code{time.zone = Sys.timezone()}
}
\code{options("time.in.format")} provides default values for the \code{in.format} argument to \code{\link{utime}}.  When parsing a character time value, each member of \code{in.format} will be tried in succession until success.  With the above values for \code{time.in.format} any of the following time strings can be parsed with the \code{\link{utime}} function:
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
An object of type \code{"\linkS4class{utime}"}. If the \code{val} string could not be parsed successfully with any \code{in.format} the result will be NA.
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
\tabular{ll}{
?command \tab link\cr
\code{class? utime} \tab \code{"\linkS4class{utime}"}\cr
\code{? strftime} \tab \code{\link{strftime}}\cr                                        
\code{? timezones} \tab \code{\link{timezones}}\cr                                      
}                                        
For valid US time zones on GNU/Linux or MacOSX, see the names of files in \file{/usr/share/zoneinfo/US}. For example "US/Central" is a valid time zone name.

See \file{/usr/share/zoneinfo} for other time zones.
}
\examples{
t2 = utime()    # current time

# Parse time using formats in getOption("time.in.format"),
# and time zone in getOption("time.zone")
t1 = utime("2014 jan 13 01:32:49.717")
cat("time difference is:",t2-t1,"seconds\n")

# Format time to a string
tpacstr = format(t1,format="\%F \%H:\%M:\%OS4",time.zone="US/Pacific")
cat("t1 in Pacific time zone is",tpacstr,"\n")

# Convert to a list, using getOption("time.zone")
t1l = as(t1,"list")

options(time.zone="US/Eastern")
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
