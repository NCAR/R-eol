\name{utime-class}
\Rdversion{1.1}
\docType{class}
\alias{utime-class}
\alias{[,utime-method}
\alias{[<-,utime,ANY,missing,ANY-method}
\alias{[<-,utime,ANY,missing,numeric-method}
\alias{[<-,utime,ANY,missing,utime-method}
\alias{abline,missing,missing,missing,utime-method}
\alias{all,utime-method}
\alias{any,utime-method}
\alias{c,utime-method}
\alias{coerce,character,utime-method}
\alias{coerce,list,utime-method}
\alias{coerce,numeric,utime-method}
\alias{coerce,POSIXct,utime-method}
\alias{coerce,timeDate,utime-method}
\alias{coerce,utime,character-method}
\alias{coerce,utime,list-method}
\alias{coerce,utime,POSIXct-method}
\alias{coerce,utime,timeDate-method}
\alias{deltat,utime-method}
\alias{end<-,nts,utime-method}
\alias{end<-,spectra,utime-method}
\alias{format,utime-method}
\alias{Math,utime-method}
\alias{monthly,utime,utime-method}
\alias{Ops,numeric,utime-method}
\alias{Ops,utime,numeric-method}
\alias{Ops,utime,utime-method}
\alias{positions<-,nts,utime-method}
\alias{prod,utime-method}
\alias{readts,netcdf,character,utime,utime-method}
\alias{readts,netcdf,list,utime,utime-method}
\alias{show,utime-method}
\alias{start<-,nts,utime-method}
\alias{start<-,spectra,utime-method}
\alias{sum,utime-method}
\alias{summary,utime-method}
\alias{Summary,utime-method}

\title{Class \code{"utime"}}
\description{
A class for manipulating time, based on POSIX time, see \url{http://en.wikipedia.org/wiki/Unix_time}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{utime(val)}, where \code{val} is numeric, character, utime, POSIXct, splusTimeDate::timeDate or a list.

Since utime is represented by a number of seconds since Jan 1, 1970 00:00 UTC, math operations with utimes, such as adding or subtracting, are performed numerically in terms of seconds, unless indicated otherwise.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"numeric"}, the number of non-leap seconds since Jan 1, 1970, 00:00 UTC}
  }
}
\section{Extends}{
Class \code{"\linkS4class{positionsCalendar}"}, directly.
Class \code{"\linkS4class{numeric}"}, from data part.
Class \code{"\linkS4class{vector}"}, by class "numeric", distance 2.
Class \code{"\linkS4class{positionsNumeric}"}, by class "numeric", distance 2.
Class \code{"\linkS4class{positions}"}, by class "numeric", distance 3.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "utime")}: access an element of a vector of utimes: ut[i] }
    \item{[<-}{\code{signature(x = "utime", i = "ANY", j = "missing", value = "ANY")}: assign an element of a vector of utimes from a supported type: \code{ut[2] <- "2013 oct 13 00:14:55"} }
    \item{[<-}{\code{signature(x = "utime", i = "ANY", j = "missing", value = "numeric")}: assign an element of a vector of utimes from a numeric number of seconds since Jan 1 1970, 00:)) UTC: \code{x[2] <- 1401744136} }
    \item{[<-}{\code{signature(x = "utime", i = "ANY", j = "missing", value = "utime")}: assign an element of a vector of utimes from a utime } 
    \item{abline}{\code{signature(a = "missing", b = "missing", h = "missing", v = "utime")}: draw a vertical line on a time series plot at time v }
    \item{all}{\code{signature(x = "utime")}: implementation of all() function for utime }
    \item{any}{\code{signature(x = "utime")}: implementation of any() function for utime }
    \item{c}{\code{signature(x = "utime")}: combine utimes }
    \item{coerce}{\code{signature(from = "character", to = "utime")}: convert from character to POSIXct to utime, using options("time.zone") }
    \item{coerce}{\code{signature(from = "list", to = "utime")}: convert from a list to utime, using options("time.zone") }
    \item{coerce}{\code{signature(from = "numeric", to = "utime")}: ... }
    \item{coerce}{\code{signature(from = "POSIXct", to = "utime")}: ... }
    \item{coerce}{\code{signature(from = "timeDate", to = "utime")}: ... }
    \item{coerce}{\code{signature(from = "utime", to = "character")}: ... }
    \item{coerce}{\code{signature(from = "utime", to = "list")}: ... }
    \item{coerce}{\code{signature(from = "utime", to = "POSIXct")}: ... }
    \item{coerce}{\code{signature(from = "utime", to = "timeDate")}: ... }
    \item{deltat}{\code{signature(x = "utime")}: ... }
    \item{end<-}{\code{signature(x = "nts", value = "utime")}: ... }
    \item{end<-}{\code{signature(x = "spectra", value = "utime")}: ... }
    \item{format}{\code{signature(x = "utime")}: ... }
    \item{Math}{\code{signature(x = "utime")}: ... }
    \item{monthly}{\code{signature(from = "utime", to = "utime")}: ... }
    \item{Ops}{\code{signature(e1 = "numeric", e2 = "utime")}: ... }
    \item{Ops}{\code{signature(e1 = "utime", e2 = "numeric")}: ... }
    \item{Ops}{\code{signature(e1 = "utime", e2 = "utime")}: ... }
    \item{positions<-}{\code{signature(object = "nts", value = "utime")}: ... }
    \item{prod}{\code{signature(x = "utime")}: ... }
    \item{readts}{\code{signature(con = "netcdf", variables = "character", start = "utime", end = "utime")}: ... }
    \item{readts}{\code{signature(con = "netcdf", variables = "list", start = "utime", end = "utime")}: ... }
    \item{show}{\code{signature(object = "utime")}: ... }
    \item{start<-}{\code{signature(x = "nts", value = "utime")}: ... }
    \item{start<-}{\code{signature(x = "spectra", value = "utime")}: ... }
    \item{sum}{\code{signature(x = "utime")}: ... }
    \item{summary}{\code{signature(object = "utime")}: ... }
    \item{Summary}{\code{signature(x = "utime")}: ... }
	 }
}
\references{
\url{http://en.wikipedia.org/wiki/Unix_time}.
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
\code{? utime} \tab \code{\link{utime}}\cr
\code{class? nts} \tab \code{\linkS4class{nts}}\cr
\code{? nts} \tab \code{\link{nts}}\cr
\code{? dpar} \tab \code{\link{dpar}}\cr
\code{? eolts} \tab \code{\link[eolts:eolts-package]{eolts}}\cr
}
}
\examples{
options(time.zone="US/Central")

# Parse time using formats in getOption("time.in.format"),
# and time zone in getOption("time.zone")
t1 = utime("2013 May 13 01:04")
t1 = t1 + 3600      # add an hour

# format to a string, US Pacific time
tstr = format(t1,format="\%F \%T",time.zone="US/Pacific")
cat("time is",tstr,"\n")

t1 = t1 + 0.2584      # fractional seconds

# Use \%OSn to print n digits of fractional seconds
# If n is not specified, it defaults to getOption("digits.secs")
format(t1,format="\%F \%H:\%M\%OS4",time.zone="US/Pacific")

options(digits.secs=6)
format(t1,format="\%F \%H:\%M\%OS",time.zone="UTC")

# Example using \%OS to read fractional seconds, \%j to read day of year, 1-366:
t1 = utime("2014 87 20:43:19.993",in.format="\%Y \%j \%H:\%M:\%OS",time.zone="UTC")

# display fractional amount using modulus operator
t1 \%\% 1

# class details
showClass("utime")
}
\keyword{classes}
