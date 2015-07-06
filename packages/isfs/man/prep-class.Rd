\name{prep-class}
\Rdversion{1.1}
\docType{class}
\alias{prep-class}
\alias{close,prep-method}
\alias{pid,prep-method}
\alias{readts,prep,missing,character,character-method}
\alias{readts,prep,missing,missing,missing-method}
\alias{readts,prep,missing,utime,utime-method}

\title{Class \code{"prep"}}
\description{
Connection for reading high-rate ISFS data from the "prep" program.
}
\section{Objects from the Class}{
Use the \code{\link{prep}} function to create an object of class \code{prep}, passing it one or more variable names to be read, and an optional sampling rate.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{variables}:}{Object of class \code{"character"} The names of the variables to be read. }
    \item{\code{rate}:}{Object of class \code{"numeric"} The re-sampling rate, in Hz. A value of zero means resample the variables at the raw rate of the first variable specified. }
    \item{\code{dsm}:}{Object of class \code{"character"} If the raw-data files for a particular dsm have not been merged for the project, this can be used to indicate the name of the DSM. which will be passed as the value of the -d option to the prep program. With this option, prep will look in the <dsm> section of the XML for an archive <output> element, in order to determine the raw-data file names. }
    \item{\code{cppPtr}:}{Object of class \code{"raw"} Slot for a pointer to a C++ object, only used by the underlying C++ code.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{close(con)}}{\code{signature(con = "prep")}: Close an existing prep connection, shutting down the prep process. }
    \item{\code{pid(con)}}{\code{signature(con = "prep")}: Return the UNIX process ID of the prep process. Will be -1 if the process is not running. }
    \item{\code{readts(con,start,end)}}{\code{signature(con = "prep", start = "character", end = "character")}: Start the prep process if it hasn't already started, and read an \code{\link{nts}} time series for the variables in the time period [ \code{utime(start)} : \code{utime(end) } )}
    \item{\code{readts(con,start,end)}}{\code{signature(con = "prep", start = "missing", end = "missing")}: Start the prep process if it hasn't already started, and read an \code{\link{nts}} time series in the time period [ \code{dpar("start")} : \code{dpar("end")} )}
    \item{\code{readts(con,start,end)}}{\code{signature(con = "prep", start = "utime", end = "utime")}: Start the prep process if it hasn't already started, and read an \code{\link{nts}} time series in the time period [ \code{start} : \code{end} ) }
	 }
}
\references{
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
\code{? prep} \tab \code{\link{prep}}\cr
\code{class? nts} \tab \code{\linkS4class{nts}}\cr
\code{methods? readts} \tab \code{\link{readts-methods}}\cr
}
}
\examples{
\dontrun{
con = prep(c("u.3m.far","v.3m.far","w.3m.far","tc.3m.far","diagbits.3m.far"),rate=20)   
dpar(start="2013 oct 13 15:00",lenmin=30)
x = readts(con)
close(con)
}
}
\keyword{classes}
\keyword{ts}
