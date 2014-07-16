\name{readts-methods}
\docType{methods}
\alias{readts-methods}
\alias{readts,prep,missing,character,character-method}
\alias{readts,prep,missing,missing,missing-method}
\alias{readts,prep,missing,utime,utime-method}
\title{Read a time series from a \linkS4class{prep} object }
\description{
Read a time series from a \linkS4class{prep} object.
}
\section{Methods}{
\describe{
\item{\code{readts(con,start,end)}}{\code{signature(con = "prep", start = "character", end = "character")}: Start the prep process if it hasn't already started, and read an \code{\link{nts}} time series for the variables in the time period [ \code{utime(start)} : \code{utime(end) } )}

\item{\code{readts(con)}}{\code{signature(con = "prep", start = "missing", end = "missing")}: Start the prep process if it hasn't already started, and read an \code{\link{nts}} time series in the time period [ \code{dpar("start")} : \code{dpar("end")} )}

\item{\code{readts(con,start,end)}}{\code{signature(con = "prep", start = "utime", end = "utime")}: Start the prep process if it hasn't already started, and read an \code{\link{nts}} time series in the time period [ \code{start} : \code{end} ) }
}}
\seealso{
\tabular{ll}{
?command \tab link\cr
\code{class? prep} \tab \code{\linkS4class{prep}}\cr
\code{class? nts} \tab \code{\linkS4class{nts}}\cr
}
}
\keyword{methods}
\keyword{ ts }
