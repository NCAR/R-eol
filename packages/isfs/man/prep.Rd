\name{prep}
\alias{prep}
\title{
Create a \code{\linkS4class{prep}} object for reading high-rate ISFS data.
}
\description{
The \code{\linkS4class{prep}} object is the connection to the ISFS "prep" program, which generates calibrated high-rate time series from a raw archive.

The NIDAS software and the project configuration environment and XML must be installed on the user's system, and the raw high-rate data archive for the time period of interest must be accessible.
}
\usage{
prep(variables, rate = 0)
}
\arguments{
  \item{variables}{A character vector containing the names of the variables to be read. }
  \item{rate}{The re-sampling rate, a numeric value, in Hz. A value of zero means resample the variables at the raw rate of the first variable specified. }
}
\details{
}
\value{
A \code{\linkS4class{prep}} object for use with \code{\link{readts}} and \code{\link{close}}.
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
\code{class? prep} \tab \code{\linkS4class{prep}}\cr
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
