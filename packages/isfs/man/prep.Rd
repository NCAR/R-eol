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
prep(variables, rate = 0, dsm="", config = "")
}
\arguments{
  \item{variables}{A character vector containing the names of the variables to be read. }
  \item{rate}{The re-sampling rate, a numeric value, in Hz. A value of zero means resample the variables at the raw rate of the first variable specified. }
  \item{dsm}{The name of the DSM. If the raw-data files for a particular dsm have not been merged for the project, you can specify the name of the DSM, which will be passed as the value of the -d option to the prep program. prep will then look in the <dsm> section of the XML for an archive <output> element, in order to determine the raw-data file names. }
  \item{config}{The name of a configuration in \code{"$ISFF/projects/$PROJECT/ISFF/config/configs.xml"}. If config is an empty string, the configuration is guessed by finding the first configuration whose times fit the read request.}
}
\details{
}
\value{
A \code{\linkS4class{prep}} object for use with \code{\link{readts-methods}} and \code{\link{close-methods}}.
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
dataset("qc_geo_tiltcor")
con = prep(c("u","v","w","tc","diagbits","3m.far",sep="."),rate=20)   
dpar(start="2013 oct 13 15:00",lenmin=30)
x1 = readts(con)
dpar.next()
x2 = readts(con)
close(con)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
