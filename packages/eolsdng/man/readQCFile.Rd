\name{readQCFile}
\alias{readQCFile}
\title{Read in QCed EOL Sounding Data in eol file Format}
\description{
 Reads in QCed EOL sounding data, including radiosonde(upsonde), dropsonde and driftsonde, in eol file format.
}
\usage{
readQCFile(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
Full path name of QC file to be read.
}
}
\details{
}
\value{
A time series containing all variables read from the D file.
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
\code{\link{readDFile}}
}
\examples{
    \dontrun{
        x = readQCFile("D20080815_175400_PQC.eol")
        # time series plot of pressure, temp and rh
        plot(x[,c("p","temp","rh")])
        plotSprofile(x)
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sounding }
