\name{readDFile}
\alias{readDFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read in Raw EOL Sounding Data in Dfile Format}
\description{
Reads in raw EOL sounding data, including radiosonde(upsonde), dropsonde and driftsonde, in Dfile format.
}
\usage{
readDFile(file, sta_clean = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
Full path name of D file to be read.
}
  \item{sta_clean}{
Logical value, specifying whether the "sta" sounding field in the D file is used to screen the PTU and wind/GPS data. Values for "p", "temp" and "rh" will be set to \code{NA_real_} if the first digit in the corresponding sta value is 1: \code{S10 or S11}.  Values for wind and gps variables will be set to \code{NA_real_} if the second digit in the corresponding sta value is 1:\code{S01 or S11}
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
\code{\link{readSoundings}}
\code{\link{readQCFile}}
}
\examples{
# Read sample D file contained in eolsdng R package
dir = system.file("unitTests","data","D-file",package="eolsdng")
dfile = file.path(dir,"D20080815_172011_P.1")
x = readDFile(dfile)
# time series plot of pressure, temp and rh
plot(x[,c("p","temp","rh")])
# profile plot
plotSprofile(x[,c("p","temp","rh","wspd","wdir")])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sounding }
