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
Logical value, specifying whether the "STA" sounding field in the D file is used to screen the PTU and wind/GPS data. If sta_clean is \code{TRUE}, then only records with an "STA" record starting with "S" will be kept, values for "P", "T" and "RH" will be set to \code{NA_real_} if the first digit in the corresponding STA value is 1: \code{S10 or S11} and values for wind and gps variables will be set to \code{NA_real_} if the second digit in the corresponding STA value is 1:\code{S01 or S11}
}
}
\details{
\code{readDFile} reads the "AVAPS-" and "STA" columns in the D file, and makes no assumptions about the number of lines in the header. The names of the variables are extracted from the first two AVAPS-T, COM lines in the file, and the units from the third AVAPS-T, COM line.

From the variable names in the Dfile header, the following translation from "Header Name" to "Column Name" is applied to generate column names in the sounding. Also shown are the missing data value for each column, and the good STA value, which is used if sta_clean is TRUE:
\tabular{llrl}{
    Header Name \tab Column Name \tab Missing Data \tab Good STA \cr
    AirPress \tab P \tab  9999 \tab S00 or S01\cr
    AirTemp \tab T \tab 99 \tab S00 or S01\cr
    RelHumid \tab RH \tab 999 \tab S00 or S01\cr
    WindDir \tab Wdir \tab 999 \tab S00 or S10\cr
    WindSpd \tab Wspd \tab 999 \tab S00 or S10\cr
    VertVeloc \tab Vz \tab 99 \tab S00 or S10\cr
    GPSLongitude \tab lon \tab 999 \tab S00 or S10\cr
    GPSLatitude \tab lat \tab 99 \tab S00 or S10\cr
    GeopotenAltitude \tab Alt_gp \tab 99999, 9999 \tab S00 or S10\cr
    GPSWnd \tab wsat \tab 999 \tab S00 or S10\cr
    SondeRH1 \tab RH1 \tab 999 \tab S00 or S01\cr
    SondeRH2 \tab RH2 \tab 999 \tab S00 or S01\cr
    GPSSnd \tab ssat \tab 999 \tab S00 or S10\cr
    WindError \tab Werr \tab 99 \tab S00 or S10\cr
    GPSAltitude \tab Alt_gps \tab 99999 \tab S00 or S10\cr
}

The data values are extracted from the AVAPS-D rows in the file. Values matching the missing data value are set to NA_real_.

The times are extracted from the UTCDate and UTCTime columns of the data lines in the Dfile. The resultant time series is sorted in time.

Then if sta_clean is TRUE, and the STA value in a record does not match the Good STA value shown above, the corresponding data values are overwritten with NA_real_.

\code{readDFile} uses the R function \code{\link{read.table}}. Therefore it supports reading files which have been compressed with 'gzip2', 'bzip2', 'xz' or 'lzma'.
}
\value{
A sorted time series containing all data columns read from the D file.
}
\references{
}
\author{
Gordon Maclean
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{readSoundings}} \code{\link{readQCFile}} \code{\link{read.table}}
}
\examples{
# Read sample D file contained in eolsonde R package
dir = system.file("unitTests","data","D-file",package="eolsonde")
dfile = file.path(dir,"D20080815_172011_P.1.gz")
x = readDFile(dfile)
# time series plot of P, T and RH
plot(x[,c("P","T","RH")])
# profile plot of T, RH vs P
sprofile(x,c("T","RH"),"P")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sounding }
