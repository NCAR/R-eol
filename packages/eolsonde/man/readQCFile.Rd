\name{readQCFile}
\alias{readQCFile}
\title{Read in QCed EOL Sounding Data in QC file Format}
\description{
 Reads in QCed EOL sounding data, including radiosonde(upsonde), dropsonde and driftsonde, in the EOL QC file format.
}
\usage{
readQCFile(file)
}
\arguments{
  \item{file}{
Full path name of QC file to be read.
}
}
\details{
\code{readQCFile} finds the "UTC Launch Time" line in the file header to determine the launch date. It then looks for the line in the file starting with "----". The previous two lines contain the variable names and the units. It then reads the data, after the "----".

From the variable names, the following translation from "Header Name" to "Column Name" is applied to generate column names in the sounding. Also shown are the missing data values for each column:
\tabular{llr}{
    Header Name \tab Column Name \tab Missing Data\cr
    Press \tab P \tab  -999 \cr
    Temp \tab T \tab -999 \cr
    Dewpt \tab Dewpt \tab -999 \cr
    RH \tab RH \tab -999 \cr
    Uwind \tab U \tab -999 \cr
    Vwind \tab V \tab -999 \cr
    WindSpd \tab Wspd \tab -999 \cr
    Dir \tab Wdir \tab -999 \cr
    dZ \tab Vz \tab -999 \cr
    GeoPoAlt \tab Alt_gp \tab -999 \cr
    Lon \tab lon \tab -999 \cr
    Lat \tab lat \tab -999 \cr
    GPSAlt \tab Alt_gps \tab -999 \cr
}

Data values matching the missing data value are set to NA_real_.

The data times are determined from the "hh", "mm" and "ss" columns in the file, combined with the launch date. The resultant time series is sorted in time.

\code{readQCFile} uses the R function \code{\link{read.table}}. Therefore it supports reading files which have been compressed with 'gzip2', 'bzip2', 'xz' or 'lzma'.
}
\value{
A sorted time series containing all data columns read from the QC file.
}
\references{
}
\author{
Gordon Maclean
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{readSoundings}} \code{\link{readDFile}} \code{\link{read.table}}
}
\examples{
dir = system.file("unitTests","data","qc.eol",package="eolsonde")
dfile = file.path(dir,"D20080815_172011_PQC.eol.gz")
x = readQCFile(dfile)
# time series plot of P, T and RH
plot(x[,c("P","T","RH")])
# profile plot of T, RH vs P
sprofile(x,c("T","RH"),"P")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sounding }
