\name{readDFile}
\alias{readDFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read in Raw EOL Sounding Data in D file Format}
\description{
Reads in raw EOL sounding data, including radiosonde(upsonde), dropsonde and driftsonde, in "D" file format.
}
\usage{
readDFile(file, checkStatus=dpar("checkSondeStatus"), sondeRecords=dpar("sondeRecords"))
}
\arguments{
  \item{file}{
Full path name of D file to be read.
}
  \item{checkStatus}{
Logical value, specifying whether the second column in the D file is used to screen record type, and the PTU and wind/GPS data.
  }
  \item{sondeRecords}{
Character vector, containing one or more of the letters "A" (aircraft data system record), "P" (pre-launch) or "S" sounding, indicating what records should be kept from the D file.
}
}
\details{
\code{readDFile} decodes the first and second text columns in the D file, making no assumptions about the number of lines in the header. The names of the variables are extracted from the first two AVAPS-T, COM lines in the file, and the units from the third AVAPS-T, COM line.

The trailing digits of the first column are converted to integer and written into the "avaps" column in the time series result. These typically indicate the AVAPS radio channel.  For example "AVAPS-D05" will have an AVAPS value of 5.

The second column in a D file indicates the record type, and is one of the following:
\tabular{ll}{
STA \tab start time \cr
COM \tab comment \cr
LAU \tab launch time \cr
Ann \tab aircraft data system record, where n is a digit, 0 or 1 \cr
Pnn \tab pre-launch record \cr
Snn \tab sounding record \cr
VER \tab AVAPS software version \cr
FMT \tab D file format version \cr
TOF \tab  \cr
END \tab end time \cr
}

For the Ann, Pnn, or Snn record types, the nn indicates the transmission status of the PTU and GPS data, where n=0 indicates the tranmission was OK and n=1 indicates a potential problem. The first digit is for PTU values, and the second digit for GPS values.

If checkStatus is \code{TRUE}, then only "*nn" records with the leading character matching one of the values in \code{sondeRecords} will be kept. Values for "P", "T" and "RH" will be set to \code{NA_real_} if the PTU status digit is 1, i.e. if nn is "10" or "11". Values for wind and gps variables will be set to \code{NA_real_} if the GPS status digit is 1, i.e. if nn is "01" or "11".

The second column is converted to a numeric column in the time series, named "status", based on the following table, where the first character is converted to a multiple of of 10, "A" = 0, "P" = 10, and "S" = 20, and the status digits to their binary value:
\tabular{lrr}{
2nd col \tab description \tab "status" value \cr
A00 \tab aircraft record \tab 0 \cr
P11 \tab pre-launch record \tab 13 \cr
S00 \tab sounding record \tab 20 \cr
S01 \tab sounding record \tab 21 \cr
S10 \tab sounding record \tab 22 \cr
S11 \tab sounding record \tab 23 \cr
}

The times are extracted from the UTCDate and UTCTime columns of the data lines in the D file. The resultant time series is sorted in time.

The remaining data values are extracted from the AVAPS-D rows in the file. Values matching the missing data value are set to NA_real_.

From the variable names in the D file header, the following translation from "Header Name" to "Column Name" is applied to generate column names in the sounding. Also shown are the missing data value for each column:

\tabular{lll}{
    Header Name \tab Column Name \tab Missing Data \cr
    AirPress \tab P \tab  9999 \cr
    AirTemp \tab T \tab 99 \cr
    RelHumid \tab RH \tab 999 \cr
    WindDir \tab Wdir \tab 999 \cr
    WindSpd \tab Wspd \tab 999 \cr
    VertVeloc \tab Vz \tab 99 \cr
    GPSLongitude \tab lon \tab 999 \cr
    GPSLatitude \tab lat \tab 99 \cr
    GeopotenAltitude \tab Alt_gp \tab 99999, 9999 \cr
    GPSWnd \tab wsat \tab 999 \cr
    SondeRH1 \tab RH1 \tab 999 \cr
    SondeRH2 \tab RH2 \tab 999 \cr
    GPSSnd \tab ssat \tab 999 \cr
    WindError \tab Werr \tab 99 \cr
    GPSAltitude \tab Alt_gps \tab 99999 \cr
}

\code{readDFile} uses the R function \code{\link{read.table}}. Therefore it supports reading files which have been compressed with 'gzip2', 'bzip2', 'xz' or 'lzma'.

}
\value{
A sorted time series containing the data columns read from the D file.
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
