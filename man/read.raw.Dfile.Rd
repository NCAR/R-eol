\name{read.raw.Dfile}
\alias{read.raw.Dfile}
\title{Read in Raw EOL Sounding Data in Dfile Format}
\description{
 Reads in raw EOL sounding data, including radiosonde(upsonde), dropsonde and driftsonde, in Dfile format.
}
\usage{
 read.raw.Dfile(file = stop("'file' must be specified"))
}
\arguments{
 \item{file}{A string indicating where and what file to read from.}
}
\value{
 A data frame with all raw sounding variable (total 20 columns).
 Be aware that all valid and invalid data have been returned.
}
\details{
 This function reads in a EOL raw sounding data file, remove the file header
 (should always be 6 lines) and file tails (should always be 19 lines), and 
 returns the file body with 20 named columns. 
 The 20 columns are: ava,sta,sid,utc.date,utc.time,p,temp,rh,wdir,wspd,vvel,
 lon,lat,gp.alt,wsat,rh1,rh2,ssat,werr,gps.alt.
 Be aware that all data types including Sxx, Axx, Pxx are read and returned.
}
\author{Liangying Zhang \email{lzhang@ucar.edu}}
\seealso{
 \code{\link[EOLsounding]{read.qc.eolfile}}.
}
\examples{
file <- system.file("DropsondeData","D-file","D20080815_172011_P.1",package="EOLsounding")
read.raw.Dfile(file = file)
}
\keyword{IO}
