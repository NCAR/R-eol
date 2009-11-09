\name{read.qc.eolfile}
\alias{read.qc.eolfile}
\title{Read in QCed EOL Sounding Data in eol file Format}
\description{
 Reads in QCed EOL sounding data, including radiosonde(upsonde), dropsonde and driftsonde, in eol file format.
}
\usage{
 read.qc.eolfile(file = stop("'file' must be specified"), nskip = 14)
}
\arguments{
 \item{file}{A string indicating where and what file to read from.}
 \item{nskip}{Number of lines of header to skip, don't change the number unless 
 the file format has been changed.}
}
\value{
 A data frame with QCed sounding variable (total 17 columns).
}
\details{
 This function reads in a EOL qced sounding data file, remove the file header
 (should always be 14 lines), and returns the file body with 17 named columns.
 The 17 columns are: time,utc.hh,utc.mm,utc.ss,p,temp,dewpt,rh,u,v,wspd,wdir,
 dz,gp.alt,lon,lat,gps.alt.
}
\author{Liangying Zhang \email{lzhang@ucar.edu}}
\seealso{
 \code{\link[EOLsounding]{read.raw.Dfile}}.
}
\examples{
file <- system.file("DropsondeData","qc.eol","D20080815_172011_PQC.eol",package="EOLsounding")
read.qc.eolfile(file = file)
}
\keyword{IO}
