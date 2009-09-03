\name{read.qc.eolfile}
\alias{read.qc.eolfile}
\title{Read in QCed EOL sounding data in eol file format}
\description{
 Reads in QCed EOL sounding data, including dropsonde and driftsonde, in eol file format.
}
\usage{
 read.qc.eolfile(file, nskip = 14)
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
dir <- "/scr/sci2/lzhang/TPARC/AF-C130/qc.eol"
f   <- "D20080815_230339_PQC.eol"
file <- paste(dir, f, sep = "/")
read.qc.eolfile(file = file)
}
\keyword{IO}
