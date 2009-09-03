\name{read.raw.Dfile}
\alias{read.raw.Dfile}
\title{Read in raw EOL sounding data in Dfile format}
\description{
 Reads in raw EOL sounding data, including dropsonde and driftsonde, in Dfile format.
}
\usage{
 read.raw.Dfile(file)
}
\arguments{
 \item{file}{A string indicating where and what file to read from.}
}
\value{
 A data frame with all raw sounding variable (total 20 columns).
}
\details{
 This function reads in a EOL raw sounding data file, remove the file header
 (should always be 6 lines) and file tails (should always be 19 lines), and 
 returns the file body with 20 named columns.
 The 20 columns are: ava,sta,sid,utc.date,utc.time,p,temp,rh,wdir,wspd,vvel,
 lon,lat,gp.alt,wsat,rh1,rh2,ssat,werr,gps.alt.
}
\author{Liangying Zhang \email{lzhang@ucar.edu}}
\seealso{
 \code{\link[EOLsounding]{read.qc.eolfile}}.
}
\examples{
dir <- "/scr/sci2/lzhang/TPARC/AF-C130/D-files"
f   <- "D20080815_230339_P.1"
file <- paste(dir, f, sep = "/")
read.raw.Dfile(file = file)
}
\keyword{IO}
