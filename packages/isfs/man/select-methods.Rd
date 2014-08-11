\name{select-methods}
\docType{methods}
\alias{select-methods}
\alias{select,dat-method}
\title{ Select columns of an object }
\description{
Method to select columns of an object.
}
\section{Methods}{
\describe{

\item{\code{signature(x = "dat", stns = "numeric", hts = "numeric", sites = "character", sfxs = "character")}}{
Select columns of a \code{\linkS4class{dat}} time series by station, sampling height, site name or suffix.

If a parameter named \code{stns} is specified, it should be a numeric vector of integers to select
columns whose station, as returned by \code{stations(x)}, have an exact match in \code{stns}.
In the ISFS variable naming convention, variables from different stations may have the same name,
indicating they are a similar measurement, and will have an associated station number which is greater than 0.
These variables will be stored with a station dimension in NetCDF files, and this is copied to
the \code{stations} slot of the \code{\linkS4class{nts}} time series.  Station 0 is the
"non-station" for those variables whose name designates a single measurement from one station or
site.

If a parameter named \code{hts} is passed, it should be a numeric vector, to select the columne in
\code{x} by height.  The associated measurement height above ground, or depth, of an
ISFS variable is specified in its name, as a dot, followed by a numeric field, followed by
"m" for heights or "cm" for depths, followed by a dot or the end of the name.  For example,
the height of u.35m.near is 35 meters, and the "height" of Tsoil.3.1cm.flr is -0.031 meters.
An exact match of height is subject to precision errors, and so a height it matched if it is within 0.01 mm.
}
}}
\keyword{ts}
