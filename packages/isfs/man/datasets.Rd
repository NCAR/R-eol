\name{datasets}
\alias{datasets}
\title{
List available NetCDF datasets.
}
\description{
The list of available NetCDF datasets must have been set in the current session by \code{\link{find_datasets}}.
}
\usage{
datasets(all = FALSE, warn = TRUE)
}
\arguments{
  \item{all}{
Some datasets may have been disabled. Specifing \code{all=TRUE} will return all datasets, including disabled datasets.
}
  \item{warn}{
Whether to issue a \code{warning} if datasets are not found in the current session.
}
}
\details{
}
\value{
}
\author{
Gordon Maclean
}
\note{
}

\seealso{
\code{\link{find_datasets}}, \code{\link{dataset}}
}
\examples{
\dontrun{
# Look for datasets in /data/netcdf
find_datasets("/data","netcdf")
# Look for datasets in /data, in sub-directories with names starting with netcdf
find_datasets("/data","^netcdf")
# List the names of found datasets
names(datasets())
# select a dataset by name
dataset("geo_tiltcor")
# or select the first dataset found
dataset(1)
}
}
\keyword{ ts }
