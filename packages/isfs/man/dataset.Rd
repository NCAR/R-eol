\name{dataset}
\alias{dataset}
\title{
Select a dataset, a collection of NetCDF files.
}
\description{
Select, by name or number, a dataset from the list that has been found by \code{\link{find_datasets}}. When a dataset is selected, system environment variables such as NETCDF_FILE, NETCDF_DIR, and DATASET are set, along with dpar parameters such as start and datacoords. See the details section, below.
}
\usage{
dataset(which, verbose = FALSE)
}
\arguments{
  \item{which}{
an optional character string of the name of a dataset, or integer index of a dataset.
}
  \item{verbose}{
optional logical value. If TRUE the details of the selected or current dataset are printed, or if argument \code{which} is missing, the names of the available datasets found by \code{\link{find_datasets}} are displayed.
}
}
\details{
If \code{which} is specified, then the current dataset is selected by name or index from those which must have been found previously by \code{\link{find_datasets}}. Selecting the current dataset sets system environment variables NETCDF_DIR, NETCDF_FILE and DATASET, along with the \code{\link{dpar}} parameters datacoords, start, lenfile,  h2o_flux_corrected, co2_flux_corrected, gsoil_philip_corrected, and lambdasoil_fat_corrected. The name of the current dataset is set in the user's session, and the list of parameters, comprising the dataset is returned.

If \code{which} is missing, the current dataset, which must have been set by an earlier call to \code{dataset(which)}, is returned.
}
\value{
A list composed of the following elements:
  \item{desc }{Description of the dataset}
  \item{ncd }{NetCDF directory}
  \item{ncf }{NetCDF file name format}
  \item{datacoords}{3D wind coordinates of dataset}
  \item{file_length_seconds}{3D wind coordinates of dataset}
  \item{h2o_flux_corrected}{See \link{find_datasets}.}
  \item{co2_flux_corrected}{See \link{find_datasets}.}
  \item{gsoil_philip_corrected}{See \link{find_datasets}.}
  \item{lambdasoil_fat__corrected}{See \link{find_datasets}.}
}
\references{
}
\author{
Gordon Maclean
}
\note{
}

\seealso{
\code{\link{find_datasets}}, \code{\link{datasets}}
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
