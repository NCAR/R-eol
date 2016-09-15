\name{find_datasets}
\alias{find_datasets}
\title{
Search a path for NetCDF datasets.
}
\description{
Search a path for directories matching a pattern, selecting each of those directories containing NetCDF files whose names match a date and time format convention. For each matching directory, parse the files in that directory and assemble a list of parameters, containing sufficient information to access those files with R. The list of available datasets is saved to the the user's session environment and returned.
}
\usage{
find_datasets(path = NULL, pattern = "^netcdf")
}
\arguments{
  \item{path}{
Character string containing a path to one or more directories matching \code{pattern}. If NULL, then path is set to \code{file.path(Sys.getenv("ISFS"), "projects", Sys.getenv("PROJECT"), "ISFS")}.
}
  \item{pattern}{
Character string containing a regular expression, compatible with \code{\link{list.files}}, to find directories of NetCDF files.
}
}
\details{
\code{list.files(path,pattern,include.dirs=TRUE)} is used to determine the possible list of directories of NetCDF files. Then each of those directories is scanned for files with a ".nc" or ".cdf" file extension.

Then the names of matching files are scanned to find a unique date and time pattern, containing numeric fields YYYY or YY and mm, dd, HH, MM and SS as listed, where the fields must be in order, from larger time period to smaller:
    \itemize{
        \item YYYY, 4 digit year: 20[0-9][0-9] or 1[3-9][0-9][0-9]
        \item YY, 2 digit year: [0-9][0-9]
        \item mm, 2 digit month: [01][0-9]
        \item dd, 2 digit day of month: [0-3][0-9]
        \item HH, 2 digit hour: [0-2][0-9]
        \item MM, 2 digit minute: [0-5][0-9]
        \item SS, 2 digit second: [0-5][0-9]
    }
   
Other non-numeric characters may be found between the fields and not all the fields must be found in the names, but after removing the matching fields from the NetCDF file names, the remaining strings must be identical. For example, if these files are found:
    \itemize{
        \item abcd_20141014.nc
        \item abcd_20141015.nc
        \item abcd_20141016.nc
    }
after removing the date and time fields, all the names reduce to \code{abcd_.nc} and constitute a dataset.

The following files do not constitute a dataset:
    \itemize{
        \item abcd_20141014.nc
        \item abcd_20141015.nc
        \item xyz_20141016.nc
    }

The following list of files will be problematic, with non-numeric month string. The day field will be parsed as a month, which will present difficulties:
    \itemize{
        \item abcd_2014_may_19.nc
        \item abcd_2014_may_20.nc
    }

This file name will be parsed correctly, the year will be parsed as 2014, and the month and day fields  are matched after the year:
    \itemize{
        \item acme_99_20141014.nc
    }

The date in this file name will not be parsed as intended. The name does not contain a match for a 4 digit year, and therefore '99' will be parsed as the year:
    \itemize{
        \item acme_99_981014.nc
    }

Once the date and time patterns are recognized, a \code{\link{utime}} is parsed from each file name. This \code{utime} is assumed to be the starting time of the data in each file.

The following global attributes are then read from a selection of 10 of the matching NetCDF files:
    \itemize{
        \item dataset
        \item dataset_description
        \item wind3d_horiz_coordinates
        \item h2o_flux_corrected
        \item co2_flux_corrected
        \item gsoil_philip_corrected
        \item lambdasoil_fat_corrected
    }
}

\value{
A list of datasets, each dataset is a list with the following named elements:
\itemize{
    \item{enable}{: Logical value, TRUE.}
    \item{desc}{: Value of dataset_description global attribute}
    \item{ncd}{: Directory path of NetCDF files}
    \item{ncf}{: File name pattern of NetCDF files with "\%X" descriptors for parsing date and time.}
    \item{start}{: utime, the start time of the dataset, parsed from first file name.}
    \item{datacoords}{: Coordinate system of 3d winds}
    \item{h2o_flux_corrected}{: whether the water vapor covariances have been corrected for WEBB, et al, and spatial decorrelation.}
    \item{co2_flux_corrected}{: whether the CO2 covariances have been corrected for WEBB, et al, and spatial decorrelation.}
    \item{gsoil_philip_corrected}{: whether the Philip correction has been applied to the soil head flux measurements}
    \item{lambdasoil_fat_corrected}{: whether soil heat diffusivity from a Hukseflux TP01 has been corrected for F(at).}
}
}
\references{
}
\author{
Gordon Maclean
}
\note{
}

\seealso{
\code{\link{dataset}}
}
\examples{
\dontrun{
find_datasets("/data","netcdf")
# List the names of available datasets
names(datasets())
# select a dataset by name
dataset("geo_tiltcor")
# or select the first dataset found
dataset(1)
}
}
\keyword{ ts }
