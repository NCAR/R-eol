\name{readSoundings}
\alias{readSoundings}
\title{
Function to read one or more soundings.
}
\description{
This function scans a directory for one or more files matching a name format given in the file parameter. A list, containing one or more sounding time series is returned.
}
\usage{
readSoundings(
    dir = c(paste(Sys.getenv("SONDE_ROOT"), "projects", Sys.getenv("PROJECT"),
            Sys.getenv("PLATFORM"), Sys.getenv("DATASET"), sep = .Platform$file.sep),
        Sys.getenv("SONDE_DATA")),
    file = c("D\%Y\%m\%d_\%H\%M\%S_P\\\\.[0-9]+", "D\%Y\%m\%d_\%H\%M\%S_P\\\\.[0-9]+.gz",
        "D\%Y\%m\%d_\%H\%M\%S\\\\.[0-9]+", "D\%Y\%m\%d_\%H\%M\%S\\\\.[0-9]+.gz"),
    start = dpar("start"), end = dpar("end"), sta_clean = TRUE)
}
\arguments{
  \item{dir}{
    A character vector containing one or more directories to scan. The default value is built from environment variables:
    
    \code{
    dir = c(paste(Sys.getenv("SONDE_ROOT"), "projects", Sys.getenv("PROJECT"), Sys.getenv("PLATFORM"), Sys.getenv("DATASET"), sep = .Platform$file.sep), Sys.getenv("SONDE_DATA"))
    }

    From a unix shell, these values are (for Windows, substitute \\ for /):
    \itemize{
        \item $SONDE_ROOT/projects/$PROJECT/$PLATFORM/$DATASET
        \item $SONDE_DATA
    }
    Or, one can override this default:  dir="C:\\\\AVAPS_DATA\\D-files"
  }
  \item{file}{
    A character vector containing \itemize{
        \item a name of one or more files
        \item a name pattern, compatible with \code{\link{list.files}}.
        \item a name pattern with time descriptors compatible with \code{\link{strptime}}.
    }
    The default value:
    
    \code{
    file = c("D\%Y\%m\%d_\%H\%M\%S_P.[0-9]+", "D\%Y\%m\%d_\%H\%M\%S_P.[0-9]+.gz","D\%Y\%m\%d_\%H\%M\%S.[0-9]+", "D\%Y\%m\%d_\%H\%M\%S.[0-9]+.gz")
    }
    
    will match various types of D files.
    }
  \item{start}{
A \code{\link{utime}} object containing the beginning time of files to be matched. This will only work if one uses a file name pattern containing \code{\link{strptime}} time descriptors.
}
  \item{end}{
A \code{\link{utime}} object containing the end time of files to be matched. This will only work if one uses a file name pattern containing \code{\link{strptime}} time descriptors.
}
  \item{sta_clean}{See \code{\link{readDFile}}.}
}
\details{
}
\value{
A list of sounding time series objects. The names of the elements of the list are the file names containing the corresponding. Each time series element is a \code{\link{dat}} object, containing a data.frame, with a column for each variable in the sounding. The time of each row in the time series can be accessed with the \code{\link{positions}} method.
Be aware that all data types including Sxx, Axx, Pxx are read and returned.
}
\references{
}
\author{
\email{maclean@ucar.edu}
}
\note{
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

options(time.zone="UTC")

# Read sample D files provided in the eolsonde R package
dir = system.file(package="eolsonde","unitTests","data","D-file")

# start time for search of soundings
t1 = utime("2008 8 15 17:20")
# end time
t2 = t1 + 60 * 20   # add 20 minutes

xs = readSoundings(dir=dir,start=t1,end=t2)

# display the names of the soundings
names(xs)

# display the column (variable) names of a sounding
colnames(xs[[1]])

# Plot profiles of rh vs p for all soundings.
plotSvarProfile(xs,"rh","p")

# loop over the soundings and generate multiple plots
par(mfrow=c(2,ceiling(length(xs)/2)))

for (fname in names(xs)) {
    x = xs[[fname]]
    # Use @data to get numeric vector from the time series
    press = x@data[,"p"]
    temp = x@data[,"temp"]
    # generic XY plot
    plot(temp,press)
    title(fname)
}

# use plotSprofile for similar plots
par(mfrow=c(2,ceiling(length(xs)/2)))
for (fname in names(xs)) {
    x = xs[[fname]]
    plotSprofile(x[,c("temp","p")],title=fname)
}

\dontrun{
# set the default directory for readSoundings
Sys.setenv(SONDE_DATA="/net/isf/dropsonde3/kbeierle/MPEX-2013/Dfiles")
# file name pattern
file = "D%Y%m%d_%H%M%S_P.[0-9].PresCorr"
t1 = utime("2013 05 16 09:00")
t2 = t1 + 7200
xs = readSoundings(file=file,start=t1,end=t2)
plotScontour(xs,"p","temp")
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ timeseries }
