\name{project_plots}
\alias{project_plots}
\title{
Generate a canned set of plots of soundings.
}
\description{
Read available soundings from a specified directory and generate a set of plots.
}
\usage{
project_plots(dataDir = Sys.getenv("SONDE_DATA"), plotDir = file.path(dataDir, "postflight"), dlim = list(RH = c(-5, 105), T = c(-60, 30), Vz = c(-50, 0), Wspd = c(0, 50)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataDir}{
Directory containing the sounding files, passed as the dir argument to \link{readSoundings}.
}
  \item{plotDir}{
Directory path to save the plot PNG files. If NULL, the plots are created on the default graphics device, and the user is prompted to hit return to see the next plot.
}
  \item{dlim}{
A list by data variable name of the limits to apply to data before plotting.
}
}
\details{
\code{readSoundings(dir=dataDir,file = "D\%Y\%m\%d_\%H\%M\%S_P\\.[0-9]+")} is used to read the sounding files, which in this case are expected to be D files.

Then \link{scontour} is called to generate level plots of "T", "RH", "Wspd" and "Vz", of all available soundings,  with a fixed color scale for each variable, as specified in dlim.
}
\value{
NULL
}
\author{
    Gordon Maclean
}
\seealso{
\code{\link{scontour}}
\code{\link{readSoundings}}
}
\examples{
    dir = system.file(package="eolsonde","unitTests","data","D-file")
    Sys.setenv(SONDE_DATA=dir)
    dpar(start="2008 8 15 17:20",end="2008 8 15 18:00")
    project_plots(plotDir=NULL)
\dontrun{
    # To save PNG files to a directory
    project_plots(plotDir="/myplotdir")
}
}
\keyword{ sounding }
