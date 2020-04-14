\name{sprofile}
\alias{sprofile}
\title{Plot EOL Sounding Profiles}
\description{
Plot sounding profiles for EOL raw sounding data and/or QCed sounding data. The plot is made on the current graphics device. If a device is not active in an interactive session, the default graphics device will be started.
}
\usage{
sprofile(sdng, xnames=NULL, yname=NULL, title=names(sdng)[1], type = "b",
    xlim = NULL, xlab=NULL, xaxt = par("xaxt"), xaxs = par("xaxs"),
    ylim = NULL, ylab=NULL, yaxt = par("yaxt"), yaxs = par("yaxs"),
    col = c("black", "red", "green", "blue", "purple", "cyan",
        "orange", "yellow", "gray", "pink"), tlwd = par("lwd"),
    pcex=0.25, debug=FALSE, ...)
}
\arguments{
  \item{sdng}{
A sounding or a list of soundings, such as is returned from \code{\link{readDFile}}, or \code{\link{readSoundings}}. If sdng is a list, a profile plot will be made of each sounding in the list.
}
  \item{xnames}{
Names of the variables to plot on the X axis. If xnames is NULL, all variables in the sounding are plotted.
}
  \item{yname}{
Name of the variable to plot on the Y axis. If yname is NULL, it is chosen from  the last column of \code{c("P","Alt_gp","Alt_gps")} that is found in the sounding data.frame.
}
  \item{title}{
Title of the plot. The default value is the name of the sounding, which will be NULL if sdng is not a list.
}
  \item{type}{
Type of trace, see \code{\link{plot.default}}.
}
  \item{xlim}{
Limits of the x axis. If more than one variable is plotted on the X axis, then xlim can be a list, with an entry for each variable name on the X axis.
}
  \item{xlab}{
Label for x axis, see \code{\link{plot.default}}. If NULL, the names of the variables plotted on the x axis will be shown.
}
  \item{xaxt}{
See \code{\link{par}}.
}
  \item{xaxs}{
See \code{\link{par}}.
}
  \item{ylim}{
Limits of the y axis, see \code{\link{plot.default}}.
}
  \item{ylab}{
Label for y axis, see \code{\link{plot.default}}. If ylab is NULL, the name of the variable plotted on the y axis will be shown.
}
  \item{yaxt}{
See \code{\link{par}}.
}
  \item{yaxs}{
See \code{\link{par}}.
}
  \item{col}{
Colors used on the plot.  See \code{\link{par}}.  The first color is used for the plotting box, the remaining colors for the profile traces.
}
  \item{tlwd}{
Trace line width. See \code{plot.default}
}
  \item{pcex}{
Plotting character magnification, relative to default. See \code{par} for info on cex.
}
  \item{\dots}{
Other arguments passed to plot.
}
}
\details{
An xy plot is created of the sounding, with the yname variable plotted on the Y axis, and the rest of the variables with their own scales on the X axis.
}
\value{
None. A plot is generated on the current graphics device. If a graphics device is not active in an interactive session, the default graphics device will activated.
}
\references{
}
\author{
Gordon Maclean
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
}
\examples{
# Read sample D files provided in the eolsonde R package
dir = system.file(package="eolsonde","unitTests","data","D-file")

# start time for search of soundings
t1 = utime("2008 8 15 17:20",time.zone="UTC")
# end time
t2 = t1 + 60 * 20   # add 20 minutes

xs = readSoundings(dir=dir,start=t1,end=t2)

# profile plots of RH, T, wind speed and Vz against pressure for all soundings in xs
par(mfrow=c(2,2))
sprofile(xs,c("RH","T","Wspd","Vz"),"P")

# one profile plot of T, RH and wind, versus pressure.
sprofile(xs[2],c("RH","T","Wspd","Wdir","Vz"),"P")

# fixed limits on one or more variables
xlim = list(RH=c(0,100),T=c(-40,30),Wspd=c(0,30),Vz=c(-20,20))
sprofile(xs[2],c("RH","T","Wspd","Vz"),"P",xlim=xlim)

# Or one can specify clipping limits and let sprofile dynamically
# rescale using the data remaining after the clipping
clip("T",TRUE,c(-40,30))
clip("Wspd",TRUE,c(0,30))
clip("Vz",TRUE,c(-20,20))
sprofile(xs[2],c("RH","T","Wspd","Vz"),"P")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sounding }
