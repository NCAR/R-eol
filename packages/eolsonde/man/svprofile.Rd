\name{svprofile}
\alias{svprofile}
\title{Plot EOL Sounding Profiles for one variable with multi soundings}
\description{
Plot profiles of one variable from multiple soundings.
}
\usage{
svprofile(sdngs, xname, yname, type = "b",
    xlim = NULL, xlab, xaxt = par("xaxt"), xaxs = par("xaxs"),
    ylim = NULL, ylab, yaxt = par("yaxt"), yaxs = par("yaxs"),
    col = c("black", "red", "green", "blue", "purple", "cyan",
        "orange", "gold", "gray", "pink", "violet", "brown",
        "magenta", "chocolate", "tomato", "darkgreen"),
    title = "",tlwd=par("lwd"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sdngs}{
    \code{sdngs} A list of soundings, as returned by \code{\link{readSoundings}}
}
  \item{xname}{
Name of variable to plot on X axis.
}
  \item{yname}{
Name of variable to plot on Y axis.
}
  \item{type}{
Type of plot, see \code{\link{plot.default}}.
}
  \item{xlim}{
Limits of the X axis, see \code{\link{plot.default}}. If not specified, the X axis will be dynamically scaled, after applying any clip limits defined for the variable.
}
  \item{xlab}{
Label for the X axis, see \code{\link{plot.default}}. If missing, the names of the variable plotted on the X axis will be shown.
}
  \item{xaxt}{
See \code{\link{par}}.
}
  \item{xaxs}{
See \code{\link{par}}.
}
  \item{ylim}{
Limits of the Y axis, see \code{\link{plot.default}}. If not specified, the Y axis will be dynamically scaled, after applying any clip limits defined for the variable. Specify as c(ymin,ymax) with ymax > ymin, even in the case where the y axis is automatically flipped if plotting pressure.
}
  \item{ylab}{
Label for the Y axis, see \code{\link{plot.default}}. If missing, the names of the variable plotted on the Y axis will be shown.
}
  \item{yaxt}{
See \code{\link{par}}.
}
  \item{yaxs}{
See \code{\link{par}}.
}
  \item{col}{
Colors used on the plot.  See \code{\link{par}}. The first color is used for the plotting box, the remaining colors for the profile traces.
}
  \item{title}{
Type of plot, see \code{\link{plot.default}}.
}
  \item{tlwd}{
Trace line width. See \code{plot.default}
}
  \item{\dots}{
Parameters passed to \code{\link{plot.default}}.
}
}
\details{
The units of the Y variable are checked to see if it is an air pressure ("mb"), in which case the yaxis is flipped.
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
# Read sample D files contained in eolsonde R package
dir = system.file("unitTests","data","D-file",package="eolsonde")
t1 = utime("2008 8 15 17:20",time.zone="UTC")
t2 = t1 + 60 * 20   # add 10 minutes
xs = readSoundings(dir=dir,start=t1,end=t2)
svprofile(xs,"RH","P")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sounding }
