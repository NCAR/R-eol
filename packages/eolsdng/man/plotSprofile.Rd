\name{plotSprofile}
\alias{plotSprofile}
\title{Plot EOL Sounding Profiles}
\description{
Plot sounding profiles for EOL raw sounding data and/or QCed sounding data. The plot is made on the current graphics device. If a device is not active in an interactive session, the default graphics device will be started.
}
\usage{
plotSprofile(raw = NULL, qc = NULL, title = NULL, type = "b",
    xlim = NULL, xlab, xaxt = par("xaxt"), xaxs = par("xaxs"),
    ylim = NULL, ylab, yaxt = par("yaxt"), yaxs = par("yaxs"),
    col = c("black", "red", "green", "blue", "purple", "cyan",
        "orange", "yellow", "gray", "pink"), tlwd = par("lwd"),
    ...)
}
\arguments{
  \item{raw}{
A sounding, such as is returned from \code{\link{readDFile}}, or one of the elements returned from \code{\link{readSoundings}}.
}
  \item{qc}{
A sounding. If \code{raw} is also specified, both the raw and qc sounding are plotted.
}
  \item{title}{
Title of the plot.
}
  \item{type}{
Type of trace, see \code{\link{plot.default}}.
}
  \item{xlim}{
Limits of the x axis, see \code{\link{plot.default}}.
}
  \item{xlab}{
Label for x axis, see \code{\link{plot.default}}. If missing, the names of the variables plotted on the x axis will be shown.
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
Label for y axis, see \code{\link{plot.default}}. If missing, the names of the variable plotted on the y axis will be shown.
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
  \item{\dots}{
Other arguments passed to plot.
}
}
\details{
An xy plot is created of the sounding, of all numeric columns. The variable plotted on the Y axis is chosen from  the last column of \code{c("p","pressure","gp.alt","gps.alt")} that is found in the sounding data.frame. All the other variables will be plotted on the X axis.
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
    \dontrun{
        xs = readSoundings()
        plotSprofile(xs[[2]][,c("rh","temp","wspd","wdir","p")])
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sounding }
