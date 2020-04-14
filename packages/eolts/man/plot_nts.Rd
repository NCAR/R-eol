\name{plot_nts}
\alias{plot_nts}
\title{
Make a time series plot of an \code{\link{nts}} object.
}
\description{
\code{plot_nts(x,...)} creates a time series plot of \code{x}.

Calling \code{plot(x,...)}, with x an object of type \code{\linkS4class{nts}} will invoke \code{plot_nts(x,...)}.
}
\usage{
plot_nts(x, type = "l", axes = TRUE, xlab=TRUE, xlim, xaxs = "i", xaxt="s", ylab, ylim, yaxs=par("yaxs"), yaxt=par("yaxt"), log = "", col, pch, lty, time.zone = x@time.zone, remargin = "xy", debug=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A time series object of class \code{\linkS4class{nts}}.
}
  \item{type}{
See parameter \code{type} for \code{\link{plot.default}}. "l" for a line plot, "p" for points, "b" for both.
}
  \item{axes}{
Whether both axes should be drawn. See \code{plot.default}.
}
  \item{xlab}{
X axis title, logical or character. If TRUE, an appropriate time axis label is shown on the bottom axis. To change the time label, set to a character string, or set to FALSE or "" for no label.
}
  \item{xlim}{
Time limits, in \code{utime} to plot. If not specified, the entire nts time series is plotted.
}
  \item{xaxs}{
See \code{\link{par}}.
}
  \item{xaxt}{
See \code{\link{par}}.
}
  \item{ylab}{
Title for the left-hand Y axis. The default is a string of the form \code{paste0(colnames(x)," (",units(x),")")} for a 1 column time series, or the name of the y argument if more than one column. Set to "" for no y label.
}
  \item{ylim}{
Y limits.
}
  \item{yaxs}{
See \code{\link{par}}.
}
  \item{yaxt}{
See \code{\link{par}}.
}
  \item{log}{
See \code{plot.default}.
}
  \item{col}{
Plotting color, see \code{\link{par}}.
}
  \item{pch}{
Plotting character, see \code{\link{par}}.
}
  \item{lty}{
Line type, see \code{\link{par}}.
}
  \item{time.zone}{
Time zone to use for labeling X axis. If not specified, the time.zone is provided in the nts object.
}
  \item{remargin}{
A character string, "xy" meaning adjust margins on both x and y axis, or "x" or "y" for one axis, or "" for neither axis. If logical, TRUE means "xy" and FALSE means "".
}
  \item{debug}{
Enable some cat statements to print variable values.
}
  \item{\dots}{
Other par plotting parameters, see \code{\link{par}}.
}
}
\details{
\code{\link{timeaxis_setup}} and \code{\link{timeaxis}} is used to create and label the bottom X (time) axis. \code{plot_nts} is like \code{\link{plot.default}}, in that tick marks and labels for the top and right axes are not generated. Use \code{timeaxis(side=3,...)} to add a top time axis, and \code{axis(side=4,...)} for a right Y axis. If there are multiple columns in x, then all columns are plotted together.

If there are multiple figures on a page, e.g. \code{par(mfrow=c(4,1))}, only the bottom X axis of the bottom plot will be labeled. The bottom X axes of the plots above it will have ticks, but no labels.

Depending on the value of \code{remargin}, the margins of the plot are adjusted to reduce wasted space, by reducing the margin lines for axis labels and title with \code{par(mgp=c(1.5, 0.5, 0))} and corresponding reductions in the values for the margins on each side of the plot, \code{par("mar")}.
}
\value{
A list of time axis labels and ticks.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
\code{\link{plot.default}}: \code{? plot.default}

\code{\link{timeaxis_setup}}: \code{? timeaxis_setup}

\code{\link{timeaxis}}: \code{? timeaxis}

\code{\linkS4class{nts}}: \code{class? nts}

\code{\link{nts}}: \code{? nts}

\code{\link{isfs::plot_dat}}: \code{? plot_dat}
}
\examples{
# make an nts out of the Keeling Mauna Loa CO2 from the R datasets package, and plot it
tco2 <- tsp(co2)
# Convert tsp time values to utimes, approximate
t1 <- utime(paste(as.integer(tco2[1]), "01 01 00:00:00"),
    in.format="\%Y \%d \%m \%H:\%M:\%S") + (tco2[1] \%\% 1) * 365.25 * 86400
t2 <- utime(paste(as.integer(tco2[2]), "01 01 00:00:00"),
    in.format="\%Y \%d \%m \%H:\%M:\%S") + (tco2[2] \%\% 1) * 365.25 * 86400
tx <- seq(from=t1, to=t2, length=length(co2@.Data))
co2x <- nts(co2@.Data, tx, names="CO2", units="ppm", time.zone="UTC")
plot(co2x)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts plot }
