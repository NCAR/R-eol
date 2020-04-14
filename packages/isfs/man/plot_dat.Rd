\name{plot_dat}
\alias{plot_dat}
\alias{plot,dat}
\title{
Make a time series plot of a \code{\link{dat}} object.
}
\description{
\code{plot_dat} creates a time series plot of \code{x}, like \code{\link{plot_nts}}, with some additional features to simplify time series plotting.

Calling \code{plot(x,...)}, with x an object of type \code{\linkS4class{dat}} will invoke \code{plot_dat(x,...)}. 
}
\usage{
plot_dat(x, type = "l", xlab, xlim, xaxs = "i", xaxt = "s",
	ylab, ylim = NULL, yaxs = par("yaxs"), yaxt = par("yaxt"),
	log = "", cols, one_scale = FALSE,
	tlwd = par("lwd"), remargin = TRUE, title, logo = TRUE,
	debug = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A time series object of class \code{\linkS4class{dat}}.
}
  \item{type}{
See parameter \code{type} for \code{\link{plot.default}}. "l" for a line plot, "p" for points, "b" for both.
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
Title for the left-hand Y axis. The default is a string of the form \code{paste0(colnames(x)," (",units(x),")")} for a 1 column time series, or simply the units if more than one trace have the same units.
}
  \item{ylim}{
Y limits.  May be a \code{list} by variable name, with Y limits for each variable.
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
  \item{cols}{
Trace colors.
}
  \item{one_scale}{
Plot all columns on the same scale.
}
  \item{tlwd}{
Trace line width, see \code{par("lwd")}.
}
  \item{remargin}{
A character string, "xy" meaning adjust margins on both x and y axis, or "x" or "y" for one axis, or "" for neither axis. If logical, TRUE means "xy" and FALSE means "".
}
  \item{title}{
To specify a title for the top plot on the page, use the \code{title="string"} argument, or to suppress a title, use \code{title=""} or \code{title=FALSE}. Otherwise, a title is placed at the top of the page. If the \code{x} objects that were plotted have a title slot, \code{x@title}, it is used in the title, otherwise the column names are concatenated for the title.
}
  \item{logo}{
Logo to be placed at the bottom of the page.
}
  \item{debug}{
Enable some cat statements to print variable values.
}
  \item{\dots}{
Other par plotting parameters, see \code{\link{par}}.
}
}
\details{
If \code{one.scale=FALSE}, and \code{x} contains more than one column, then multiple traces are plotted, with different colors, and a color legend is added to the top of the plot. Multiple Y axes are generated, alternating left and right, if the columns have different units, and the units will be shown in the Y axis title.  A right-hand Y axes is always shown, with ticks, but will not be labeled if all traces have the same units.

If multiple figures are plotted on a page, e.g. \code{par(mfrow=c(4,1))}, the top figure
If there are multiple figures on a page, e.g. \code{par(mfrow=c(4,1))}, only the bottom X axis of the bottom plot will be labeled. The bottom X axes of the plots above it will have ticks, but no labels. The top X axis of the top plot will be labeled using time.zone="GMT".

By default, a title is placed at the top of the page. See the \code{title} parameter.
}
\value{
%%  ~Describe the value returned
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
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plot.default}}.

\code{\linkS4class{dat}}: \code{class? dat}

\code{\link{dat}}: \code{? dat}

\code{\link{plot_nts}}: \code{? plot_nts}
}
\examples{
# make a dat out of the Keeling Mauna Loa CO2 from the R datasets package, and plot it
tco2 <- tsp(co2)
# Convert tsp time values to utimes, approximate
t1 <- utime(paste(as.integer(tco2[1]), "01 01 00:00:00"),
    in.format="\%Y \%d \%m \%H:\%M:\%S") + (tco2[1] \%\% 1) * 365.25 * 86400
t2 <- utime(paste(as.integer(tco2[2]), "01 01 00:00:00"),
    in.format="\%Y \%d \%m \%H:\%M:\%S") + (tco2[2] \%\% 1) * 365.25 * 86400
tx <- seq(from=t1, to=t2, length=length(co2@.Data))
co2x <- dat(nts(co2@.Data, tx, names="CO2", units="ppm", time.zone="UTC"))
plot(co2x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
