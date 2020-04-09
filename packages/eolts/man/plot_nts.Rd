\name{plot_nts}
\alias{plot_nts}
\title{
Make a time series plot of an nts object.
}
\description{
Create a time series plot of an nts object. Appropriate time labels on the X axis are provided for a wide range of times, from a plot spanning on the order of seconds to a plot of several years.
}
\usage{
plot_nts(x, type = "l", xlab=TRUE, xlim, ylab, ylim, xaxs = "i", xaxt, yaxt, col, pch, lty, time.zone = x@time.zone, axes = TRUE, log = "", cex = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A nts time series object.
}
  \item{type}{
Usually plot parameter, "l" for a line plot, "p" for points, "b" for both.
}
  \item{xlab}{
X axis label, if you want to override the default time lable. Set to FALSE for no label.
}
  \item{xlim}{
Time limits to plot. If not specified, the entire nts time series is plotted.
}
  \item{ylab}{
Y axis label.
}
  \item{ylim}{
Y limits.
}
  \item{xaxs}{
See \code{par}.
}
  \item{xaxt}{
See \code{par}.
}
  \item{yaxt}{
See \code{par}.
}
  \item{col}{
Plotting color, see \code{par}.
}
  \item{pch}{
Plotting character, see \code{par}.
}
  \item{lty}{
Line type, see \code{par}.
}
  \item{time.zone}{
Time zone to use for labeling X axis. If not specified, the time.zone is provided in the nts object.
}
  \item{axes}{
Whether both axes should be drawn. See \code{plot.default}.
}
  \item{log}{
See \code{plot.default}.
}
  \item{remargin}{
Whether to adjust the X and Y plot margins, removing wasted space..
}
  \item{remarginY}{
Whether to adjust the Y plot margins, removing wasted space. If TRUE and remargin is TRUE, only the X plot margins are adjusted.
}
  \item{\dots}{
Other par plotting parameters, see \code{par}.
}
}
\details{
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
\code{\link{plot.default}}.
}
\examples{
# make an nts out of the Keeling Mauna Loa CO2 dataset, and plot it
tco2 <- tsp(co2)
# Convert tsp time values to utimes, approximate
t1 <- utime(paste(as.integer(tco2[1]), "01 01 00:00:00"),
    in.format="\%Y \%d \%m \%H:\%M:\%S") + (tco2[1] \%\% 1) * 365.25 * 86400
t2 <- utime(paste(as.integer(tco2[2]), "01 01 00:00:00"),
    in.format="\%Y \%d \%m \%H:\%M:\%S") + (tco2[2] \%\% 1) * 365.25 * 86400
tx <- seq(from=t1, to=t2, length=length(co2@.Data))
co2x <- nts(co2@.Data, tx, names="CO2", units="ppm", time.zone="UTC")
plot(co2x)

# How to add a timeaxis to a plot not created with plot_nts,
# in this case a heatmap created with image()
nr <- 200
nc <- 50
freqs <- seq(from=100, to=1000, length=nc)
t1 <- utime("2020 Feb 13 00:00")
t2 <- utime("2020 Feb 13 02:00")
tx <- seq(from=t1, to=t2, length=nr)
z <- nts(matrix(rnorm(nr*nc),nrow=nr),tx, names=paste0("F",1:nc))

ncolors <- 20
colors <- hcl.colors(ncolors, "Heat", rev=TRUE)

# setup time axis scaling, ticks and labels
tparams <- timeaxis_setup(t1, t2)

# Remove tparams$toffset from times
image(z=z@data, x=tx - tparams$toffset, y=freqs, col=colors, ylab="MHz",
    main="Heatmap of Random Data", xaxt="n", xlab="")
timeaxis(1)
timeaxis(3, labels=FALSE)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts plot }
