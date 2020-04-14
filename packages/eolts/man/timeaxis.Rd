\name{timeaxis}
\alias{timeaxis}
\title{
Add an Axis to a Time Series Plot
}
\description{
Like \code{\link{axis}}, but generate labels and tick marks appropriate for a time series plot.
}
\usage{
timeaxis(side, labels = TRUE, tick = TRUE, xaxt = par("xaxt"), xlab = labels, time.zone = getOption("time.zone"), date.too = labels, line = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{side}{
an integer specifying which side of the plot the axis is to be drawn on. The axis is placed as follows: 1=below, 2=left, 3=above and 4=right.
}
  \item{labels}{
Logical value, whether time labels should be added to the major tic marks of the plot.
}
  \item{tick}{
Logical value, whether tickmarks and an axis line should be drawn. Tick marks are drawn using the value of \code{par("tcl")}.
}
  \item{xaxt}{
See \code{par("xaxt")}, "n" to suppress plotting of the axis, though the axis title will be shown if xlab is not an empty string. Any other value will result in an axis.
}
  \item{xlab}{
Logical or character value. If TRUE a time axis title will be drawn.  Set to a string for a custom label, or FALSE for no time axis title.
}
  \item{time.zone}{
Time zone to use for the time axis labels.
}
  \item{date.too}{
Should a date be added to the first time label.
}
  \item{line}{
What margin line to draw the axis, typically 0.
}
  \item{\dots}{
Other \code{\link{par}} graphical parameters.
}
}
\details{
\code{timeaxis} 
}
\value{
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Gordon Maclean
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plot_nts}}.
}
\examples{
# How to add a timeaxis to a plot not created with plot_nts,
# in this case a heatmap created with image()
nr <- 200
nc <- 50
freqs <- seq(from=100, to=1000, length=nc)
t1 <- utime("2020 Feb 13 00:00")
t2 <- utime("2020 Feb 13 02:00")
tx <- seq(from=t1, to=t2, length=nr)
# random numbers
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
\keyword{ ts }% use one of  RShowDoc("KEYWORDS")
