\name{timeaxis_setup}
\alias{timeaxis_setup}
\title{
Determine tick intervals and labels for a timeaxis
}
\description{
Given a time period to be plotted, return a list containing the times for major and minor ticks, and the \code{\link{utime}} format for the major tick labels.
}
\usage{
timeaxis_setup(t1, t2, time.zone = getOption("time.zone"), cex.axis = par("cex.axis"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t1}{
First time to be plotted, a \code{\linkS4class{utime}}.
}
  \item{t2}{
Last time to be plotted, a \code{\linkS4class{utime}}.
}
  \item{time.zone}{
Time zone for labels.
}
  \item{cex.axis}{
Character magnification for time axis notation, relative to the current setting of cex.
}
}
\details{
Using the width of a 12 character label (\code{strwidth}) on the current graphics device, an appropriate major tick interval is determined so that there will be between 2 and 8 labels across the axis.
}
\value{

Returns a list, containing
\item{toffset}{Offset that is removed from all times before plotting to avoid loss of floating point precision, simply \code{t1}.}
\item{majtics}{Location of labeled major ticks, vector of \code{linkS4class{utime}}s.}
\item{mintics}{Location of unlabeled minor ticks, vector of \code{linkS4class{utime}}s}.
\item{format}{\code{linkS4class{utime}} format to use when generating major tick labels}.
\item{extraformat}{\code{linkS4class{utime}} format to use on the first, leftmost time label, typically including the date.}
\item{new}{\code{linkS4class{utime}} Whether the \code{majtics} vector differs from the previous value - used as a guide of whether to label the axis on a stack of plots.}

This list is also written to \code{.timeaxis_params} on \code{.eoltsEnv}, where it is read by a subsequent call to \code{\link{timeaxis}}.
}
\references{
}
\author{
Gordon Maclean
}
\note{
}

\seealso{
\code{\link{plot_nts}}, \code{\link{timeaxis}}, \code{\linkS4class{utime}}, \code{\linkS4class{nts}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
