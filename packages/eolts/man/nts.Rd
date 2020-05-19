\name{nts}
\alias{nts}
\title{
Construct an object of class \code{"\linkS4class{nts}"}.
}
\description{
Create an object of \code{"\linkS4class{nts}"} from a matrix or data.frame and an associated vector of \code{"\linkS4class{utime}"}s, whose length must be the same as the number of rows in the matrix or data frame.  Each column of the matrix or data.frame is a different time series variable. Each row is a different observation in time.
}
\usage{
nts(data, positions, units, names, long_names=NULL, weights = NULL, weightmap = NULL, stations = NULL, wss = NULL, time.format = getOption("time.out.format"), time.zone = getOption("time.zone"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Matrix or data.frame. Each column of the matrix or data.frame is a different time series variable. Each row is a different observation in time.
}
  \item{positions}{
Vector of \code{"\linkS4class{utime}"}s, representing the time of the observations, whose length must be the same as the number of rows in the matrix or data frame.
%%     ~~Describe \code{positions} here~~
}
  \item{units}{
Character vector of length \code{ncol(data)}, containing the units of the column variables.
}
  \item{names}{
Character vector of length \code{ncol(data)}, containing the names of the columns, typically a name for each of the sampled variables. If specified, \code{colnames(data)} will be set to \code{names}. If missing, \code{colnames(data)} will be unchanged.
}
  \item{weights}{
A matrix, containing the numeric weights to be assigned each observation. The number of rows in \code{weights} should be the same as \code{nrow(data)} and \code{length(positions)}. \code{weights} is often the number of points in each time average, as read from a NetCDF file, and is used the the \code{\link{average}} method.
}
  \item{weightmap}{
For each column in \code{data}, the index of its column in \code{weights}. More than one column in \code{data} can use the same column in \code{weights}.
}
  \item{stations}{
If not NULL, a vector of observation station numbers to be associated with each column in \code{data}. 
}
  \item{wss}{
Window squared and summed.  A numeric value containing the sum of the squared window coefficients * N.  Windowing data is commonly done in spectral analysis. For a square window of all 1's, wss = N^2.
}
  \item{time.format}{
Character string containing the \code{\link{utime}} time format to be used when formatting times in methods such as \code{show} and \code{write}.
}
  \item{time.zone}{
Character string containing the \code{\link{utime}} time zone to be used when formatting times in methods such as \code{show} and \code{write}.
}
  \item{long_names}{
Character string containing more descriptive variable names.
}
}
\details{
}
\value{
An object of type \code{"\linkS4class{nts}"}.
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
\code{\link{utime}}
\code{\link{plot_nts}}
}
\examples{
# create a 20x4 time series of random data, at 10 second time resolution
options(time.zone="US/Central")
nr = 20
nc = 4
dt = 10
tx = seq(from=utime("2013 oct 13 14:00"), by=dt, length=nr)
dx = matrix(runif(nr*nc), ncol=nc)
x = nts(dx, tx, units=rep("m",nc), names=c("V1","V2","V3","V4"))

# Time series line plot
plot(x[,c("V1","V3")])

# Medians of each column. For actual data, na.rm argument is commonly used.
apply(x, 2, median, na.rm=TRUE)

# Max of each row.
apply(x, 1, max, na.rm=TRUE)

# select rows from x by a range of time
it = c(utime("2013 oct 13 14:00:30"),utime("2013 oct 13 14:00:59"))
y = x[it,]

# Use tlocator function to select a start and end time from a plot, and then replot.
it = tlocator(2)
plot(x[it,])

# change some values in x
x[x < 0.5] <- -x[x < 0.5]
x[x > 0.9] <- 99

# A similiar expression, but will result in an error.
# You can't do x[i,] <- y if i is a multi-column nts,
# must do x[i] <- y, as above.
\dontrun{
x[x < 0.9, ] <- 99
}

# An nts on the right-hand-side of an expression that is
# indexed with only one argument is no longer an nts.
# In this case y will be a numeric vector with all
# time information lost.
y <- x[x < 0.5]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
