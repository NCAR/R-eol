\name{nts-class}
\Rdversion{1.1}
\docType{class}
\alias{nts-class}
\alias{autocorr}
\alias{average}
\alias{Cbind}
\alias{crosscorr}
\alias{cumsum}
\alias{detrend}
\alias{end}
\alias{fftw}
\alias{lag}
\alias{mean}
\alias{Ops}
\alias{Rbind}
\alias{replace.nas}
\alias{seriesConcat}
\alias{seriesMerge}
\alias{start}
\alias{start<-}
\alias{stations}
\alias{stations<-}
\alias{trend}
\alias{trend}
\alias{tspar}
\alias{units}
\alias{units<-}
\alias{var}
\alias{window}
\alias{write}
\alias{writets}

\title{Class \code{"nts"}}
\description{
Yet another time series class in R. 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("nts", ...)}, though the usual way is with the \code{\link{nts}} function.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{A \code{matrix} or \code{data.frame}, containing a row for each observation in time, and a column for each variable that was sampled.}
    \item{\code{positions}:}{Object of class \code{"positions"}, containing the times of the observations, whose length should be equal to \code{nrow(data)}. The \code{\link{nts}} function converts the positions to \code{\link{utime}}s.}
    \item{\code{units}:}{Object of class \code{"character"}, containing the units of each column variable.}
    \item{\code{weights}:}{Object of class \code{"matrix"}, containing the numeric weights to be assigned each observation. The number of rows in \code{weights} should be the same as \code{nrow(data)} and \code{length(positions)}. \code{weights} is often the number of points in each time average, as read from a NetCDF file, and is used the the \code{\link{average}} method. }
    \item{\code{weightmap}:}{Object of class \code{"integer"}, with a length equal to \code{ncol(data)}.  For each column in \code{data}, the index of its column in \code{weights}. More than one column in \code{data} can use the same column in \code{weights}.} 
    \item{\code{stations}:}{Object of class \code{"integer"}, a vector of observation station numbers to be associated with each column in \code{data}, or NULL.}
    \item{\code{deltat}:}{Object of class \code{"numeric"}, computed for each \code{\link{nts}} with the \code{\link{deltat}} function.}
    \item{\code{wss}:}{Object of class \code{"numeric"}. Window squared and summed.  A numeric value containing the sum of the squared window coefficients * N.  Windowing data is commonly done in spectral analysis. For a square window of all 1's, wss = N^2.}
    \item{\code{time.format}:}{Object of class \code{"character"}, containing the \code{\link{utime}} time format to be used when formatting times in methods such as \code{show} and \code{write}.}
    \item{\code{time.zone}:}{Object of class \code{"character"}, containing the \code{\link{utime}} time zone to be used when formatting times in methods such as \code{show} and \code{write}.}
    \item{\code{fiscal.year.start}:}{Not used. A remnant of splusTimeSeries::timeSeries.}
    \item{\code{type}:}{Not used. A remnant of splusTimeSeries::timeSeries.}
    \item{\code{start.position}:}{Object of class \code{"positions"}.}
    \item{\code{end.position}:}{Object of class \code{"positions"}.}
    \item{\code{future.positions}:}{Not used. A remnant of splusTimeSeries::timeSeries. }
    \item{\code{title}:}{Object of class \code{"character"}. A hack for putting titles on plots.}
    \item{\code{documentation}:}{Object of class \code{"character"}, generally not used.}
    \item{\code{attributes}:}{Object of class \code{"ANY"} Any associated attributes.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{timeSeries}"}, directly.
Class \code{"\linkS4class{series}"}, by class "timeSeries", distance 2.
Class \code{"\linkS4class{seriesVirtual}"}, by class "timeSeries", distance 2.
}
\section{Methods}{
  \describe{
    \item{!}{\code{signature(x = "nts")}: ... }
    \item{[}{\code{signature(x = "nts")}: ... }
    \item{[<-}{\code{signature(x = "nts", i = "ANY", j = "ANY", value = "nts")}: ... }
    \item{[<-}{\code{signature(x = "nts", i = "ANY", j = "ANY", value = "vector")}: ... }
    \item{\%*\%}{\code{signature(x = "matrix", y = "nts")}: ... }
    \item{\%*\%}{\code{signature(x = "nts", y = "matrix")}: ... }
    \item{align}{\code{signature(x = "nts")}: ... }
    \item{approx}{\code{signature(x = "nts")}: ... }
    \item{as.logical}{\code{signature(x = "nts")}: ... }
    \item{as.numeric}{\code{signature(x = "nts")}: ... }
    \item{atan2}{\code{signature(y = "ANY", x = "nts")}: ... }
    \item{atan2}{\code{signature(y = "nts", x = "ANY")}: ... }
    \item{atan2}{\code{signature(y = "nts", x = "nts")}: ... }
    \item{autocorr}{\code{signature(x = "nts", lags = "missing")}: ... }
    \item{autocorr}{\code{signature(x = "nts", lags = "numeric")}: ... }
    \item{average}{\code{signature(x = "nts")}: ... }
    \item{Cbind}{\code{signature(x1 = "ANY", x2 = "nts")}: ... }
    \item{Cbind}{\code{signature(x1 = "nts", x2 = "ANY")}: ... }
    \item{Cbind}{\code{signature(x1 = "nts", x2 = "nts")}: ... }
    \item{coerce}{\code{signature(from = "nts", to = "logical")}: ... }
    \item{coerce}{\code{signature(from = "nts", to = "matrix")}: ... }
    \item{coerce}{\code{signature(from = "nts", to = "numeric")}: ... }
    \item{Complex}{\code{signature(z = "nts")}: ... }
    \item{crosscorr}{\code{signature(x = "nts", lags = "missing")}: ... }
    \item{crosscorr}{\code{signature(x = "nts", lags = "numeric")}: ... }
    \item{cumsum}{\code{signature(x = "nts")}: ... }
    \item{deltat}{\code{signature(x = "nts")}: ... }
    \item{deltat<-}{\code{signature(x = "nts", value = "numeric")}: ... }
    \item{detrend}{\code{signature(x = "nts", trnd = "matrix")}: ... }
    \item{detrend}{\code{signature(x = "nts", trnd = "missing")}: ... }
    \item{dim}{\code{signature(x = "nts")}: ... }
    \item{dim<-}{\code{signature(x = "nts", value = "numeric")}: ... }
    \item{dimnames}{\code{signature(x = "nts")}: ... }
    \item{dimnames<-}{\code{signature(x = "nts", value = "list")}: ... }
    \item{end}{\code{signature(x = "nts")}: ... }
    \item{end<-}{\code{signature(x = "nts", value = "utime")}: ... }
    \item{fftw}{\code{signature(x = "nts", inverse = "missing", use.mvfft = "logical")}: ... }
    \item{fftw}{\code{signature(x = "nts", inverse = "missing", use.mvfft = "missing")}: ... }
    \item{is.finite}{\code{signature(x = "nts")}: ... }
    \item{is.infinite}{\code{signature(x = "nts")}: ... }
    \item{is.na}{\code{signature(x = "nts")}: ... }
    \item{lag}{\code{signature(x = "nts")}: ... }
    \item{lines}{\code{signature(x = "nts")}: ... }
    \item{Math}{\code{signature(x = "nts")}: ... }
    \item{mean}{\code{signature(x = "nts")}: ... }
    \item{Ops}{\code{signature(e1 = "ANY", e2 = "nts")}: ... }
    \item{Ops}{\code{signature(e1 = "nts", e2 = "ANY")}: ... }
    \item{Ops}{\code{signature(e1 = "nts", e2 = "missing")}: ... }
    \item{Ops}{\code{signature(e1 = "nts", e2 = "nts")}: ... }
    \item{Ops}{\code{signature(e1 = "nts", e2 = "timeSeries")}: ... }
    \item{Ops}{\code{signature(e1 = "timeSeries", e2 = "nts")}: ... }
    \item{plot}{\code{signature(x = "nts", y = "missing")}: Calls \code{\link{plot_nts}(x,...)}}
    \item{plot}{\code{signature(x = "nts", y = "nts")}: ... }
    \item{plot}{\code{signature(x = "nts", y = "numeric")}: ... }
    \item{plot_nts}{\code{signature(x = "nts", ...)}: See \link{plot_nts} }
    \item{points}{\code{signature(x = "nts")}: ... }
    \item{positions}{\code{signature(object = "nts")}: ... }
    \item{positions<-}{\code{signature(object = "nts", value = "ANY")}: ... }
    \item{positions<-}{\code{signature(object = "nts", value = "utime")}: ... }
    \item{Rbind}{\code{signature(x1 = "nts", x2 = "nts")}: ... }
    \item{replace.nas}{\code{signature(x = "nts", warn = "logical")}: ... }
    \item{replace.nas}{\code{signature(x = "nts", warn = "missing")}: ... }
    \item{seriesConcat}{\code{signature(x1 = "nts", x2 = "nts")}: ... }
    \item{seriesMerge}{\code{signature(x1 = "nts", x2 = "nts")}: ... }
    \item{seriesMerge}{\code{signature(x1 = "nts", x2 = "numeric")}: ... }
    \item{seriesMerge}{\code{signature(x1 = "nts", x2 = "timeSeries")}: ... }
    \item{seriesMerge}{\code{signature(x1 = "timeSeries", x2 = "nts")}: ... }
    \item{show}{\code{signature(object = "nts")}: ... }
    \item{start}{\code{signature(x = "nts")}: ... }
    \item{start<-}{\code{signature(x = "nts", value = "utime")}: ... }
    \item{stations}{\code{signature(x = "nts")}: ... }
    \item{stations<-}{\code{signature(x = "nts", value = "ANY")}: ... }
    \item{stations<-}{\code{signature(x = "nts", value = "integer")}: ... }
    \item{summary}{\code{signature(object = "nts")}: ... }
    \item{Summary}{\code{signature(x = "nts")}: ... }
    \item{t}{\code{signature(x = "nts")}: ... }
    \item{trend}{\code{signature(x = "nts", use.lsfit = "logical")}: ... }
    \item{trend}{\code{signature(x = "nts", use.lsfit = "missing")}: ... }
    \item{tspar}{\code{signature(x = "nts")}: ... }
    \item{units}{\code{signature(x = "nts")}: ... }
    \item{units<-}{\code{signature(x = "nts", value = "character")}: ... }
    \item{var}{\code{signature(x = "nts", y = "ANY")}: ... }
    \item{window}{\code{signature(x = "nts")}: ... }
    \item{write}{\code{signature(x = "nts")}: ... }
    \item{writets}{\code{signature(con = "netcdf", x = "nts")}: ... }
}
}
\references{
}
\author{
Gordon Maclean
}
\note{
}

\seealso{
\tabular{ll}{
?command \tab link\cr
\code{? nts} \tab \code{\link{nts}}\cr

\code{? plot_nts} \tab \code{\link{plot_nts}}\cr

\code{class? utime} \tab \code{\linkS4class{utime}}\cr

\code{? utime} \tab \code{\link{utime}}\cr
}
}
\examples{
# create a 20x4 time series of random data, at 10 second time resolution
nr = 20
nc = 4
dt = 10
options(time.zone="US/Central")
tx = seq(from=utime("2013 oct 13 14:00"), by=dt, length=nr)
dx = matrix(runif(nr*nc), ncol=nc)
x = nts(dx, tx, units=rep("m",nc), names=paste0("X",1:nc))

# Time series line plot of selected columns
plot(x[,c("X1","X3")])

# Medians of each column. For actual data, na.rm argument is commonly used.
apply(x, 2, median, na.rm=TRUE)

# Maximum of each row.
apply(x, 1, max, na.rm=TRUE)

# select rows from x by a range of time
it = c(utime("2013 oct 13 14:00:30"),utime("2013 oct 13 14:00:59"))
y = x[it,]

# Use tlocator function to select a start and end time from a plot, and then replot.
it = tlocator(2)
plot(x[it,])

# change some values in x. This is an example of selecting
# elements of an nts with an nts.
x[x < 0.5] <- -x[x < 0.5]
x[x > 0.9] <- 99

# A similiar expression, but will result in an error.
# You can't do x[i,] <- y if i is a multi-column nts.
\dontrun{
x[x > 0.9, ] <- 99
}
# Instead do, as above:
x[x > 0.9] <- 99
# Or to change multiple columns of all matching rows, use
# the form:  x[i,j] <- val, where i is a one column
# nts, and j is either empty or selects the columns to
# replace in x:
x[x[,1] > 0.9,] <- 99
x[x[,1] > 0.9, c(2,3)] <- 99

# An nts on the right-hand-side of an assignment which is
# indexed with only one argument is no longer an nts.
# In this case the result in xx will be a numeric vector with all
# time information lost.
xx <- x[x < 0.5]

# create time series with slightly different times than x,
# and different number of observations, but same number
# of columns
nry <- nr - 5
ncy <- nc
ty <- tx[4:(nry+3)] + runif(nry)
dy <- matrix(runif(nry*ncy), ncol=ncy)
y <- nts(dy, ty, units=rep("m",ncy), names=paste0("Y",1:ncy))

# Select values in x with times that are within options("dt.eps") of
# the times in y, and for which the expression "y < 0.5" is TRUE.
# If options("dt.eps") is NULL, then a dt.eps of deltat(x) / 10 is used.
# Result is a numeric vector, not an nts.
xx <- x[y < 0.5]

# Select rows in x with times that are within options("dt.eps") of
# the times in y, and for which the expression "y[,1] < 0.5" is TRUE.
# If options("dt.eps") is NULL, by default, then a dt.eps of
# deltat(x) / 10 is used.  The result is an nts.
xx <- x[y[,1] < 0.5,]

# Replace values in x with times that are within options("dt.eps") of the
# times in y, and for which the expression "y < 0.5" is TRUE.
x[y < 0.5] <- y[y < 0.5] * 2

# Replace rows in x whose times match an nts logical expression
# with time series rows from the RHS of the assignment.
x[y[,1] < 0.5,] <- y[y[,1] < 0.5,] * 2
}
\keyword{classes}
