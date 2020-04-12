\name{dat}
\alias{dat}
\title{
Return one or more time series by variable name.
}
\description{
Read one or more time series which match a variable name.
}
\usage{
dat(what, derived = TRUE, cache = getOption("dcache"), avg, smooth = FALSE, ...)
}
\arguments{
  \item{what}{
Character vector containing one or more variable names.
}
  \item{derived}{
Should dat look for a function which provides a derivation of a variable?
}
  \item{cache}{
Should the values be stored and fetched from a cache?
}
  \item{avg}{
Should averaging be done on the results of dat. See below for details.
}
  \item{smooth}{
Should smoothing be done on the results of dat. See below for details.
}
  \item{\dots}{
Parameters which are passed to derivation functions.
}
}
\details{

\emph{Derivation}

If \code{derived} is TRUE, a search is done for a function to provide the derivation. For a value of \code{dat("a.b.c",derived=TRUE)}, a search is done for a function with the following names
\itemize{
    \item dat.a.b.c
    \item dat.a.b
    \item dat.a
}

The first function that is found is called to perform the derivation.

After the derivation, the columns in the derived time series are selected using the \code{\link{select-methods}} for \code{\linkS4class{dat}}, and the \code{\link{dpar_isfs}} values for \code{stns}, \code{hts}, \code{sfxs} and \code{sites}.

If smoothing or averaging is in effect, as discussed below, that is performed and the value returned.

\emph{NetCDF Variables}

If a derivation function is not found, the current set of NetCDF files is searched for variables with names, or short_name attributes matching \code{what}. For a value of \code{what="RH"}, a variable with a name or short_name attribute of "RH" is read from the NetCDF files. Also any variables with a short_name beginning with "RH.", such as "RH.3m.rim" are read.

Next the columns in the time series that was read from NetCDF are selected using the \code{\link{select-methods}} for \code{\linkS4class{dat}}, and the \code{\link{dpar_isfs}} values for \code{stns}, \code{hts}, \code{sfxs} and \code{sites}.

Then, averaging or smoothing are performed, if enabled, and the result is returned.

% \code{\S4method{select}{dat}(x)} creates a string
%   "## S4 method for signature 'dat' select(x)"

\emph{Averaging}

Averaging of the time series is controlled by two parameters:
\enumerate{
    \item the "avg" dpar parameter:
        \itemize{
           \item \code{dpar(avg=c(nonsimple.period,simple.period))}
           \item \code{dpar(avg=simple.period)}
        }
    \item the avg=TRUE/FALSE parameter to the \code{dat} function.
}

If dpar("avg") is a numeric vector, and avg=TRUE in the call to \code{\link{dat}},
then averaging is done before the time series is returned by \code{\link{dat}}.

The \code{avg} argument to \code{\link{dat}} defaults to FALSE, unless this is the
first call to dat in the stack, in which case it defaults to TRUE.
The top-most call is detected by checking \code{sys.nframe}, and placing
it in \code{.isfsEnv}.

If a derivation function exists for \code{what}, then \code{avg} is not passed
to the derivation function, which then defaults to FALSE.  The result returned by
the derivation function will then be averaged by \code{dat}, if the \code{avg} argument
to the top level call to \code{dat} is TRUE.

This scheme results in derived variables being computed from non-averaged values,
then averaged after the derivation: i.e. lazy, at the last minute, averaging.
For example, this dat function derives a value for \code{dat("X")} from
\code{dat("Y")^2}:\cr

\code{dat.X <- function(what,...) \{ dat("Y") ^ 2 \}}\cr

and if the user has requested averaging, with \code{dpar("avg")} is non-NULL
this will result in \code{average(Y^2)}, where Y is the value returned by \code{dat("Y")}.

To average values before the derivation, specify \code{avg=TRUE}
in the dat calls for the input variables within the derivation function.
This function:\cr

\code{dat.X <- function(what,...) \{ dat("Y",avg=TRUE,smooth=TRUE) ^ 2 \}}\cr

would result in \code{average(Y)^2}.

Therefore each dat derivation function must consider if it wants to work with averaged
inputs or average the result.

\emph{Averaging Higher Moments}

If \code{dpar("avg")} is a numeric vector of length 1 and its value, in seconds, is greater than the time-spacing of the time series that was derived or read from NetCDF, then simple time averaging is done by \code{dat} of any returned variables.

If \code{dpar("avg")} is a numeric vector of length 2, then the first value is the number of seconds for non-simple time averaging.  For first moments, all averaging is simple. For higher moments, non-simple averaging also includes the variance of the first moments, so that for example a non-simple average of a variable called \code{w'h2o'} would include the addition of the covariance of \code{w} and \code{h2o}.  If the second element of avg is greater than the first, then simple time averaging at \code{dpar("avg")[2]} seconds is done on the results of the first average.

For non-simple averaging of higher moments to be successful, then the first moments must be available for reading or derivation. All the variables that are combined into an average should also have the same weight for each observation. The observation weights are typically read from the corresponding "counts" variable in the NetCDF dataset for each time-series.

See \code{\link{dpar_isfs}} for details about averaging and higher moments.

\emph{Smoothing}

Similar treatment applies to the \code{smooth} argument to dat except that
it always defaults to FALSE, even in the top frame, i.e. the user must
specify it if they want smoothing.  However dat functions can set avg to
true in their calls to dat to get input variables (typically used to smooth
turbulent input variables).

Like averaging, smoothing is accomplished by computing means.  However
specifying smooth=TRUE, and a smooth time period in dpar results in
simple running averages of the selected data variables, with an output
interval equal to the current deltaT of the data.

}
\value{
An object of class \code{"\linkS4class{dat}"} containing a time series of one or more columns.
}
\references{
}
\author{
Gordon Maclean
}
\note{
}
\seealso{
\code{\linkS4class{dat}}
\code{\link{plot_dat}}
\code{\link{nts}}
}
\examples{
\dontrun{
# Look for datasets in /data/netcdf
find_datasets("/data","netcdf")
# Look for datasets in /data, in sub-directories with names starting with netcdf
find_datasets("/data","^netcdf")
# List the names of found datasets
names(datasets())
# select a dataset by name
dataset("geo_tiltcor")
# or select the first dataset found
dataset(1)

# select a time period
dpar(start="2002 jan 12 00:00")
dpar(end="2002 jan 19 00:00")

# Read all RH variables
x = dat("RH")
plot(x)
}

# build a dat from a sample netcdf file
datadir <- file.path(system.file("unitTests", package="eolts"),"data")
ncfile <- "test_\%Y\%m\%d.nc"

t1  <- utime("2012 oct 10 19:00:00",time.zone="US/Mountain")
t2  <- utime("2012 oct 11 20:00:00",time.zone="US/Mountain")

con <- netcdf(file=ncfile,dir=datadir,start=t1,end=t2)
# read u.1m from 21 stations
x <- dat(readts(con,"u.1m", start=t1, end=t2))
close(con)

}
\keyword{ ts }
