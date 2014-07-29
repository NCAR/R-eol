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

If \code{derived} is TRUE, a search is done for a function to provide the derivation. For a value of \code{dat(what="a.b.c",derived=TRUE)}, a search is done for a function with the following names
\itemize{
    \item dat.a.b.c
    \item dat.a.b
    \item dat.a
}

The first function that is found is called to perform the derivation.

Next, the current set of NetCDF files is searched for variables with names, or short_name attributes matching \code{what}. For a value of \code{what="RH"}, a variable with a name or short_name attribute of "RH" is read from the NetCDF files. Also any variables with a short_name beginning with "RH.", such as "RH.3m.rim" are read.

Any result of the derivation and the NetCDF reads are combined with Cbind into a multi-column time series.

\strong{Averaging}

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

The avg argument to \code{\link{dat}} defaults to FALSE, unless this is the
first call to dat in the stack, in which case it defaults to TRUE.
The top-most call is detected by checking \code{sys.nframe}, and placing
it in \code{.isfsEnv}.

If a derivation function exists for \code{what}, then \code{avg} is not passed
to the derivation function, which then defaults to FALSE.  The result returned by
the derivation function will then be averaged by \code{dat}, if avg is TRUE.

This scheme results in derived variables being computed from non-averaged values,
then averaged after the derivation: i.e. lazy, at the last minute, averaging.
For example, this dat function derives a value for \code{dat("X")} from
\code{dat("Y")^2}:\cr

\code{dat.X <- function(what,...) \{ dat("Y") ^ 2 \}}\cr

and if the user has requested averaging, with \code{dpar(avg=non_zero)},
this will result in \code{average(Y^2)}, where Y is the value returned by \code{dat("Y")}.

To average values before the derivation, specify \code{avg=TRUE}
in the dat calls for the input variables within the derivation function.
This function:\cr

\code{dat.X <- function(what,...) \{ dat("Y",avg=TRUE,smooth=TRUE) ^ 2 \}}\cr

would result in \code{average(Y)^2}.

Therefore each dat function must consider if it wants to work with averaged
inputs or average the result.

\emph{Averaging Higher Moments}

See \code{\link{dpar_isfs}} for details about averaging and higher moments.

\strong{Smoothing}

The same treatment applies to the smooth argument to dat except that
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
}
\examples{
\dontrun{
    x = dat("RH")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
