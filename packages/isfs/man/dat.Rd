\name{dat}
\alias{dat}
\title{
Return one or more time series matching a variable name.
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

If \code{derived} is TRUE, then a search is done for a function to provide the derivation. For a value of \code{dat(what="a.b.c",derived=TRUE)}, then a search is done for a function with the following names
\itemize{
    \item dat.a.b.c
    \item dat.a.b
    \item dat.a
}

The first function that is found is called to perform the derivation.

Then the current set of NetCDF files is searched for variables with names, or short_name attributes matching \code{what}. For a value of \code{what="RH"}, then a variable with a name or short_name attribute of "RH" is read from the NetCDF files. Also any variables with a short_name beginning with "RH." are read.

The result of the derivation, if done, and the NetCDF reads are combined with Cbind into a multi-column time series.

Averaging

Averaging is controlled by two parameters:
\enumerate{
    \item the "avg" dpar parameter:
        \itemize{
           \item \code{dpar(avg=c(nonsimple.period,simple.period))}
           \item \code{dpar(avg=simple.period)}
        }
    \item the avg=TRUE/FALSE parameter to the \code{dat} function.
}

If dpar("avg") is non-zero, and avg=TRUE in the call to dat, then averaging
is done.

The avg argument to dat defaults to FALSE, unless this is the top-most call to dat,
in which case it defaults to TRUE.  The top-most call is detected by checking
\code{sys.nframe}, and placing it in \code{.isfsEnv}.

If a derived function exists for \code{what}, then \code{avg} is not passed
to the derived function, defaulting to FALSE.  The result returned by
the derived function will be averaged by \code{dat} if avg is TRUE.

This scheme results in derived variables being computed from non-reduced data,
then averaged after the derivation: i.e. lazy, at the last minute, averaging.
For example, this dat function does the default in deriving a \code{dat("X")}
from \code{dat("Y")}^2:\cr

\code{dat.X <- function(what,...) \{ dat("Y") ^ 2 \}}\cr

and results in \code{average(Y^2)}, if \code{dpar(avg=nonzero)}, where Y is the value
returned by \code{dat("Y"}.

To override this behavior within a dat function, specify \code{avg=TRUE}
in the dat calls for the input variables.

This function:\cr

\code{dat.X <- function(what,...) \{ dat("Y",avg=TRUE,smooth=TRUE) ^ 2 \}}\cr

would result in \code{average(Y)^2}.

Therefore each dat function must consider if it wants to work with averaged
inputs or average the result.

The same treatment applies to the smooth argument to dat except that
it always defaults to FALSE, even in the top frame, i.e. the user must
specify it if they want smoothing.  However dat functions can set avg to
true in their calls to dat to get input variables (typically used to smooth
turbulent input variables).

Like averaging, smoothing is accomplished by computing means.  However
specifying smooth=TRUE, and a smooth time period in dpar results in
simple running averages of the selected data variables, with an output
interval equal to the current deltaT of the data.

Specifying avg=TRUE and the avg time periods in dpar results in more
complicated processing, where for example, averages of
second moments will include the deviation of the means.

Data is cached before smoothing or averaging.

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
