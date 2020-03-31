\name{dat-class}
\Rdversion{1.1}
\docType{class}
\alias{dat-class}
\alias{conform}
\alias{crack}
\alias{heights}
\alias{match.columns}
\alias{plot.dat}
\alias{select}
\alias{seriesConcat}
\alias{sites}
\alias{suffixes}
\alias{tilt}

\title{Class \code{"dat"}}
\description{
A time series class based on \code{\linkS4class{nts}}.
}
\section{Objects from the Class}{
dat objects are typically created with a call to function \code{\link{dat}}.
}
\section{Slots}{
  \describe{
    \item{\code{weights}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{weightmap}:}{Object of class \code{"integer"} ~~ }
    \item{\code{stations}:}{Object of class \code{"integer"} ~~ }
    \item{\code{deltat}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{wss}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{time.format}:}{Object of class \code{"character"} ~~ }
    \item{\code{time.zone}:}{Object of class \code{"character"} ~~ }
    \item{\code{fiscal.year.start}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{type}:}{Object of class \code{"character"} ~~ }
    \item{\code{data}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{positions}:}{Object of class \code{"positions"} ~~ }
    \item{\code{start.position}:}{Object of class \code{"positions"} ~~ }
    \item{\code{end.position}:}{Object of class \code{"positions"} ~~ }
    \item{\code{future.positions}:}{Object of class \code{"positions"} ~~ }
    \item{\code{units}:}{Object of class \code{"character"} ~~ }
    \item{\code{title}:}{Object of class \code{"character"} ~~ }
    \item{\code{documentation}:}{Object of class \code{"character"} ~~ }
    \item{\code{attributes}:}{Object of class \code{"ANY"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{nts}"}, directly.
Class \code{"\linkS4class{timeSeries}"}, by class "nts", distance 2.
Class \code{"\linkS4class{series}"}, by class "nts", distance 3.
Class \code{"\linkS4class{seriesVirtual}"}, by class "nts", distance 3.
}
\section{Methods}{
  \describe{
    \item{average}{\code{signature(x = "dat")}: ... }
    \item{clip}{\code{signature(x1 = "dat")}: ... }
    \item{conform}{\code{signature(x = "ANY", y = "dat")}: ... }
    \item{conform}{\code{signature(x = "dat", y = "dat")}: ... }
    \item{conform}{\code{signature(x = "numeric", y = "dat")}: ... }
    \item{crack}{\code{signature(x = "dat")}: ... }
    \item{dimnames<-}{\code{signature(x = "dat", value = "list")}: ... }
    \item{heights}{\code{signature(x = "dat")}: ... }
    \item{match.columns}{\code{signature(y = "dat", x = "dat")}: ... }
    \item{plot}{\code{signature(x = "dat", y = "missing")}: ... }
    \item{select}{\code{signature(x = "dat", stns = "numeric", hts = "numeric", sites = "character", sfxs = "character")}: Select columns of a \code{\linkS4class{dat}} time series by stations, sampling heights, site names or suffixes. See \code{\link{select-methods}} for details. }
    \item{seriesConcat}{\code{signature(x1 = "dat", x2 = "dat")}: ... }
    \item{sites}{\code{signature(x = "dat")}: ... }
    \item{suffixes}{\code{signature(x = "dat")}: ... }
    \item{tilt}{\code{signature(x = "dat", tiltp = "dat")}: ... }
	 }
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
\code{\link{dat}}, \code{\link{select-methods}}, \code{\linkS4class{nts}}, \code{\link{nts}}
}
\examples{
}
\keyword{classes}
