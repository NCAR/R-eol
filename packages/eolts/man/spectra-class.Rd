\name{spectra-class}
\Rdversion{1.1}
\docType{class}
\alias{spectra-class}
\alias{Cbind}
\alias{deltaf}
\alias{deltaf<-}
\alias{fftw}
\alias{frequencies}
\alias{frequencies<-}
\alias{plot.spectra}
\alias{specsmooth}

\title{Class \code{"spectra"}}
\description{
spectra is a base class for data series with associated frequencies, such as FFTs (class fftw), power spectra (class pspectra), cross-spectra (class xspectra), co-spectra  (class cospectra), quadrature spectra (quadspectra), phase and coherence.
Class supporting generation of spectra from nts time series, along with smoothing, analysis and plotting.
}
\section{Objects from the Class}{
Spectra objects are typically created from FFTs performed on a time series, \code{xs <- pspectra(fftw(window(ts)))}.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"matrix"} Spectral estimates, stored in the usual way for FFTs. See docs for fftw. }
    \item{\code{type}:}{Object of class \code{"character"}, indicating how the FFT was done, such as "forwardcomplex", "inversecomplex", "halfcomplexodd"," halfcomplexeven" or "real".  }
    \item{\code{frequencies}:}{Object of class \code{"numeric"} frequency values for the spectra}
    \item{\code{units}:}{Object of class \code{"character"} Units of the data in the matrix. }
    \item{\code{funits}:}{Object of class \code{"character"} frequency units, typically "Hz". }
    \item{\code{stations}:}{Object of class \code{"integer"} Associated measurement stations. }
    \item{\code{start}:}{Object of class \code{"utime"} Start time of the original time series.  }
    \item{\code{end}:}{Object of class \code{"utime"} End time of the original time series. }
    \item{\code{deltat}:}{Object of class \code{"numeric"} Sampling period of the original time series. }
    \item{\code{deltaf}:}{Object of class \code{"numeric"} Frequency difference (Hz) of the spectra, typically \code{1 / N / dT}. }
    \item{\code{wss}:}{Object of class \code{"numeric"} Window coefficients squared and summed, if original time series was windowed. }
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "spectra")}: ... }
    \item{[<-}{\code{signature(x = "spectra", i = "ANY", j = "ANY", value = "ANY")}: ... }
    \item{Cbind}{\code{signature(x1 = "spectra", x2 = "spectra")}: ... }
    \item{coerce}{\code{signature(from = "spectra", to = "matrix")}: ... }
    \item{Complex}{\code{signature(z = "spectra")}: ... }
    \item{deltaf}{\code{signature(x = "spectra")}: ... }
    \item{deltaf<-}{\code{signature(x = "spectra", value = "numeric")}: ... }
    \item{deltat}{\code{signature(x = "spectra")}: ... }
    \item{deltat<-}{\code{signature(x = "spectra", value = "numeric")}: ... }
    \item{dim}{\code{signature(x = "spectra")}: ... }
    \item{dimnames}{\code{signature(x = "spectra")}: ... }
    \item{dimnames<-}{\code{signature(x = "spectra", value = "list")}: ... }
    \item{end}{\code{signature(x = "spectra")}: ... }
    \item{end<-}{\code{signature(x = "spectra", value = "utime")}: ... }
    \item{fftw}{\code{signature(x = "spectra", inverse = "logical", use.mvfft = "logical")}: ... }
    \item{fftw}{\code{signature(x = "spectra", inverse = "logical", use.mvfft = "missing")}: ... }
    \item{fftw}{\code{signature(x = "spectra", inverse = "missing", use.mvfft = "missing")}: ... }
    \item{frequencies}{\code{signature(x = "spectra")}: ... }
    \item{frequencies<-}{\code{signature(x = "spectra", value = "numeric")}: ... }
    \item{is.infinite}{\code{signature(x = "spectra")}: ... }
    \item{is.na}{\code{signature(x = "spectra")}: ... }
    \item{lines}{\code{signature(x = "spectra")}: ... }
    \item{Math}{\code{signature(x = "spectra")}: ... }
    \item{Ops}{\code{signature(e1 = "ANY", e2 = "spectra")}: ... }
    \item{Ops}{\code{signature(e1 = "spectra", e2 = "ANY")}: ... }
    \item{Ops}{\code{signature(e1 = "spectra", e2 = "missing")}: ... }
    \item{Ops}{\code{signature(e1 = "spectra", e2 = "spectra")}: ... }
    \item{plot}{\code{signature(x = "spectra", y = "ANY")}: ... }
    \item{points}{\code{signature(x = "spectra")}: ... }
    \item{specsmooth}{\code{signature(x = "spectra", type = "character")}: ... }
    \item{specsmooth}{\code{signature(x = "spectra", type = "missing")}: ... }
    \item{start}{\code{signature(x = "spectra")}: ... }
    \item{start<-}{\code{signature(x = "spectra", value = "utime")}: ... }
    \item{stations}{\code{signature(x = "spectra")}: ... }
    \item{stations<-}{\code{signature(x = "spectra", value = "ANY")}: ... }
    \item{stations<-}{\code{signature(x = "spectra", value = "integer")}: ... }
    \item{Summary}{\code{signature(x = "spectra")}: ... }
    \item{units}{\code{signature(x = "spectra")}: ... }
    \item{units<-}{\code{signature(x = "spectra", value = "character")}: ... }
	 }
}
\references{
http://fftw.org
}
\author{
Gordon Maclean
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\linkS4class{nts}}
}
\examples{
nr <- 100
per <- 10
dt <- .01
# 2 column time series of sines and cosines
x <- nts(matrix(
        c(sin((0:(nr-1)) * (per / nr) * 2 * pi),
          cos((0:(nr-1)) * (per / nr) * 2 * pi)), nrow=nr),
        names=c("sin","cos"),
        seq(from=utime("2002 feb 01 00:00"),length=nr,by=dt))
# compute power spectra and plot it. Spike at per / (nr * dt) = 10Hz
px <- pspectra(fftw(x))
plot(px)
# co-spectra
cx <- cospectra(fftw(x))
plot(cx)
# cross spectra
xx <- xspectra(fftw(x))
# Plot real part, same as co-spectra
plot(Re(xx))

# time series from a sample netcdf file 
# see ?dat for easier ways to read an ISFS netcdf file
datadir <- file.path(system.file("unitTests", package="eolts"),"data")
ncfile <- "test_\%Y\%m\%d.nc"

t1  <- utime("2012 oct 10 19:00:00",time.zone="US/Mountain")
t2  <- utime("2012 oct 11 20:00:00",time.zone="US/Mountain")

con <- netcdf(file=ncfile,dir=datadir,start=t1,end=t2)
# read u.1m from 21 stations
x <- readts(con,"u.1m", start=t1, end=t2)
close(con)
plot(x[,1:2])
# see effect of windowing before FFT
plot(window(x[,1:2]))
px <- specsmooth(pspectra(fftw(window(x[,1:2]))))
plot(px)
}
\keyword{ ts }
