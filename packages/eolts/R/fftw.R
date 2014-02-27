# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
#               Copyright (C) by UCAR
# 
setClass("spectra",
    slots=c(
        data="matrix",
        type="character",
        frequencies="numeric",
        units="character",
        funits="character",
        stations="integer",
        start="utime",
        end="utime",
        deltat="numeric",
        deltaf="numeric",
        wss="numeric"
    )
)

setClass("fftw","spectra")

# Constructor of an fftw object.
# Typically a user does not use this function.
#
# The coefficients are stored in the order expected by the
# FFTW package, and the comments below are taken from the FFTW docs.

# If this is an fft of a real input series, then type="halfcomplex", and
# 'data` is a non-complex numeric matrix, with
# the coefficients in "half-complex" order:
# r0, r1, r2, ..., rn/2, i(n+1)/2-1, ..., i2, i1
# Here, rk is the real part of the kth output, and ik is the imaginary part.
# and integer division is rounded down.

# If this is an inverse fft of a halfcomplex fft, then type="real".

# If this is an fft of a complex input series, then type="complex",
# and the coefficients are in the standard output ordering--the k-th
# output corresponds to the frequency k/n (or k/T, where T is your
# total sampling period). For those who like to think in terms of
# positive and negative frequencies, this means that the positive
# frequencies are stored in the first half of the output and the
# negative frequencies are stored in backwards order in the second
# half of the output. (The frequency -k/n is the same as the frequency
# (n-k)/n.)

fftw.ctor <- function(data,type,units,deltat,deltaf,start,end,wss)
{

    # if (!is.matrix(data)) data <- matrix(data,ncol=1)
    nr <- nrow(data)
    mid <- trunc(nr / 2) + 1

    # cat("fftw.ctor, type=",type,", nr=",nr,", mid=",mid,
    #     ", is.complex(data)=",is.complex(data),"\n")

    if (type == "forwardcomplex") {
        data <- data[c((mid+1):nr,1:mid),,drop=FALSE]
        frequencies <- c( (mid-nr):-1, 0:(mid-1) ) / nr
    }
    else if (type == "inversecomplex") {
        frequencies <- numeric(0)
    }
    else if (type == "halfcomplexodd") {
        if (is.complex(data)) {
            data <- data[1:mid,,drop=F]
        }
        else {
            data <- matrix(complex(real=data[1:mid,],
                imaginary=apply(data[nr:(mid+1),,drop=FALSE],2,function(x) c(0,x))),
                nrow=mid,
                dimnames=dimnames(data))
        }
        frequencies <- (0 : (mid-1)) / nr
    }
    else if (type == "halfcomplexeven") {
        if (is.complex(data)) {
            # matrix returned by mvfft is complex. The upper half is
            # the complex conjugate of the lower half, in reverse,
            # and can be discarded.
            data <- data[1:mid,,drop=F]
            data[mid,] <- complex(real=Re(data[mid,]),imaginary=0)
        }
        else {
            # matrix returned by fftw is stored as real, but the
            # upper half is the imaginary part in reverse order.
            data <- matrix(complex(real=data[1:mid,],
                imaginary=apply(data[nr:(mid+1),,drop=FALSE],2,function(x) c(0,x,0))),
                nrow=mid,
                dimnames=dimnames(data))
        }
        frequencies <- (0 : (mid-1)) / nr
    }
    else if (type == "real") {
        frequencies <- numeric(0)
    }

    if (length(frequencies) > 0 && length(frequencies) != nrow(data))
        stop(paste("length(frequencies)=",length(frequencies),
                "is not equal to nrow(data)=",nrow(data)))

    # cat("fftw.ctor, dim(data)=",dim(data),", is.complex(data)",is.complex(data),"\n")

    res <- new("fftw",data=data,type=type,frequencies=frequencies,funits="")

    if (!missing(units)) res@units <- units
    else res@units <- rep("",ncol(res@data))

    if (!missing(deltat)) res@deltat <- deltat
    else res@deltat <- 1

    if (!missing(deltaf)) res@deltaf <- deltaf
    else res@deltaf <- 1 / nr

    if (!missing(start)) res@start <- start
    if (!missing(end)) res@end <- end

    if (!missing(wss)) res@wss <- wss
    else res@wss <- nr^2
    res
}

setGeneric("fftw",function(x,inverse,use.mvfft) standardGeneric("fftw"))

setMethod("fftw",signature(x="matrix",inverse="logical",use.mvfft="logical"),
    function(x,inverse,use.mvfft)
    {
        nr <- dim(x)[1]
        nc <- dim(x)[2]

        # cat("fftw: inverse=",inverse,", use.mvfft=",use.mvfft,", nr=",nr,", nc=",nc, ", is.complex(x)=",is.complex(x),"\n")

        # browser()

        if (is.complex(x)) {
            # compute an fft for each column of a complex matrix
            # fft is returned in the standard way:
            #     fft[1] is for frequency 0 (DC)
            #     fft[2:(N/2+1)] are the N/2 frequencies up to the nyquist:
            #     1/N/dt, 2/N/dt, ..., 1/2/dt
            #     fft[(N/2+1):N] are the neg. frequencies: -1/2/dt, ... -1/N/dt
            restype <- if (inverse) "inversecomplex" else "forwardcomplex"

            if (use.mvfft || .Platform$OS.type == "windows")
                x <- mvfft(x,inverse=inverse)
            else
                x <- .Call("R_cfftw", nr, nc, x, inverse, PACKAGE="eolts")
            fftw.ctor(x,type=restype)
        }
        else {
            if (any(is.na(x))) x <- replace.nas(x,warn=TRUE)

            # compute an fft for each column of a real matrix
            #     fft[1] is for frequency 0 (DC)
            #     fft[2:(N/2+1)] are the N/2 frequencies up to the nyquist:
            #         1/N/dt, 2/N/dt, ..., 1/2/dt
            #     fft[(N/2+1):N] are the neg. frequencies: -1/2/dt, ... -1/N/dt

            restype <- if (inverse) "real" else if ((nr %% 2) == 1) "halfcomplexodd"
                else "halfcomplexeven"
            if (use.mvfft || .Platform$OS.type == "windows")
                x <- mvfft(x,inverse=inverse)
            else 
                x <- .Call("R_rfftw", nr, nc, x, inverse, PACKAGE="eolts")

            # cat("fftw x dimnames=",paste(dimnames(x)[[2]],collapse=","),"\n")
            fftw.ctor(x,type=restype)
        }
    }
)

setMethod("fftw",signature(x="matrix",inverse="logical",use.mvfft="missing"),
    function(x,inverse,use.mvfft)
    {
        fftw(x,inverse=inverse,use.mvfft=.Platform$OS.type == "windows")
    }
)

setMethod("fftw",signature(x="matrix",inverse="missing",use.mvfft="missing"),
    function(x,inverse,use.mvfft)
    {
        fftw(x,inverse=FALSE,use.mvfft=.Platform$OS.type == "windows")
    }
)

setMethod("fftw",signature(x="spectra",inverse="logical",use.mvfft="logical"),
    function(x,inverse,use.mvfft)
    {

        nr <- nrow(x@data)
        mid <- trunc(nr / 2) + 1
        # browser()

        if (x@type == "forwardcomplex") 
            x <- x@data[c(mid:nr,1:(mid-1)),,drop=FALSE]
        else if (x@type == "inversecomplex")
            x <- x@data
        else if (x@type == "halfcomplexodd") {
            if (use.mvfft) x <- rbind(x@data,Conj(x@data[nr:2,,drop=FALSE]))
            else x <- rbind(Re(x@data),Im(x@data[nr:2,,drop=FALSE]))
        }
        else if (x@type == "halfcomplexeven") {
            if (use.mvfft) x <- rbind(x@data,Conj(x@data[(nr-1):2,,drop=FALSE]))
            else x <- rbind(Re(x@data),Im(x@data[(nr-1):2,,drop=FALSE]))
        }
        else if (x@type == "real")
            x <- x@data
        else stop(paste("unknown spectra type:",x@type))

        fftw(x,inverse=inverse,use.mvfft=use.mvfft)
    }
)


setMethod("fftw",signature(x="spectra",inverse="logical",use.mvfft="missing"),
    function(x,inverse,use.mvfft)
    {
        fftw(x,inverse=inverse,use.mvfft=(.Platform$OS.type == "windows"))
    }
    )

setMethod("fftw",signature(x="spectra",inverse="missing",use.mvfft="missing"),
    function(x,inverse,use.mvfft)
    {
        fftw(x,inverse=TRUE,use.mvfft=(.Platform$OS.type == "windows"))
    }
    )

setMethod("fftw",signature(x="nts",inverse="missing",use.mvfft="logical"),
    function(x,inverse,use.mvfft)
    {
        nr <- nrow(x@data)

        # fftw removes NAs if necessary
        res <- fftw(x@data,inverse=FALSE,use.mvfft=use.mvfft)

        dt <- deltat(x)
        if (abs(dt["max"]-dt["min"])/dt[1] > .5)
            warning(paste("Time series is not evenly sampled, deltat",paste(names(dt),dt,sep="=",collapse=", ")," seconds"))

        df <- 1/ nr / dt[1]
        names(df) <- NULL

        stations(res) <- stations(x)
        units(res) <- units(x)
        deltat(res) <- dt[1]
        deltaf(res) <- df
        start(res) <- start(x)
        end(res) <- end(x)

        if (length(x@wss) != 0) res@wss <- x@wss
        else res@wss <- nr^2

        # res@frequencies <- (0 : (nr/2) ) * df
        res@frequencies <- res@frequencies / dt[1]
        res@funits <- "Hz"
        res
    }
)

setMethod("fftw",signature(x="nts",inverse="missing",use.mvfft="missing"),
    function(x,inverse,use.mvfft)
    {
        fftw(x,use.mvfft=(.Platform$OS.type == "windows"))
    }
    )

subscript.op.spectra <- function(x,i,j,...,drop=FALSE)
{
    class.x <- class(x)


    #		nargs()
    # x[]:	2   !has.i, !has.j
    # x[i]:	2   has.i, !has.j
    # x[i,]:	3   has.i, !has.j
    # x[,j]:	3   !has.i, has.j
    # x[i,j]:	3   has.i, has.j

    has.i <- !missing(i)
    if(nargs() == 2) {
        if (!has.i || is.null(i)) return(x)
        # with only one indice, x[i] is no longer an fft.
        else return(x@data[i])
    }
    has.j <- !missing(j)

    if(!has.i) i <- NULL

    if(!has.j) j <- NULL

    nr <- nrow(x)
    nc <- ncol(x)

    if (length(i) == 0) i <- 1:nr

    if (length(j) == 0) j <- 1:nc
    else if (is.character(j)) j <- match(j,dimnames(x@data)[[2]],nomatch=0)

    x@data <- x@data[i,j,drop=FALSE]

    if (!length(x)) {
        warning("spectra object has zero length")
        return(x)
    }

    if (length(x@frequencies) > 0) x@frequencies <- x@frequencies[i]

    x@stations <- x@stations[j]
    x@units <- x@units[j]
    class(x) <- class.x

    x
}

setMethod("[",signature(x="spectra"), subscript.op.spectra)
setMethod("[",signature(x="fftw"), subscript.op.spectra)

subscript.assign.op.spectra <- function(x,i,j,...,value)
{

    # class.x <- class(x)

    if (.hasSlot(value,"data")) value <- value@data


    # x[] <- y      nargs()==3, !has.i, !has.j
    # x[i] <- y     nargs()==3, has.i, !has.j
    # x[i,] <- y    nargs()==4, has.i, !has.j
    # x[,j] <- y    nargs()==4, !has.i, has,j
    # x[i,j] <- y   nargs()==4, has.i, has.j

    has.i <- !missing(i)
    has.j <- !missing(j)

   if (!has.i && !has.j) {
        x@data[] <- value
        return(x)
    }

    nr <- nrow(x)
    nc <- ncol(x)

    # x[,j] <- val
    if (!has.i) i <- 1:nr

    # cat("length(i)=",length(i)," length(j)=",length(j)," length(value)=",length(value),"\n")

    # x[i,] <- val
    if (!has.j && nargs() == 4) j <- 1:nc

    if (is.character(j)) j <- match(j,dimnames(x)[[2]],nomatch=0)

    # browser()

    if (nargs()==3) x@data[i] <- value
    else x@data[i,j] <- value

    # class(x) <- class.x
    x
}

setReplaceMethod("[",signature(x="spectra",value="ANY"),
    subscript.assign.op.spectra)

setReplaceMethod("[",signature(x="fftw",value="ANY"),
    subscript.assign.op.spectra)


setMethod("Math","spectra",
    function(x) {
        x@data <- callGeneric(x@data)
        x
    }
    )

# setMethod("Math","fftw",
#   function(x) {
#     x@data <- callGeneric(x@data)
#     x
#   }
# )


setMethod("Ops",signature(e1="spectra",e2="spectra"),
    function(e1,e2) {
        e1@data <- callGeneric(e1@data,e2@data)
        e1
    }
    )
setMethod("Ops",signature(e1="spectra",e2="ANY"),
    function(e1,e2) {
        e1@data <- callGeneric(e1@data,e2)
        e1
    }
    )

setMethod("Ops",signature(e1="spectra",e2="missing"),
    function(e1,e2) {
        e1@data <- callGeneric(e1@data)
        e1
    }
    )

setMethod("Ops",signature(e1="ANY",e2="spectra"),
    function(e1,e2) {
        e2@data <- callGeneric(e1,e2@data)
        e2
    }
    )

setMethod("Summary","spectra",
    function(x,...,na.rm=FALSE) {
        callGeneric(x@data,na.rm=na.rm)
    }
    )


setMethod("is.infinite",signature(x="spectra"),
    function(x) {
        x@data <- is.infinite(x@data)
        x
    }
    )

setMethod("Complex",signature(z="spectra"),
    function(z) {
        z@data <- callGeneric(z@data)
        z
    }
    )

if (FALSE) {
# setMethod("Re",signature(z="fftw"),
#   function(z) {
#     z@data <- Re(z@data)
#     z
#   }
# )


setMethod("Im",signature(z="spectra"),
    function(z) {
        z@data <- Im(z@data)
        z
    }
    )

# setMethod("Im",signature(z="fftw"),
#   function(z) {
#     z@data <- Im(z@data)
#     z
#   }
# )

setMethod("Arg",signature(z="spectra"),
    function(z) {
        z@data <- Arg(z@data)
        z
    }
    )

setMethod("Mod",signature(z="spectra"),
    function(z) {
        z@data <- Mod(z@data)
        z
    }
    )

setMethod("Conj",signature(z="spectra"),
    function(z) {
        z@data <- Conj(z@data)
        z
    }
    )
}

# Cannot do setGeneric("is.complex"). It results in error:
# "methods may not be defined for primitive function ‘is.complex’ in this version of R"
# setGeneric("is.complex",function(x) standardGeneric("is.complex"))
# Without the setGeneric, this setMethod seems to accomplish nothing
if (FALSE) {
setMethod("is.complex",signature(x="spectra"),
    function(x) {
        is.complex(x@data)
    }
    )
}

# is.complex is not generic, so these S3 methods are not automatically invoked
# on an fftw or spectra
is.complex.spectra <- function(x)
{
    is.complex(x@data)
}
is.complex.fftw <- function(x)
{
    is.complex(x@data)
}

# setMethod("Conj",signature(z="fftw"),
#   function(z) {
#     z@data <- Conj(z@data)
#     z
#   }
# )

setMethod("is.na",signature(x="spectra"),
    function(x) {
        x@data <- is.na(x@data)
        x
    }
    )

setMethod("units", signature(x="spectra"),
    function(x) {
        x@units
    }
    )

setReplaceMethod("units", signature(x="spectra",value="character"),
    function(x,value) {
        if (length(value) == 0) return(x)
        if (length(value) != ncol(x@data))
            warning(paste("Length of units (",length(value),") is not equal to number of data columns (",ncol(x@data),")."))
        x@units <- value
        x
    }
    )

# setReplaceMethod("units", signature(x="fftw",value="character"),
#   function(x,value) {
#     if (length(value) == 0) return(x)
#     if (length(value) != ncol(x@data))
#       warning(paste("Length of units (",length(value),") is not equal to number of data columns (",ncol(x@data),")."))
#     x@units <- value
#     x
#   }
# )

setMethod("start", signature(x="spectra"),
    function(x,...) {
        x@start
    }
    )

setMethod("end", signature(x="spectra"),
    function(x,...) {
        x@end
    }
    )

setReplaceMethod("start", signature(x="spectra",value="utime"),
    function(x,value) {
        x@start <- value
        x
    }
    )

# setReplaceMethod("start", signature(x="fftw",value="utime"),
#   function(x,value) {
#     x@start <- value
#     x
#   }
# )

setReplaceMethod("end", signature(x="spectra",value="utime"),
    function(x,value) {
        x@end <- value
        x
    }
    )

# setReplaceMethod("end", signature(x="fftw",value="utime"),
#   function(x,value) {
#     x@end <- value
#     x
#   }
# )

setMethod("dim", signature(x="spectra"),
    function(x) {
        dim(x@data)
    }
    )

setMethod("dimnames", signature(x="spectra"),
    function(x) {
        dimnames(x@data)
    }
    )

setReplaceMethod("dimnames", signature(x="spectra",value="list"),
    function(x,value) {
        dimnames(x@data) <- value
        x
    }
    )

# setReplaceMethod("dimnames", signature(x="fftw"),
#   function(x,value) {
#     dimnames(x@data) <- value
#     x
#   }
# )

setMethod("stations", signature(x="spectra"),
    function(x) {
        x@stations
    }
    )

setReplaceMethod("stations", signature(x="spectra",value="integer"),
    function(x,value) {
        x@stations <- value
        x
    }
    )

setReplaceMethod("stations", signature(x="spectra",value="ANY"),
    function(x,value)
    {
        nv <- names(value)
        if (is.null(nv)) nv <- rep("",length(value))
        if (!is.integer(value)) {
            if (is.numeric(value)) value <- round(value)
            value <- as.integer(value)
        }
        names(value) <- nv
        x@stations <- value
        x
    }
    )


setMethod("deltat", signature(x="spectra"),
    # deltat on splus library has args: x,...
    function(x,...) {
        x@deltat
    }
    )

setReplaceMethod("deltat", signature(x="spectra",value="numeric"),
    function(x,value) {
        x@deltat <- value
        x
    }
    )

setGeneric("deltaf",function(x) standardGeneric("deltaf"))
setMethod("deltaf", signature(x="spectra"),
    function(x) {
        x@deltaf
    }
    )

setGeneric("deltaf<-",function(x,value) standardGeneric("deltaf<-"))
setReplaceMethod("deltaf", signature(x="spectra",value="numeric"),
    function(x,value) {
        x@deltaf <- value
        x
    }
    )


setGeneric("frequencies",function(x) standardGeneric("frequencies"))

setMethod("frequencies", signature(x="spectra"),
    function(x) {
        x@frequencies
    }
    )

setGeneric("frequencies<-",function(x,value) standardGeneric("frequencies<-"))
setReplaceMethod("frequencies", signature(x="spectra",value="numeric"),
    function(x,value) {
        x@frequencies <- value
        x
    }
    )

