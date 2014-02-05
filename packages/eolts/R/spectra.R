# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
#               Copyright (C) by UCAR
# 
setClass("pspectra",contains="spectra")

setMethod("[","pspectra", subscript.op.spectra)

setReplaceMethod("[",signature(x="pspectra",value="ANY"),
    subscript.assign.op.spectra)

if (!isGeneric("pspectra",where=1))
    setGeneric("pspectra",function(x) standardGeneric("pspectra"))

setMethod("pspectra",signature(x="fftw"),
    function(x) {
        # Computation of normalized one-sided or two-sided power spectra
        # density from FFT.
        #
        # If the input fft has estimates at negative frequency, then
        # the two sided spectra density is computed, otherwise the one-sided
        # is computed.
        #
        # Computation of One-sided Power Spectra density from FFT:
        #
        # Discrete form of Parseval's Theorem in S notation:
        #     sum (h * Conj(h)) = 1 / N * sum(H * Conj(H))
        #     h is the time series, H are the discrete Fourier transforms from fft()
        #
        # Power of a de-meaned time series:
        #     Power = sum(h * Conj(h)) / N
        # so  Power = sum(H * Conj(H)) / N^2
        #
        # If S is the power spectral density:
        #     Power = integral over all freq ( S(f) df )  which is approximated by
        #       = sum( S ) / ( N * dt)
        #     where dt is the sampling interval, df = 1/(N * dt) is the fundamental
        #     frequency and S are the discrete power spectral densities.
        #
        # So  S = (H * Conj(H)) * (N * dt) / N^2
        #
        # If the data is windowed, the relation is:
        #     S = (H * Conj(H)) * (N * dt) / wss
        #
        # For real time series H(f) = Conj(H(-f)) and the integral is
        # commonly performed over non-negative frequencies from 0 to the
        # Nyquist, i.e. a one-sided integral.  The one-sided PSDs are then:
        #
        #     P = 2 * S
        #
        # In the section "Power Spectrum Estimation" in "Numerical Recipes in C"
        # the PSD estimates at frequencies 0 and the Nyquist are 1/2
        # of the above, apparently to compensate for the fact that the frequency
        # bin width at the extremes is 1/2 the width of the other bins.  This
        # ensures that:
        #     Power = sum(P) / (N * dt)  over non-negative frequencies
        #

        df = x@deltaf
        wss = x@wss

        # cat("pspectra, spectra type=",x@type,"\n")
        if (x@type == "forwardcomplex")
            x@data = Re(x@data * Conj(x@data)) / wss / df
        else if (x@type == "halfcomplexodd") {
            x@data = 2. * Re(x@data * Conj(x@data)) / wss / df
            x@data[1,] = x@data[1,] / 2.
        }
        else if (x@type == "halfcomplexeven") {
            x@data = 2. * Re(x@data * Conj(x@data)) / wss / df
            nr = nrow(x@data)
            x@data[c(1,nr),] = x@data[c(1,nr),] / 2.
        }
        else {
            warning("pspectra undefined for spectra of type",x@type)
            x@data = Re(x@data * Conj(x@data)) / wss / df
        }
        res = new("pspectra",data=x@data,type=x@type,frequencies=x@frequencies,
            funits=x@funits,units=x@units,stations=x@stations,
            start=x@start,end=x@end,deltat=x@deltat,deltaf=x@deltaf,
            wss=x@wss)
        units(res) <- paste("(",units(res),")^2 s",sep="")
        res
    }
    )

setClass("xspectra",contains="spectra",slots=c(combinations="matrix"))

if (!isGeneric("xspectra",where=1))
    setGeneric("xspectra",function(x) standardGeneric("xspectra"))

setMethod("xspectra",signature(x="fftw"),
    function(x) {

        nc = ncol(x@data)
        if (nc < 2) return(NULL)
        dns <- dimnames(x@data)[[2]]

        combinations <- matrix(F,ncol=nc,nrow=nc)
        combinations[col(combinations) < row(combinations)] <- T

        i1 <- matrix(rep(1:nc,nc),ncol=nc,byrow=TRUE)[combinations]
        # first index of cross
        # combinations
        i2 <- matrix(rep(1:nc,nc),ncol=nc)[combinations]      # 2nd index

        df = x@deltaf
        wss = x@wss

        # cat("xspectra, spectra type=",x@type,"\n")
        if (x@type == "forwardcomplex")
            x@data = x@data[,i1,drop=FALSE] * Conj(x@data[,i2,drop=FALSE]) / wss / df
        else if (x@type == "halfcomplexodd") {
            x@data = 2. * x@data[,i1,drop=FALSE] * Conj(x@data[,i2,drop=FALSE]) / wss / df
            x@data[1,] = x@data[1,] / 2.
        }
        else if (x@type == "halfcomplexeven") {
            x@data = 2. * x@data[,i1,drop=FALSE] * Conj(x@data[,i2,drop=FALSE]) / wss / df
            nr = nrow(x@data)
            x@data[c(1,nr),] = x@data[c(1,nr),] / 2.
        }
        else {
            warning("xspectra undefined for spectra of type",x@type)
            x@data = x@data[,i1,drop=FALSE] * Conj(x@data[,i2,drop=FALSE]) / wss / df
        }

        # Convert to complex conjugate
        # Without doing this we were seeing the wrong sign
        # in the quadspectra.
        # Perhaps it is due to the fact that a forward
        # transform in FFTW uses a negative exponent:
        # 	e^(-2pi*i*j*k/n), whereas
        # Lumley and Panofsky assume a positive exponent.
        # Needs checking...
        #
        x@data = Conj(x@data)

        dimnames(x@data) <- list(NULL,paste(dns[i1],":",dns[i2],sep=""))
        dimnames(combinations) <- list(dns,dns)
        combinations[combinations] <- 1:length(i1)
        combinations[!combinations] <- NA

        res = new("xspectra",data=x@data,frequencies=x@frequencies,
            funits=x@funits,units=x@units,stations=x@stations,
            start=x@start,end=x@end,deltat=x@deltat,deltaf=x@deltaf,
            wss=x@wss,combinations=combinations)
        res@units <- paste(x@units[i1],x@units[i2],"s",sep=" ")
        res
    }
    )

setClass("cospectra",contains="xspectra")

if (!isGeneric("cospectra",where=1))
    setGeneric("cospectra",function(x) standardGeneric("cospectra"))
setMethod("cospectra",signature(x="xspectra"),
    function(x) {
        x@data = Re(x@data)
        class(x) = "cospectra"
        x
    }
    )

setMethod("cospectra",signature(x="fftw"),
    function(x) {
        cospectra(xspectra(x))
    }
    )

setClass("quadspectra",contains="xspectra")

if (!isGeneric("quadspectra",where=1))
    setGeneric("quadspectra",function(x) standardGeneric("quadspectra"))
setMethod("quadspectra",signature(x="xspectra"),
    function(x) {
        # Lumley & Panofsky, Structure of Atmos. Turbulence, equation 1-56,
        # quadrature spectra is negative imaginary part of cross spectra.
        # If you flip the input variables (x,y) -> (y,x) it changes
        # the sign of the quadrature.
        #
        x@data = -Im(x@data)
        class(x) = "quadspectra"
        x
    }
    )

setMethod("quadspectra",signature(x="fftw"),
    function(x) {
        quadspectra(xspectra(x))
    }
    )

subscript.op.xspectra = function(x,i,j,k,...,drop=FALSE)
{
    class.x = class(x)

    #           nargs()
    # x[]:      2   !has.i, !has.j
    # x[i]:     2   has.i, !has.j
    # x[i,]:    3   has.i, !has.j
    # x[,j]:    3   !has.i, has.j
    # x[i,j]:   3   has.i, has.j

    has.i <- !missing(i)
    if(nargs() == 2) {
        if (!has.i || is.null(i)) return(x)
        # with only one indice, x[i] is no longer a spectra
        else return(x@data[i])
    }

    if(!has.i) i <- NULL

    has.j <- !missing(j)
    if(!has.j) j <- NULL

    has.k <- !missing(k)
    if(has.k) {
        if (is.null(j)) j = x@combinations[,k]
        else {
            dns <- dimnames(x@combinations)[[1]]
            if (is.character(j)) j <- match(j,dns,nomatch=0)
            if (is.character(k)) k <- match(k,dns,nomatch=0)
            mx <- k > j
            if (any(mx)) {
                tk <- k
                k[mx] <- j[mx]
                j[mx] <- tk[mx]
            }
            j <- x@combinations[j,k]
        }
        j <- j[!is.na(j)]
        if (length(j) < 1) return(NULL)

        if (is.null(i)) i <- 1:nrow(x@data)
        return(x[i,j])	# recursive
    }

    if (is.null(j)) j = 1:ncol(x)

    combs = x@combinations
    combs[match(combs,j,nomatch=0)==0] = NA
    combs = combs - min(j) + 1

    class(x) = "spectra"
    x = x[i,j]
    class(x) = class.x
    x@combinations = combs
    x
}

setMethod("[","xspectra",subscript.op.xspectra)

setMethod("[","cospectra",subscript.op.xspectra)

setMethod("[","quadspectra",subscript.op.xspectra)

subscript.assign.op.xspectra = function(x,i,j,k,..,value)
{
    class.x = class(x)

    if (.hasSlot(value,"data")) value <- value@data

    # x[] <- y      nargs()==3, !has.i, !has.j
    # x[i] <- y     nargs()==3, has.i, !has.j
    # x[i,] <- y    nargs()==4, has.i, !has.j
    # x[,j] <- y    nargs()==4, !has.i, has,j
    # x[i,j] <- y   nargs()==4, has.i, has.j

    has.i <- !missing(i)
    has.j <- !missing(j)
    has.k <- !missing(k)

    if (!has.i && !has.j) {
        x@data[] <- value
        return(x)
    }

    if (!has.i) i <- NULL
    if (!has.j) j <- NULL
    if (!has.k) k <- NULL

    if (!is.null(k)) {
        if (is.null(j)) j = x@combinations[,k]
        else {
            dns <- dimnames(x@combinations)[[1]]
            if (is.character(j)) j <- match(j,dns,nomatch=0)
            if (is.character(k)) k <- match(k,dns,nomatch=0)
            mx <- k > j
            if (any(mx)) {
                tk <- k
                k[mx] <- j[mx]
                j[mx] <- tk[mx]
            }
            j <- x@combinations[j,k]
        }
        j <- j[!is.na(j)]
        if (length(j) < 1) return(x)	# no change

        if (is.null(i)) i <- 1:nrow(x@data)
        x[i,j] <- value		# recursive
        return(x)
    }

    combs = x@combinations
    class(x) = "spectra"
    x[i,j] <- value
    class(x) = class.x
    x@combinations = combs
    x
}

setMethod("[<-",signature(x="xspectra",value="ANY"),
    subscript.assign.op.xspectra)

setMethod("[<-",signature(x="cospectra",value="ANY"),
    subscript.assign.op.xspectra)

setMethod("[<-",signature(x="quadspectra",value="ANY"),
    subscript.assign.op.xspectra)

setClass("phase",contains="xspectra")

if (!isGeneric("phase",where=1))
    setGeneric("phase",function(x) standardGeneric("phase"))
setMethod("phase",signature(x="xspectra"),
    function(x) {
        x@data = Arg(x@data)
        class(x) = "phase"
        units(x) <- rep("",ncol(x@data))
        x
    }
    )

setMethod("phase",signature(x="fftw"),
    function(x) {
        phase(xspectra(x))
    }
    )

setClass("coherence",contains="xspectra")
if (!isGeneric("coherence",where=1))
    setGeneric("coherence",function(x,type,bpd) standardGeneric("coherence"))
setMethod("coherence",signature(x="fftw",type="character",bpd="numeric"),
    function(x,type,bpd) {

        if (ncol(x) < 2) stop("Cannot do coherence of 1 column fft")

        xx = specsmooth(xspectra(x),type=type,bpd=bpd)
        combs = xx@combinations
        mc = match(1:ncol(xx),as.vector(combs),nomatch=0)
        i1 = as.vector(row(combs))[mc]
        i2 = as.vector(col(combs))[mc]

        pspec = specsmooth(pspectra(x),type=type,bpd=bpd)

        x = xx * Conj(xx) / pspec[,i1] / pspec[,i2]
        class(x) = "coherence"
        x
    }
    )

setMethod("coherence",signature(x="fftw",type="missing",bpd="numeric"),
    function(x,type,bpd) { coherence(x,"BPD",bpd) }
    )

if (!isGeneric("autocorr",where=1))
    setGeneric("autocorr",function(x,lags) standardGeneric("autocorr"))

setMethod("autocorr",signature(x="fftw",lags="missing"),
    function(x,lags) {
        #
        # Computation of auto-correlation function from fft
        #

        nr <- dim(x)[1]
        nc <- dim(x)[2]
        type <- x@type
        dt = deltat(x)

        # number of points in original time series
        N = if(type=="halfcomplexodd") (nr * 2) - 1 
        else if (type == "halfcomplexeven") nr * 2 - 2
        else if (type == "forwardcomplex") nr
        else if (type == "inversecomplex") nr

        # browser()
        # Fourier transform of correlation function. 
        x = Re(x * Conj(x)) / N^2
        # Integrate power spectra to get total power
        vars <- apply(x[-1,]@data,2,sum)

        # cat("pspectra, spectra type=",x@type,"\n")

        # double power if half-sided
        if (type == "halfcomplexodd") vars = vars * 2
        else if (type == "halfcomplexeven") vars = vars * 2 - x@data[nr,]

        # Fourier transform of correlation function. 
        x <- fftw(x,inverse=TRUE) / matrix(rep(vars,N),ncol=nc,byrow=TRUE)

        # Put in conventional order
        n1 <- nr - 1
        n2 = if(type=="halfcomplexodd") nr + 1 
        else if (type == "halfcomplexeven") nr

        nts(rbind(x@data[n2:N,,drop=FALSE], x@data[1:nr,,drop=FALSE]), (-n1:n1) * dt[1],
            names=dimnames(x)[[2]], units = rep("", nc))

    }
    )
setMethod("autocorr",signature(x="nts",lags="numeric"),
    function(x,lags)
    {
        #
        # Computation of auto-correlation function using fft
        #

        nr <- dim(x)[1]
        nc <- dim(x)[2]
        dt <- deltat(x)[1]

        # If it hasn't been detrended, remove mean
        meanx = apply(x@data,2,mean,na.rm=TRUE)
        x <- x - matrix(rep(meanx,nr),ncol=nc,byrow=TRUE)

        if (length(lags) == 1) lags <- c(-lags,lags)
        maxlag <- max(lags)

        nlags <- round(maxlag / dt) + 1

        newt <- utime(seq(from=as.numeric(tspar(x)[nr])+dt,length=nlags,by=dt))

        x <- Rbind(x,nts(matrix(0,ncol=nc,nrow=nlags),newt))

        x <- autocorr(fftw(x))
        mx <- as.numeric(tspar(x)) >= lags[1] & as.numeric(tspar(x)) <= lags[2]
        x[mx,]
    }
)
setMethod("autocorr",signature(x="nts",lags="missing"),
    function(x,lags)
    {
        nr <- dim(x)[1]
        lags <- .1 * (tspar(x)[nr] - tspar(x)[1])
        autocorr(x,lags)
    }
)

if (!isGeneric("crosscorr",where=1))
    setGeneric("crosscorr",function(x,lags) standardGeneric("crosscorr"))
setMethod("crosscorr",signature(x="fftw",lags="missing"),
    function(x,lags)
    {
        #
        # Computation of cross-correlation function from fft
        #

        nr <- dim(x)[1]
        nc <- dim(x)[2]

        if (nc < 2) stop(paste("Can't do crosscor on less than two columns. nc=",nc))
        dns <- dimnames(x)[[2]]
        type <- x@type
        dt = deltat(x)

        # number of points in original time series
        N = if(type=="halfcomplexodd") (nr * 2) - 1 
        else if (type == "halfcomplexeven") nr * 2 - 2
        else if (type == "forwardcomplex") nr
        else if (type == "inversecomplex") nr

        # browser()
        psd = Re(x@data * Conj(x@data)) / N^2
        # Integrate power spectra to get total power
        # cat("pspectra, spectra type=",x@type,"\n")
        vars <- apply(psd[-1,],2,sum)
        if (type == "halfcomplexodd") vars = vars * 2
        else if (type == "halfcomplexeven") vars = vars * 2 - psd[nr,]
        devs <- sqrt(vars)

        combinations <- matrix(F,ncol=nc,nrow=nc)
        combinations[col(combinations) < row(combinations)] <- T

        # first index of cross combinations
        i1 <- matrix(rep(1:nc,nc),ncol=nc,byrow=TRUE)[combinations]
        # 2nd index
        i2 <- matrix(rep(1:nc,nc),ncol=nc)[combinations]

        # Fourier transform of correlation function. 
        x = x[,i1] * Conj(x[,i2]) / N^2

        x <- fftw(x,inverse=TRUE) /
        matrix(rep(devs[i1],N),nrow=N,byrow=TRUE) /
        matrix(rep(devs[i2],N),nrow=N,byrow=TRUE)

        # Put in conventional order
        n1 = nr - 1
        n2 = if(type=="halfcomplexodd") nr + 1 
        else if (type == "halfcomplexeven") nr

        nts(rbind(x@data[n2:N,,drop=FALSE], x@data[1:nr,,drop=FALSE]), (-n1:n1) * dt[1],
            names=paste(dns[i1],dns[i2],sep=":"),
            units = rep("", length(i1)))

    }
)
setMethod("crosscorr",signature(x="nts",lags="numeric"),
    function(x,lags)
    {

        #
        # Computation of cross-correlation function using fft
        #

        nr <- dim(x)[1]
        nc <- dim(x)[2]
        dt <- deltat(x)[1]

        # detrend
        x <- detrend(x)

        if (length(lags) == 1) lags <- c(-lags,lags)
        maxlag <- max(lags)

        nlags <- round(maxlag / dt) + 1

        newt <- utime(seq(from=as.numeric(tspar(x)[nr])+dt,length=nlags,by=dt))

        x <- Rbind(x,nts(matrix(0,ncol=nc,nrow=nlags),newt))

        x <- crosscorr(fftw(x))
        mx <- as.numeric(tspar(x)) >= lags[1] & as.numeric(tspar(x)) <= lags[2]
        x[mx,]
    }
)
setMethod("crosscorr",signature(x="nts",lags="missing"),
    function(x,lags)
    {
        nr <- dim(x)[1]
        lags <- .1 * as.numeric((tspar(x)[nr] - tspar(x)[1]))
        crosscorr(x,lags)
    }
)

specsmooth.spectra = function(x,type,...)
{
    class.x = class(x)

    nr <- dim(x)[1]
    nc <- dim(x)[2]
    freq <- x@frequencies
    dns = dimnames(x)[[2]]

    # Default smoothing is BPD, bins per decade
    if (type == "BPD") {

        args = list(...)
        bpd = args$bpd
        if (is.null(bpd)) bpd = 10

        posfreq <- freq > 0.
        if (any(!posfreq)) {
            freq <- freq[posfreq]
            x <- x[posfreq,]
            nr <- dim(x)[1]
        }

        # lower bound of frequency bins in log units
        # lowest: log0 = log10(f[0]) = floor(log10(freq[1])*bpd)/ bpd
        # highest: logm = log10(f[m]) = floor(log10(freq[nr]) * bpd) / bpd
        # m = (logm - log0) * bpd
        # 
        # lower bounds of frequency bins in non-log units:
        # 10 ^ seq(from=log0,to=logm,by=1/bpd)
        # f[0] = 10^log0
        # f[i] = f[i-1] * d ^ i   for i = 1:m
        #  d = 10^(1/bpd)
        #

        log10freq = log10(freq)
        lftrunc = floor(log10freq * bpd) / bpd

        lbounds = seq(from=lftrunc[1],to=lftrunc[nr],by=1/bpd)
        lbindices = match.delta(lbounds,lftrunc,1/bpd/2,cond="first")

        lbounds = lbounds[lbindices != 0]
        lbinz = lbindices[lbindices != 0]
        ubinz = c(lbinz[-1]-1,nr)

        # If the last frequency bin has less than .05 the number of
        # points that are in the next-to-last, then discard them.
        nf = length(lbinz)
        if (nf > 1) {
            nfreqlast = ubinz[nf] - lbinz[nf] + 1
            nfreqnext = ubinz[nf-1] - lbinz[nf-1] + 1
            # cat("nf=",nf,"nfreqlast=",nfreqlast,"nfreqnext=",nfreqnext,"\n")
            if (nfreqlast < nfreqnext * .05) {
                lbinz = lbinz[-nf]
                ubinz = ubinz[-nf]
                lbounds = lbounds[-nf]
                # cat("removed last freq\n")
            }
        }

        x@data = matrix(sapply(1:length(lbinz),
            function(i,d,lb,ub) {
                apply(d[lb[i]:ub[i],,drop=FALSE],2,mean,na.rm=TRUE)
            }, x@data,lbinz,ubinz),byrow=TRUE,ncol=nc,dimnames=list(NULL,dns))

        x@frequencies = 10 ^ (lbounds + 1 / bpd / 2)
        deltaf(x) <- numeric(0)
    }
    else stop("Current version only supports BPD smoothing")
    class(x) = class.x
    x
}

if (!isGeneric("specsmooth",where=1))
    setGeneric("specsmooth",function(x,type,...) standardGeneric("specsmooth"))

setMethod("specsmooth",signature(x="spectra",type="character"),
    specsmooth.spectra)


setMethod("specsmooth",signature(x="spectra",type="missing"),
    function(x,type,...)
    {
        type = "BPD"
        specsmooth(x,"BPD",...)
    }
)

setMethod("specsmooth",signature(x="fftw",type="character"),
    specsmooth.spectra)

setMethod("specsmooth",signature(x="fftw",type="missing"),
    function(x,type,...)
    {
        specsmooth(x,"BPD",...)
    }
)

setMethod("specsmooth",signature(x="pspectra",type="character"),
    specsmooth.spectra)

setMethod("specsmooth",signature(x="pspectra",type="missing"),
    function(x,type,...)
    {
        specsmooth(x,"BPD",...)
    }
)

setMethod("specsmooth",signature(x="xspectra",type="character"),
    specsmooth.spectra)

setMethod("specsmooth",signature(x="xspectra",type="missing"),
    function(x,type,...)
    {
        specsmooth(x,"BPD",...)
    }
)

setMethod("specsmooth",signature(x="cospectra",type="character"),
    specsmooth.spectra)

setMethod("specsmooth",signature(x="cospectra",type="missing"),
    function(x,type,...)
    {
        specsmooth(x,"BPD",...)
    }
)

setMethod("specsmooth",signature(x="quadspectra",type="character"),
    specsmooth.spectra)

setMethod("specsmooth",signature(x="quadspectra",type="missing"),
    function(x,type,...)
    {
        specsmooth(x,"BPD",...)
    }
)

if (!isGeneric("Cbind"))
    setGeneric("Cbind",function(x1,x2,...) standardGeneric("Cbind"))

Cbind.spectra = function(x1,x2,...)
{
    args <- list(...)

    if (length(args) > 0) {
        x1 <- cbind(x1,x2)
        for (iarg in 1:length(args)) x1 <- cbind(x1,args[[iarg]])
        return(x1)
    }
    if (!identical(x1@type,x2@type))
        warning(paste("Cbinding spectra of different types.",
                "x1@type=",x1@type,", x2@type=",x2@type))

    f1 <- x1@frequencies
    nc1 <- ncol(x1)
    df1 = deltaf(x1)

    f2 <- x2@frequencies
    nc2 <- ncol(x2)
    df2 = deltaf(x2)

    df = NA
    if (length(df1) > 0 || length(df2) > 0) df = min(df1,df2,na.rm=TRUE) / 2
    if (is.na(df)) df = min(f1[f1>0],f2[f2>0]) / 2

    # for each frequency in f2, its position in f1
    from2 <- match.delta(f2,f1,df)
    f2only = f2[from2==0]
    f1only = f1[match.delta(f1,f2,df) == 0]

    # the union of x & y frequencies
    f12 = sort(c(f1[from2],f1only,f2only))

    xx <- matrix(NA,ncol=nc1+nc2,nrow=length(f12),
        dimnames=list(NULL,c(dimnames(x1)[[2]],dimnames(x2)[[2]])))

    from1 <- match.delta(f1,f12,df)
    xx[from1,1:nc1] <- x1@data[from1!=0,,drop=FALSE]

    from2 <- match.delta(f2,f12,df)
    xx[from2,(nc1+1):(nc1+nc2)] <- x2@data[from2!=0,,drop=FALSE]

    xx = new("spectra",data=xx,type=x1@type,
        frequencies=f12,units=c(x1@units,x2@units),
        funits=x1@funits,stations=c(x1@stations,x2@stations),
        start=x1@start,end=x1@end,deltat=x1@deltat,
        deltaf=x1@deltaf,wss=x1@wss)
    class(xx) <- class(x1)
    xx
}

setMethod("Cbind",signature(x1="spectra",x2="spectra"),Cbind.spectra)
setMethod("Cbind",signature(x1="fftw",x2="fftw"),Cbind.spectra)
setMethod("Cbind",signature(x1="pspectra",x2="pspectra"),Cbind.spectra)

# Haven't implemented handling the combinations element
# setMethod("Cbind",signature(x1="xspectra",x2="xspectra"),Cbind.spectra)
# setMethod("Cbind",signature(x1="cospectra",x2="cospectra"),Cbind.spectra)
# setMethod("Cbind",signature(x1="quadspectra",x2="quadspectra"),Cbind.spectra)
