# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolts" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

test.fftw <- function()
{
    if (.Platform$OS.type == "windows") 
        do.test.fftw(TRUE)
    else {
        do.test.fftw(FALSE)
        do.test.fftw(TRUE)
        check.fftw()
    }
}
sawfunc <- function(n,m,f,k)
{
    # a trivial real function for spectra testing.
    # combination of a sawtooth and trig function f.
    # sawtooth of length n, m periods, amplitude -1 to 1,
    # plus a trig function f, k*m periods
    y <- c(rep(c(0,1,0,-1),m),0)
    x <- seq(from=1,to=n,length=length(y))
    approx(x,y,1:n)$y + f(seq(from=0,to=2*pi*k*m,length=n))
}
do.test.fftw <- function(use.mvfft)
{

    eps <- 1.e-12
    eps2 <- 5.e-6
    nr <- 100
    m <- 10

    cat("\n---- testing fftw(x,inverse=FALSE) on sin((0:n-1) *",m,"/ n * 2 * pi) for n=",
        nr,"\n")

    x <- matrix(sin((0:(nr-1)) * (m / nr) * 2 * pi),nrow=nr,
        dimnames=list(NULL,paste("col",1,sep="")))
    fx <- fftw(x,inverse=F,use.mvfft=use.mvfft)
    mid <- trunc(nr / 2) + 1

    checkTrue(is.complex(fx@data) && length(dim(fx)) == 2 && all(dim(fx) == c(mid,1)))
    # cat(paste("dimnames(fx)=",paste(dimnames(fx)[[2]],collapse=","),"\n"))
    checkTrue(identical(dimnames(x),dimnames(fx)))

    m1 <- m + 1

    # browser()
    checkTrue(all(abs(Re(fx)) < eps) && all(abs(Im(fx[m1,]) + nr / 2) < eps) &&
        all(Mod(fx[-m1,]) < eps))

    cat("\n---- testing fftw(fftw(x,inverse=FALSE),inverse=TRUE)/N with x real, n=",nr,"\n")
    x <- matrix(c(sawfunc(nr,m,sin,3), sawfunc(nr,m,cos,3)),ncol=2)

    xx <- fftw(fftw(x,inverse=FALSE,use.mvfft=use.mvfft),inverse=TRUE,use.mvfft=use.mvfft) / nr
    checkTrue(max(Mod(xx-x)) < eps)
    checkTrue(max(abs(Im(xx))) < eps)

    nr <- 101
    cat("\n---- testing fftw(fftw(x,inverse=FALSE),inverse=TRUE)/N with x real, n=",nr,"\n")
    x <- matrix(c(sawfunc(nr,m,sin,3), sawfunc(nr,m,cos,3)),ncol=2)

    xx <- fftw(fftw(x,inverse=FALSE,use.mvfft=use.mvfft),inverse=TRUE,use.mvfft=use.mvfft) / nr
    checkTrue(max(abs(Im(xx))) < eps)
    checkTrue(max(abs(Re(xx)-x)) < eps)

    # Generate real matrix of random numbers
    nr <- 99
    nc <- 3
    cat("\n---- testing fftw(fftw(x,inverse=FALSE),inverse=TRUE)/N with x random and real, n=",
        nr,", nc=",nc,"\n")
    x <- matrix(rnorm(nr*nc),nrow=nr,dimnames=list(NULL,paste("quack",1:nc,sep="")))

    xx <- fftw(fftw(x,inverse=FALSE,use.mvfft=use.mvfft),inverse=TRUE,use.mvfft=use.mvfft) / nr

    # browser()
    checkTrue(max(abs(Im(xx))) < eps)
    checkTrue(max(abs(Re(xx)-x)) < eps)

    cat("\n---- testing fftw(fftw(x,inverse=FALSE),inverse=TRUE)/N with x complex\n")
    x <- matrix(complex(real=x,imag=-x),ncol=nc)

    xx <- fftw(fftw(x,inverse=F,use.mvfft=use.mvfft),inverse=T,use.mvfft=use.mvfft) / nr

    checkTrue(max(Mod(xx - x)) < eps)

    nr <- 101
    nc <- 2
    cat("\n---- testing fftw(fftw(nts(),inverse=FALSE),inverse=TRUE), with nts random, n=",
        nr,"\n")
    options(time.zone="GMT")

    xunits <- c("foo", "bar")

    x <- nts(matrix(rnorm(nr*nc),nrow=nr),
        seq(from=utime("2002 feb 01 00:00"),length=nr,by=1),
        names=c("col1","col2"),units=xunits, stations=c(0,3))

    xx <- fftw(fftw(x,use.mvfft=use.mvfft),inverse=T,use.mvfft=use.mvfft) / nr
    checkTrue(max(Mod(xx - x)) < eps)
    checkTrue(identical(units(xx), xunits))

    nr <- 100
    m <- 10
    cat("\n---- testing pspectra(fftw(nts(sin((0:n-1) * 10 /",m,"* 2 * pi) for n=",
        nr,"\n")
    nc <- 1
    dt <- .01
    m1 <- m + 1
    x <- nts(matrix(sin((0:(nr-1)) * (m / nr) * 2 * pi),nrow=nr,
            dimnames=list(NULL,paste("quack",1:nc,sep=""))),
        seq(from=utime("2002 feb 01 00:00"),length=nr,by=dt),
        stations=rep(0,nc))

    # browser()
    xx <- pspectra(fftw(x))
    checkTrue(max(abs(xx[-m1,])) < eps && all(abs(xx[m1,] - (nr/2*dt)) < eps))

    xxpower <- sum(xx) / nr / dt
    xpower <- as.numeric(var(x)) * (nr-1) / nr
    checkTrue(abs(xxpower - xpower) < eps2)

    nr <- 901
    nc <- 2
    cat("\n---- testing pspectra(fftw(nts())), with nts random, n=",nr,"\n")
    dt <- .01
    x <- nts(matrix(rnorm(nr*nc),nrow=nr,
            dimnames=list(NULL,paste("quack",1:nc,sep=""))),
        seq(from=utime("2002 feb 01 00:00"),length=nr,by=dt),
        names=c("col1","col2"),stations=c(0,3))

    xx <- pspectra(fftw(x))

    xxpower <- apply(xx@data[-1,],2,sum) / nr / dt
    xpower <- var(x) * (nr-1) / nr
    xpower <- xpower[row(xpower)==col(xpower)]    # diagonal
    checkTrue(all(abs(xxpower - xpower) < eps2))
    xxmeansq <- xx[1,] / nr / dt
    xmeansq <- apply(x@data,2,mean)^2
    checkTrue(all(abs(xxmeansq - xmeansq) < eps2))

    nr <- 100
    m <- 5
    cat("\n---- testing sign of quadspectra\n")
    #
    # Test suggested by Tom from Lumley and Panofsky.
    #
    u <- matrix(cos((0:(nr-1)) * (m / nr) * 2 * pi),nrow=nr)
    theta <- pi / 3.0
    w <- matrix(cos((0:(nr-1)) * (m / nr) * 2 * pi - theta),nrow=nr)
    fu <- fftw(u,inverse=F,use.mvfft=use.mvfft)
    fw <- fftw(w,inverse=F,use.mvfft=use.mvfft)

    xuw <- xspectra(Cbind(fu,fw))
    cuw1 <- cospectra(xuw)
    quw1 <- quadspectra(xuw)

    cuw2 <- cos(theta) * pspectra(fu)
    quw2 <- sin(theta) * pspectra(fu)

    checkTrue(all(abs(cuw1 - cuw2) < eps))

    checkTrue(all(abs(quw1 - quw2) < eps))

    return()
}
check.fftw <- function()
{
    eps <- 1.e-12
    # Generate real matrix of random numbers
    nr <- 99
    nc <- 2
    cat("\n---- testing fftw(x,inverse=FALSE),n=",nr,"using fftw3 and mvfft\n")
    x <- matrix(rnorm(nr*nc),nrow=nr,dimnames=list(NULL,paste("quack",1:nc,sep="")))

    x1 <- fftw(x,inverse=FALSE,use.mvfft=TRUE)
    x2 <- fftw(x,inverse=FALSE,use.mvfft=FALSE)

    checkTrue(max(Mod(x1-x2)) < eps)

    x1 <- fftw(x1,inverse=TRUE,use.mvfft=TRUE)
    x2 <- fftw(x2,inverse=TRUE,use.mvfft=FALSE)
    checkTrue(max(Mod(x1-x2)) < eps)

    nr <- 100
    nc <- 2
    cat("\n---- testing fftw(x,inverse=FALSE),n=",nr,"using fftw3 and mvfft\n")
    x <- matrix(rnorm(nr*nc),nrow=nr,dimnames=list(NULL,paste("quack",1:nc,sep="")))

    x1 <- fftw(x,inverse=FALSE,use.mvfft=TRUE)
    x2 <- fftw(x,inverse=FALSE,use.mvfft=FALSE)

    checkTrue(max(Mod(x1-x2)) < eps)

    x1 <- fftw(x1,inverse=TRUE,use.mvfft=TRUE)
    x2 <- fftw(x2,inverse=TRUE,use.mvfft=FALSE)
    checkTrue(max(Mod(x1-x2)) < eps)

    return()
}


