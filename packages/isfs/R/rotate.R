# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

#  Description:
#    Rotate sonic wind data in a time series by a fixed angle
#    angle is in radians

setGeneric("rotate",function(x,...) standardGeneric("rotate"))

setMethod("rotate",signature(x="dat"),
    function(x,...)
    {
        args <- list(...)

        if (is.null(args$angle)) stop("angle not specified")
        else angle <- args$angle

        if (is.null(args$trivar)) trivar <- F
        else trivar <- args$trivar

        #
        # rotate wind means and higher moments clockwise by angle "angle",
        # in radians.
        #
        # Or, to view it in the way commonly used:
        # result vector wind components and higher moments will
        # be relative to a coordinate system that is horizontally
        # rotated "angle" counter-clockwise from the input wind
        # coordinates.
        #
        # 
        # If angle is the cartesian wind direction,
        #	DIRcart=atan2(v,u),
        # (not the meteorological wind direction, DIRmet=atan(-u,-v)=270-DIRcart)
        # then the result will be winds in streamline coordinates.
        #
        # If angle is the geographic compass direction of the sonic +V axis
        # the result will be winds in meteorological coordinates.
        #
        # dat version 9/98 spo
        #
        # For the purpose of horizontal rotation, w is considered a scalar

        dns <- dimnames(x)[[2]]
        dnsw1 <- words(dns,1,1,sep=".")

        # figure out what scalar covariances are in x
        covars <- nwords(dnsw1,sep="'") > 1
        scalars <- NULL
        if (any(covars)) {
            covars <- dnsw1[covars]
            uvars <- words(covars,1,1,sep="'") == "u"
            scalars <- words(covars[uvars],2,2,sep="'")
            scalars <- scalars[is.na(match(scalars,c("u","v")))]
        }

        nsc <- length(scalars)

        # many things (scalars) don't require rotating
        xout <- x
        # create rotation values either from constant input angle or from
        # mean wind direction
        if (is.null(angle)) angle <- atan2(x[,dnsw1=="v"],x[,dnsw1=="u"])

        st <- sin(angle)
        ct <- cos(angle)

        # rotate mean wind 
        xout[,dnsw1=="u"] <- x[,dnsw1=="u"]*ct + x[,dnsw1=="v"]*st
        xout[,dnsw1=="v"] <- -x[,dnsw1=="u"]*st + x[,dnsw1=="v"]*ct
        # rotate scalar fluxes
        for (sc in scalars)
        {
            uxc <- dnsw1==paste("u'",sc,"'",sep="")
            vxc <- dnsw1==paste("v'",sc,"'",sep="")
            ux <- x[,uxc]
            vx <- x[,vxc]
            xout[,uxc] <- ux*ct + vx*st
            xout[,vxc] <- -ux*st + vx*ct
        }
        # rotate wind covariance matrix
        uu <- x[,dnsw1=="u'u'"]
        uv <- x[,dnsw1=="u'v'"]
        vv <- x[,dnsw1=="v'v'"]
        xout[,dnsw1=="u'u'"] <- ct*(uu*ct + uv*st) + st*(uv*ct + vv*st)
        xout[,dnsw1=="u'v'"] <- -st*(uu*ct + uv*st) + ct*(uv*ct + vv*st)
        xout[,dnsw1=="v'v'"] <- -st*(-uu*st + uv*ct) + ct*(-uv*st + vv*ct)

        # rotate scalar trivariances (note two types)
        if (trivar) for (i in 1:nsc)
        {
            for (j in i:nsc)
            {
                uxyc <- dnsw1==paste("u'",scalars[i],"'",scalars[j],"'",sep="")
                vxyc <- dnsw1==paste("v'",scalars[i],"'",scalars[j],"'",sep="")
                uxy <- x[,uxyc]
                vxy <- x[,vxyc]
                xout[,uxyc] <- uxy*ct + vxy*st
                xout[,vxyc] <- -uxy*st + vxy*ct
            }
            uuxc <- dnsw1==paste("u'u'",scalars[i],"'",sep="")
            uvxc <- dnsw1==paste("u'v'",scalars[i],"'",sep="")
            vvxc <- dnsw1==paste("v'v'",scalars[i],"'",sep="")
            uux <- x[,uuxc]
            uvx <- x[,uvxc]
            vvx <- x[,vvxc]
            xout[,uuxc] <- ct*(uux*ct + uvx*st) + st*(uvx*ct + vvx*st)
            xout[,uvxc] <- -st*(uux*ct + uvx*st) + ct*(uvx*ct + vvx*st)
            xout[,vvxc] <- -st*(-uux*st + uvx*ct) + ct*(-uvx*st + vvx*ct)
        }
        # rotate wind trivariances
        if (trivar)
        {
            uuu <- x[,dnsw1=="u'u'u'"]
            uuv <- x[,dnsw1=="u'u'v'"]
            uvv <- x[,dnsw1=="u'v'v'"]
            vvv <- x[,dnsw1=="v'v'v'"]
            xout[,dnsw1=="u'u'u'"] <- ct*ct*(uuu*ct+uuv*st) + ct*st*(uuv*ct+uvv*st) +
            st*ct*(uuv*ct+uvv*st) + st*st*(uvv*ct+vvv*st)
            xout[,dnsw1=="u'u'v'"] <- -ct*st*(uuu*ct+uuv*st) + ct*ct*(uuv*ct+uvv*st) -
            st*st*(uuv*ct+uvv*st) + ct*st*(uvv*ct+vvv*st)
            xout[,dnsw1=="u'v'v'"] <- -ct*st*(-uuu*st+uuv*ct) + 
            ct*ct*(-uuv*st+uvv*ct) -
            st*st*(-uuv*st+uvv*ct) + ct*st*(-uvv*st+vvv*ct)
            xout[,dnsw1=="v'v'v'"] <- st*st*(-uuu*st+uuv*ct) - 
            st*ct*(-uuv*st+uvv*ct) -
            ct*st*(-uuv*st+uvv*ct) + ct*ct*(-uvv*st+vvv*ct)
        }
        # return rotated array
        xout
    }
    )

if (!isGeneric("tsrotate",where=1))
    setGeneric("tsrotate",function(x,angle,trivar) standardGeneric("tsrotate"))

setMethod("tsrotate",signature(x="dat",angle="dat",trivar="logical"),
    function(x,angle,trivar) {

        xstns <- stations(x)
        stns <- unique(xstns)
        for (stn in stns) {

            as <- select(angle,stns=stn)
            xs <- select(x,stns=stn)

            # Duplicate last row of as so that it covers span of xs
            t2 <- positions(xs)[nrow(xs)]
            if (positions(as)[nrow(as)] < t2) {
                as2 <- as[nrow(as),]
                positions(as2) <- t2 + 0.001
                as <- Rbind(as,as2)
            }
            for (r in (1:(nrow(as) - 1))) {
                t1 <- tspar(as)[r]
                # Avoid rotating same sample twice, so subtract a bit from end time
                t2 <- utime(as.numeric(tspar(as)[r+1]) - 0.001)

                if (tspar(xs)[nrow(xs)] >= t1 && tspar(xs)[1] <= t2) {
                    xas <- as.vector(as[r,])	# in radians
                    xxs <- xs[utime(c(t1,t2)),]
                    xxs <- rotate(xxs,angle=xas,trivar=trivar)
                    xs[utime(c(t1,t2)),] <- xxs
                }
            }
            xsd <- dimnames(xs)[[2]]
            xd <- dimnames(x)[[2]]
            xm <- match(xd,xsd)
            x[,(!is.na(xm) & xstns == stn)] <- xs[,xm[xstns==stn]]
        }
        x
    }
    )

setMethod("tsrotate",signature(x="dat",angle="dat",trivar="missing"),
    function(x,angle,trivar)
    {
        tsrotate(x,angle,FALSE)
    }
    )
