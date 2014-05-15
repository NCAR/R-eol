# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "isfs" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

vcross <- function(x,y)
    c(x[2] * y[3] - x[3] * y[2],x[3] * y[1] - x[1] * y[3],x[1] * y[2] - x[2] * y[1])

vmag <- function(x) sqrt(x[1]*x[1] + x[2]*x[2] + x[3]*x[3])

sonic_tilt_matrix <- function(lean,leanaz,sonic.w.is.up=F)
{

    # return sonic tilt matrix, given lean and lean azimuth
    # For info, see $ASTER/app/calib/sonic_tilt_matrix.c

    lean <- lean * pi / 180
    leanaz <- leanaz * pi / 180

    sinlean <- sin(lean)
    coslean <- cos(lean)
    sinaz <- sin(leanaz)
    cosaz <- cos(leanaz)

    #
    # Uf,Vf,Wf flow coordinate axes, in sonic coords
    # Us,Vs,Ws sonic coordinate axes
    #
    #
    # This is Wf, the flow W axis in the sonic UVW system
    #
    Wf <- c(sinlean * cosaz,sinlean * sinaz, coslean)

    if (sonic.w.is.up) {
        #
        # Vs cross Wf
        # Uf <- vcross(c(0,1,0),Wf)
        Uf <- c(coslean,0,-sinlean * cosaz)

        # Uf is normal to Vs, therefore it is in Us,Ws plane
        # and is normal to Wf.  This uses sonic normal as
        # best guess of "UP" direction for orienting things.
    }
    else {
        # Wf cross Us is normal to plane containing Wf & Us.
        # WfXUs <- vcross(Wf,c(1,0,0))
        WfXUs <- c(0, coslean, -sinlean *sinaz)

        # Uf is in plane of Us and Wf, and is normal to Wf
        # This uses flow normal as best guess of "UP" direction.
        #
        Uf <- vcross(WfXUs,Wf)
    }

    Uf <- Uf / vmag(Uf)

    Vf <- vcross(Wf,Uf)

    # Rows of matrix are flow axes in sonic coordinates.
    # Therefore, to rotate a vector in sonic coordinates to flow
    # coordinates, take dot product with rows of matrix, which is
    # done with the matrix product:  sonic.tilt.matrix %*% sonic.vector

    matrix(c(Uf,Vf,Wf),ncol=3,byrow=T) 
}
