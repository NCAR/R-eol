// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 * 2013,2014, Copyright University Corporation for Atmospheric Research
 * 
 * This file is part of the "eolsonde" package for the R software environment.
 * The license and distribution terms for this file may be found in the
 * file LICENSE in this package.
 */

#ifdef HAVE_GEOGRAPHICLIB

#include <GeographicLib/Geoid.hpp>

#include <R.h>
#include <Rinternals.h>

extern "C" {
    SEXP geoid(SEXP latp, SEXP lonp, SEXP geoidName, SEXP cubicp);
}


SEXP geoid(SEXP latp, SEXP lonp, SEXP geoidNamep, SEXP cubicp)
{

    const char* geoidName = "egm96-5";
    if (Rf_xlength(geoidNamep) > 0) {
        const char* cptr = CHAR(STRING_ELT(geoidNamep,0));
        if (strlen(cptr) > 0) geoidName = cptr;
    }

    bool cubic = true;
    if (Rf_xlength(cubicp) > 0) {
        cubic = INTEGER(cubicp)[0];
    }

    try {
        GeographicLib::Geoid g(geoidName,"",cubic);

        size_t nvals = (size_t)Rf_xlength(latp);

        const double* lats = REAL(latp);
        const double* lons = REAL(lonp);

        SEXP heights = PROTECT(Rf_allocVector(REALSXP,nvals));

        for (size_t i = 0; i < nvals; i++) {
            REAL(heights)[i] = g(lats[i],lons[i]);
        }
        UNPROTECT(1);
        return heights;
    }
    catch (const GeographicLib::GeographicErr& e) {
        Rf_error(e.what());
    }
}

#endif // HAVE_GEOGRAPHICLIB
