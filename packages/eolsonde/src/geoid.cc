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
#include <GeographicLib/Geodesic.hpp>

#include <Rcpp.h>

// using namespace Rcpp;

extern "C" {
    SEXP geoid(SEXP latp, SEXP lonp, SEXP geoidName, SEXP cubicp);
    SEXP geodesicHeadings(SEXP lat_, SEXP lon_);
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
    size_t nvals = (size_t)Rf_xlength(latp);

    Rcpp::NumericVector heights(nvals);

    try {
        GeographicLib::Geoid geoid(geoidName,"",cubic);

        const double* lats = REAL(latp);
        const double* lons = REAL(lonp);

        for (size_t i = 0; i < nvals; i++) {
            heights[i] = geoid(lats[i],lons[i]);
        }
    }
    catch (const GeographicLib::GeographicErr& e) {
        forward_exception_to_r(e);
    }

    return Rcpp::wrap(heights);
}

SEXP geodesicHeadings(SEXP lat_, SEXP lon_)
{
    /* Comute heading (azimuth w.r.t north) and distances between
     * successive positions, as specified by lat_ and lon_.
     * Uses GeographicLib::Geodisic::Inverse() function.
     * See http://geographiclib.sourceforge.net/
     */
    
    const double* latp = REAL(lat_);
    const double* lonp = REAL(lon_);
    size_t nvals = (size_t)Rf_xlength(lat_);

    Rcpp::NumericMatrix result(nvals-1,3);

    Rcpp::CharacterVector colnames(3);
    colnames[0] = "s12";
    colnames[1] = "az1";
    colnames[2] = "az2";
    Rcpp::List dimnames(2);
    dimnames[1] = colnames;
    result.attr("dimnames") = dimnames;
    
    try {
        // const GeographicLib::Geodesic& geod = GeographicLib::Geodesic::WGS84();
        GeographicLib::Geodesic geod(GeographicLib::Constants::WGS84_a(),
                GeographicLib::Constants::WGS84_f());

        for (size_t i = 0; i < nvals - 1; i++,latp++,lonp++) {
            double az1, az2, s12;
            geod.Inverse(*latp, *lonp, *(latp+1), *(lonp+1), s12, az1, az2);
            result(i,0) = s12;
            result(i,1) = az1;
            result(i,2) = az2;
        }
    }
    catch (const GeographicLib::GeographicErr& e) {
        forward_exception_to_r(e);
    }
    return Rcpp::wrap(result);
}

#endif // HAVE_GEOGRAPHICLIB
