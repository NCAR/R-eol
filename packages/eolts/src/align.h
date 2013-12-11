/*************************************************************************
 *
 * © 1998-2012 TIBCO Software Inc. All rights reserved. 
 * Confidential & Proprietary 
 *
*************************************************************************/

#ifndef UTIMELIB_ALIGN_H
#define UTIMELIB_ALIGN_H

#include <Rinternals.h>

extern "C" {
    SEXP utime_align( SEXP time_obj, SEXP align_pos, SEXP how_obj, SEXP match_tol );
}

#endif  // TIMELIB_ALIGN_H
