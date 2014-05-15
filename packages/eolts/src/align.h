// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 * 2013,2014, Copyright University Corporation for Atmospheric Research
 * 
 * This file is part of the "eolts" package for the R software environment.
 * The license and distribution terms for this file may be found in the
 * file LICENSE in this package.
 */
/*************************************************************************
 *
 * Copyright © 1998-2012 TIBCO Software Inc. All rights reserved. 
 * COPYRIGHT HOLDER: TIBCO Software Inc.
 * ORGANIZATION: TIBCO Software Inc
 *
 *************************************************************************/

#ifndef UTIMELIB_ALIGN_H
#define UTIMELIB_ALIGN_H

#include <Rinternals.h>

extern "C" {
    SEXP utime_align( SEXP time_obj, SEXP align_pos, SEXP how_obj, SEXP match_tol );
}

#endif  // TIMELIB_ALIGN_H
