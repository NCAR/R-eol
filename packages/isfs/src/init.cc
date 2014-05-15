// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 * 2013,2014, Copyright University Corporation for Atmospheric Research
 * 
 * This file is part of the "isfs" package for the R software environment.
 * The license and distribution terms for this file may be found in the
 * file LICENSE in this package.
 */
/*
 *  Description:
 *	execute static initialization functions for some classes.
 *	This is called by R when this library is loaded.
 */

#ifdef HAVE_PREP

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>   // DllInfo

#include "R_prep.h"

using namespace isfs;

extern "C" {

    void R_init_isfs(DllInfo* dllinfo)
    {
#ifdef DEBUG
        Rprintf("static R_init_isfs\n");
#endif

        R_prep::variablesSlotName = Rf_install("variables");
        R_prep::rateSlotName = Rf_install("rate");
        R_prep::cppSlotName = Rf_install("cppPtr");
    }

    void R_unload_isfs(DllInfo* dllinfo)
    {
        Rprintf("static R_unload_isfs\n");
    }
}

#endif  // HAVE_PREP
