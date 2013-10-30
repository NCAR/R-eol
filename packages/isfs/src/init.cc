// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.6 $
 *  $Date: 2004/02/11 17:15:23 $
 * 
 *  Description:
 *	execute static initialization functions for some classes.
 *	This is called by R when this library is loaded.
 */

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

