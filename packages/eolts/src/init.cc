// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 * 2013,2014, Copyright University Corporation for Atmospheric Research
 * 
 * This file is part of the "eolts" package for the R software environment.
 * The license and distribution terms for this file may be found in the
 * file LICENSE in this package.
 */
/*
 *  Description:
 *	execute static initialization functions for some classes.
 *	This is called by R when this library is loaded.
 */

#include "R_netcdf.h"
#include "R_NetcdfVariable.h"
#include "R_utime.h"
#include "R_nts.h"

#include <R.h>  // Rf_install
#include <R_ext/Rdynload.h>   // DllInfo

using namespace eolts;

extern "C" {

    void R_init_eolts(DllInfo* dllinfo)
    {
#ifdef DEBUG
        Rprintf("static R_init_eolts\n");
#endif

        R_netcdf::fileSlotName = Rf_install("file");
        R_netcdf::dirSlotName = Rf_install("dir");
        R_netcdf::timeNamesSlotName = Rf_install("timeNames");
        R_netcdf::cppSlotName = Rf_install("cppPtr");
        R_netcdf::serverSlotName = Rf_install("server");
        R_netcdf::intervalSlotName = Rf_install("interval");
        R_netcdf::lenfileSlotName = Rf_install("lenfile");

        R_NetcdfVariable::nameSlotName = Rf_install("name");
        R_NetcdfVariable::modeSlotName = Rf_install("mode");
        R_NetcdfVariable::nctypeSlotName = Rf_install("nctype");
        R_NetcdfVariable::dimensionSlotName = Rf_install("dimension");
        R_NetcdfVariable::attributeSlotName = Rf_install("attributes");

        R_nts::dataSlotName = Rf_install("data");
        R_nts::posSlotName = Rf_install("positions");
        R_nts::unitsSlotName = Rf_install("units");
        R_nts::weightsSlotName = Rf_install("weights");
        R_nts::weightMapSlotName = Rf_install("weightmap");
        R_nts::stationsSlotName = Rf_install("stations");
        R_nts::longNamesSlotName = Rf_install("long_names");
        R_nts::startposSlotName = Rf_install("start");
        R_nts::endposSlotName = Rf_install("end");
        R_nts::timeFormatSlotName = Rf_install("time.format");
        R_nts::timeZoneSlotName = Rf_install("time.zone");

        // R_utime::dotDataSlotName = Rf_install(".Data");

    }

    void R_unload_eolts(DllInfo* dllinfo)
    {
        Rprintf("static R_unload_eolts\n");
    }

}

