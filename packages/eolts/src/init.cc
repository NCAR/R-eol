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

#include "R_NetcdfConnection.h"
#include "R_NetcdfVariable.h"
#include "R_utime.h"
#include "R_nts.h"

#include <R.h>  // install
#include <R_ext/Rdynload.h>   // DllInfo

using namespace eolts;

extern "C" {

    void R_init_eolts(DllInfo* dllinfo)
    {
#ifdef DEBUG
        Rprintf("static R_init_eolts\n");
#endif

        R_NetcdfConnection::fileSlotName = install("file");
        R_NetcdfConnection::dirSlotName = install("dir");
        R_NetcdfConnection::cppSlotName = install("cppPtr");

        R_NetcdfVariable::nameSlotName = install("name");
        R_NetcdfVariable::modeSlotName = install("mode");
        R_NetcdfVariable::nctypeSlotName = install("nctype");
        R_NetcdfVariable::dimensionSlotName = install("dimension");
        R_NetcdfVariable::attributeSlotName = install("attributes");

        R_nts::dataSlotName = install("data");
        R_nts::posSlotName = install("positions");
        R_nts::unitsSlotName = install("units");
        R_nts::weightsSlotName = install("weights");
        R_nts::weightMapSlotName = install("weightmap");
        R_nts::stationsSlotName = install("stations");
        R_nts::startposSlotName = install("start");
        R_nts::endposSlotName = install("end");
        R_nts::timeFormatSlotName = install("time.format");
        R_nts::timeZoneSlotName = install("time.zone");

        R_utime::dotDataSlotName = install(".Data");

    }

    void R_unload_eolts(DllInfo* dllinfo)
    {
        Rprintf("static R_unload_eolts\n");
    }

}

