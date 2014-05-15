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
 *	A NetCDF dimension
 */

#include "NcDim.h"

using std::string;

using namespace eolts;

NcDim::NcDim(NcFile *ncf,int dimid) throw(NcException):
    _dimid(dimid),_name(),_length(0)
    //,_valid(false)
{
    char name[NC_MAX_NAME];
    int status = nc_inq_dimname(ncf->getNcid(),_dimid,name);
    if (status != NC_NOERR)
        throw NcException("nc_inq_dimname",
                ncf->getName(),status);
    _name = string(name);

    status = nc_inq_dimlen(ncf->getNcid(),_dimid,&_length);
    if (status != NC_NOERR)
        throw NcException("nc_inq_dimlen",ncf->getName(),
                status);
    // _valid = true;
}
void NcDim::readLength(NcFile *ncf) throw(NcException)
{
    int status = nc_inq_dimlen(ncf->getNcid(),_dimid,&_length);
    if (status != NC_NOERR)
        throw NcException("nc_inq_dimlen",ncf->getName(),
                status);
}

