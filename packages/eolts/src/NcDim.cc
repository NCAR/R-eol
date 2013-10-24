// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.3 $
 *  $Date: 2008/09/09 23:42:36 $
 * 
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

