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

NcDim::NcDim(NcFile *ncf,int dimid) NCEXCEPTION_CLAUSE:
_dimid(dimid),_length(0),_valid(false)
{
    char name[NC_MAX_NAME];
    int status = nc_inq_dimname(ncf->getNcid(),_dimid,name);
    if (status != NC_NOERR)
#ifdef THROW_NCEXCEPTION
        throw NcException("nc_inq_dimname",
                ncf->getName(),status);
#else
    {
        Rprintf("Error %s: %d: nc_inq_dimname: %s\n",
                ncf->getName().c_str(),dimid,::nc_strerror(status));
        return;
    }
#endif
    _name = string(name);

    status = nc_inq_dimlen(ncf->getNcid(),_dimid,&_length);
    if (status != NC_NOERR)
#ifdef THROW_NCEXCEPTION
        throw NcException("nc_inq_dimlen",ncf->getName(),
                status);
#else
    Rprintf("Error %s: %s: nc_inq_dimlen: %s\n",
            ncf->getName().c_str(),_name.c_str(),::nc_strerror(status));
#endif
    _valid = true;
}
void NcDim::readLength(NcFile *ncf) NCEXCEPTION_CLAUSE
{
    int status = nc_inq_dimlen(ncf->getNcid(),_dimid,&_length);
    if (status != NC_NOERR) {
#ifdef THROW_NCEXCEPTION
        throw NcException("nc_inq_dimlen",ncf->getName(),
                status);
#else
        Rprintf("Error %s: %s: nc_inq_dimlen: %s\n",
                ncf->getName().c_str(),_name.c_str(),::nc_strerror(status));
        _valid = false;
#endif
    }
}

