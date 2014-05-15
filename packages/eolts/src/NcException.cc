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
 *	General exception for errors encountered while accessing a NetCDF file.
 *
 */

#include "NcException.h"
#include "NcVar.h"

using namespace eolts;

NcException::NcException() :
    _what(""),_status(NC_NOERR)
{
}

NcException::NcException(const std::string& op, const std::string& fname, int status) :
    _what(std::string(op) + " " + fname + ": "),_status(status)
{
    if (::nc_strerror(status)) _what += std::string(::nc_strerror(status));
    else _what += std::string("unknown error");
}

NcException::NcException(const std::string&  op, const std::string& fname,
        const std::string& vname, int status) :
    _what(std::string(op) + " " + fname + ", variable " +
            vname),_status(status)
{
    if (::nc_strerror(status)) _what += std::string(::nc_strerror(status));
    else _what += std::string("unknown error");
}

NcException::NcException(const std::string& fname,const std::string& what) :
    _what(fname + ":" + what),_status(NC_NOERR) {}

