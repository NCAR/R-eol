// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.3 $
 *  $Date: 2008/09/09 23:42:36 $
 * 
 *  Description:
 *	A NetCDF file.
 */
#include <sstream>

#include <R.h>
// #include <R_ext/Print.h>

#include "NcFile.h"
#include "NcVar.h"
#include "NcDim.h"
#include "NcAttr.h"

using std::vector;
using std::string;
using std::map;
using std::pair;

NcFile::NcFile(const string& n): _name(n),_ncid(-1),_nvars(0),_ndims(0),
    _unlimitedDim(0)
#ifndef THROW_NCEXCEPTION
                    ,_valid(true)
#endif
{
    unsigned int i = _name.rfind('/');
    if (i < string::npos)_shortname = _name.substr(i+1);
    else _shortname = _name;
}
NcFile::~NcFile()
{
    clearMaps();
    try {
        close();
    }
    catch (const NcException& e) {
            Rprintf("Error: %e\n",e.what());
    }
}

/**
 * open a NetCDF file, if it isn't open already.
 */
void NcFile::open() NCEXCEPTION_CLAUSE
{
    int status = NC_NOERR;
    if (_ncid < 0) {
        status = nc_open(_name.c_str(),NC_SHARE,&_ncid);
        if (status != NC_NOERR) 
#ifdef THROW_NCEXCEPTION
            throw NcException("opening",_name,status);
#else
        {
            Rprintf("Error opening %s: %s\n",
                    getName().c_str(),::nc_strerror(status));
            _valid = false;
            return;
        }
#endif
        readDimensions();
    }
    else if (_unlimitedDim) _unlimitedDim->readLength(this);

#ifndef THROW_NCEXCEPTION
    _valid = true;
#endif
}

/**
 * Note: When we close the file we don't delete the NcVars
 * that have been created for the variables we're interested in.
 * If we later re-open the same file, and the number of variables has
 * not changed, then all the information in a NcVar should
 * still be correct.  We do re-read the length of the unlimited
 * dimension, in case it has grown.
 *
 * This assumes:
 * 1. that the varid only depends on the order of variables in the
 *    header
 * 2. that the order of variables or dimensions does not change.
 * 3. that the attributes do not change.
 *
 * It is not likely that the dimensions would change, except for
 * the unlimited dimension.
 * We won't care if the attributes change.
 * #1 I believe to be true - can't imagine varids being assigned in
 * any other way.
 *
 */
void NcFile::close() NCEXCEPTION_CLAUSE {
    if (_ncid >= 0) {
        int status = nc_close(_ncid);
        _ncid = -1;
        if (status != NC_NOERR)
#ifdef THROW_NCEXCEPTION
            throw NcException("closing",_name,status);
#else
        {
            Rprintf("Error closing %s: %s\n",
                    getName().c_str(),::nc_strerror(status));
            _valid = false;
        }
#endif
    }
}

/**
 * delete the NcVars that have been created, and
 * clear the maps.
 */
void NcFile::clearVarMap(void) {
    NcVarMapIterator im;
    for (im = _vars.begin(); im != _vars.end(); ++im) {
        delete im->second;
        im->second = 0;
    }
    _vars.clear();
    _varvec.clear();
}
void NcFile::clearDimMap(void) {
    NcDimMapIterator im;
    for (im = _dims.begin(); im != _dims.end(); ++im) {
        delete im->second;
        im->second = 0;
    }
    _dims.clear();
}
void NcFile::clearAttrMap(void) {
    NcAttrMapIterator im;
    for (im = _attrs.begin(); im != _attrs.end(); ++im) {
        delete im->second;
        im->second = 0;
    }
    _attrs.clear();
}

void NcFile::clearMaps(void)
{
    clearVarMap();
    clearDimMap();
    clearAttrMap();
}

/**
 * Get number of variables in a netcdf file.
 * This value is not cached, it is read from the file,
 * so don't put in in a loop, like so:
 *   for (i = 0; i < getNumVariables(); i++)
 * instead, get it once:
 *   int nv = getNumVariables();
 *   for (i = 0; i < nv; i++)
 */
int NcFile::getNumVariables() NCEXCEPTION_CLAUSE {
    int status = nc_inq_nvars(_ncid,&_nvars);
    if (status != NC_NOERR)
#ifdef THROW_NCEXCEPTION
        throw NcException("nc_inq_nvars",getName(),status);
#else
    {
        Rprintf(stderr,"Error %s: nc_inq_nvars: %s\n",
                getName().c_str(),::nc_strerror(status));
        _valid = false;
        return 0;
    }
#endif
    return _nvars;
}

string NcFile::getVariableName(int varid) NCEXCEPTION_CLAUSE
{
    char name[NC_MAX_NAME];
    int status = nc_inq_varname(getNcid(),varid,name);
    if (status != NC_NOERR)
#ifdef THROW_NCEXCEPTION
        throw NcException("nc_inq_varname",getName(),status);
#else
    {
        Rprintf("Error %s: nc_inq_varname: %s\n",
                getName().c_str(),::nc_strerror(status));
        _valid = false;
        name[0] = 0;
    }
#endif
    return string(name);
}

vector<string> NcFile::getVariableNames() NCEXCEPTION_CLAUSE
{
    vector<string> names;
    unsigned int i;
    for (i = 0; i < _varvec.size(); i++)
        names.push_back(_varvec[i]->getName());

    getNumVariables();
#ifndef THROW_NCEXCEPTION
    if(!isValid()) return names;
#endif
    for ( ; i < (unsigned) _nvars; i++)
        names.push_back(getVariableName(i));
    return names;
}

NcVar* NcFile::getVariable(int varid) NCEXCEPTION_CLAUSE {
    if (varid >= _nvars) return 0;
    // note i starts at _varvec.size() - doesn't re-read variables.
    for (int i = _varvec.size(); i <= varid; i++) {
        NcVar *var = new NcVar(this,i);
#ifndef THROW_NCEXCEPTION
        if (!var->isValid()) {
            delete var;
            return 0;
        }
#endif
        pair<string,NcVar *> p(var->getName(),var);
        _vars.insert(p);
        _varvec.push_back(var);
    }
    return _varvec[varid];
}

NcVar* NcFile::getVariable(const string &name) NCEXCEPTION_CLAUSE {
    NcVarMapIterator i = _vars.find(name);
    if (i != _vars.end()) return i->second;
    NcVar *var = new NcVar(this,name);
#ifndef THROW_NCEXCEPTION
    if (!var->isValid()) {
        delete var;
        return 0;
    }
#endif
    pair<string,NcVar *> p(var->getName(),var);
    _vars.insert(p);
    return var;
}

vector<NcVar *> NcFile::getVariables() NCEXCEPTION_CLAUSE
{
    getNumVariables();
#ifndef THROW_NCEXCEPTION
    if(!isValid()) return _varvec;
#endif
    // note i starts at _varvec.size() - doesn't re-read variables.
    for (int i = _varvec.size(); i < _nvars; i++)
        if (!getVariable(i)) break;
    return _varvec;
}

int NcFile::getNumDimensions() NCEXCEPTION_CLAUSE {
    int status = nc_inq_ndims(getNcid(),&_ndims);
    if (status != NC_NOERR)
#ifdef THROW_NCEXCEPTION
        throw NcException("nc_inq_ndims",getName(),status);
#else
    {
        Rprintf("Error %s: nc_inq_ndims: %s\n",
                getName().c_str(),::nc_strerror(status));
        _valid = false;
        return 0;
    }
#endif
    return _ndims;
}

const NcDim* NcFile::getDimension(int dimid) NCEXCEPTION_CLAUSE
{
    if (dimid >= _ndims) readDimensions();
#ifndef THROW_NCEXCEPTION
    if (!isValid()) return 0;
#endif
    if (dimid >= _ndims) {
#ifdef THROW_NCEXCEPTION
        std::ostringstream ost;
        ost << getName() << ": dimid=" << dimid << " exceeds max=" << _ndims-1;
        throw NcException(getName(),ost.str());
#else
        Rprintf("%s: dimid=%d exceeds max: %d\n",
                getName().c_str(),dimid,_ndims-1);
        return 0;
#endif
    }
    return _dimVec[dimid];
}

const NcDim* NcFile::getUnlimitedDimension() NCEXCEPTION_CLAUSE
{
    if (!_unlimitedDim) readDimensions();
#ifndef THROW_NCEXCEPTION
    if (!isValid()) return 0;
#endif
    return _unlimitedDim;
}

vector<const NcDim*> NcFile::getDimensions() NCEXCEPTION_CLAUSE
{
#ifndef THROW_NCEXCEPTION
    if (!isValid()) return vector<const NcDim*>();
#endif

    if (getNumDimensions() != (signed)_dimVec.size()) readDimensions();
#ifndef THROW_NCEXCEPTION
    if (!isValid()) return vector<const NcDim*>();
#endif
    return vector<const NcDim*>(_dimVec.begin(),_dimVec.end());
}

const NcDim* NcFile::getDimension(const string& name) NCEXCEPTION_CLAUSE
{
    if (_dims.size() == 0) readDimensions();
#ifndef THROW_NCEXCEPTION
    if (!isValid()) return 0;
#endif
    NcDimMapIterator i = _dims.find(name);
    if (i == _dims.end()) return 0;
    return i->second;
}

void NcFile::readDimensions() NCEXCEPTION_CLAUSE
{
    getNumDimensions();
#ifndef THROW_NCEXCEPTION
    if (!isValid()) return;
#endif
    int unlimdimid = getUnlimitedDimid();

    for (int i = 0; i < _ndims; i++) {
        NcDim* dim = new NcDim(this,i);
        if (dim->getId() == unlimdimid) _unlimitedDim = dim;
        _dimVec.push_back(dim);
        pair<string,NcDim*> p;
        p.first = dim->getName();
        p.second = dim;
        _dims.insert(p);
    }
}

int NcFile::getUnlimitedDimid() NCEXCEPTION_CLAUSE {
    int unlimdim;
    // unlimdim is -1 if no unlimited dimension
    int status = nc_inq_unlimdim(getNcid(),&unlimdim);
    if (status != NC_NOERR)
#ifdef THROW_NCEXCEPTION
        throw NcException("nc_inq_unlimdim",getName(),status);
#else
    {
        Rprintf("Error %s: nc_inq_unlimdim: %s\n",
                getName().c_str(),::nc_strerror(status));
        return -1;
    }
#endif
    return unlimdim;
}



