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
 *	A NetCDF file.
 */
#include <sstream>

#include "NcFile.h"
#include "NcVar.h"
#include "NcDim.h"
#include "NcAttr.h"

#include <R.h>

using std::vector;
using std::string;
using std::map;
using std::set;

using namespace eolts;

NcFile::NcFile(const string& n): _name(n),_shortname(),_ncid(-1),_nvars(0),_ndims(0),
    _nattrs(0),_varvec(),_vars(),_dimVec(),_dims(),_unlimitedDim(0),_timeDim(0),
    _tsVars(),_hasTSVariableWithoutStationDimension(false),_nscannedVars(0),
    _attrMap(), _attrVec(), _readAttr(false)

{
    size_t i = _name.rfind('/');
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
void NcFile::open() throw(NcException)
{
    if (_ncid < 0) {
        int status = nc_open(_name.c_str(),NC_SHARE,&_ncid);
        if (status != NC_NOERR) {
            // Rprintf("error on open, _ncid=%d\n",_ncid);
            throw NcException("opening",_name,status);
        }
        readDimensions();
    }
    else if (_unlimitedDim) _unlimitedDim->readLength(this);
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
void NcFile::close() throw(NcException) {
    if (_ncid >= 0) {
        int status = nc_close(_ncid);
        _ncid = -1;
        if (status != NC_NOERR)
            throw NcException("closing",_name,status);
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
    for (im = _attrMap.begin(); im != _attrMap.end(); ++im) {
        delete im->second;
        im->second = 0;
    }
    _attrMap.clear();
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
 * so don't put in a loop, like so:
 *   for (i = 0; i < getNumVariables(); i++)
 * instead, get it once:
 *   int nv = getNumVariables();
 *   for (i = 0; i < nv; i++)
 */
int NcFile::getNumVariables() throw(NcException) {
    int status = nc_inq_nvars(_ncid,&_nvars);
    if (status != NC_NOERR)
        throw NcException("nc_inq_nvars",getName(),status);
    return _nvars;
}

string NcFile::getVariableName(int varid) throw(NcException)
{
    // should only throw exception if file is corrupt
    char name[NC_MAX_NAME];
    int status = nc_inq_varname(getNcid(),varid,name);
    if (status != NC_NOERR)
        throw NcException("nc_inq_varname",getName(),status);
    return string(name);
}

vector<string> NcFile::getVariableNames() throw(NcException)
{
    vector<string> names;
    unsigned int i;
    for (i = 0; i < _varvec.size(); i++)
        names.push_back(_varvec[i]->getName());

    getNumVariables();
    for ( ; i < (unsigned) _nvars; i++)
        names.push_back(getVariableName(i));
    return names;
}

NcVar* NcFile::getVariable(int varid) throw(NcException)
{
    // should only throw exception if file is corrupt
    if (varid >= _nvars) return 0;
    // note i starts at _varvec.size() - doesn't re-read variables.
    for (int i = _varvec.size(); i <= varid; i++) {
        NcVar *var = 0;
        var = new NcVar(this,i);
        _vars[var->getName()] = var;
        _varvec.push_back(var);
    }
    return _varvec[varid];
}

NcVar* NcFile::getVariable(const string &name) throw()
{
    NcVarMapIterator i = _vars.find(name);
    if (i != _vars.end()) return i->second;

    getVariables();
    i = _vars.find(name);
    if (i != _vars.end()) return i->second;

    return 0;
}

vector<NcVar *> NcFile::getVariables() throw(NcException)
{
    // should only throw exception if file is corrupt
    getNumVariables();
    // note i starts at _varvec.size() - doesn't re-read variables.
    for (int i = _varvec.size(); i < _nvars; i++)
        getVariable(i);
    return _varvec;
}

int NcFile::getNumDimensions() throw(NcException) {
    int status = nc_inq_ndims(getNcid(),&_ndims);
    if (status != NC_NOERR)
        throw NcException("nc_inq_ndims",getName(),status);
    return _ndims;
}

const NcDim* NcFile::getDimension(int dimid) throw(NcException)
{
    // should only throw exception if file is corrupt
    if (dimid >= _ndims) readDimensions();
    if (dimid >= _ndims) {
        std::ostringstream ost;
        ost << getName() << ": dimid=" << dimid << " exceeds max=" << _ndims-1;
        throw NcException(getName(),ost.str());
    }
    return _dimVec[dimid];
}

const NcDim* NcFile::getUnlimitedDimension() throw(NcException)
{
    if (!_unlimitedDim) readDimensions();
    return _unlimitedDim;
}

vector<const NcDim*> NcFile::getDimensions() throw(NcException)
{

    if (getNumDimensions() != (signed)_dimVec.size()) readDimensions();
    return vector<const NcDim*>(_dimVec.begin(),_dimVec.end());
}

const NcDim* NcFile::getDimension(const string& name) throw(NcException)
{
    if (_dims.size() == 0) readDimensions();
    NcDimMapIterator i = _dims.find(name);
    if (i == _dims.end()) return 0;
    return i->second;
}

void NcFile::readDimensions() throw(NcException)
{
    getNumDimensions();
    int unlimdimid = getUnlimitedDimid();

    for (int i = 0; i < _ndims; i++) {
        NcDim* dim = new NcDim(this,i);
        if (dim->getId() == unlimdimid) _unlimitedDim = dim;
        _dimVec.push_back(dim);
        _dims[dim->getName()] = dim;
    }
}

int NcFile::getUnlimitedDimid() throw(NcException) {
    int unlimdim;
    // unlimdim is -1 if no unlimited dimension
    int status = nc_inq_unlimdim(getNcid(),&unlimdim);
    if (status != NC_NOERR)
        throw NcException("nc_inq_unlimdim",getName(),status);
    return unlimdim;
}

void NcFile::readAttrs() throw(NcException)
{
    int natts;
    int status = nc_inq_varnatts(getNcid(),NC_GLOBAL,&natts);
    if (status != NC_NOERR) {
        throw NcException("nc_inq_varnatts", getName(),"NC_GLOBAL",status);
    }

    for (int i = 0; i < natts; i++) {
        NcAttr *attr = NcAttr::readNcAttr(this,i);
        std::pair<string,NcAttr *> p(attr->getName(),attr);
        _attrMap.insert(p);
        _attrVec.push_back(attr);
    }
    _readAttr = true;
}

int NcFile::getNumAttrs() throw(NcException)
{
    if (!_readAttr) readAttrs();
    return _attrVec.size();
}

const std::vector<const NcAttr*> NcFile::getAttributes() throw(NcException)
{
    if (!_readAttr) readAttrs();
    return std::vector<const NcAttr*>(_attrVec.begin(),_attrVec.end());
}

const NcAttr *NcFile::getAttribute(const string& name) throw(NcException)
{
    if (!_readAttr) readAttrs();
    map<string,NcAttr *>::iterator itr = _attrMap.find(name);
    if (itr != _attrMap.end()) return itr->second;
    else return 0;
}

void NcFile::addTimeSeriesVariable(const string& name,NcVar* var)
{

    _tsVars[name] = var;

    if (!var->getDimension("station") && var->getName() != "time") {
#ifdef DEBUG
        Rprintf("var %s doesn't have station dimension\n",
                var->getName().c_str());
#endif
        _hasTSVariableWithoutStationDimension = true;
    }
}


NcVar* NcFile::getTimeSeriesVariable(const string& name,const NcDim* tdim)
    throw(NcException)
{

    // first try already mapped names
    NcVarMapIterator itr = _tsVars.find(name);
    if (itr != _tsVars.end()) return itr->second;

    // try regular variable name. Does not throw exception
    NcVar* var = getVariable(name);
    if (var && var->matchDimension(tdim,0)) {
        addTimeSeriesVariable(var->getName(),var);
        return var;
    }

    unsigned int nvars = getNumVariables();
    for (; _nscannedVars < nvars; _nscannedVars++) {
        // could throw exception if file is corrupt
        var = getVariable(_nscannedVars);
        if (var->matchDimension(tdim,0)) {
            string shortName = var->getCharAttribute("short_name");
            if (shortName.size() > 0) {
                addTimeSeriesVariable(shortName,var);
                if (shortName == name) return var;
            }
        }
    }
    return 0;   // variable exists, but doesn't have time dimension
}

vector<NcVar*> NcFile::getTimeSeriesVariables(const NcDim* tdim)
    throw(NcException)
{

    vector <NcVar*> result;
    unsigned int nvars = getNumVariables();

    NcVarMapIterator itr;
    for(itr = _tsVars.begin(); itr != _tsVars.end(); ++itr) {
        NcVar* var = itr->second;
        result.push_back(var);
    }

    for (; _nscannedVars < nvars; _nscannedVars++) {
        // this can throw an exception, but shouldn't on a valid file,
        // since it is just inquiring by the variable id
        NcVar* var = getVariable(_nscannedVars);

        if (var->matchDimension(tdim,0)) {
            string shortName = var->getCharAttribute("short_name");
            if (shortName.size() > 0 && shortName != var->getName())
                addTimeSeriesVariable(shortName,var);
            addTimeSeriesVariable(var->getName(),var);
            result.push_back(var);
        }
    }
    return result;
}

void NcFile::scanForTSVarWithoutStationDim(const NcDim* tdim)
    throw(NcException)
{
    unsigned int nvars = getNumVariables();
    for (; _nscannedVars < nvars; _nscannedVars++) {
        NcVar* var = getVariable(_nscannedVars);
        if (var->matchDimension(tdim,0)) {
            string shortName = var->getCharAttribute("short_name");
            if (shortName.size() > 0 && shortName != var->getName())
                addTimeSeriesVariable(shortName,var);
            addTimeSeriesVariable(var->getName(),var);

            if (_hasTSVariableWithoutStationDimension) break;
        }
    }
}

const NcDim* NcFile::getTimeDimension(const set<string>& possibleNames)
    throw(NcException)
{
    if (_timeDim) return _timeDim;			// already found

    _timeDim = getUnlimitedDimension();
    if (_timeDim && possibleNames.find(_timeDim->getName()) != possibleNames.end())
        return _timeDim;

    set<string>::iterator itr;
    for (itr = possibleNames.begin();
            !_timeDim && itr != possibleNames.end(); ++itr) {
        const NcDim* dim = getDimension(*itr);
        if (dim) {
            _timeDim = dim;
            return _timeDim;
        }
    }

    if (_timeDim) Rprintf("Unlimited dimension (%s) does not match an expected name for time\n",
            _timeDim->getName().c_str());

    return _timeDim;
}

map<int,string> NcFile::getStations(const NcDim *tdim) throw(NcException)
{
    map<int,string> stations;

    scanForTSVarWithoutStationDim(tdim);
    if (_hasTSVariableWithoutStationDimension) {
#ifdef DEBUG
        Rprintf("%s has TSVariableWithoutStationDim\n",getName().c_str());
#endif
        stations[0] = string("");
    }

    const NcDim *stationDim = getDimension("station");
    if (!stationDim) return stations;

    size_t nstations = stationDim->getLength();

    NcVar* var = getVariable("station");
    if (!var) return stations;

    if (var->getNcType() != NC_CHAR) {
        std::ostringstream ost;
        ost << "error in " << getName() << ": station variable is not character";
        Rf_warning(ost.str().c_str());
        return stations;
    }
    const NcDim* stnVarDim = 0;
    if (var->getNumDimensions() == 2) stnVarDim = var->getDimension(0);

    if (!stnVarDim || stnVarDim->getId() != stationDim->getId()) {
        std::ostringstream ost;
        ost << "error in " << getName() << ": station variable does not have dimensions of (station,namelen)";
        Rf_warning(ost.str().c_str());
        return stations;
    }

    size_t count[2];
    size_t start[2];
    count[0] = 1;
    count[1] = var->getEdge(1);
    start[1] = 0;

    for (unsigned int istn = 0; istn < nstations; istn++) {
        start[0] = istn;
        vector<char> cname(count[1]+1);
        int status = nc_get_vara_text(_ncid,var->getId(),start,count,&cname.front());
        if (status != NC_NOERR) {
            throw NcException("nc_get_vara_text",
                    var->getFileName(),var->getName(),status);
        }
        cname[count[1]] = 0;
        // set end of string to last non-blank char
        for (int j = count[1] - 1; j >= 0; j--) {
            if (cname[j] != ' ') break;
            cname[j] = 0;
        }

        stations[istn+1] = string(&cname.front());
    }
    return stations;
}

