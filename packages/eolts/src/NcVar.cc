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
 *    A NetCDF variable.
 * 
 */

#include "NcFile.h"
#include "NcVar.h"
#include "NcDim.h"
#include "NcAttrT.h"

using std::string;
using std::vector;
using std::map;
using std::pair;

using namespace eolts;


NcVar::NcVar(NcFile *nch,int varid) throw(NcException):
    _nch(nch),_varid(varid),_name(),_type(NC_NAT),
    _edges(0),_length(0),_dims(),_attrVec(),_attrMap(),_readAttr(false)
{
    char name[NC_MAX_NAME];
    int status = nc_inq_varname(getNcid(),_varid,name);

    if (status != NC_NOERR) {
        throw NcException("nc_inq_varname", _nch->getName(),status);
    }
    _name = string(name);
    readNcType();
    readDimensions();
}

NcVar::NcVar(NcFile *nch,const std::string& name) throw(NcException):
    _nch(nch),_varid(-1),_name(name),_type(NC_NAT),
    _edges(0),_length(0),_dims(),_attrVec(),_attrMap(),_readAttr(false)
{
    int status = nc_inq_varid(getNcid(),name.c_str(),&_varid);
    if (status != NC_NOERR) {
        throw NcException("nc_inq_varid", _nch->getName(),name,status);
    }
    readNcType();
    readDimensions();
}

NcVar::~NcVar() {
    delete [] _edges;
    for (unsigned int i = 0; i < _attrVec.size(); i++) delete _attrVec[i];
}

void NcVar::readDimensions() throw(NcException)
{
    int ndims;
    int status = nc_inq_varndims(getNcid(),_varid,&ndims);
    if (status != NC_NOERR) {
        throw NcException("nc_inq_varndims",getFileName(),
                getName(),status);
    }

    int *dimids = new int[ndims];
    delete [] _edges;
    _edges  = new size_t[ndims];

    status = nc_inq_vardimid(getNcid(),_varid,dimids);
    if (status != NC_NOERR) {
        delete [] dimids;
        throw NcException("nc_inq_vardimid",getFileName(),getName(),status);
    }

    _dims.clear();
    _length = 1;
    for (int i = 0; i < ndims; i++) {
        const NcDim *dim = _nch->getDimension(dimids[i]);
        _dims.push_back(dim);
        _length *= (_edges[i] = dim->getLength());
    }
    delete [] dimids;
}

int NcVar::getNumAttrs() throw(NcException)
{
    if (!_readAttr) readAttrs();
    return _attrVec.size();
}

const std::vector<const NcAttr*> NcVar::getAttributes() throw(NcException)
{
    if (!_readAttr) readAttrs();
    return std::vector<const NcAttr*>(_attrVec.begin(),_attrVec.end());
}

const NcAttr *NcVar::getAttribute(const string& name) throw(NcException)
{
    if (!_readAttr) readAttrs();
    string sname(name);
    map<string,NcAttr *>::iterator itr = _attrMap.find(sname);
    if (itr != _attrMap.end()) return itr->second;
    else return 0;
}

const string& NcVar::getCharAttribute(const string& name) throw(NcException)
{
    const NcAttr *attr = getAttribute(name);
    if (attr && attr->getNcType() == NC_CHAR) {
        const NcAttrT<string>* sattr = (const NcAttrT<string>*) attr;
        const vector<string>& val = sattr->getValue();
        if (val.size() > 0) return val[0];
    }
    static string res;
    return res;
}

void NcVar::readAttrs() throw(NcException)
{
    int natts;
    int status = nc_inq_varnatts(getNcid(),_varid,&natts);
    if (status != NC_NOERR) {
        throw NcException("nc_inq_varnatts", getFileName(),getName(),status);
    }

    for (int i = 0; i < natts; i++) {
        NcAttr *attr = NcAttr::createNcAttr(this,i);
        pair<string,NcAttr *> p(attr->getName(),attr);
        _attrMap.insert(p);
        _attrVec.push_back(attr);
    }
    _readAttr = true;
}

const string& NcVar::getUnits() throw(NcException)
{
    return getCharAttribute("units");
}

void NcVar::readNcType() throw(NcException)
{
    int status = nc_inq_vartype(_nch->getNcid(),_varid,&_type);
    if (status != NC_NOERR) {
        throw NcException("nc_inq_vartype", getFileName(),getName(),status);
    }
}

const NcDim* NcVar::getDimension(const string& name) const {
    for (unsigned int i = 0; i < _dims.size(); i++)
        if (_dims[i]->getName() == name) return _dims[i];
    return 0;
}

int NcVar::whichDimension(const string& name) const {
    for (unsigned int i = 0; i < _dims.size(); i++)
        if (_dims[i]->getName() == name) return i;
    return -1;
}

bool NcVar::matchDimension(const NcDim *dim, unsigned int idim) {
    const NcDim* vdim;
    return getNumDimensions() > idim && (vdim = getDimension(idim)) &&
        vdim->getId() == dim->getId();
}

string NcVar::getNcTypeString() const {
    return typeToString(getNcType());
}

string NcVar::typeToString(nc_type type) {
    switch (type) {
    case NC_BYTE: return "NC_BYTE";
    case NC_SHORT: return "NC_SHORT";
    case NC_INT: return "NC_LONG";
    case NC_FLOAT: return "NC_FLOAT";
    case NC_DOUBLE: return "NC_DOUBLE";
    case NC_CHAR: return "NC_CHAR";
    case NC_NAT:
    default:
                  return "NC_NAT";
    }
}
