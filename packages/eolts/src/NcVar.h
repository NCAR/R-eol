// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.3 $
 *  $Date: 2008/09/09 23:42:36 $
 * 
 *  Description:
 *    A NetCDF variable.
 *    This class does not support reading/writing variables.
 *    You must use the C functions to do that.
 * 
 */
#ifndef NCVAR_H
#define NCVAR_H

#include <string>
#include <map>

#include "NcFile.h"

class NcFile;
class NcDim;
class NcAttr;

/**
 * Class providing necessary information about a NetCDF variable.
 */
class NcVar {
protected:
    NcFile* _nch;
    int _varid;
    std::string _name;
    nc_type _type;
    size_t* _edges;
    size_t _length;

    std::vector<const NcDim*> _dims;
    std::vector<NcAttr*> _attrVec;
    std::map<std::string,NcAttr*> _attrMap;
    bool _readAttr;
    bool _valid;

public:

    NcVar(NcFile* nch,int varid) NCEXCEPTION_CLAUSE;
    NcVar(NcFile* nch,const std::string& name) NCEXCEPTION_CLAUSE;
    ~NcVar();

    bool isValid() const { return _valid; }

    int getNcid() const { return _nch->getNcid(); }
    int getId() const { return _varid; }

    const std::string& getName() const { return _name; }
    const std::string& getFileName() const { return _nch->getName(); }

    nc_type getNcType() const { return _type; }
    std::string getNcTypeString() const;

    unsigned int getNumDimensions() const { return _dims.size(); }

    const NcDim* getDimension(unsigned int i) const { return getNumDimensions() > i ? _dims[i] : 0; }
    const std::vector<const NcDim*>& getDimensions() const { return _dims; }
    const NcDim* getDimension(const std::string& name) const;
    int whichDimension(const std::string& name) const;

    const size_t* getEdges() const { return _edges; }
    size_t getEdge(int i) const { return _edges[i]; }
    size_t getLength() const { return _length; }

    int getNumAttrs() NCEXCEPTION_CLAUSE;
    const NcAttr* getAttribute(const std::string& name) NCEXCEPTION_CLAUSE;
    const std::string& getCharAttribute(const std::string& name) NCEXCEPTION_CLAUSE;
    const std::vector<const NcAttr*> getAttributes() NCEXCEPTION_CLAUSE;

    const std::string& getUnits() NCEXCEPTION_CLAUSE;

    std::string typeToString() const NCEXCEPTION_CLAUSE
    { return typeToString(getNcType()); }

    static std::string typeToString(nc_type type);

    bool matchDimension(const NcDim* dim, unsigned int idim);

    void readDimensions(void) NCEXCEPTION_CLAUSE;

protected:
    void readAttrs(void) NCEXCEPTION_CLAUSE;
    void readNcType() NCEXCEPTION_CLAUSE;
};

#endif
