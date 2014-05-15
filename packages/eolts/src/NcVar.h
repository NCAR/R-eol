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
 *    This class does not support reading/writing variables.
 *    You must use the C functions to do that.
 * 
 */
#ifndef NCVAR_H
#define NCVAR_H

#include <string>
#include <map>

#include "NcFile.h"

namespace eolts {

class NcFile;
class NcDim;
class NcAttr;

/**
 * Class providing necessary information about a NetCDF variable.
 */
class NcVar {
public:

    NcVar(NcFile* nch,int varid) throw(NcException);
    NcVar(NcFile* nch,const std::string& name) throw(NcException);
    ~NcVar();

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

    int getNumAttrs() throw(NcException);
    const NcAttr* getAttribute(const std::string& name) throw(NcException);
    const std::string& getCharAttribute(const std::string& name) throw(NcException);
    const std::vector<const NcAttr*> getAttributes() throw(NcException);

    const std::string& getUnits() throw(NcException);

    std::string typeToString() const throw(NcException)
    { return typeToString(getNcType()); }

    static std::string typeToString(nc_type type);

    bool matchDimension(const NcDim* dim, unsigned int idim);

    void readDimensions(void) throw(NcException);

protected:
    void readAttrs(void) throw(NcException);
    void readNcType() throw(NcException);

private:
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

    // no copying, assignment
    NcVar(const NcVar&);
    NcVar& operator=(const NcVar&) const;
};

}   // namespace eolts

#endif
