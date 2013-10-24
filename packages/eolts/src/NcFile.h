// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.5 $
 *  $Date: 2010/09/28 23:05:47 $
 * 
 *  Description:
 *    A NetCDF file.
 * 
 */
#ifndef NCFILE_H
#define NCFILE_H

#include <string>
#include <vector>
#include <set>
#include <map>

#include <netcdf.h>

#include "NcException.h"

namespace eolts {

class NcVar;
class NcDim;
class NcAttr;

/*
 * Sometime we keep files open, sometimes not, so we want
 * to keep a handle that contains both the name and the ncid.
 * If ncid >=0, then the file is open
 */
class NcFile {
public:

    NcFile(const std::string& n);
    ~NcFile();

    const std::string& getName() const { return _name; }
    const std::string& getShortName() const { return _shortname; }

    void open() throw(NcException);
    void close() throw(NcException);

    int isOpen() const { return _ncid >= 0; }

    int getNcid() const { return _ncid; }

    int getNumVariables() throw(NcException);

    std::string getVariableName(int i) throw(NcException);

    std::vector<std::string> getVariableNames() throw(NcException);

    /**
     * Could throw exception if file is corrupt.
     */
    NcVar* getVariable(int i) throw(NcException);

    /** 
     * Return NULL if variable not found.
     */
    NcVar* getVariable(const std::string & name) throw();

    std::vector<NcVar*> getVariables() throw(NcException);

    int getNumDimensions() throw(NcException);

    const NcDim* getDimension(int i) throw(NcException);

    const NcDim* getDimension(const std::string & name) throw(NcException);

    std::vector<const NcDim*> getDimensions() throw(NcException);

    const NcDim* getUnlimitedDimension() throw(NcException);

    // NcAttr* getAttribute(const std::string & name);
    // std::vector<NcAttr*> getAttributes();

    const NcDim* getTimeDimension(const std::set<std::string>& possibleNames)
        throw(NcException);

    NcVar* getTimeSeriesVariable(const std::string& name,
            const NcDim* tdim) throw(NcException);

    NcVar* getTimeSeriesCountsVariable(const std::string& name,
            const NcDim* tdim) throw(NcException);

    std::vector<NcVar*> getTimeSeriesVariables(const NcDim* dim)
        throw(NcException);

    /**
     * retrieve mapping of station number to name.
     */
    std::map<int,std::string> getStations(const NcDim* tdim)
        throw(NcException);

private:

    void readDimensions() throw(NcException);

    int getUnlimitedDimid() throw(NcException);

    void clearVarMap(void);

    void clearDimMap(void);

    void clearAttrMap(void);

    void clearMaps(void);

    void addTimeSeriesVariable(const std::string& name,NcVar* var);

    void scanForTSVarWithoutStationDim(const NcDim* tdim)
        throw(NcException);

    typedef std::map<std::string,NcVar*>::iterator NcVarMapIterator;

    typedef std::map<std::string,NcDim*>::iterator NcDimMapIterator;

    typedef std::map<std::string,NcAttr*>::iterator NcAttrMapIterator;

    std::string _name;

    std::string _shortname;

    int _ncid;

    int _nvars;

    int _ndims;

    int _nattrs;

    std::vector<NcVar*> _varvec;

    std::map<std::string,NcVar*> _vars;

    std::vector<NcDim*> _dimVec;

    std::map<std::string,NcDim*> _dims;

    NcDim* _unlimitedDim;

    const NcDim* _timeDim;

    std::map<std::string,NcAttr*> _attrs;		// global attributes
    // std::map<std::string,NcVar*> _shortNameVars;
    // std::map<std::string,int> _shortNameVarids;
    // int _varidsScanned;
    // int _hasNonStationTSVariables;

    std::map<std::string,NcVar*> _tsVars;

    bool _hasTSVariableWithoutStationDimension;

    size_t _nscannedVars;

    // no copying, assignment
    NcFile(const NcFile&);
    NcFile& operator=(const NcFile&) const;
};

}   // namespace eolts

#endif
