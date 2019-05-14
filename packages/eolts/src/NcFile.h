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

    void open();
    void close();

    int isOpen() const { return _ncid >= 0; }

    int getNcid() const { return _ncid; }

    int getNumVariables();

    std::string getVariableName(int i);

    std::vector<std::string> getVariableNames();

    /**
     * Could throw exception if file is corrupt.
     */
    NcVar* getVariable(int i);

    /** 
     * Return NULL if variable not found.
     */
    NcVar* getVariable(const std::string & name) noexcept;

    std::vector<NcVar*> getVariables();

    int getNumDimensions();

    const NcDim* getDimension(int i);

    const NcDim* getDimension(const std::string & name);

    std::vector<const NcDim*> getDimensions();

    const NcDim* getUnlimitedDimension();

    int getNumAttrs();

    const NcAttr* getAttribute(const std::string& name);

    const std::vector<const NcAttr*> getAttributes();

    const NcDim* getTimeDimension(const std::set<std::string>& possibleNames)
       ;

    NcVar* getTimeSeriesVariable(const std::string& name,
            const NcDim* tdim);

    std::vector<NcVar*> getTimeSeriesVariables(const NcDim* dim)
       ;

    /**
     * retrieve mapping of station number to name.
     */
    std::map<int,std::string> getStations(const NcDim* tdim)
       ;

private:

    void readDimensions();

    int getUnlimitedDimid();

    void readAttrs();

    void clearVarMap(void);

    void clearDimMap(void);

    void clearAttrMap(void);

    void clearMaps(void);

    void addTimeSeriesVariable(const std::string& name,NcVar* var);

    void scanForTSVarWithoutStationDim(const NcDim* tdim)
       ;

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

    std::map<std::string,NcVar*> _tsVars;

    bool _hasTSVariableWithoutStationDimension;

    size_t _nscannedVars;

    std::map<std::string,NcAttr*> _attrMap;		// global attributes

    std::vector<NcAttr*> _attrVec;

    bool _readAttr;

    // no copying, assignment
    NcFile(const NcFile&);
    NcFile& operator=(const NcFile&) const;
};

}   // namespace eolts

#endif
