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

// #define NETCDF_RPM_PUTS_STUFF_ON_NETCDF_3
#ifdef NETCDF_RPM_PUTS_STUFF_ON_NETCDF_3
#include <netcdf-3/netcdf.h>
#else
#include <netcdf.h>
#endif

#include <string>
#include <vector>
#include <set>
#include <map>

#ifdef THROW_NCEXCEPTION
#ifndef NCEXCEPTION_CLAUSE
#define NCEXCEPTION_CLAUSE throw(NcException)
#endif
#include "NcException.h"
#else
#define NCEXCEPTION_CLAUSE
#endif

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

#ifndef THROW_NCEXCEPTION
    bool isValid() const { return _valid; }
#endif

    const std::string& getName() const { return _name; }
    const std::string& getShortName() const { return _shortname; }

    void open() NCEXCEPTION_CLAUSE;
    void close() NCEXCEPTION_CLAUSE;

    int isOpen() const { return _ncid >= 0; }

    int getNcid() const { return _ncid; }

    int getNumVariables() NCEXCEPTION_CLAUSE;

    std::string getVariableName(int i) NCEXCEPTION_CLAUSE;

    std::vector<std::string> getVariableNames() NCEXCEPTION_CLAUSE;

    /**
     * Could throw exception if file is corrupt.
     */
    NcVar* getVariable(int i) NCEXCEPTION_CLAUSE;

    /** 
     * Return NULL if variable not found.
     */
    NcVar* getVariable(const std::string & name) throw();

    std::vector<NcVar*> getVariables() NCEXCEPTION_CLAUSE;

    int getNumDimensions() NCEXCEPTION_CLAUSE;

    const NcDim* getDimension(int i) NCEXCEPTION_CLAUSE;

    const NcDim* getDimension(const std::string & name) NCEXCEPTION_CLAUSE;

    std::vector<const NcDim*> getDimensions() NCEXCEPTION_CLAUSE;

    const NcDim* getUnlimitedDimension() NCEXCEPTION_CLAUSE;

    // NcAttr* getAttribute(const std::string & name);
    // std::vector<NcAttr*> getAttributes();

    const NcDim* getTimeDimension(const std::set<std::string>& possibleNames)
        NCEXCEPTION_CLAUSE;

    NcVar* getTimeSeriesVariable(const std::string& name,
            const NcDim* tdim) NCEXCEPTION_CLAUSE;

    NcVar* getTimeSeriesCountsVariable(const std::string& name,
            const NcDim* tdim) NCEXCEPTION_CLAUSE;

    std::vector<NcVar*> getTimeSeriesVariables(const NcDim* dim)
        NCEXCEPTION_CLAUSE;

    /**
     * retrieve mapping of station number to name.
     */
    std::map<int,std::string> getStations(const NcDim* tdim)
        NCEXCEPTION_CLAUSE;

protected:

    void readDimensions() NCEXCEPTION_CLAUSE;

    int getUnlimitedDimid() NCEXCEPTION_CLAUSE;

    void clearVarMap(void);

    void clearDimMap(void);

    void clearAttrMap(void);

    void clearMaps(void);

    void addTimeSeriesVariable(const std::string& name,NcVar* var);

    void scanForTSVarWithoutStationDim(const NcDim* tdim)
        NCEXCEPTION_CLAUSE;

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

    std::map<std::string,NcAttr*> _attrs;		// global attributes
    // std::map<std::string,NcVar*> _shortNameVars;
    // std::map<std::string,int> _shortNameVarids;
    // int _varidsScanned;
    // int _hasNonStationTSVariables;

    std::map<std::string,NcVar*> _tsVars;

    const NcDim* _timeDim;

    bool _hasTSVariableWithoutStationDimension;

    size_t _nscannedVars;
};
#endif
