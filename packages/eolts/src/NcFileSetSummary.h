// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.3 $
 *  $Date: 2006/04/18 01:50:01 $
 * 
 *  Description:
 *    A class for scanning an returning information about an NcFileSet
 * 
 */
#ifndef NCFILESETSUMMARY_H
#define NCFILESETSUMMARY_H

#include <vector>
#include <string>

#include "NcFileSet.h"

namespace eolts {

class NcFileSetSummary {
public:

    NcFileSetSummary(NcFileSet *fileset,
            const std::vector<std::string>& varnames,
            const std::vector<int>&stations,bool getCounts) throw(NcException);
    ~NcFileSetSummary();

    /** index from 0 to _fileset->getNFiles()-1 of first existing file */
    int getFirstFileIndex() const { return _firstFile; }

    /**
     * output type that will handle precision and
     * range of requested variables
     */
    nc_type getOutType() const { return _outType; }

    /**
     * return dimensions of variable i
     */
    const std::vector<size_t> &getVariableDimensions(int i) const { return _dims[i]; }

    /**
     * return units of variable i
     */
    const std::string &getUnitsName(int i) const { return _unitsNames[i]; }

    const char *getVariableName(int ivar) { return _vnames[ivar].c_str(); }

    /** maximum value of the sample dimension of requested variables */
    // needed ext
    size_t getMaxSampleDimension() const { return _maxSampleDim; }

    /** value of "sample" dimension of a variable */
    size_t getSampleDimension(int i) const { return _sampleDims[i]; }

    /** value of "station" dimension of a variable. 1 if no station dim */
    size_t getStationDimension(int i) const { return _stationDims[i]; }

    /** number of columns in output matrix of requested variables */
    size_t getNumOutputColumns() const { return _ncolOut; }

    /** starting value of station dimension */
    size_t getStationStart() const { return _stationStart; }

    /** how many stations requested */
    size_t getStationCount() const { return _stationCount; }

    /** does variable i have a station dimension */
    int isStationVariable(int i) const { return _stationVar[i]; }

private:

    void checkType(NcVar* );

    void resolveOutType();

    NcFileSet *_fileset;

    std::vector <std::string> _vnames;
    int _nvars;

    int _firstFile;	/* first file with a requested variable */

    nc_type _outType;

    /* dimensions of each chosen variable */
    std::vector <std::vector<size_t> > _dims;

    /** units of each variable */
    std::vector <std::string> _unitsNames;

    /* size of first dimension of requested variables in each file */
    std::vector <size_t> _firstDims;

    /** value of "sample" dimension of each variable */
    std::vector <size_t> _sampleDims;

    /** value of "station" dimension of each variable. 1 if no station dim */
    std::vector <size_t> _stationDims;

    /** logical for each variable, if it has a "station" dimension */
    std::vector <bool> _stationVar;

    int _nfoundvars;

    int _firstVar;

    /** maximum value of the sample dimension of requested variables */
    size_t _maxSampleDim;

    /** number of columns in output matrix of requested variables */
    size_t _ncolOut;

    /** starting value of station dimension */
    size_t _stationStart;

    /** how many stations requested */
    size_t _stationCount;

    // no copying, assignment
    NcFileSetSummary(const NcFileSetSummary&);
    NcFileSetSummary& operator=(const NcFileSetSummary&) const;
};

}   // namespace eolts

#endif
