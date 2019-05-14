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
 *    A collection of NetCDF files
 * 
 */
#ifndef NCFILESET_H
#define NCFILESET_H

#include <vector>
#include <set>
#include <map>
#include <string>

#include "NcFile.h"
#include "NcVar.h"
#include "NcDim.h"

namespace eolts {

/*
 * An ordered sequence of files, not all of which are open.
 * They are accessed in sequence, which allows some simplification
 * in how they are managed.
 */
class NcFileSet {
public:

    NcFileSet(const std::vector<std::string>& fnames,
        const std::vector<std::string>& tnames);
    virtual ~NcFileSet();

    int getNFiles() const { return _nfiles; }
    NcFile *getNcFile(int ifile);
    const std::string& getFileName(int ifile);

    std::vector<std::string> getVariableNames();

    // std::vector<std::string> getStationNames();
    // int hasNonStationTSVariables();

    const std::vector<const NcDim*>& getVariableDimensions(std::string vname);

    void addTimeDimensionName(const std::string& name);


    const std::set<std::string>& getTimeDimensionNames() const
    {
        return _possibleTimeDimensionNames;
    }

    NcVar* getTimeVariable(NcFile* ncf);

    /**
     * Retrieve mapping of station number to name, read from
     * first file with a station dimension and variable.
     */
    std::map<int,std::string> getStations();

private:
    const int MAX_FILES_OPEN;

    NcFile **_files;

    int _nfiles;

    int _nopen;

    int _nextToOpen;

    int _nextToClose;

    // int _hasNonStationTSVariables;
    //

    std::set<std::string> _possibleTimeDimensionNames;

    // no copying, assignment
    NcFileSet(const NcFileSet&);
    NcFileSet& operator=(const NcFileSet&) const;

};

}   // namespace eolts

#endif
