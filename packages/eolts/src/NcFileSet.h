// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.14 $
 *  $Date: 2006/03/29 03:28:17 $
 * 
 *  Description:
 *    A collection of NetCDF files
 * 
 */
#ifndef NCFILESET_H
#define NCFILESET_H

#include <vector>
#include <string>

#include "NcFile.h"
#include "NcVar.h"
#include "NcDim.h"

class NcFileSetFactory;

/*
 * An ordered sequence of files, not all of which are open.
 * They are accessed in sequence, which allows some simplification
 * in how they are managed.
 */
class NcFileSet {
protected:
    const int MAX_FILES_OPEN;
    NcFile **_files;
    int _nfiles;
    int _nopen;
    int _nextToOpen;
    int _nextToClose;

    // int _hasNonStationTSVariables;

public:

    NcFileSet(const std::vector<std::string>& fnames,NcFileSetFactory* factory);
    virtual ~NcFileSet();

    int getNFiles() const { return _nfiles; }
    NcFile *getNcFile(int ifile);
    const std::string& getFileName(int ifile);

    std::vector<std::string> getVariableNames() NCEXCEPTION_CLAUSE;
    // std::vector<std::string> getStationNames();
    // int hasNonStationTSVariables();
    const std::vector<const NcDim*>& getVariableDimensions(std::string vname) NCEXCEPTION_CLAUSE;

};

#endif
