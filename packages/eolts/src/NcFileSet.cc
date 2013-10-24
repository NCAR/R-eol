// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.21 $
 *  $Date: 2008/09/09 23:42:36 $
 * 
 *  Description:
 *    A collection of NetCDF files
 * 
 */

// #include <string.h>

#include <vector>
#include <string>
#include <set>
#include <map>

#include "NcFileSet.h"
#include "NcFile.h"
#include "NcVar.h"

#include <R.h>  // Rprintf()

using std::vector;
using std::string;
using std::set;
using std::map;

using namespace eolts;

NcFileSet::NcFileSet(const vector<string>& fnames):
    MAX_FILES_OPEN(40),_files(0),_nfiles(fnames.size()),
    _nopen(0),_nextToOpen(0),_nextToClose(0),
    _possibleTimeDimensionNames()
{
    _files = new NcFile*[_nfiles];

    for (int i = 0; i < _nfiles; i++) 
        _files[i] = new NcFile(fnames[i]);

    addTimeDimensionName(string("time"));
}

NcFileSet::~NcFileSet() {
#ifdef DEBUG
    Rprintf("~NcFileSet\n");
#endif
    int i;
    for (i = 0; i < _nfiles; i++) delete _files[i];
    delete [] _files;
#ifdef DEBUG
    Rprintf("~NcFileSet done\n");
#endif
}

void NcFileSet::addTimeDimensionName(const string& name)
{
    _possibleTimeDimensionNames.insert(name);
}

NcFile* NcFileSet::getNcFile(int ifile)
{
    if (ifile < 0 || ifile >= _nfiles) return 0;

    NcFile*ncf = _files[ifile];
    if (!ncf) return 0;

    if (!ncf->isOpen()) {
        try {
            ncf->open();
        }
        catch (const NcException &nce) {
            Rprintf("Notice: %s\n",nce.what());
            return ncf;	// not a fatal error
        }
        _nopen++;

        while (_nopen >= MAX_FILES_OPEN) {
            NcFile* fst = _files[_nextToClose++];
            if (fst != ncf && fst->isOpen()) {
                try {
                    fst->close();
                }
                catch (const NcException &nce) {
                }
                _nopen--;
            }
            if (_nextToClose == _nfiles) _nextToClose = 0;
        }
    }
    return ncf;
}
const string& NcFileSet::getFileName(int ifile) {
    static string res;
    NcFile* ncf = getNcFile(ifile);
    if (!ncf) return res;
    return ncf->getName();
}

/**
 * Return an vector of names of all variables.
 */
vector<string> NcFileSet::getVariableNames() throw(NcException)
{

    set<string> vset;

    for (int ifile = 0; ifile < getNFiles(); ifile++) {
        NcFile* ncf = getNcFile(ifile);
        if (ncf == 0 || !ncf->isOpen()) continue;
        vector<string> vnames = ncf->getVariableNames();
        vset.insert(vnames.begin(),vnames.end());
    }

    return vector<string>(vset.begin(), vset.end());
}

const vector<const NcDim*>& NcFileSet::getVariableDimensions(string vname)
    throw(NcException)
{

    vector<NcDim*> dims;
    for (int ifile = 0; ifile < getNFiles(); ifile++) {
        NcFile* ncf = getNcFile(ifile);
        if (ncf == 0 || !ncf->isOpen()) continue;

        NcVar *var = ncf->getVariable(vname);
        if (!var) continue;
        return var->getDimensions();
    }
    static vector<const NcDim*> res;
    return res;
}

map<int,string> NcFileSet::getStations() throw(NcException)
{

    size_t nstations = 0;
    map<int,string> stations;
    for (int ifile = 0; ifile < getNFiles(); ifile++) {
        NcFile* ncf = getNcFile(ifile);
        if (ncf == 0 || !ncf->isOpen()) continue;

        const NcDim* stnDim = ncf->getDimension("station");
        if (stnDim && stnDim->getLength() > nstations)
                nstations = stnDim->getLength();

        const NcDim* tdim = ncf->getTimeDimension(_possibleTimeDimensionNames);
        stations = ncf->getStations(tdim);
        // stations may have an extra member for station 0
        if (stations.size() > 0) return stations;
    }

    char cname[6];
    for (unsigned int i = 0; i < nstations; i++) {
        if (stations.find(i+1) == stations.end()) {
            sprintf(cname,"%d",i+1);
            stations[i+1] = string(cname);
        }
    }
    return stations;
}

