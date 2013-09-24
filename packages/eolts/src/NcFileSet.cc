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
#include <R.h>

#include <string.h>

#include <vector>
#include <string>
#include <set>

using std::vector;
using std::string;
using std::set;
using std::pair;

#include "NcFileSet.h"
#include "NcFileSetFactory.h"
#include "NcFile.h"
#include "NcVar.h"

NcFileSet::NcFileSet(const vector<string>& fnames,NcFileSetFactory *factory):
    MAX_FILES_OPEN(40),_files(0),_nfiles(fnames.size()),
    _nopen(0),_nextToOpen(0),_nextToClose(0)
{
    _files = new NcFile*[_nfiles];

    for (int i = 0; i < _nfiles; i++) 
        _files[i] = factory->createNcFile(fnames[i]);
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

NcFile* NcFileSet::getNcFile(int ifile) {
    if (ifile < 0 || ifile >= _nfiles) return 0;

    NcFile*ncf = _files[ifile];
    if (!ncf) return 0;

    if (!ncf->isOpen()) {
#ifdef THROW_NCEXCEPTION
        try {
            ncf->open();
        }
        catch (const NcException &nce) {
            Rprintf("Notice: %s\n",nce.what());
            return ncf;	// not a fatal error
        }
#else
        ncf->open();
        if (!ncf->isValid()) return ncf;
#endif
        _nopen++;

        while (_nopen >= MAX_FILES_OPEN) {
            NcFile* fst = _files[_nextToClose++];
            if (fst != ncf && fst->isOpen()) {
#ifdef THROW_NCEXCEPTION
                try {
                    fst->close();
                }
                catch (const NcException &nce) {
                }
#else
                fst->close();
#endif
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
vector<string> NcFileSet::getVariableNames() NCEXCEPTION_CLAUSE
{

    set<string> vset;

    for (int ifile = 0; ifile < getNFiles(); ifile++) {
        NcFile* ncf = getNcFile(ifile);
        if (ncf == 0 || !ncf->isOpen()) continue;
        vector<string> vnames = ncf->getVariableNames();
        vset.insert(vnames.begin(),vnames.end());
    }

#ifdef sun
    vector<string> vvect;
    set<string>::iterator si;
    for (si = vset.begin(); si != vset.end(); si++) vvect.push_back(*si);
    return vvect;
#else
    return vector<string>(vset.begin(), vset.end());
#endif
}

const vector<const NcDim*>& NcFileSet::getVariableDimensions(string vname)
    NCEXCEPTION_CLAUSE
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

