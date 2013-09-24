// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.25 $
 *  $Date: 2009/04/01 01:35:53 $
 * 
 *  Description:
 *    A connection from Splus to one or more NetCDF files.
 * 
 */

#include <string.h>

#include <set>
#include <sstream>

#include "R_NetcdfConnection.h"

#include "R_NetcdfVariable.h"
#include "R_NamedVector.h"
#include "NcFileSetFactory.h"
#include "NetcdfReader.h"

using std::vector;
using std::string;
using std::set;
using std::pair;

/*
 * Collection of R_NetcdfConnection instances
 */
set<R_NetcdfConnection *>R_NetcdfConnection::_openSet;

SEXP R_NetcdfConnection::fileSlotName;
SEXP R_NetcdfConnection::dirSlotName;
SEXP R_NetcdfConnection::cppSlotName;

SEXP open_netcdf(SEXP obj)
{

#ifdef DEBUG
    SEXP cobj = getAttrib(obj,R_ClassSymbol);
    if (isString(cobj)) {
        Rprintf("string ClassSymbol, length=%d\n",length(cobj));
        SEXP str = STRING_ELT(cobj,0);
        string classname(CHAR(str));
        Rprintf("class=%s\n",classname.c_str());
    }
#endif
    new R_NetcdfConnection(obj);
    return obj;
}

SEXP is_netcdf_open(SEXP obj)
{
    R_NetcdfConnection *con = R_NetcdfConnection::getR_NetcdfConnection(obj);

    // return a logical
    SEXP ans = PROTECT(allocVector(LGLSXP,1));
    LOGICAL(ans)[0] = con != 0;
    UNPROTECT(1);
    return ans;
}

SEXP close_netcdf(SEXP obj)
{
    R_NetcdfConnection *con = R_NetcdfConnection::getR_NetcdfConnection(obj);
    if (!con) {
        warning("netcdf object is not open. Has it already been closed? You must reopen with netcdf(...)");
    }
    delete con;

    // return a logical
    SEXP ans = PROTECT(allocVector(LGLSXP,1));
    *LOGICAL(ans) = con != 0;
    UNPROTECT(1);       // ans
    return ans;
}

SEXP get_variables(SEXP obj)
{
#ifdef DEBUG
    Rprintf("get_variables\n");
#endif

    R_NetcdfConnection *con = R_NetcdfConnection::getR_NetcdfConnection(obj);
    if (!con) {
        error("netcdf object is not open. Has it already been closed? You must reopen with netcdf(...)");
    }

#ifdef THROW_NCEXCEPTION
    try {
        return con->getVariables();
    }
    catch (const NcException& nce) {
        error(nce.toString().c_str());
    }
    return 0;
#else
    return con->getVariables();
#endif
}

SEXP read_netcdf(SEXP obj,SEXP variables, SEXP startreq, SEXP countreq)
{
    int i;

    R_NetcdfConnection *con = R_NetcdfConnection::getR_NetcdfConnection(obj);
    if (!con) {
        error("netcdf object is not open. Has it already been closed? You must reopen with netcdf(...)");
    }

    int nvars = length(variables);
#ifdef DEBUG
    Rprintf("::read nvars = %d\n",nvars);
#endif
    if (nvars == 0)
        error("netcdf read error: length of variables argument is zero");

    vector<string> vnames;
    for (size_t i = 0; i < (unsigned)length(variables); i++) {
        SEXP dn = STRING_ELT(variables,i);
        vnames.push_back(CHAR(dn));
    }

    vector<size_t> start;
    for (i = 0; i < length(startreq); i++) start.push_back((size_t)INTEGER(startreq)[i]);

    vector<size_t> count;
    for (i = 0; i < length(countreq); i++) count.push_back((size_t)INTEGER(countreq)[i]);

#ifdef THROW_NCEXCEPTION
    try {
        return con->read(vnames,start,count);
    }
    catch (const NcException& nce) {
        PROBLEM "Error: %s",nce.toString().c_str()
            RECOVER(NULL_ENTRY);
    }
    return 0;
#else
    return con->read(vnames,start,count);
#endif
}

R_NetcdfConnection::R_NetcdfConnection(SEXP obj):_fileset(0) ,_factory(0)
{

#ifdef DEBUG
    Rprintf("R_NetcdfConnection ctor, this = %p\n",this);
#endif

    SEXP cslot = getAttrib(obj,R_NetcdfConnection::cppSlotName);

#ifdef DEBUG
    Rprintf("cpp slot length=%d, type=%d\n",length(cslot),TYPEOF(cslot));
#endif
    if (length(cslot) == 8 && TYPEOF(cslot) == RAWSXP) {
        *((R_NetcdfConnection **)RAW(cslot)) = this;
    }
    else error("slot cppPtr invalid");

    addConnection(this);

    openFileSet(obj);
}

R_NetcdfConnection::~R_NetcdfConnection()
{
    removeConnection(this);
    delete _fileset;
    delete _factory;
}

bool R_NetcdfConnection::findConnection(R_NetcdfConnection *con)
{
    // these accesses of _openSet should be protected with a mutex...
    set<R_NetcdfConnection*>::const_iterator si = _openSet.find(con);
    return si != _openSet.end();
}

bool R_NetcdfConnection::addConnection(R_NetcdfConnection *con) {

    // these accesses of _openSet should be protected with a mutex...
    if (findConnection(con)) return false;
    _openSet.insert(con);
    return true;
}

bool R_NetcdfConnection::removeConnection(R_NetcdfConnection *con)
{
    // these accesses of _openSet should be protected with a mutex...
    set<R_NetcdfConnection*>::const_iterator si = _openSet.find(con);
    if (si != _openSet.end()) {
        _openSet.erase(si);
        return true;
    }
    return false;
}

/**
 * Get pointer to C++ R_NetcdfConnection object from R object.
 * Verify that it is valid pointer.
 */
R_NetcdfConnection *R_NetcdfConnection::getR_NetcdfConnection(SEXP obj)
{
#ifdef DEBUG
    Rprintf("looking for cppPtr slot\n");
#endif
    SEXP cslot = getAttrib(obj,R_NetcdfConnection::cppSlotName);
#ifdef DEBUG
    Rprintf("cpp slot: %p, length=%d, type=%d\n",cslot,
            (cslot ? length(cslot):0),(cslot ? TYPEOF(cslot): NILSXP));
#endif
    R_NetcdfConnection *con = *(R_NetcdfConnection **)RAW(cslot);
    if (!findConnection(con)) con = 0;
    return con;
}


void R_NetcdfConnection::openFileSet(SEXP obj)
{
    SEXP fslot = getAttrib(obj,R_NetcdfConnection::fileSlotName);
#ifdef DEBUG
    Rprintf("file slot length=%d, type=%d\n",length(fslot),TYPEOF(fslot));
#endif

    vector<string> fnames;
    for (size_t i = 0; i < (unsigned)length(fslot); i++) {
        SEXP dn = STRING_ELT(fslot,i);
        fnames.push_back(CHAR(dn));
    }

    SEXP dslot = getAttrib(obj,R_NetcdfConnection::dirSlotName);
#ifdef DEBUG
    Rprintf("dir slot length=%d, type=%d\n",length(dslot),TYPEOF(dslot));
#endif

    vector<string> dnames;
    for (size_t i = 0; i < (unsigned)length(dslot); i++) {
        SEXP dn = STRING_ELT(dslot,i);
        dnames.push_back(CHAR(dn));
    }

    vector<string> fullnames = makeFileNameList(fnames,dnames);

    _fileset = getNcFileSetFactory()->createNcFileSet(fullnames);
}

vector<string> R_NetcdfConnection::makeFileNameList(const vector<string>& fnames,
        const vector<string>& dnames)
{
    vector<string> res;
    const char pathSeparator = '/';
    for (unsigned int i = 0; i < fnames.size(); i++) {
        unsigned int nd = 0;
        if (dnames.size() > 0) nd = i % dnames.size();
        if (dnames[nd].size() > 0)
            res.push_back(dnames[nd] + pathSeparator + fnames[i]);
        else
            res.push_back(fnames[i]);
    }
    return res;
}

NcFileSetFactory* R_NetcdfConnection::getNcFileSetFactory() {
    if (!_factory) _factory = new NcFileSetFactory();
    return _factory;
}

SEXP R_NetcdfConnection::getVariables() NCEXCEPTION_CLAUSE
{
#ifdef DEBUG
        Rprintf("getVariables\n");
#endif

    int nfiles = _fileset->getNFiles();

    set<string> vnames;

    vector<NcVar *> vars;

    for (int ifile = 0; ifile < nfiles; ifile++) {
        NcFile* ncf = _fileset->getNcFile(ifile);
        if (!ncf) continue;
#ifdef DEBUG
        Rprintf("file=%s, open=%d\n",ncf->getName().c_str(),ncf->isOpen());
#endif
        if (!ncf->isOpen()) continue;

        int nvars = ncf->getNumVariables();
        for (int ivar = 0; ivar < nvars; ivar++) {
            NcVar* var = ncf->getVariable(ivar);
            if (!var) {
                std::ostringstream ost;
                ost << "bad variable in " << ncf->getName();
                error(ost.str().c_str());
            }
#ifdef DEBUG
            Rprintf("file=%s, var=%s\n",ncf->getName().c_str(),var->getName().c_str());
#endif
            if (vnames.find(var->getName()) == vnames.end()) {
                vnames.insert(var->getName());
                vars.push_back(var);
            }
        }
    }

    SEXP result = PROTECT(allocVector(VECSXP,vars.size()));
    SEXP resnames = getAttrib(result,R_NamesSymbol);

    if (!resnames || TYPEOF(resnames) != STRSXP ||
        (unsigned) length(resnames) != vars.size()) {
#ifdef DEBUG
        Rprintf("allocating R_NamesSymbol for variables\n");
#endif
        resnames = PROTECT(allocVector(STRSXP,vars.size()));
        setAttrib(result,R_NamesSymbol,resnames);
        UNPROTECT(1);   // resnames
    }
#ifdef DEBUG
    Rprintf("resnames length=%d\n",length(resnames));
#endif

    for (unsigned int i = 0; i < vars.size(); i++) {
        R_NetcdfVariable rvar(this,vars[i]);
        SET_STRING_ELT(resnames,i,mkChar(rvar.getName().c_str()));
        SET_VECTOR_ELT(result,i,rvar.getRObject());
    }
    UNPROTECT(1);       // result
    return result;
}

SEXP R_NetcdfConnection::read(const vector<string> &vnames,
        const vector<size_t> &start, const vector<size_t> &count)
    NCEXCEPTION_CLAUSE
{

    NetcdfReader reader(this);

    return reader.read(vnames,start,count);
}


