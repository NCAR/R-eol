// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:

/*
 *               Copyright (C) by UCAR
 */

// #include <string.h>

#include <set>
#include <sstream>

#include "R_netcdf.h"

#include "R_NetcdfVariable.h"
#include "R_NamedVector.h"
#include "R_utime.h"
#include "NetcdfReader.h"

using std::vector;
using std::string;
using std::set;

using namespace eolts;

/*
 * Collection of R_netcdf instances
 */
set<R_netcdf *>R_netcdf::_openSet;

SEXP R_netcdf::fileSlotName;
SEXP R_netcdf::dirSlotName;
SEXP R_netcdf::timeNamesSlotName;
SEXP R_netcdf::cppSlotName;

SEXP open_netcdf(SEXP con,SEXP cdlfile, SEXP rpcTimeout, SEXP rpcBatchPeriod)
{

#ifdef DEBUG
    SEXP cobj = getAttrib(con,R_ClassSymbol);
    if (isString(cobj)) {
        Rprintf("string ClassSymbol, length=%d\n",length(cobj));
        SEXP str = STRING_ELT(cobj,0);
        string classname(CHAR(str));
        Rprintf("class=%s\n",classname.c_str());
    }
#endif
    new R_netcdf(con,cdlfile,rpcTimeout,rpcBatchPeriod);
    return con;
}

SEXP is_netcdf_open(SEXP obj)
{
    R_netcdf *con = R_netcdf::getR_netcdf(obj);

    // return a logical
    SEXP ans = PROTECT(allocVector(LGLSXP,1));
    LOGICAL(ans)[0] = con != 0;
    UNPROTECT(1);
    return ans;
}

SEXP close_netcdf(SEXP obj)
{
    R_netcdf *con = R_netcdf::getR_netcdf(obj);
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

SEXP get_variables(SEXP obj, SEXP allobj)
{
#ifdef DEBUG
    Rprintf("get_variables\n");
#endif

    R_netcdf *con = R_netcdf::getR_netcdf(obj);
    if (!con) {
        error("netcdf object is not open. Has it already been closed? You must reopen with netcdf(...)");
    }

    bool all = LOGICAL(allobj)[0];

    try {
        if (all) return con->getVariables();
        else return con->getTimeSeriesVariables();
    }
    catch (const NcException& nce) {
        error(nce.toString().c_str());
    }
    return 0;
}

SEXP get_stations(SEXP obj)
{
#ifdef DEBUG
    Rprintf("get_variables\n");
#endif

    R_netcdf *con = R_netcdf::getR_netcdf(obj);
    if (!con) {
        error("netcdf object is not open. Has it already been closed? You must reopen with netcdf(...)");
    }

    try {
        return con->getStations();
    }
    catch (const NcException& nce) {
        error(nce.toString().c_str());
    }
    return 0;
}

SEXP read_netcdf(SEXP obj,SEXP variables, SEXP startreq, SEXP countreq)
{
    int i;

    R_netcdf *con = R_netcdf::getR_netcdf(obj);
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

    try {
        return con->read(vnames,start,count);
    }
    catch (const NcException& nce) {
        error(nce.toString().c_str());
    }
    return 0;
}

SEXP read_netcdf_ts(SEXP args)
{
    vector<string> vnames;
    double startTime=0.0,endTime=0.0;
    std::vector<int> stations;
    std::vector<std::string> tnames;
    std::string btname;
    std::string timezone;
    SEXP con=0;

#ifdef DEBUG
    int iarg = 1;
#endif

    args = CDR(args);
    if (args != R_NilValue) {
#ifdef DEBUG
        Rprintf("read_netcdf_ts: arg #%d has name %s\n",
                iarg++,isNull(TAG(args)) ? "none" : CHAR(PRINTNAME(TAG(args))));
#endif
        con = CAR(args);
        args = CDR(args);
    }
    if (args != R_NilValue) {
        SEXP vars = CAR(args);
        unsigned int nvars = length(vars);
        if (nvars == 0)
            error("netcdf read error: length of variables argument is zero");

        for (size_t i = 0; i < nvars; i++) {
            SEXP dn = STRING_ELT(vars,i);
            vnames.push_back(CHAR(dn));
        }
        args = CDR(args);
    }
    if (args != R_NilValue) {
#ifdef DEBUG
        Rprintf("read_netcdf_ts, TYPEOF(args)=%d\n", TYPEOF(args));
#endif
        SEXP obj = CAR(args);
#ifdef DEBUG
        Rprintf("read_netcdf_ts, TYPEOF(obj)=%d\n", TYPEOF(obj));
#endif
        R_utime ut(obj);
        startTime = ut.getTime(0);
        args = CDR(args);
    }
    if (args != R_NilValue) {
        SEXP obj = CAR(args);
        R_utime ut(obj);
        endTime = ut.getTime(0);
        args = CDR(args);
    }
    if (args != R_NilValue) {
        SEXP obj = CAR(args);
        R_NamedVector<int> stns(INTSXP,obj);
        int *iptr = stns.getDataPtr();
        for (unsigned int i = 0; i < stns.getLength(); i++)
            stations.push_back(*iptr++);
        args = CDR(args);
    }
    if (args != R_NilValue) {
        SEXP obj = CAR(args);
        for (int i = 0; i < length(obj); i++)
            tnames.push_back(CHAR(STRING_ELT(obj,i)));
        args = CDR(args);
    }
    if (args != R_NilValue) {
        SEXP obj = CAR(args);
        btname = string(CHAR(STRING_ELT(obj,0)));
        args = CDR(args);
    }
    if (args != R_NilValue) {
        SEXP obj = CAR(args);
        timezone = string(CHAR(STRING_ELT(obj,0)));
        args = CDR(args);
    }
    
    R_netcdf *nccon = R_netcdf::getR_netcdf(con);
    if (!nccon) {
        error("netcdf object is not open. Has it already been closed? You must reopen with netcdf(...)");
    }

    try {
        return nccon->read(vnames,startTime,endTime,stations,tnames,btname,timezone);
    }
    catch (const NcException& nce) {
        error(nce.toString().c_str());
    }
    return 0;
}

R_netcdf::R_netcdf(SEXP con, SEXP cdlfile,
        SEXP rpcTimeout, SEXP rpcBatchPeriod):_fileset(0)
{

#ifdef DEBUG
    Rprintf("R_netcdf ctor, this = %p\n",this);
#endif

    SEXP cslot = getAttrib(con,cppSlotName);

#ifdef DEBUG
    Rprintf("cpp slot length=%d, type=%d\n",length(cslot),TYPEOF(cslot));
#endif

    // store a pointer to the C++ object into the cppPtr slot of the
    // con argument.
    if (TYPEOF(cslot) != RAWSXP || length(cslot) != 8)
        error("slot cppPtr of object is not of type \"raw\", length 8");

    *((R_netcdf **)RAW(cslot)) = this;

    addConnection(this);

    openFileSet(con);
}

R_netcdf::~R_netcdf()
{
    removeConnection(this);
    delete _fileset;
}

bool R_netcdf::findConnection(R_netcdf *con)
{
    // these accesses of _openSet should be protected with a mutex...
    set<R_netcdf*>::const_iterator si = _openSet.find(con);
    return si != _openSet.end();
}

bool R_netcdf::addConnection(R_netcdf *con) {

    // these accesses of _openSet should be protected with a mutex...
    if (findConnection(con)) return false;
    _openSet.insert(con);
    return true;
}

bool R_netcdf::removeConnection(R_netcdf *con)
{
    // these accesses of _openSet should be protected with a mutex...
    set<R_netcdf*>::const_iterator si = _openSet.find(con);
    if (si != _openSet.end()) {
        _openSet.erase(si);
        return true;
    }
    return false;
}

/**
 * Get pointer to C++ R_netcdf object from R object.
 * Verify that it is valid pointer.
 */
R_netcdf *R_netcdf::getR_netcdf(SEXP obj)
{
#ifdef DEBUG
    Rprintf("looking for cppPtr slot\n");
#endif
    SEXP cslot = getAttrib(obj,cppSlotName);
#ifdef DEBUG
    Rprintf("cpp slot: %p, length=%d, type=%d\n",cslot,
            (cslot ? length(cslot):0),(cslot ? TYPEOF(cslot): NILSXP));
#endif
    R_netcdf *con = *(R_netcdf **)RAW(cslot);
    if (!findConnection(con)) con = 0;
    return con;
}


void R_netcdf::openFileSet(SEXP obj)
{
    SEXP slot = getAttrib(obj,fileSlotName);
#ifdef DEBUG
    Rprintf("file slot length=%d, type=%d\n",length(slot),TYPEOF(slot));
#endif

    vector<string> fnames;
    for (size_t i = 0; i < (unsigned)length(slot); i++) {
        SEXP dn = STRING_ELT(slot,i);
        fnames.push_back(CHAR(dn));
    }

    slot = getAttrib(obj,dirSlotName);
#ifdef DEBUG
    Rprintf("dir slot length=%d, type=%d\n",length(slot),TYPEOF(slot));
#endif

    vector<string> dnames;
    for (size_t i = 0; i < (unsigned)length(slot); i++) {
        SEXP dn = STRING_ELT(slot,i);
        dnames.push_back(CHAR(dn));
    }

    vector<string> fullnames = makeFileNameList(fnames,dnames);

    slot = getAttrib(obj,timeNamesSlotName);

    vector<string> tnames;
    for (size_t i = 0; i < (unsigned)length(slot); i++) {
        SEXP dn = STRING_ELT(slot,i);
        tnames.push_back(CHAR(dn));
    }

    delete _fileset;
    _fileset = new NcFileSet(fullnames,tnames);
}

vector<string> R_netcdf::makeFileNameList(const vector<string>& fnames,
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

SEXP R_netcdf::getVariables() throw(NcException)
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

SEXP R_netcdf::getTimeSeriesVariables() throw(NcException)
{

    int nfiles = _fileset->getNFiles();

    set<string> vnames;

    vector<NcVar *> tvars;
    const set<string>& timeDimensionNames = _fileset->getTimeDimensionNames();

    for (int ifile = 0; ifile < nfiles; ifile++) {
        NcFile* ncf = _fileset->getNcFile(ifile);
        if (!ncf) continue;
        if (!ncf->isOpen()) continue;

        const NcDim* timeDim = ncf->getTimeDimension(timeDimensionNames);

        vector<NcVar*> vars = ncf->getTimeSeriesVariables(timeDim);

        for (unsigned int ivar = 0; ivar < vars.size(); ivar++) {
            if (vnames.find(vars[ivar]->getName()) == vnames.end()) {
                vnames.insert(vars[ivar]->getName());
                tvars.push_back(vars[ivar]);
            }
        }
    }

    SEXP result = PROTECT(allocVector(VECSXP,tvars.size()));
    SEXP resnames = getAttrib(result,R_NamesSymbol);

    if (!resnames || TYPEOF(resnames) != STRSXP ||
        (unsigned) length(resnames) != tvars.size()) {
#ifdef DEBUG
        Rprintf("allocating R_NamesSymbol for variables\n");
#endif
        resnames = PROTECT(allocVector(STRSXP,tvars.size()));
        setAttrib(result,R_NamesSymbol,resnames);
        UNPROTECT(1);   // resnames
    }

    for (unsigned int i = 0; i < tvars.size(); i++) {
        R_NetcdfVariable rvar(this,tvars[i]);
        SET_STRING_ELT(resnames,i,mkChar(rvar.getName().c_str()));
        SET_VECTOR_ELT(result,i,rvar.getRObject());
    }
    UNPROTECT(1);       // result
    return result;
}

SEXP R_netcdf::getStations() throw(NcException)
{
    getFileSet();
    std::map<int,string> stations = _fileset->getStations();

#ifdef DEBUG
    Rprintf("stations.size=%zd\n",stations.size());
#endif

    R_NamedVector<int> namedStations(INTSXP,stations.size());

    int *stnptr = namedStations.getDataPtr();
    vector<string> names;

    int i = 0;
    std::map<int,string>::const_iterator itr;
    for (itr = stations.begin(); itr != stations.end(); ++itr) {
        stnptr[i++] = itr->first;
        names.push_back(itr->second);
    }

    namedStations.setNames(names);
    return namedStations.getRObject();
}

SEXP R_netcdf::read(const vector<string> &vnames,
        const vector<size_t> &start, const vector<size_t> &count)
    throw(NcException)
{

    NetcdfReader reader(this);

    return reader.read(vnames,start,count);
}

SEXP R_netcdf::read(const vector<string> &vnames,
        double start, double end,
        const vector<int> &stations,
        const vector<string> &tnames,
        const string &btname,
        const string& timezone)
    throw(NcException)
{
    NetcdfReader reader(this);
    return reader.read(vnames,start,end,stations,tnames,btname,timezone);
}
