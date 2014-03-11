// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:

/*
 *               Copyright (C) by UCAR
 */

#include "R_netcdf.h"
#include "R_NetcdfVariable.h"
#include "R_NamedVector.h"
#include "R_utime.h"
#include "NetcdfReader.h"
#include "R_nts.h"

#include <set>
#include <sstream>

using std::string;
using std::vector;
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
SEXP R_netcdf::serverSlotName;
SEXP R_netcdf::intervalSlotName;
SEXP R_netcdf::lenfileSlotName;

/** Utility functions in anonymous namespace */
namespace {

    vector<string> makeFileNameList(const string& dir, const vector<string>& fnames)
    {
        vector<string> res;
        const char pathSeparator = '/';
        for (unsigned int i = 0; i < fnames.size(); i++) {
            if (dir.length() > 0)
                res.push_back(dir + pathSeparator + fnames[i]);
            else
                res.push_back(fnames[i]);
        }
        return res;
    }

}

SEXP open_netcdf(SEXP con,SEXP files, SEXP cdlfile, SEXP rpcTimeout, SEXP rpcBatchPeriod)
{

#ifdef DEBUG
    SEXP cobj = Rf_getAttrib(con,R_ClassSymbol);
    if (isString(cobj)) {
        Rprintf("string ClassSymbol, length=%d\n",Rf_length(cobj));
        SEXP str = STRING_ELT(cobj,0);
        string classname(CHAR(str));
        Rprintf("class=%s\n",classname.c_str());
    }
#endif
    new R_netcdf(con,files,cdlfile,rpcTimeout,rpcBatchPeriod);
    return con;
}

SEXP is_netcdf_open(SEXP obj)
{
    R_netcdf *con = R_netcdf::getR_netcdf(obj);

    // return a logical
    SEXP ans = PROTECT(Rf_allocVector(LGLSXP,1));
    LOGICAL(ans)[0] = con != 0;
    UNPROTECT(1);
    return ans;
}

SEXP close_netcdf(SEXP obj)
{
    R_netcdf *con = R_netcdf::getR_netcdf(obj);
    if (!con) {
        Rf_warning("netcdf object is not open. Has it already been closed? You must reopen with netcdf(...)");
    }
    delete con;

    // return a logical
    SEXP ans = PROTECT(Rf_allocVector(LGLSXP,1));
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
        Rf_error("netcdf object is not open. Has it already been closed? You must reopen with netcdf(...)");
    }

    bool all = LOGICAL(allobj)[0];

    try {
        if (all) return con->getVariables();
        else return con->getTimeSeriesVariables();
    }
    catch (const NcException& nce) {
        Rf_error(nce.toString().c_str());
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
        Rf_error("netcdf object is not open. Has it already been closed? You must reopen with netcdf(...)");
    }

    try {
        return con->getStations();
    }
    catch (const NcException& nce) {
        Rf_error(nce.toString().c_str());
    }
    return 0;
}

SEXP read_netcdf(SEXP obj,SEXP variables, SEXP startreq, SEXP countreq)
{
    int i;

    R_netcdf *con = R_netcdf::getR_netcdf(obj);
    if (!con) {
        Rf_error("netcdf object is not open. Has it already been closed? You must reopen with netcdf(...)");
    }

    int nvars = Rf_length(variables);
#ifdef DEBUG
    Rprintf("::read nvars = %d\n",nvars);
#endif
    if (nvars == 0)
        Rf_error("netcdf read error: length of variables argument is zero");

    vector<string> vnames;
    for (size_t i = 0; i < (size_t)Rf_length(variables); i++) {
        SEXP dn = STRING_ELT(variables,i);
        vnames.push_back(CHAR(dn));
    }

    vector<size_t> start;
    for (i = 0; i < Rf_length(startreq); i++) start.push_back((size_t)INTEGER(startreq)[i]);

    vector<size_t> count;
    for (i = 0; i < Rf_length(countreq); i++) count.push_back((size_t)INTEGER(countreq)[i]);

    try {
        return con->read(vnames,start,count);
    }
    catch (const NcException& nce) {
        Rf_error(nce.toString().c_str());
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
                iarg++,Rf_isNull(TAG(args)) ? "none" : CHAR(PRINTNAME(TAG(args))));
#endif
        con = CAR(args);
        args = CDR(args);
    }
    if (args != R_NilValue) {
        SEXP vars = CAR(args);
        size_t nvars = Rf_xlength(vars);
        if (nvars == 0)
            Rf_error("netcdf read error: length of variables argument is zero");

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
        for (size_t i = 0; i < (size_t)Rf_xlength(obj); i++)
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
        Rf_error("netcdf object is not open. Has it already been closed? You must reopen with netcdf(...)");
    }

    try {
        return nccon->read(vnames,startTime,endTime,stations,tnames,btname,timezone);
    }
    catch (const NcException& nce) {
        Rf_error(nce.toString().c_str());
    }
    return 0;
}

#ifdef HAVE_NC_SERVER

SEXP write_ts_ns(SEXP args)
{
    args = CDR(args);

    if (args == R_NilValue) Rf_error("connection not specified");
    SEXP obj = CAR(args);
    R_netcdf *con = eolts::R_netcdf::getR_netcdf(obj);
    if (!con) {
        Rf_error("netcdf object is not open. Has it already been closed? You must reopen with netcdf(...)");
    }

    args = CDR(args);

    SEXP datap = 0;
    if (args != R_NilValue) {
        datap = CAR(args);
        args = CDR(args);
    }
    eolts::R_nts nts(datap);

    vector<string> nonTimeDimNames;
    if (args != R_NilValue) {
        SEXP robj = CAR(args);
        size_t ndims = (size_t)Rf_xlength(robj);
        for (size_t i = 0; i < ndims; i++) {
            SEXP dn = STRING_ELT(robj,i);
            nonTimeDimNames.push_back(CHAR(dn));
        }
        args = CDR(args);
    }

    vector<eolts::NcDim> nonTimeDims;
    if (args != R_NilValue) {
        SEXP robj = CAR(args);
        size_t ndims = Rf_xlength(robj);
        if (ndims != nonTimeDimNames.size())
            Rf_error("write_ts_ns error: length of non_time_dimensions argument is not the same as the length of the non_time_dimension names");

        for (size_t i = 0; i < ndims; i++) {
            nonTimeDims.push_back(NcDim(nonTimeDimNames[i],(size_t)INTEGER(robj)[i]));
        }
        args = CDR(args);
    }

    double dt = 0.0;
    if (args != R_NilValue) {
        SEXP robj = CAR(args);
        if (Rf_length(robj) != 1)
            Rf_error("write_ts_ns error: length of dt argument is not one");
        dt = REAL(robj)[0];
        args = CDR(args);
    }

    double fill = 1.e37;
    if (args != R_NilValue) {
        SEXP robj = CAR(args);
        if (Rf_length(robj) != 1)
            Rf_error("write_ts_ns error: length of fill_value argument is not one");
        fill = REAL(robj)[0];
        args = CDR(args);
    }

    try {
        con->write(nts,nonTimeDims,dt,fill);
    }
    catch (const RPC_Exception& rpcerr) {
        Rf_error(rpcerr.toString().c_str());
    }

    SEXP ans = PROTECT(Rf_allocVector(INTSXP,1));
    INTEGER(ans)[0] = 0;
    UNPROTECT(1);
    return ans;
}

SEXP write_history_ns(SEXP args)
{
    args = CDR(args);

    if (args == R_NilValue) Rf_error("connection not specified");
    SEXP obj = CAR(args);
    R_netcdf *con = eolts::R_netcdf::getR_netcdf(obj);
    if (!con) {
        Rf_error("netcdf object is not open. Has it already been closed? You must reopen with netcdf(...)");
    }

    args = CDR(args);

    vector<string> history;
    if (args != R_NilValue) {
        SEXP robj = CAR(args);
        size_t ndims = Rf_xlength(robj);
        for (size_t i = 0; i < ndims; i++) {
            SEXP dn = STRING_ELT(robj,i);
            history.push_back(CHAR(dn));
        }
        args = CDR(args);
    }

    try {
        for (unsigned int i = 0; i < history.size(); i++) 
            con->writeHistory(history[i]);
    }
    catch (const RPC_Exception& e) {
        Rf_error(e.what());
    }

    SEXP ans = PROTECT(Rf_allocVector(INTSXP,1));
    INTEGER(ans)[0] = 0;
    UNPROTECT(1);
    return ans;
}

#endif

R_netcdf::R_netcdf(SEXP con, SEXP files, SEXP cdlfile,
        SEXP rpcTimeout, SEXP rpcBatchPeriod):
    _dir(),_filenamefmt(),_fileset(0)
#ifdef HAVE_NC_SERVER
    ,_server(),_cdlfile(),
    _lenfile(0),_interval(0),_clnt(0),_id(-1),
    _rpcBatchPeriod(),_rpcWriteTimeout(),_rpcOtherTimeout(),
    _batchTimeout(),_ntry(0),_NTRY(10),
    _lastNonBatchWrite(::time(0)),_groups()
#endif
{

#ifdef DEBUG
    Rprintf("R_netcdf ctor, this = %p\n",this);
#endif

    SEXP slot = Rf_getAttrib(con,cppSlotName);

#ifdef DEBUG
    Rprintf("cpp slot length=%d, type=%d\n",Rf_length(slot),TYPEOF(slot));
#endif

    // store a pointer to the C++ object into the cppPtr slot of the
    // con argument.
    if (TYPEOF(slot) != RAWSXP || Rf_length(slot) != 8)
        Rf_error("cppPtr of object is not of type \"raw\", length 8");

    *((R_netcdf **)RAW(slot)) = this;

#ifdef HAVE_NC_SERVER
    slot = Rf_getAttrib(con,serverSlotName);
    if (TYPEOF(slot) != STRSXP || Rf_length(slot) != 1)
        Rf_error("server slot of netcdf object is not string, length 1");
    _server = string(CHAR(STRING_ELT(slot,0)));

    slot = Rf_getAttrib(con,intervalSlotName);
    if (TYPEOF(slot) != REALSXP || Rf_length(slot) != 1)
        Rf_error("interval slot of netcdf object is not numeric, length 1");
    _interval = REAL(slot)[0];

    slot = Rf_getAttrib(con,lenfileSlotName);
    if (TYPEOF(slot) != INTSXP || Rf_length(slot) != 1)
        Rf_error("lenfile slot of netcdf object is not integer, length 1");
    _lenfile = INTEGER(slot)[0];

    /*
     * Set the RPC timeouts for open/close, and for writes when
     * _rpcBatchPeriod is 0, or when the batch
     * queue is flushed every _rpcBatchPeriod seconds.
     */
    if (TYPEOF(rpcTimeout) != INTSXP || Rf_length(rpcTimeout) != 1)
        Rf_error("rpcTimeout is not integer, length 1");
    setRPCTimeoutSecs(INTEGER(rpcTimeout)[0]);

    if (TYPEOF(rpcBatchPeriod) != INTSXP || Rf_length(rpcBatchPeriod) != 1)
        Rf_error("rpcBatchPeriod is not integer, length 1");
    _rpcBatchPeriod = INTEGER(rpcBatchPeriod)[0];
#endif

    slot = Rf_getAttrib(con,fileSlotName);
    if (TYPEOF(slot) != STRSXP || Rf_length(slot) != 1)
        Rf_error("file slot of netcdf object is not string, length 1");
    _filenamefmt = string(CHAR(STRING_ELT(slot,0)));

    slot = Rf_getAttrib(con,dirSlotName);
    if (TYPEOF(slot) != STRSXP || Rf_length(slot) != 1)
        Rf_error("directory slot of netcdf object is not string, length 1");
    _dir = string(CHAR(STRING_ELT(slot,0)));

    addConnection(this);

    openFileSet(con,files);
}

R_netcdf::~R_netcdf()
{
#ifdef HAVE_NC_SERVER
    if (_clnt) {
        int result = 0;
        enum clnt_stat clnt_stat;

        if ((clnt_stat = clnt_call (_clnt, SYNCFILES,
                (xdrproc_t) xdr_void, (caddr_t) NULL,
                (xdrproc_t) xdr_int, (caddr_t) &result,
                _rpcOtherTimeout)) != RPC_SUCCESS)
        {
            Rf_warning(clnt_sperror(_clnt,"nc_server sync failed"));
        }

        if ((clnt_stat = clnt_call(_clnt, CLOSECONNECTION,
                        (xdrproc_t) xdr_int, (caddr_t) &_id,
                        (xdrproc_t) xdr_int, (caddr_t) &result,
                        _rpcOtherTimeout)) != RPC_SUCCESS)
        {
            Rf_warning(clnt_sperror(_clnt,"nc_server close failed"));
        }
        clnt_destroy(_clnt);
        _clnt = 0;
        Rprintf("Closed nc_server connection, Id=%d\n",_id);
    }

    for (unsigned int i = 0; i < _groups.size(); i++) delete _groups[i];
#endif

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
 * Verify that it is in the set of current pointers.
 */
R_netcdf *R_netcdf::getR_netcdf(SEXP obj)
{
#ifdef DEBUG
    Rprintf("looking for cppPtr slot\n");
#endif
    SEXP cslot = Rf_getAttrib(obj,cppSlotName);
#ifdef DEBUG
    Rprintf("cpp slot: %p, length=%d, type=%d\n",cslot,
            (cslot ? Rf_length(cslot):0),(cslot ? TYPEOF(cslot): NILSXP));
#endif
    R_netcdf *con = *(R_netcdf **)RAW(cslot);
    if (!findConnection(con)) con = 0;
    return con;
}

void R_netcdf::openFileSet(SEXP obj,SEXP files)
{
#ifdef DEBUG
    Rprintf("files slot length=%d, type=%d\n",Rf_length(files),TYPEOF(files));
#endif

    vector<string> fnames;
    for (size_t i = 0; i < (size_t)Rf_xlength(files); i++) {
        SEXP dn = STRING_ELT(files,i);
        fnames.push_back(CHAR(dn));
    }

    vector<string> fullnames = makeFileNameList(_dir,fnames);

    SEXP slot = Rf_getAttrib(obj,timeNamesSlotName);

    vector<string> tnames;
    for (size_t i = 0; i < (size_t)Rf_xlength(slot); i++) {
        SEXP dn = STRING_ELT(slot,i);
        tnames.push_back(CHAR(dn));
    }

    delete _fileset;
    _fileset = new NcFileSet(fullnames,tnames);
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
                Rf_error(ost.str().c_str());
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

    SEXP result = PROTECT(Rf_allocVector(VECSXP,vars.size()));
    SEXP resnames = Rf_getAttrib(result,R_NamesSymbol);

    if (!resnames || TYPEOF(resnames) != STRSXP ||
            (size_t)Rf_xlength(resnames) != vars.size()) {
#ifdef DEBUG
        Rprintf("allocating R_NamesSymbol for variables\n");
#endif
        resnames = PROTECT(Rf_allocVector(STRSXP,vars.size()));
        Rf_setAttrib(result,R_NamesSymbol,resnames);
        UNPROTECT(1);   // resnames
    }
#ifdef DEBUG
    Rprintf("resnames length=%ld\n",Rf_xlength(resnames));
#endif

    for (size_t i = 0; i < vars.size(); i++) {
        R_NetcdfVariable rvar(this,vars[i]);
        SET_STRING_ELT(resnames,i,Rf_mkChar(rvar.getName().c_str()));
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

    SEXP result = PROTECT(Rf_allocVector(VECSXP,tvars.size()));
    SEXP resnames = Rf_getAttrib(result,R_NamesSymbol);

    if (!resnames || TYPEOF(resnames) != STRSXP ||
            (size_t) Rf_length(resnames) != tvars.size()) {
#ifdef DEBUG
        Rprintf("allocating R_NamesSymbol for variables\n");
#endif
        resnames = PROTECT(Rf_allocVector(STRSXP,tvars.size()));
        Rf_setAttrib(result,R_NamesSymbol,resnames);
        UNPROTECT(1);   // resnames
    }

    for (unsigned int i = 0; i < tvars.size(); i++) {
        R_NetcdfVariable rvar(this,tvars[i]);
        SET_STRING_ELT(resnames,i,Rf_mkChar(rvar.getName().c_str()));
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

#ifdef HAVE_NC_SERVER

void R_netcdf::rpcopen(void) throw(RPC_Exception)
{
    connection conn;
    int result;
    enum clnt_stat clnt_stat;

#ifdef DEBUG
    Rprintf("rpcopen: _rpcBatchPeriod=%d,_rpcWriteTimeout=%d,%d,_rpcOtherTimeout=%d,%d\n",
            _rpcBatchPeriod,
            _rpcWriteTimeout.tv_sec, _rpcWriteTimeout.tv_usec,
            _rpcOtherTimeout.tv_sec, _rpcOtherTimeout.tv_usec);
#endif

    _clnt = clnt_create(_server.c_str(), NETCDFSERVERPROG, NETCDFSERVERVERS,"tcp");
    if (_clnt == (CLIENT *) NULL)
        throw RPC_Exception(clnt_spcreateerror(_server.c_str()));

    conn.filenamefmt = (char *)_filenamefmt.c_str();
    conn.outputdir = (char *)_dir.c_str();
    conn.cdlfile = (char *)_cdlfile.c_str();
    conn.filelength = _lenfile;
    conn.interval = _interval;

#ifdef DEBUG
    Rprintf("conn: file=%s,dir=%s,cdlfile=%s,filelength=%d,interval=%f\n",
            conn.filenamefmt,conn.outputdir,conn.cdlfile,_lenfile,_interval);
#endif

    result = 0;
    if ((clnt_stat = clnt_call(_clnt, OPENCONNECTION,
                    (xdrproc_t) xdr_connection, (caddr_t) &conn,
                    (xdrproc_t) xdr_int,  (caddr_t) &result,
                    _rpcOtherTimeout)) != RPC_SUCCESS) {
        RPC_Exception e(clnt_sperror(_clnt,(string("server: ")+ _server +
                        " open").c_str()));
        clnt_destroy(_clnt);
        _clnt = 0;
        throw e;
    }

    _id = result;
    if (_id < 0) {
        clnt_destroy(_clnt);
        _clnt = 0;
        string msg("nc_server connection failed: perhaps ");
        msg = msg + _dir + " does not exist on server";
        throw RPC_Exception(msg);
    }

    Rprintf("Opened nc_server connection, Id=%d\n",_id);

    _lastNonBatchWrite = time((time_t *)0);
}

void R_netcdf::setRPCTimeoutSecs(int secs)
{
    _rpcWriteTimeout.tv_sec = secs;
    _rpcWriteTimeout.tv_usec = 0;
    _rpcOtherTimeout.tv_sec = secs * 5;
    _rpcOtherTimeout.tv_usec = 0;
}

void R_netcdf::setBatchPeriod(int secs)
{
    _rpcBatchPeriod = secs;
}

void R_netcdf::write(R_nts &nts,
        const vector<NcDim>& dims,	// vector of non-time dimensions
        double dt, double fillvalue) throw(RPC_Exception)
{
    if (!_clnt) rpcopen();

    unsigned int i;

    R_Matrix<double> dmatrix(REALSXP,nts.getMatrix());

    R_utime times(nts.getPositions());

    size_t nr = dmatrix.getNrows();
    size_t nc = dmatrix.getNcols();
    vector<string> vnames = dmatrix.getColumnNames();
    vector<string> vunits = nts.getUnits();

    double *data = dmatrix.getDataPtr();

    int stn;
    vector<int> stations = nts.getStationNumbers();
    if (stations.size() == 0) {
        Rf_warning("No stations in time series");
        stn = -1;
    }
    else stn = stations[0] - 1;

    for (i = 1; i < stations.size(); i++) {
        if (stations[0] != stations[i]) {
            std::ostringstream ost;
            ost << "Cannot write time series from more than one station. stations=c("
                << stations[0] << ',' << stations[i] << ")";
            Rf_error(ost.str().c_str());
        }
    }
    vector<size_t> start,count;
    start.push_back(stn);
    count.push_back(1);

    R_Matrix<double> wts(REALSXP,nts.getWeights());
    vector<int> wm = nts.getWeightMap();
    int wc;
    if (wm.size() == 0) wc = -1;
    else if (wm.size() != nc) {
        std::ostringstream ost;
        ost << "Length of weightmap(" << wm.size() <<
            ") not equal to number of columns(" << nc << ")";
        Rf_warning(ost.str().c_str());
        wc = -1;
    }
    else wc = wm[0] - 1;

    /* Check that weightmap indices are all the same. This should have been
     * caught in the writets R function, which would then call this
     * in a loop, each time with columns from the time series with the same weights.
     */
    for (i = 1; i < wm.size(); i++)
        if (wm[0] != wm[i]) {
            std::ostringstream ost;
            ost << "Cannot write time series with more than one weights column. weightmap=c(" << wm[0] << ',' << wm[i] << ")";
            Rf_error(ost.str().c_str());
        }

    double *wtsArray = wts.getDataPtr();
    double *wtsPtr = wtsArray + (wc * nr);
    double dummyWts = -1;
    string cntsName;
    if (wc < 0) wtsPtr = &dummyWts;
    else {
        cntsName = "counts_" + vnames[0];
        // convert dots, tics, commas, parens to underscores so that
        // it is a legal NetCDL name
        string::size_type ic;
        while((ic = cntsName.find_first_of(".'(),*",0)) != string::npos)
            cntsName[ic] = '_';
#ifdef DEBUG
        Rprintf("wc=%d, cntsName=%s\n",wc,cntsName.c_str());
#endif
    }

    NSVarGroupFloat* vg = getVarGroupFloat(
            vnames,vunits,NS_TIMESERIES,dt,dims,fillvalue,cntsName);
#ifdef DEBUG
    Rprintf("getVarGroupFloat for vnames[0]=%s: %s\n",
            vnames[0].c_str(),vg->toString().c_str());
#endif

    for (i = 0; i < nr; i++) {
        double tval = times.getTime(i);
#ifdef DEBUG
        Rprintf("i=%d,nr=%d,tval=%g,ts=%s\n",i,nr,tval,
                R_utime::format(tval,"%Y %b %d %H%M%S %Z","UTC").c_str());
#endif
        vg->write(tval,data,nr,nc,&start.front(),&count.front(),*wtsPtr);
        if (wc >= 0) wtsPtr++;
        data++;
    }
}

int R_netcdf::writeHistory(const string& hist) throw(RPC_Exception)
{
    if (!_clnt) rpcopen();
    int result;
    enum clnt_stat clnt_stat;
    historyrec outRec;

    outRec.connectionId = _id;
    outRec.history = const_cast<char*>(hist.c_str());

    result = 0;

    if (_rpcBatchPeriod > 0) {
        clnt_stat = clnt_call(_clnt, WRITEHISTORYRECBATCH,
                (xdrproc_t) xdr_historyrec, (caddr_t) &outRec,
                (xdrproc_t) NULL , (caddr_t) NULL,
                _batchTimeout);
        if (clnt_stat != RPC_SUCCESS) {
            throw RPC_Exception(clnt_sperror(_clnt,(string("server: ")+ _server +
                            " write history").c_str()));
        }
    }
    else {
        clnt_stat = clnt_call(_clnt, WRITEHISTORYREC,
                (xdrproc_t) xdr_historyrec, (caddr_t) &outRec,
                (xdrproc_t) xdr_int, (caddr_t) &result ,
                _rpcWriteTimeout);
        if (clnt_stat != RPC_SUCCESS) {
            throw RPC_Exception(clnt_sperror(_clnt,(string("server: ")+ _server +
                            " write history").c_str()));
        }
        else if (result != 0) {
            Rprintf("nc_server history write status = %d\n",result);
        }
        return result;
    }
    return 0;
}

R_netcdf::NSVarGroupFloat* R_netcdf::addVarGroupFloat(NS_rectype rectype,
        double interval,const vector<NcDim>& dims,double fillValue,const string& cntsName)
{

#ifdef DEBUG
    Rprintf("addVarGroupFloat, ngroups=%d ndims=%d\n",
            _groups.size(),dims.size());
#endif

#ifdef DEBUG
    Rprintf("calling NSVarGroupFloat ctor\n");
#endif

    NSVarGroupFloat *g = new NSVarGroupFloat(this,rectype,dims,fillValue,cntsName);

    _groups.push_back(g);

#ifdef DEBUG
    Rprintf("exiting addVarGroupFloat, ngroups=%d ndims=%d, g=0x%x\n",_groups.size(),dims.size(),g);
#endif
    return g;
}

R_netcdf::NSVarGroupFloat* R_netcdf::getVarGroupFloat(
        const vector<string>& vnames,
        const vector<string>& vunits,
        NS_rectype rectype,
        double interval,
        const vector<NcDim>& dims,
        double fillValue,const string& cntsName) throw(RPC_Exception)
{
    size_t ndims = dims.size();
    size_t nvars = vnames.size();

#ifdef DEBUG
    Rprintf("getVarGroupFloat, _groups.size()=%d, ndims=%d, nvars=%d\n",
            _groups.size(),ndims,nvars);
#endif

    for (size_t i = 0; i < _groups.size(); i++) {
        NSVarGroupFloat *vg = _groups[i];
#ifdef DEBUG
        Rprintf("getVarGroupFloat, gp=0x%x, i=%d, ndims=%d,nvars=%d\n",
                vg,i,vg->getNumDimensions(),vg->getNumVariables());
#endif
        if (vg->getNumDimensions() != ndims) continue;
        size_t j;
        for (j = 0; j < ndims; j++) {
            if (vg->getDimension(j).getName() != dims[j].getName()) break;
            if (vg->getDimension(j).getLength() != dims[j].getLength()) break;
        }
        if (j < ndims) continue;	/* not identical dimensions */

        if (vg->getNumVariables() != nvars) continue;
        for (j = 0; j < nvars; j++) {
            if (vg->getVariable(j).getName() != vnames[j]) break;
            if (vg->getVariable(j).getUnits() != vunits[j]) break;
        }
        if (j < nvars) continue;	/* not identical variables */

        if (cntsName != vg->getCountsName()) continue;

        /* NOTE: other attributes are not checked. */

        /* Same variables, same dimension, same counts */
        return vg;
    }

    NSVarGroupFloat *vg = addVarGroupFloat(rectype,interval,dims,fillValue,cntsName);

    for (size_t j = 0; j < vnames.size(); j++)
        vg->addVariable(vnames[j], vunits[j]);

    vg->open();

    return vg;
}

int R_netcdf::write(datarec_float *rec) throw(RPC_Exception)
{
    enum clnt_stat clnt_stat;
#ifdef DEBUG
    Rprintf("in R_netcdf::write(datarec_float)\n");
#endif

    if (_rpcBatchPeriod == 0 || (time(0) - _lastNonBatchWrite > _rpcBatchPeriod))
        return nonBatchWrite(rec);

    clnt_stat = clnt_call(_clnt, WRITEDATARECBATCH_FLOAT,
            (xdrproc_t) xdr_datarec_float, (caddr_t) rec,
            (xdrproc_t) NULL, (caddr_t) NULL,
            _batchTimeout);
    if (clnt_stat != RPC_SUCCESS) {
        throw RPC_Exception(clnt_sperror(_clnt,(string("server: ")+ _server +
                        " write data").c_str()));
    }
    return 0;
}

int R_netcdf::nonBatchWrite(datarec_float *rec) throw(RPC_Exception)
{
    int result = 0;
    enum clnt_stat clnt_stat;
    for ( ; ; ) {
        clnt_stat = clnt_call(_clnt, WRITEDATAREC_FLOAT,
                (xdrproc_t) xdr_datarec_float, (caddr_t) rec,
                (xdrproc_t) xdr_int, (caddr_t) &result,
                _rpcWriteTimeout);
        if (clnt_stat != RPC_SUCCESS) {
            bool serious = (clnt_stat != RPC_TIMEDOUT && clnt_stat != RPC_CANTRECV) ||
                _ntry++ >= _NTRY;
            if (serious) {
                throw RPC_Exception(clnt_sperror(_clnt,(string("server: ")+ _server +
                                " write data").c_str()));
            }
            if (_ntry > _NTRY / 2) {
                clnt_perror(_clnt,
                        "nc_server not responding to write datarec_float");
                Rprintf("timeout=%d secs, ntry=%d\n",
                        _rpcWriteTimeout.tv_sec,_ntry);
                Rprintf("sample time = %s\n",
                        R_utime::format(rec->time,"%Y %b %d %H:%M:%OS","UTC").c_str());
            }
        }
        else {
            if (!result && _ntry > 0) Rprintf("nc_server OK\n");
            _ntry = 0;
            if (result) checkError();
            _lastNonBatchWrite = time((time_t*)0);
            return result;
        }
    }
    return result;
} 

void R_netcdf::checkError() throw(RPC_Exception)
{
    char* errormsg = 0;
    enum clnt_stat clnt_stat = clnt_call(_clnt,CHECKERROR,
            (xdrproc_t) xdr_int, (caddr_t) &_id,
            (xdrproc_t) xdr_wrapstring, (caddr_t) &errormsg,
            _rpcWriteTimeout);

    if (clnt_stat != RPC_SUCCESS) {
        bool serious = (clnt_stat != RPC_TIMEDOUT && clnt_stat != RPC_CANTRECV) ||
            _ntry++ >= _NTRY;
        if (serious) {
            throw RPC_Exception(clnt_sperror(_clnt,(string("server: ")+ _server +
                            " checkError").c_str()));
        }
        const char* errstr = clnt_sperror(_clnt,(string("server: ")+ _server +
                    " checkError").c_str());
        Rprintf("%s: timeout=%d secs, ntry=%d\n",errstr,
                _rpcWriteTimeout.tv_sec ,_ntry);
    }
    else {
        if (!errormsg[0] && _ntry > 0) Rprintf("nc_server OK\n");
        _ntry = 0;
        _lastNonBatchWrite = time((time_t*)0);
        /*
           If error string is non-empty, then an error occured on nc_server.
           The returned string must be freed.
           */
        if (errormsg[0]) {
            RPC_Exception err(errormsg);
            xdr_free((xdrproc_t)xdr_wrapstring,(char*)&errormsg);
            throw err;
        }
        xdr_free((xdrproc_t)xdr_wrapstring,(char*)&errormsg);
    }
}

void R_netcdf::NSVar::addAttribute(const string& name, const string& val)
{
    if (name == "units") _units = string(val);
    else {
        NcAttrT<string> attr(name,NC_CHAR);
        std::vector<std::string> vals;
        vals.push_back(val);
        attr.setValue(vals);
        _attrs.push_back(attr);
    }
}

R_netcdf::NSVarGroupFloat::NSVarGroupFloat(R_netcdf *conn,NS_rectype rectype,
        const vector<NcDim>& dims, double fillValue,const string& cntsName) :
    _interval(conn->getInterval()), _conn(conn),_id(-1),
    _vars(), _dims(dims),_rectype(rectype),
    _fillValue(fillValue),_rec(),_cntsName(cntsName)

{
    _rec.connectionId = _conn->getId();
    _rec.datarecId = _id;

    _rec.cnts.cnts_val = 0;
    _rec.cnts.cnts_len = 0;

    int n = _dims.size();
    _rec.start.start_len = n;
    _rec.count.count_len = n;
    _rec.start.start_val = new int[n];
    _rec.count.count_val = new int[n];

}
R_netcdf::NSVarGroupFloat::~NSVarGroupFloat()
{
    delete [] _rec.start.start_val;
    delete [] _rec.count.count_val;
}

void R_netcdf::NSVarGroupFloat::addVariable(const string& name, const string& units)
{
    _vars.push_back(NSVar(name,units,_cntsName));
}

int R_netcdf::NSVarGroupFloat::open() throw(RPC_Exception)
{
    datadef ddef; 
    int result;
    enum clnt_stat clnt_stat;
    int i,j,n,m,ntry;
    CLIENT *clnt = _conn->getRPCClient();

    ddef.connectionId = _conn->getId();
    ddef.rectype = _rectype;
    ddef.datatype = NS_FLOAT;
    ddef.fillmissingrecords = 1;
    ddef.floatFill = (float) _fillValue;
    ddef.intFill = 0;
    ddef.interval = _interval;

    ddef.dimensions.dimensions_val = 0;
    ddef.dimensions.dimensions_len = n = _dims.size();
#ifdef DEBUG
    Rprintf("_open _dims.size()=%d\n",n);
#endif
    if (n > 0) {
        ddef.dimensions.dimensions_val = new dimension[n];
        for(i = 0; i < n; i++) {
            ddef.dimensions.dimensions_val[i].name = (char *)_dims[i].getName().c_str();
            ddef.dimensions.dimensions_val[i].size = _dims[i].getLength();
        }
    }

    n = _vars.size();
    ddef.fields.fields_val = new field[n];
    ddef.fields.fields_len = n;

    ddef.attrs.attrs_val = new str_attrs[n];
    ddef.attrs.attrs_len = n;

#ifdef DEBUG
    Rprintf("_open _vars.size()=%d\n",n);
#endif
    for (i=0; i < n; i++) {
#ifdef DEBUG
        Rprintf("_open i=%d name=%s\n",i,_vars[i].getName().c_str());
#endif
        ddef.fields.fields_val[i].name =  (char *) _vars[i].getName().c_str();
        ddef.fields.fields_val[i].units = (char *) _vars[i].getUnits().c_str();

        ddef.attrs.attrs_val[i].attrs.attrs_val = 0;
        ddef.attrs.attrs_val[i].attrs.attrs_len = m = _vars[i].getNumAttributes();
        ddef.attrs.attrs_val[i].attrs.attrs_val = new str_attr[m];
        for (j = 0; j < m; j++) {
            NcAttrT<string> &a = _vars[i].getAttribute(j);
            str_attr *s = ddef.attrs.attrs_val[i].attrs.attrs_val + j;
            s->name = (char *)a.getName().c_str();
            if (a.getLength() > 0) {
                const vector<string>& val = a.getValue();
                // must make sure this pointer stays valid until
                // the clnt_call.
                s->value = (char *)val[0].c_str();
            }
            else {
                s->value = (char*)"";
            }

        }
    }

#ifdef DEBUG
    Rprintf("calling DEFINEDATAREC\n");
#endif
    for (ntry = 0; ntry < 5; ntry++) {
        result = 0;
        clnt_stat = clnt_call(clnt, DEFINEDATAREC,
                (xdrproc_t) xdr_datadef, (caddr_t) &ddef,
                (xdrproc_t) xdr_int, (caddr_t) &result,
                _conn->getRPCOtherTimeout());
#ifdef DEBUG
        Rprintf("called DEFINEDATAREC, clnt_stat=%d\n",clnt_stat);
#endif
        if (clnt_stat == RPC_SUCCESS) break;
        Rprintf(
                "nc_server DEFINEDATAREC failed: %s\n",clnt_sperrno(clnt_stat));
        Rprintf("timeout=%d secs, ntry=%d\n",
                _conn->getRPCOtherTimeout().tv_sec ,ntry+1);
        if (clnt_stat != RPC_TIMEDOUT && clnt_stat != RPC_CANTRECV) break;
    }
    if (ntry > 0 && clnt_stat == RPC_SUCCESS) 
        Rprintf("nc_server OK\n");

    delete [] ddef.fields.fields_val;
    delete [] ddef.dimensions.dimensions_val;

    for (i=0; i < n; i++)
        delete [] ddef.attrs.attrs_val[i].attrs.attrs_val;
    delete [] ddef.attrs.attrs_val;

    if (clnt_stat != RPC_SUCCESS) return -1;

    // If return is < 0, fetch the error string, and throw exception
    if (result < 0) _conn->checkError();

    _rec.datarecId = _id = result;

    return _id;
}

int R_netcdf::NSVarGroupFloat::write(double t, double *d,
        size_t nr, size_t nc,
        size_t *start, size_t *count,double dcnts) throw(RPC_Exception)
{
    vector<float> dout(nc);

    for (unsigned int i = 0; i < nc; i++) dout[i] = d[i*nr];	// effects a transpose

    /* constant members of rec have been initialized */
    _rec.time = t;         /* time in seconds since 1970 Jan 1 00:00 GMT */
    _rec.data.data_val = &dout.front();
    _rec.data.data_len = nc;

    _rec.cnts.cnts_val = 0;
    _rec.cnts.cnts_len = 0;
    int cnts = (int)round(dcnts);

    if (cnts >= 0) {
        _rec.cnts.cnts_val = (int*)&cnts;
        _rec.cnts.cnts_len = 1;
    }

    /* non-time dimensions */
    /* the start and count arrays have been allocated previously */
    size_t n = _dims.size();
    for (unsigned int i = 0; i < n; i++) {
        _rec.start.start_val[i] = start[i];
        _rec.count.count_val[i] = count[i];
    }
    int res = _conn->write(&_rec);
    return res;
}
string R_netcdf::NSVarGroupFloat::toString()
{
    string out;
    char tmp[64];

    out.append("NSVarGroupFloat: dims=(");
    for (unsigned int i = 0; i < _dims.size(); i++) {
        if (i > 0) out.append(", ");
        out.append(_dims[i].getName());
        out.append("[");
        sprintf(tmp,"%lu",_dims[i].getLength());
        out.append(tmp);
        out.append("]");
    }
    out.append("), vars=");
    for (unsigned int i = 0; i < _vars.size(); i++) {
        if (i > 0) out.append(", ");
        out.append(_vars[i].getName());
    }
    return out;
}
#endif
