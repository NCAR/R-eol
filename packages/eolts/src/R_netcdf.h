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
 *    A connection from R to one or more NetCDF files.
 * 
 */
#ifndef R_NETCDFCONNECTION_H
#define R_NETCDFCONNECTION_H

#include <R.h>
#include <Rinternals.h>

#include "NcFileSet.h"
#include "R_nts.h"

#include <set>

#ifdef HAVE_NC_SERVER
#include "NcAttrT.h"
#include "NcDim.h"
#include "nc_server_rpc.h"
#include "RPC_Exception.h"
#endif

extern "C" {
    SEXP open_netcdf(SEXP con,SEXP files, SEXP cdlfile, SEXP rpcTimeout,
            SEXP rpcBatchPeriod);

    SEXP read_netcdf(SEXP obj,SEXP variables, SEXP start, SEXP count);

    SEXP read_netcdf_ts(SEXP args);

    SEXP read_global_attrs(SEXP obj);

#ifdef HAVE_NC_SERVER
    SEXP write_ts_ns(SEXP args);

    SEXP write_history_ns(SEXP args);

    SEXP write_global_attrs_ns(SEXP args);
#endif

    SEXP get_variables(SEXP obj, SEXP all, SEXP ncverbose);

    SEXP get_stations(SEXP obj);

    SEXP close_netcdf(SEXP obj);

    SEXP get_cxxPointer(SEXP obj);

    SEXP is_netcdf_open(SEXP obj);
}

namespace eolts {

class R_netcdf {
public:

    R_netcdf(SEXP obj, SEXP files, SEXP cdlfile, SEXP rpcTimeout, SEXP rpcBatchPeriod);

    virtual ~R_netcdf();

    static SEXP fileSlotName;

    static SEXP dirSlotName;

    static SEXP timeNamesSlotName;

    static SEXP serverSlotName;

    static SEXP intervalSlotName;

    static SEXP lenfileSlotName;

    /**
     * perhaps this should be an EXTPTRSXP instead of a raw(8)?
     * See section 5.13 of R Externals, page 122.
     */
    static SEXP cppSlotName;

    NcFileSet* getFileSet()
    {
        return _fileset;
    }

    SEXP read(const std::vector<std::string>& vnames,
            const std::vector<size_t> &start,
            const std::vector<size_t> &count) throw(NcException);

    SEXP read(const std::vector<std::string>& vnames,
            double start, double end,
            const std::vector<int>& stations,
            const std::vector<std::string>& tnames,
            const std::string& btname,
            const std::string& timezone,
            int verbose) throw(NcException);

    SEXP readGlobalAttrs() throw(NcException);

#ifdef HAVE_NC_SERVER

    int getId() const { return _id; }

    void write(R_nts& nts, const std::vector<NcDim>& dims,
            double dt, double fillValue) throw(RPC_Exception);

    int writeHistory(const std::string&) throw(RPC_Exception);

    int writeGlobalAttr(const std::string& name, const std::string& val) throw(RPC_Exception);

    int writeGlobalAttr(const std::string& name, int val) throw(RPC_Exception);

    int write(datarec_float *rec) throw(RPC_Exception);

    int nonBatchWrite(datarec_float *rec) throw(RPC_Exception);

    /**
     * How long to wait for a non-batch write, or a batch queue flush.
     */
    void setRPCTimeoutSecs(int secs);

    /**
     * If the batch period is > 0 then RPC calls operate in a batch mode,
     * and do not wait for a response on each request.
     * After the batch period has elapsed, the batch queue
     * of requests is flushed, and any error reported back.
     */
    void setBatchPeriod(int secs);

    /**
     * The delta-T of the time variable in the NetCDF files.
     */
    double getInterval(void) const { return _interval; }

    CLIENT *getRPCClient(void) const { return _clnt; }

    struct timeval& getRPCWriteTimeout(void) { return _rpcWriteTimeout; }
    struct timeval& getRPCOtherTimeout(void) { return _rpcOtherTimeout; }

    void checkError() throw(RPC_Exception);

#endif

    SEXP getVariables(int verbose) throw(NcException);

    SEXP getTimeSeriesVariables(int verbose) throw(NcException);

    SEXP getStations() throw(NcException);

    static bool addConnection(R_netcdf *);

    static bool removeConnection(R_netcdf *);

    static bool findConnection(R_netcdf *);

    static R_netcdf *getR_netcdf(SEXP  obj);

private:
    // declared private to prevent copying and assignment
    R_netcdf(const R_netcdf &);
    R_netcdf &operator=(const R_netcdf &) const;

    static std::set<R_netcdf*> _openSet;

    /** Directory of NetCDF files */
    std::string _dir;

    /** file name, usually contains date format descriptors, see man cftime */
    std::string _filenamefmt;

    NcFileSet* _fileset;

    void openFileSet(SEXP obj, SEXP files);

#ifdef HAVE_NC_SERVER

    class NSVar {
    private:
        std::string _name;
        std::string _units;
        std::vector<NcAttrT<std::string> > _attrs;
    public:
        NSVar(const std::string& n, const std::string& u,const std::string& c) :
            _name(n),_units(u),_attrs()
        {
            if (c.length() > 0) addAttribute("counts",c);
        }

        NSVar() :_name(),_units(),_attrs() {}

        void addAttribute(const std::string& name, const std::string& val);

        std::string& getName() { return _name; }
        std::string& getUnits() { return _units; }

        int getNumAttributes() const { return _attrs.size(); }
        NcAttrT<std::string> &getAttribute(int i) { return _attrs[i]; }
    };

    class NSVarGroupFloat {
    public:
        NSVarGroupFloat(R_netcdf *conn,NS_rectype,
                const std::vector<NcDim>& dims,double fillValue,
                const std::string& cntsName);
        // NSVarGroupFloat() {}
        ~NSVarGroupFloat(void);

    private:
        NSVarGroupFloat(const NSVarGroupFloat&);	// prevent copying
        NSVarGroupFloat& operator =(const NSVarGroupFloat&);	// prevent ass

    public:

        int open(void) throw(RPC_Exception);

        int write(double t,double *d,size_t nr, size_t nc,
                size_t *start, size_t *count,double cnts) throw(RPC_Exception);

        void addVariable(const std::string& name,const std::string& units);

        unsigned int getNumVariables() const { return _vars.size(); }
        NSVar &getVariable(int i) { return _vars[i]; }

        unsigned int getNumDimensions() const { return _dims.size(); }
        NcDim &getDimension(int i) { return _dims[i]; }

        const std::string& getCountsName() const { return _cntsName; }

        std::string toString();

    protected:
        void init_datarec(void);

    private:
        double _interval;
        R_netcdf *_conn;
        int _id;
        std::vector<NSVar> _vars;
        std::vector<NcDim> _dims;
        NS_rectype _rectype;
        float _fillValue;
        datarec_float _rec;
        std::string _cntsName;
    };

    NSVarGroupFloat *getVarGroupFloat(const std::vector<std::string>& vnames,
        const std::vector<std::string>& vunits, NS_rectype rectype,
        double interval, const std::vector<NcDim>& dims,
        double fillvalue,const std::string& cntsName) throw(RPC_Exception);

    NSVarGroupFloat *addVarGroupFloat(NS_rectype rectype,
        double interval, const std::vector<NcDim>& dims,
        double fillvalue, const std::string& cntsName);

    void rpcopen(void) throw(RPC_Exception);

    std::string _server;

    std::string _cdlfile;

    unsigned int _lenfile;

    double _interval;

    CLIENT *_clnt;
    int _id;
    
    int _rpcBatchPeriod;

    /**
     * RPC write timeout if _rpcBatchPeriod is 0, or when the batch
     * queue is flushed every _rpcBatchPeriod seconds.
     */
    struct timeval _rpcWriteTimeout;

    /**
     * RPC timeout value for opens and closes of connections.
     */
    struct timeval _rpcOtherTimeout;

    /**
     * timeval of zeros, used to tell RPC not to wait for a response
     * if _rpcBatchPeriod is > 0
     */
    struct timeval _batchTimeout;

    int _ntry;
    int _NTRY;
    time_t _lastNonBatchWrite;

    std::vector<NSVarGroupFloat*> _groups;
#endif

};

}   // namespace eolts
#endif

