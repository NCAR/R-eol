// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.18 $
 *  $Date: 2008/09/09 23:42:36 $
 * 
 *  Description:
 *    A connection from Splus to one or more NetCDF files.
 * 
 */
#ifndef R_NETCDFCONNECTION_H
#define R_NETCDFCONNECTION_H

#include <set>

#include "NcFileSet.h"

#include <R.h>
#include <Rinternals.h>

extern "C" {
    SEXP open_netcdf(SEXP con,SEXP cdlfile, SEXP rpcTimeout, SEXP rpcBatchPeriod);

    SEXP read_netcdf(SEXP obj,SEXP variables, SEXP start, SEXP count);

    SEXP read_netcdf_ts(SEXP args);

    SEXP write_netcdf_ts(SEXP args);

    SEXP write_history(SEXP obj,SEXP history);

    SEXP get_variables(SEXP obj, SEXP all);

    SEXP get_ts_variables(SEXP obj);

    SEXP get_stations(SEXP obj);

    SEXP close_netcdf(SEXP obj);

    SEXP get_cxxPointer(SEXP obj);

    SEXP is_netcdf_open(SEXP obj);
}

namespace eolts {

class R_netcdf {
public:

    R_netcdf(SEXP obj, SEXP cdlfile, SEXP rpcTimeout, SEXP rpcBatchPeriod);
    virtual ~R_netcdf();

    static std::vector<std::string> makeFileNameList(
            const std::vector<std::string>& fnames, 
            const std::vector<std::string>& dnames);

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
            const std::string& timezone) throw(NcException);

#ifdef SUPPORT_WRITE
    int write(R_nts& nts, const std::vector<NSVarDim>& ntdv,
            double dt, double fillValue) throw(NcException);

    int write(datarec_float *rec) RPCEXCEPTION_CLAUSE;

    int nonBatchWrite(datarec_float *rec) RPCEXCEPTION_CLAUSE;
#endif

    SEXP getVariables() throw(NcException);

    SEXP getTimeSeriesVariables() throw(NcException);

    SEXP getStations() throw(NcException);

    static bool addConnection(R_netcdf *);

    static bool removeConnection(R_netcdf *);

    static bool findConnection(R_netcdf *);

    static R_netcdf *getR_netcdf(SEXP  obj);

    static SEXP fileSlotName;

    static SEXP dirSlotName;

    static SEXP timeNamesSlotName;

    /**
     * perhaps this should be an EXTPTRSXP instead of a raw(8)?
     * See section 5.13 of R Externals, page 122.
     */
    static SEXP cppSlotName;


protected:

    static std::set<R_netcdf*> _openSet;

    NcFileSet* _fileset;

    void openFileSet(SEXP obj);

private:
    // declared private to prevent copying and assignment
    R_netcdf(const R_netcdf &);
    R_netcdf &operator=(const R_netcdf &) const;

};

}   // namespace eolts

#endif
