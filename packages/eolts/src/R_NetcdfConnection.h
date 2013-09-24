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

#include <R.h>
#include <Rinternals.h>

#include "NcFileSet.h"
#include "NcFileSetFactory.h"


extern "C" {
    SEXP open_netcdf(SEXP obj);
    SEXP read_netcdf(SEXP obj,SEXP variables, SEXP start, SEXP count);
    SEXP get_variables(SEXP obj);
    SEXP close_netcdf(SEXP obj);
    SEXP get_cxxPointer(SEXP obj);
    SEXP is_netcdf_open(SEXP obj);
}

class R_NetcdfConnection {
public:

    R_NetcdfConnection(SEXP);
    virtual ~R_NetcdfConnection();

    static std::vector<std::string> makeFileNameList(
            const std::vector<std::string>& fnames, 
            const std::vector<std::string>& dnames);
    NcFileSet* getFileSet()
    {
        return _fileset;
    }

    SEXP read(const std::vector<std::string>& vnames,
            const std::vector<size_t> &start,
            const std::vector<size_t> &count) NCEXCEPTION_CLAUSE;

    SEXP getVariables() NCEXCEPTION_CLAUSE;

    static bool addConnection(R_NetcdfConnection *);
    static bool removeConnection(R_NetcdfConnection *);
    static bool findConnection(R_NetcdfConnection *);
    static R_NetcdfConnection *getR_NetcdfConnection(SEXP  obj);

    static SEXP fileSlotName;
    static SEXP dirSlotName;

    /**
     * perhaps this should be an EXTPTRSXP instead of a raw(8)?
     * See section 5.13 of R Externals, page 122.
     */
    static SEXP cppSlotName;

protected:

    static std::set<R_NetcdfConnection*> _openSet;

    NcFileSet* _fileset;
    NcFileSetFactory* _factory;

    void openFileSet(SEXP obj);

    virtual NcFileSetFactory* getNcFileSetFactory();

private:
    // declared private to prevent copying and assignment
    R_NetcdfConnection(const R_NetcdfConnection &);
    R_NetcdfConnection &operator=(const R_NetcdfConnection &);

};

#endif
