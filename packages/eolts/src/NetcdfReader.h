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
 *    Implements a read of a NcFileSet.
 * 
 */
#ifndef NETCDFREADER_H
#define NETCDFREADER_H

#include <string>
#include <vector>

#include <R.h>
#include <Rinternals.h>

#include "R_netcdf.h"
#include "R_Matrix.h"
#include "R_Array.h"

namespace eolts {

class NetcdfReader {

public:

    enum timeTests {LT,LE,GT,GE};

    NetcdfReader(R_netcdf*);

    ~NetcdfReader();

    SEXP read(const std::vector<std::string>& vnames, 
            const std::vector<size_t>& start,
            const std::vector<size_t>& count);
    
    SEXP read(const std::vector<std::string> &vnames,
        double start, double end,
        const std::vector<int> &stations,
        const std::vector<std::string> &timeVarNames,
        const std::string & baseTimeName,const std::string& timezone,
        int verbose,
        bool readCountsOnly = false);

    size_t searchTime(NcVar* var,double stime,timeTests test,double timeMult)
       ;

    SEXP readGlobalAttrs();

    static int rMode(nc_type type);

    static int checkVarTypeCompatibility(int rmode1, int rmode2);

private:

    R_netcdf* _connection;

    // no copying, assignment
    NetcdfReader(const NetcdfReader&);
    NetcdfReader& operator=(const NetcdfReader&) const;
};

}   // namespace eolts

#endif
