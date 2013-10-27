// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.15 $
 *  $Date: 2008/09/09 23:42:36 $
 * 
 *  Description:
 *    Implements a read of a NcFileSet.
 * 
 */
#ifndef NETCDFREADER_H
#define NETCDFREADER_H

#include <string>
#include <vector>

#include <R.h>

#include "R_NetcdfConnection.h"
#include "R_Matrix.h"
#include "R_Array.h"

namespace eolts {

class NetcdfReader {

public:

    enum timeTests {LT,LE,GT,GE};

    NetcdfReader(R_NetcdfConnection*);

    ~NetcdfReader();

    SEXP read(const std::vector<std::string>& vnames, 
            const std::vector<size_t>& start,
            const std::vector<size_t>& count) throw(NcException);
    
    SEXP read(const std::vector<std::string> &vnames,
        double start, double end,
        const std::vector<int> &stations,
        const std::vector<std::string> &timeVarNames,
        const std::string & baseTimeName,const std::string& timezone,
        bool readCounts = false) throw(NcException);

    size_t searchTime(NcVar* var,double stime,timeTests test,double timeMult)
        throw(NcException);

    static int rMode(nc_type type);

    static int checkVarTypeCompatibility(int rmode1, int rmode2);

private:

    R_NetcdfConnection* _connection;

    // no copying, assignment
    NetcdfReader(const NetcdfReader&);
    NetcdfReader& operator=(const NetcdfReader&) const;
};

}   // namespace eolts

#endif
