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

#include <R.h>

// #include <string>
// #include <vector>

#include "R_NetcdfConnection.h"
#include "R_Matrix.h"
#include "R_Array.h"

class NetcdfReader {
public:
    static int rMode(nc_type type);
    static std::string rModeToString(int smode);

    static int sizeOfRMode(int mode);
    // static void setToNA(char* dp,size_t c0,size_t c1,ptrdiff_t m0, int smode);
    // static void setTo0(char* dp,size_t c0,size_t c1,ptrdiff_t m0, int smode);

    static int checkVarTypeCompatibility(int smode1, int smode2);

protected:

    R_NetcdfConnection* _connection;

public:

    NetcdfReader(R_NetcdfConnection*);
    ~NetcdfReader();

    SEXPREC* read(const std::vector<std::string>& vnames, 
            const std::vector<size_t>& start,
            const std::vector<size_t>& count) NCEXCEPTION_CLAUSE;
};

#endif
