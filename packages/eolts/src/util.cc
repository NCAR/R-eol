// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.8 $
 *  $Date: 2004/02/12 01:46:45 $
 * 
 *  Description:
 *    Useful functions.
 * 
 */

#include <sstream>

#include "util.h"

using std::vector;
using std::string;

/**
 * Convert a dimension vector to a string - for debugging or error reporting.
 */
string dimToString(const size_t* d, int nd)
{
    std::ostringstream ost;
    for (int i = 0; i < nd; i++) {
        if (i > 0) ost << ',';
        ost << d[i];
    }
    return ost.str();
}

string dimToString(const vector<size_t>& d)
{
    return dimToString(&d.front(),d.size());
}

