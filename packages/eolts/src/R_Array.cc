// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.4 $
 *  $Date: 2009/04/01 01:35:53 $
 * 
 *  Description:
 *    R_Array constructors for the supported types
 * 
 */
// #include <string.h>

#include "R_Array.h"

using std::vector;
using std::string;

using namespace eolts;

extern "C" {
SEXP create_array(SEXP type, SEXP dims)
{
    SEXP ans = 0;
    if (!Rf_isInteger(dims))
        Rf_error("dims must be integer");
    vector<size_t> dvec;
    for (unsigned int i = 0; i < (unsigned)Rf_length(dims); i++)
        dvec.push_back(INTEGER(dims)[i]);

    if (Rf_isReal(type)) {
        R_Array<double> arr(REALSXP,dvec);
        ans = arr.getRObject();
    }
    else if (Rf_isInteger(type)) {
        R_Array<int> arr(INTSXP,dvec);
        ans = arr.getRObject();
    }
    else if (Rf_isLogical(type)) {
        R_Array<int> arr(LGLSXP,dvec);
        ans = arr.getRObject();
    }
    else if (Rf_isString(type)) {
        R_Array<char*> arr(LGLSXP,dvec);
        ans = arr.getRObject();
    }
    if (!ans) Rf_error("unvalid type");
    return ans;
}

SEXP set_adims(SEXP x, SEXP dims)
{
    SEXP ans = 0;
    if (!Rf_isInteger(dims))
        Rf_error("dims must be integer");
    vector<size_t> dvec;
    for (unsigned int i = 0; i < (unsigned)Rf_length(dims); i++)
        dvec.push_back(INTEGER(dims)[i]);

    if (Rf_isReal(x)) {
        R_Array<double> arr(REALSXP,x);
        arr.setDims(dvec);
        ans = arr.getRObject();
    }
    else if (Rf_isInteger(x)) {
        R_Array<int> arr(INTSXP,x);
        arr.setDims(dvec);
        ans = arr.getRObject();
    }
    else if (Rf_isLogical(x)) {
        R_Array<int> arr(LGLSXP,x);
        arr.setDims(dvec);
        ans = arr.getRObject();
    }
    else if (Rf_isString(x)) {
        R_Array<char*> arr(STRSXP,x);
        arr.setDims(dvec);
        ans = arr.getRObject();
    }
    if (!ans) Rf_error("unvalid type of x");
    return ans;
}
SEXP set_adimnames(SEXP x, SEXP names)
{
    SEXP ans = 0;
    if (!Rf_isNewList(names))
        Rf_warning("names is not a new list"); 
    if (!Rf_isList(names))
        Rf_warning("names is not a list"); 
    vector<vector<string> > dimnames;
    for (unsigned int i = 0; i < (unsigned) Rf_length(names); i++) {
        vector<string> dnames;
        SEXP dns = VECTOR_ELT(names,i);
        for (size_t j = 0; j < (unsigned)Rf_length(dns); j++) {
            SEXP dn = STRING_ELT(dns,j);
            dnames.push_back(CHAR(dn));
        }
        dimnames.push_back(dnames);
    }
    
    if (Rf_isReal(x)) {
        R_Array<double> arr(REALSXP,x);
        for (unsigned int i = 0; i < dimnames.size(); i++)
            arr.setDimNames(i,dimnames[i]);
        ans = arr.getRObject();
    }
    else if (Rf_isInteger(x)) {
        R_Array<int> arr(INTSXP,x);
        for (unsigned int i = 0; i < dimnames.size(); i++)
            arr.setDimNames(i,dimnames[i]);
        ans = arr.getRObject();
    }
    else if (Rf_isLogical(x)) {
        R_Array<int> arr(LGLSXP,x);
        for (unsigned int i = 0; i < dimnames.size(); i++)
            arr.setDimNames(i,dimnames[i]);
        ans = arr.getRObject();
    }
    else if (Rf_isString(x)) {
        R_Array<char*> arr(STRSXP,x);
        for (unsigned int i = 0; i < dimnames.size(); i++)
            arr.setDimNames(i,dimnames[i]);
        ans = arr.getRObject();
    }
    if (!ans) Rf_error("unvalid type of x");
    return ans;
}
}
