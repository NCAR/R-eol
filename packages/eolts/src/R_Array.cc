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
#include <string.h>
#include "R_Array.h"

using std::vector;
using std::string;

// template class R_Array<double>;
// template class R_Array<int>;

template<>
double *R_Array<double>::getDataPtr()
{
    return REAL(getRObject());
}

template<>
R_Array<double>::R_Array(int type, const vector<size_t>& dims) : R_ArrayBase(REALSXP,dims)
{
    double *fp = getDataPtr();
    double *fpend = fp + _length;
    for ( ; fp < fpend; ) *fp++ = NA_REAL;
}

template<>
R_Array<double>::R_Array(int type, SEXP obj) : R_ArrayBase(REALSXP,obj)
{
}

template<>
int *R_Array<int>::getDataPtr()
{
    return INTEGER(getRObject());
}

template<>
R_Array<int>::R_Array(int type, const vector<size_t>& dims) : R_ArrayBase(type,dims)
{
    int *fp = getDataPtr();
    int *fpend = fp + _length;
    if (type == INTSXP)
        for ( ; fp < fpend; ) *fp++ = NA_INTEGER;
    else if (type == LGLSXP)
        for ( ; fp < fpend; ) *fp++ = NA_LOGICAL;
}

template<>
R_Array<int>::R_Array(int type, SEXP obj) : R_ArrayBase(type,obj)
{
}

template<>
char** R_Array<char*>::getDataPtr()
{
    return 0;
}

template<>
R_Array<char*>::R_Array(int type, const vector<size_t>& dims) : R_ArrayBase(type,dims)
{
}

template<>
R_Array<char*>::R_Array(int type, SEXP obj) : R_ArrayBase(type,obj)
{
}

extern "C" {
SEXP create_array(SEXP type, SEXP dims)
{
    SEXP ans = 0;
    if (!isInteger(dims))
        error("dims must be integer");
    vector<size_t> dvec;
    for (unsigned int i = 0; i < (unsigned)length(dims); i++)
        dvec.push_back(INTEGER(dims)[i]);

    if (isReal(type)) {
        R_Array<double> arr(REALSXP,dvec);
        ans = arr.getRObject();
    }
    else if (isInteger(type)) {
        R_Array<int> arr(INTSXP,dvec);
        ans = arr.getRObject();
    }
    else if (isLogical(type)) {
        R_Array<int> arr(LGLSXP,dvec);
        ans = arr.getRObject();
    }
    else if (isString(type)) {
        R_Array<char*> arr(LGLSXP,dvec);
        ans = arr.getRObject();
    }
    if (!ans) error("unvalid type");
    return ans;
}

SEXP set_adims(SEXP x, SEXP dims)
{
    SEXP ans = 0;
    if (!isInteger(dims))
        error("dims must be integer");
    vector<size_t> dvec;
    for (unsigned int i = 0; i < (unsigned)length(dims); i++)
        dvec.push_back(INTEGER(dims)[i]);

    if (isReal(x)) {
        R_Array<double> arr(REALSXP,x);
        arr.setDims(dvec);
        ans = arr.getRObject();
    }
    else if (isInteger(x)) {
        R_Array<int> arr(INTSXP,x);
        arr.setDims(dvec);
        ans = arr.getRObject();
    }
    else if (isLogical(x)) {
        R_Array<int> arr(LGLSXP,x);
        arr.setDims(dvec);
        ans = arr.getRObject();
    }
    else if (isString(x)) {
        R_Array<char*> arr(STRSXP,x);
        arr.setDims(dvec);
        ans = arr.getRObject();
    }
    if (!ans) error("unvalid type of x");
    return ans;
}
SEXP set_adimnames(SEXP x, SEXP names)
{
    SEXP ans = 0;
    if (!isNewList(names))
        warning("names is not a new list"); 
    if (!isList(names))
        warning("names is not a list"); 
    vector<vector<string> > dimnames;
    for (unsigned int i = 0; i < (unsigned) length(names); i++) {
        vector<string> dnames;
        SEXP dns = VECTOR_ELT(names,i);
        for (size_t j = 0; j < (unsigned)length(dns); j++) {
            SEXP dn = STRING_ELT(dns,j);
            dnames.push_back(CHAR(dn));
        }
        dimnames.push_back(dnames);
    }
    
    if (isReal(x)) {
        R_Array<double> arr(REALSXP,x);
        for (unsigned int i = 0; i < dimnames.size(); i++)
            arr.setDimNames(i,dimnames[i]);
        ans = arr.getRObject();
    }
    else if (isInteger(x)) {
        R_Array<int> arr(INTSXP,x);
        for (unsigned int i = 0; i < dimnames.size(); i++)
            arr.setDimNames(i,dimnames[i]);
        ans = arr.getRObject();
    }
    else if (isLogical(x)) {
        R_Array<int> arr(LGLSXP,x);
        for (unsigned int i = 0; i < dimnames.size(); i++)
            arr.setDimNames(i,dimnames[i]);
        ans = arr.getRObject();
    }
    else if (isString(x)) {
        R_Array<char*> arr(STRSXP,x);
        for (unsigned int i = 0; i < dimnames.size(); i++)
            arr.setDimNames(i,dimnames[i]);
        ans = arr.getRObject();
    }
    if (!ans) error("unvalid type of x");
    return ans;
}
}

