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
 *    R_Array constructors for the supported types
 * 
 */
// #include <string.h>

#include "R_Array.h"

using std::vector;
using std::string;


namespace eolts {

template class R_Array<double>;
template class R_Array<int>;
template class R_Array<char*>;

/**
 * Specialization of getDataPtr() for R_Array<double>.
 */
template<>
double *R_Array<double>::getDataPtr()
{
    return REAL(getRObject());
}

/**
 * Specialization of constructor for R_Array<double>.
 */
template<>
R_Array<double>::R_Array(int type, const std::vector<size_t>& dims) : R_ArrayBase(REALSXP,dims)
{
    double *fp = getDataPtr();
    double *fpend = fp + _length;
    for ( ; fp < fpend; ) *fp++ = NA_REAL;
}

/**
 * Specialization of constructor for R_Array<double>.
 */
template<>
R_Array<double>::R_Array(int type, SEXP obj) : R_ArrayBase(REALSXP,obj)
{
}

/**
 * Specialization of getDataPtr() for R_Array<int>.
 */
template<>
int *R_Array<int>::getDataPtr()
{
    return INTEGER(getRObject());
}

/**
 * Specialization of constructor for R_Array<int>.
 */
template<>
R_Array<int>::R_Array(int type, const std::vector<size_t>& dims) : R_ArrayBase(type,dims)
{
    int *fp = getDataPtr();
    int *fpend = fp + _length;
    if (type == INTSXP)
        for ( ; fp < fpend; ) *fp++ = NA_INTEGER;
    else if (type == LGLSXP)
        for ( ; fp < fpend; ) *fp++ = NA_LOGICAL;
}

/**
 * Specialization of constructor for R_Array<int>.
 */
template<>
R_Array<int>::R_Array(int type, SEXP obj) : R_ArrayBase(type,obj)
{
}

/**
 * Specialization of getDataPtr() for R_Array<char*>.
 */
template<>
char** R_Array<char*>::getDataPtr()
{
    return 0;
}

/**
 * Specialization of constructor for R_Array<char*>.
 */
template<>
R_Array<char*>::R_Array(int type, const std::vector<size_t>& dims) : R_ArrayBase(type,dims)
{
}

/**
 * Specialization of constructor for R_Array<char*>.
 */
template<>
R_Array<char*>::R_Array(int type, SEXP obj) : R_ArrayBase(type,obj)
{
}

}   // namespace eolts

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
