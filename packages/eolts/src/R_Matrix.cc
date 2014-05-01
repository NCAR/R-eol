// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.7 $
 *  $Date: 2006/03/29 10:51:57 $
 * 
 *  Description:
 *    R_Matrix constructors for the various types.
 * 
 */

#include "R_Matrix.h"

using std::vector;
using std::string;

using namespace eolts;

extern "C" {
SEXP create_matrix(SEXP type, SEXP nrow,SEXP ncol)
{
    SEXP ans = 0;
    if (!Rf_isInteger(nrow) || !Rf_isInteger(ncol))
        Rf_error("nrow and ncol must be integer");
    size_t nr = INTEGER(nrow)[0];
    size_t nc = INTEGER(ncol)[0];
    if (Rf_isReal(type)) {
        R_Matrix<double> mat(REALSXP,nr,nc);
        ans = mat.getRObject();
    }
    else if (Rf_isInteger(type)) {
        R_Matrix<int> mat(INTSXP,nr,nc);
        ans = mat.getRObject();
    }
    else if (Rf_isLogical(type)) {
        R_Matrix<int> mat(LGLSXP,nr,nc);
        ans = mat.getRObject();
    }
    if (!ans) Rf_error("unvalid type");
    return ans;
}

SEXP set_dims(SEXP x, SEXP nrow,SEXP ncol)
{
    SEXP ans = 0;
    if (!Rf_isInteger(nrow) || !Rf_isInteger(ncol))
        Rf_error("nrow and ncol must be integer");
    size_t nr = INTEGER(nrow)[0];
    size_t nc = INTEGER(ncol)[0];
    if (Rf_isReal(x)) {
        R_Matrix<double> mat(REALSXP,x);
        mat.setDims(nr,nc);
        ans = mat.getRObject();
    }
    else if (Rf_isInteger(x)) {
        R_Matrix<int> mat(INTSXP,x);
        mat.setDims(nr,nc);
        ans = mat.getRObject();
    }
    else if (Rf_isLogical(x)) {
        R_Matrix<int> mat(LGLSXP,x);
        mat.setDims(nr,nc);
        ans = mat.getRObject();
    }
    if (!ans) Rf_error("unvalid type of x");
    return ans;
}
SEXP set_dimnames(SEXP x, SEXP names)
{
    SEXP ans = 0;
    if (!Rf_isNewList(names))
        Rf_error("names is not a list");
    vector<vector<string> > dimnames;
    for (size_t i = 0; i < (size_t) Rf_xlength(names); i++) {
        vector<string> dnames;
        SEXP dns = VECTOR_ELT(names,i);
        for (size_t j = 0; j < (size_t)Rf_xlength(dns); j++) {
            SEXP dn = STRING_ELT(dns,j);
            dnames.push_back(CHAR(dn));
        }
        dimnames.push_back(dnames);
    }
    
    if (Rf_isReal(x)) {
        R_Matrix<double> mat(REALSXP,x);
        for (size_t i = 0; i < dimnames.size(); i++) 
            mat.setDimNames(i,dimnames[i]);
        ans = mat.getRObject();
    }
    else if (Rf_isInteger(x)) {
        R_Matrix<int> mat(INTSXP,x);
        for (size_t i = 0; i < dimnames.size(); i++) 
            mat.setDimNames(i,dimnames[i]);
        ans = mat.getRObject();
    }
    else if (Rf_isLogical(x)) {
        R_Matrix<int> mat(LGLSXP,x);
        for (size_t i = 0; i < dimnames.size(); i++) 
            mat.setDimNames(i,dimnames[i]);
        ans = mat.getRObject();
    }
    if (!ans) Rf_error("unvalid type of x");
    return ans;
}

}   // extern "C"


