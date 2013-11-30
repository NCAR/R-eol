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

namespace eolts {

template class R_Matrix<double>;
template class R_Matrix<int>;
#ifdef DO_CHAR_MATRIX
template class R_Matrix<char*>;
#endif
template class R_Matrix<Rcomplex>;

template<>
double *R_Matrix<double>::getDataPtr()
{
    return REAL(getRObject());
}

template<>
double R_Matrix<double>::naValue() const
{
    return R_NaReal;
}

template<>
R_Matrix<double>::R_Matrix(int type, size_t nr,size_t nc) : R_MatrixBase(REALSXP,nr,nc)
{
    double *fp = getDataPtr();
    double *fpend = fp + _length;
    for ( ; fp < fpend; ) *fp++ = R_NaReal;
}

template<>
R_Matrix<double>::R_Matrix(int type, SEXP obj) : R_MatrixBase(REALSXP,obj)
{
}

template<>
int *R_Matrix<int>::getDataPtr()
{
    return INTEGER(getRObject());
}

template<>
int R_Matrix<int>::naValue() const
{
    return R_NaInt;
}

template<>
R_Matrix<int>::R_Matrix(int type, size_t nr,size_t nc) : R_MatrixBase(type,nr,nc)
{
    int *fp = getDataPtr();
    int *fpend = fp + _length;
    for ( ; fp < fpend; ) *fp++ = R_NaInt;
}

template<>
R_Matrix<int>::R_Matrix(int type, SEXP obj) : R_MatrixBase(type,obj)
{
}

#ifdef DO_CHAR_MATRIX
template<>
R_Matrix<char*>::R_Matrix(size_t nr,size_t nc) : R_MatrixBase(STRSXP,nr,nc)
{
}

template<>
R_Matrix<char*>::R_Matrix(SEXP obj) : R_MatrixBase(STRSXP,obj)
{
}
#endif

template<>
Rcomplex* R_Matrix<Rcomplex>::getDataPtr()
{
    return COMPLEX(getRObject());
}

template<>
Rcomplex R_Matrix<Rcomplex>::naValue() const
{
    const Rcomplex na = {R_NaReal,R_NaReal};
    return na;
}

template<>
R_Matrix<Rcomplex>::R_Matrix(int type, size_t nr,size_t nc) : R_MatrixBase(CPLXSXP,nr,nc)
{
    Rcomplex *fp = getDataPtr();
    Rcomplex *fpend = fp + _length;
    for ( ; fp < fpend; fp++) fp->r = fp->i = NA_REAL;
}

template<>
R_Matrix<Rcomplex>::R_Matrix(int type, SEXP obj) : R_MatrixBase(CPLXSXP,obj)
{
}


}   // namespace eolts

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
        R_Matrix<double> mat(REALSXP,x);
        for (unsigned int i = 0; i < dimnames.size(); i++) 
            mat.setDimNames(i,dimnames[i]);
        ans = mat.getRObject();
    }
    else if (Rf_isInteger(x)) {
        R_Matrix<int> mat(INTSXP,x);
        for (unsigned int i = 0; i < dimnames.size(); i++) 
            mat.setDimNames(i,dimnames[i]);
        ans = mat.getRObject();
    }
    else if (Rf_isLogical(x)) {
        R_Matrix<int> mat(LGLSXP,x);
        for (unsigned int i = 0; i < dimnames.size(); i++) 
            mat.setDimNames(i,dimnames[i]);
        ans = mat.getRObject();
    }
    if (!ans) Rf_error("unvalid type of x");
    return ans;
}

}   // extern "C"


