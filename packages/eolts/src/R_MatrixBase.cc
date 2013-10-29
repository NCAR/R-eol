// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.12 $
 *  $Date: 2004/02/11 17:15:23 $
 * 
 *  Description:
 *    A class that allows creating an Splus matrix from c++.
 * 
 */
// #include <string.h>
// #include <assert.h>

#include "R_MatrixBase.h"
#include "R_Matrix.h"
#include "util.h"

using std::vector;
using std::string;

using namespace eolts;

/*
 * The lifetime of a R_Matrix  is just during the duration of a .Call,
 * so we can save things like _data, _dim, and _dnobj between
 * method calls because they won't change. (Unless one does a COPY
 * of the matrix object, I suppose.)
 */

R_MatrixBase::R_MatrixBase(int type,size_t nrow,size_t ncol) : 
    _obj(0),_type(type),_dims(),_length(nrow*ncol),_pindx(-1),_dnobj(0)
{
    _dims[0] = nrow;
    _dims[1] = ncol;
    _obj = Rf_allocMatrix(type,nrow,ncol);
    PROTECT_WITH_INDEX(_obj,&_pindx);
    _dnobj = Rf_getAttrib(_obj,R_DimNamesSymbol);
}

/**
 * Create R_MatrixBase from existing R object. Coerce data to given type.
 */
R_MatrixBase::R_MatrixBase(int type, SEXP obj) :
    _obj(obj),_type(type),_dims(),_length(0),_pindx(-1),_dnobj(0)
{
    if (TYPEOF(obj) != type) {
        Rprintf("coercing R_MatrixBase, TYPEOF(obj)=%d,type=%d\n",
                TYPEOF(obj),type);
        _obj = Rf_coerceVector(_obj,type);
        PROTECT_WITH_INDEX(_obj,&_pindx);
    }
    SEXP dim = Rf_getAttrib(_obj,R_DimSymbol);
    if (Rf_isNull(dim)) {
        _dims[0] = LENGTH(_obj);
        _dims[1] = 1;
        dim = PROTECT(Rf_allocVector(INTSXP,2));
        INTEGER(dim)[0] = _dims[0];
        INTEGER(dim)[1] = _dims[1];
        Rf_setAttrib(_obj,R_DimSymbol,dim);
        UNPROTECT(1);
    }
    else {
        _dims[0] = INTEGER(dim)[0];
        _dims[1] = INTEGER(dim)[1];
    }
    _length = _dims[0] * _dims[1];
    _dnobj = Rf_getAttrib(_obj,R_DimNamesSymbol);
}

R_MatrixBase::~R_MatrixBase() {
#ifdef DEBUG
    Rprintf("~R_MatrixBase, _pindx=%d\n",_pindx);
#endif
    if (_pindx >= 0) UNPROTECT(1);
}

vector<string> R_MatrixBase::getDimNames(unsigned int idim)
{
    const size_t ndims = sizeof(_dims)/sizeof(_dims[0]);
    if (idim >= ndims) {
        Rprintf("R_MatrixBase::getDimNames: idim=%u exceeds maximum index for number of dimensions, %zu\n",idim,ndims);
        Rf_error("invalid idim argument");
    }

    vector<string> names;

    if (!_dnobj || Rf_length(_dnobj) == 0) return names;

    if (!Rf_isNewList(_dnobj)) {
        // Rf_warning("dimnames is not a new list");
        _dnobj = PROTECT(Rf_allocVector(VECSXP,ndims));
        Rf_setAttrib(_obj,R_DimNamesSymbol,_dnobj);
        UNPROTECT(1);
        return names;
    }

    if ((unsigned)Rf_length(_dnobj) != ndims) {
        Rprintf("R_ArrayBase::getDimNames: length of dimnames list, %u, is not equal to number of dimensions, %zu\n",
                (unsigned)Rf_length(_dnobj),ndims);
        Rf_error("internal error: bad length of dimnames list");
    }

    SEXP dobj = VECTOR_ELT(_dnobj,idim);
    if (!Rf_isString(dobj) || Rf_length(dobj) == 0) return names;

    if ((unsigned) Rf_length(dobj) != _dims[idim]) {
        Rprintf("R_MatrixBase::getDimNames: length of dimnames for dimension %d is %u, and not equal to dimension length %zu\n",
                idim,(unsigned)Rf_length(dobj),_dims[idim]);
        Rf_error("wrong length for dimnames");
    }

    for (size_t i = 0; i < _dims[idim]; i++) {
        SEXP dn = STRING_ELT(dobj,i);
        names.push_back(CHAR(dn));
    }
    return names;
}
vector<string> R_MatrixBase::getRowNames()
{
    return getDimNames(0);
}

vector<string> R_MatrixBase::getColumnNames()
{
    return getDimNames(1);
}

void R_MatrixBase::setDimNames(unsigned int idim, const vector<string>& names)
{
    const size_t ndims = sizeof(_dims)/sizeof(_dims[0]);
    if (idim >= ndims) {
        Rprintf("R_MatrixBase::getDimNames: idim=%u exceeds maximum index for number of dimensions, %zu\n",idim,ndims);
        Rf_error("invalid idim argument");
    }

    if (!_dnobj || !Rf_isNewList(_dnobj) || (unsigned) Rf_length(_dnobj) != ndims) {
        _dnobj = PROTECT(Rf_allocVector(VECSXP,ndims));
        Rf_setAttrib(_obj,R_DimNamesSymbol,_dnobj);
        UNPROTECT(1);
    }

    if (names.size() > 0 && names.size() != _dims[idim]) {
        Rprintf("R_MatrixBase::setDimNames: length of dimension %d is %zu, and not equal to number of names for that dimension, %zu\n",
                idim,_dims[idim],names.size());
        Rf_error("wrong length for dimension names");
    }

    SEXP dobj = VECTOR_ELT(_dnobj,idim);
    if (!dobj || (unsigned)Rf_length(dobj) != names.size()) {
        dobj = PROTECT(Rf_allocVector(STRSXP,names.size()));
        SET_VECTOR_ELT(_dnobj,idim,dobj);
        UNPROTECT(1);
    }
        
    for (size_t i = 0; i < names.size(); i++)
        SET_STRING_ELT(dobj,i,Rf_mkChar(names[i].c_str()));
}

void R_MatrixBase::setRowNames(const vector<string>& names)
{
    setDimNames(0,names);
}

void R_MatrixBase::setColumnNames(const vector<string>& names)
{
    setDimNames(1,names);
}

