// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.2 $
 *  $Date: 2004/02/12 01:46:45 $
 * 
 *  Description:
 *    A class that allows creating an R array from c++.
 * 
 */
// #include <string.h>

#include "R_ArrayBase.h"
#include "R_Array.h"
#include "util.h"

using std::vector;
using std::string;

using namespace eolts;

/*
 * The lifetime of a R_Array  is just during the duration of a .Call,
 * so we can save things like _data, _dim, and _dnobj between
 * method calls because they won't change. (Unless one does a COPY
 * of the array object, I suppose.)
 */

R_ArrayBase::R_ArrayBase(int type,const vector<size_t>& dims):
    _obj(0),_type(type),_dims(dims),_length(0),_pindx(-1),_dnobj(0)
{
    SEXP dimobj;
    // first protect the dimobj
    PROTECT_WITH_INDEX(dimobj = Rf_allocVector(INTSXP,_dims.size()),&_pindx);
    _length = 1;
    for (unsigned int i = 0; i < _dims.size(); i++) {
        INTEGER(dimobj)[i] = _dims[i];
        _length *= _dims[i];
    }

    // then attach the dimobj to the array, and re-use the protection index
    REPROTECT(_obj = Rf_allocArray(type,dimobj),_pindx);
}

/**
 * Create R_ArrayBase from existing R object. Coerce data to given type.
 */
R_ArrayBase::R_ArrayBase(int type, SEXP obj) :
    _obj(obj),_type(type),_dims(),_length(0),_pindx(-1),_dnobj(0)
{
    if (TYPEOF(obj) != type) {
        PROTECT_WITH_INDEX(_obj = Rf_coerceVector(_obj,type),&_pindx);
    }
    _length = LENGTH(_obj);
    SEXP dimobj = Rf_getAttrib(_obj,R_DimSymbol);
    if (Rf_isNull(dimobj)) {
        dimobj = PROTECT(Rf_allocVector(INTSXP,1));
        INTEGER(dimobj)[0] = _length;
        Rf_setAttrib(_obj,R_DimSymbol,dimobj);
        UNPROTECT(1);
        _dims.push_back(_length);
    }
    else {
        unsigned int ndim = LENGTH(dimobj);
        unsigned int len = 1;
        for (unsigned int i = 0; i < ndim; i++) {
            size_t l = INTEGER(dimobj)[i];
            _dims.push_back(l);
            len *= l;
        }
        if (len != _length) Rf_error("incorrect dimensions");
    }
    _dnobj = Rf_getAttrib(_obj,R_DimNamesSymbol);
}

R_ArrayBase::~R_ArrayBase()
{
#ifdef DEBUG
    Rprintf("~R_ArrayBase\n");
#endif
    if (_pindx >= 0) UNPROTECT(1);
}

vector<size_t> R_ArrayBase::getDims(void)
{
    return _dims;
}

vector<string> R_ArrayBase::getDimNames(unsigned int idim)
{
    vector<string> names;

    if (idim >= _dims.size()) {
        Rprintf("R_ArrayBase::getDimNames: idim=%u exceeds maximum index for number of dimensions, %zu\n",idim,_dims.size());
        Rf_error("invalid idim argument");
    }

    if (!_dnobj || Rf_length(_dnobj) == 0) return names;

    if (!Rf_isNewList(_dnobj)) {
        Rf_warning("dimnames is not a list");
        _dnobj = PROTECT(Rf_allocVector(VECSXP,_dims.size()));
        Rf_setAttrib(_obj,R_DimNamesSymbol,_dnobj);
        UNPROTECT(1);
        return names;
    }

    if ((unsigned)Rf_length(_dnobj) != _dims.size()) {
        Rprintf("R_ArrayBase::getDimNames: length of dimnames list, %u, is not equal to number of dimensions, %zu\n",
                (unsigned)Rf_length(_dnobj),_dims.size());
        Rf_error("internal error: bad length of dimnames list");
    }

    SEXP dobj = VECTOR_ELT(_dnobj,idim);
    if (!Rf_isString(dobj) || Rf_length(dobj) == 0) return names;

    if ((unsigned)Rf_length(dobj) != _dims[idim]) {
        Rprintf("R_ArrayBase::getDimNames: length of dimnames for dimension %d is %u, and not equal to dimension length %zu\n",
                idim,(unsigned)Rf_length(dobj),_dims[idim]);
        Rf_error("wrong length for dimnames");
    }

    for (size_t i = 0; i < _dims[idim]; i++) {
        SEXP dn = STRING_ELT(dobj,i);
        names.push_back(CHAR(dn));
    }
    return names;
}

void R_ArrayBase::setDimNames(unsigned int idim,const vector<string>& names)
{
    if (idim >= _dims.size()) {
        Rprintf("R_ArrayBase::setDimNames: idim=%u exceeds maximum index for number of dimensions, %zu\n",
                idim,_dims.size());
        Rf_error("invalid idim argument");
    }

    if (!_dnobj || !Rf_isNewList(_dnobj) || (unsigned) Rf_length(_dnobj) != _dims.size()) {
        _dnobj = PROTECT(Rf_allocVector(VECSXP,_dims.size()));
        Rf_setAttrib(_obj,R_DimNamesSymbol,_dnobj);
        UNPROTECT(1);
    }

    if (names.size() > 0 && names.size() != _dims[idim]) {
        Rprintf("R_ArrayBase::setDimNames: length of dimension %d is %zu, and not equal to number of names for that dimension, %zu\n",
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

