// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.3 $
 *  $Date: 2006/03/29 10:51:57 $
 * 
 *  Description:
 *    An R_ArrayBase of a given type.
 * 
 */
#ifndef R_ARRAY_H
#define R_ARRAY_H

#include "R_ArrayBase.h"

namespace eolts {

/*
*/
template<class T> class R_Array: public R_ArrayBase {
public:
    R_Array(int type, const std::vector<size_t>& dims);
    R_Array(int type, SEXP);

    T *getDataPtr();
    int getSizeOfT() { return sizeof(T); }

    /**
     * Set dimension of this R_Array.
     */
    void setDims(const std::vector<size_t>& val);

private:
    // declared private to prevent copying and assignment
    R_Array(const R_Array &);
    R_Array &operator=(const R_Array &);
};

/**
 * Specialization of getDataPtr() for R_Array<double>.
 */
template<>
double *R_Array<double>::getDataPtr();

/**
 * Specialization of constructor for R_Array<double>.
 */
template<>
R_Array<double>::R_Array(int type, const std::vector<size_t>& dims);

/**
 * Specialization of constructor for R_Array<double>.
 */
template<>
R_Array<double>::R_Array(int type, SEXP obj);

/**
 * Specialization of getDataPtr() for R_Array<int>.
 */
template<>
int *R_Array<int>::getDataPtr();

/**
 * Specialization of constructor for R_Array<int>.
 */
template<>
R_Array<int>::R_Array(int type, const std::vector<size_t>& dims);

/**
 * Specialization of constructor for R_Array<int>.
 */
template<>
R_Array<int>::R_Array(int type, SEXP obj);

/**
 * Specialization of getDataPtr() for R_Array<char*>.
 */
template<>
char** R_Array<char*>::getDataPtr();

/**
 * Specialization of constructor for R_Array<char*>.
 */
template<>
R_Array<char*>::R_Array(int type, const std::vector<size_t>& dims);

/**
 * Specialization of constructor for R_Array<char*>.
 */
template<>
R_Array<char*>::R_Array(int type, SEXP obj);

template<class T>
void R_Array<T>::setDims(const std::vector<size_t>& dims)
{
    std::vector<std::vector<std::string> > dimnames;
    for (unsigned int i = 0; i < _dims.size(); i++) {
        std::vector<std::string> dnames = getDimNames(i);
        dimnames.push_back(dnames);
    }

    _dims = dims;

    size_t length = 1;
    for (unsigned int i = 0; i < _dims.size(); i++) length *= _dims[i];

    if (length != _length) {
        if (_pindx >= 0)
            REPROTECT(_obj = Rf_lengthgets(_obj,length), _pindx);
        else
            PROTECT_WITH_INDEX(_obj = Rf_lengthgets(_obj,length),&_pindx);
        _length = length;
    }

    SEXP dimobj = PROTECT(Rf_allocVector(INTSXP,_dims.size()));
    for (unsigned int i = 0; i < _dims.size(); i++)
        INTEGER(dimobj)[i] = _dims[i];

#ifdef USE_DIMGETS
    Rf_dimgets(_obj,dimobj);
#else
    Rf_setAttrib(_obj,R_DimSymbol,dimobj);
#endif
    UNPROTECT(1);

    for (unsigned int i = 0; i < _dims.size(); i++) {
        std::vector<std::string>& dnames = dimnames[i];
        if (dnames.size() > 0 && dnames.size() != _dims[i]) dnames.resize(_dims[i]);
        setDimNames(i,dnames);
    }
}

}   // namespace eolts

#endif
