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
        _obj = lengthgets(_obj,length);
        if (_pindx >= 0)
            REPROTECT(_obj,_pindx);
        else
            PROTECT_WITH_INDEX(_obj,&_pindx);
        _length = length;
    }

    SEXP dimobj = PROTECT(allocVector(INTSXP,_dims.size()));
    for (unsigned int i = 0; i < _dims.size(); i++)
        INTEGER(dimobj)[i] = _dims[i];

#ifdef USE_DIMGETS
    dimgets(_obj,dimobj);
#else
    setAttrib(_obj,R_DimSymbol,dimobj);
#endif
    UNPROTECT(1);

    for (unsigned int i = 0; i < _dims.size(); i++) {
        std::vector<std::string>& dnames = dimnames[i];
        if (dnames.size() > 0 && dnames.size() != _dims[i]) dnames.resize(_dims[i]);
        setDimNames(i,dnames);
    }
}
#endif
