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
 *    An R_NamedVectorBase of a given type.
 * 
 */
#ifndef R_NAMEDVECTOR_H
#define R_NAMEDVECTOR_H

#include "R_NamedVectorBase.h"

namespace eolts {

/*
*/
template<class T> class R_NamedVector: public R_NamedVectorBase {
public:
    R_NamedVector(int type,size_t length);
    R_NamedVector(int type, SEXP);

    T *getDataPtr();
    int getSizeOfT() { return sizeof(T); }

private:
    // declared private to prevent copying and assignment
    R_NamedVector(const R_NamedVector &);
    R_NamedVector &operator=(const R_NamedVector &);
};

/**
 * Specialization of getDataPtr() for R_NamedVector<double>.
 */
template<>
double *R_NamedVector<double>::getDataPtr();

/**
 * Specialization of constructor for R_NamedVector<double>.
 */
template<>
R_NamedVector<double>::R_NamedVector(int type,size_t length);

/**
 * Specialization of constructor for R_NamedVector<double>.
 */
template<>
R_NamedVector<double>::R_NamedVector(int type,SEXP obj);

/**
 * Specialization of getDataPtr() for R_NamedVector<int>.
 */
template<>
int *R_NamedVector<int>::getDataPtr();

/**
 * Specialization of constructor for R_NamedVector<int>.
 */
template<>
R_NamedVector<int>::R_NamedVector(int type, size_t length);

/**
 * Specialization of constructor for R_NamedVector<int>.
 */
template<>
R_NamedVector<int>::R_NamedVector(int type, SEXP obj);

}   // namespace eolts

#endif
