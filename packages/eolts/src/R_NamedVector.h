// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.3 $
 *  $Date: 2004/02/11 17:15:23 $
 * 
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

template class R_NamedVector<double>;
template class R_NamedVector<int>;
// template class R_NamedVector<char*>;

/**
 * Specialization of getDataPtr() for R_NamedVector<double>.
 */
template<>
double *R_NamedVector<double>::getDataPtr()
{
    return REAL(getRObject());
}

/**
 * Specialization of constructor for R_NamedVector<double>.
 */
template<>
R_NamedVector<double>::R_NamedVector(int type,size_t length) :
	R_NamedVectorBase(REALSXP,length)
{
    double *fp = getDataPtr();
    double *fpend = fp + _length;
    for ( ; fp < fpend; ) *fp++ = NA_REAL;
}

/**
 * Specialization of constructor for R_NamedVector<double>.
 */
template<>
R_NamedVector<double>::R_NamedVector(int type,SEXP obj) :
	R_NamedVectorBase(REALSXP,obj)
{
}

/**
 * Specialization of getDataPtr() for R_NamedVector<int>.
 */
template<>
int *R_NamedVector<int>::getDataPtr()
{
    return INTEGER(getRObject());
}

/**
 * Specialization of constructor for R_NamedVector<int>.
 */
template<>
R_NamedVector<int>::R_NamedVector(int type, size_t length) :
	R_NamedVectorBase(type,length)
{
    int *fp = getDataPtr();
    int *fpend = fp + _length;
    if (type == INTSXP)
        for ( ; fp < fpend; ) *fp++ = NA_INTEGER;
    else if (type == LGLSXP)
        for ( ; fp < fpend; ) *fp++ = NA_LOGICAL;
}

/**
 * Specialization of constructor for R_NamedVector<int>.
 */
template<>
R_NamedVector<int>::R_NamedVector(int type, SEXP obj) :
	R_NamedVectorBase(type,obj)
{
}

}   // namespace eolts

#endif
