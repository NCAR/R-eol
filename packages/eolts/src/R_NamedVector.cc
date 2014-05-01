// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.3 $
 *  $Date: 2006/03/29 10:51:57 $
 * 
 *  Description:
 *    Constructors for R_NamedVector of the various types
 * 
 */

#include "R_NamedVector.h"

using namespace eolts;

template<>
double *R_NamedVector<double>::getDataPtr()
{
    return REAL(getRObject());
}

template<>
R_NamedVector<double>::R_NamedVector(int type,size_t length) :
	R_NamedVectorBase(REALSXP,length)
{
    double *fp = getDataPtr();
    double *fpend = fp + _length;
    for ( ; fp < fpend; ) *fp++ = NA_REAL;
}

template<>
R_NamedVector<double>::R_NamedVector(int type,SEXP obj) :
	R_NamedVectorBase(REALSXP,obj)
{
}

template<>
int *R_NamedVector<int>::getDataPtr()
{
    return INTEGER(getRObject());
}

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

template<>
R_NamedVector<int>::R_NamedVector(int type, SEXP obj) :
	R_NamedVectorBase(type,obj)
{
}

