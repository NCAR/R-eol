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
#endif
