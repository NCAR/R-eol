// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.9 $
 *  $Date: 2004/02/11 17:15:23 $
 * 
 *  Description:
 *    An R_Matrix of a given type.
 * 
 */
#ifndef R_MATRIX_H
#define R_MATRIX_H

#include "R_MatrixBase.h"

namespace eolts {

/*
*/
template<class T> class R_Matrix: public R_MatrixBase {
public:
    R_Matrix(int type, size_t nr, size_t nc);
    R_Matrix(int type, SEXP);

    T *getDataPtr();
    int getSizeOfT() { return sizeof(T); }

    T naValue() const;

    /*
     * setDim() must be a template method because when changing
     * dimensions we have to move data around.
     */
    void setDims(size_t nr,size_t nc);

private:
    // declared private to prevent copying and assignment
    R_Matrix(const R_Matrix &);
    R_Matrix &operator=(const R_Matrix &);
};

#define DEBUG
#undef DEBUG
template<class T>
void R_Matrix<T>::setDims(size_t nr,size_t nc)
{
    size_t onr = _dims[0];
    size_t onc = _dims[1];
    T *fp,*fpn,*fpo;

#ifdef DEBUG
    Rprintf("onr=%d, onc=%d, nr=%d, nc=%d\n",onr,onc,nr,nc);
#endif

    /* data is stored by column, so adjusting the number of
     * rows requires moving elements. First adjust
     * the number of rows, then number of columns */

    if (nr > onr) {
        /* number of rows has increased */
#ifdef DEBUG
        Rprintf("Rf_lengthgets %d, _pindx=%d\n",nr*onc,_pindx);
#endif
        if (_pindx >= 0)
            REPROTECT(_obj = Rf_lengthgets(_obj,nr*onc),_pindx);
        else
            PROTECT_WITH_INDEX(_obj = Rf_lengthgets(_obj,nr*onc),&_pindx);

        SEXP dimobj = PROTECT(Rf_allocVector(INTSXP,2));
        INTEGER(dimobj)[0] = nr;
        INTEGER(dimobj)[1] = onc;
#define USE_DIMGETS
#ifdef USE_DIMGETS
        Rf_dimgets(_obj,dimobj);
#else
        Rf_setAttrib(_obj,R_DimSymbol,dimobj);
#endif
        UNPROTECT(1);

        fp =  getDataPtr();
        fpn = fp + nr * onc;
        fpo = fp + onr * onc; 

        size_t nd = nr - onr;
        size_t ir;
        if (onc > 0) {
            for (size_t ic = onc - 1; ; ic--) {
#ifdef DEBUG
                Rprintf("ic=%d, onc=%d\n",ic,onc);
#endif
                // don't need to na_set starting at element onc * onr,
                // R has done it already when allocating.
                if (ic * nr + onr >= onc * onr) {
                    ir = nd;
                    fpn -= nd;
                }
                else {
                    for (ir = 0; ir < nd; ir++) {
                        *(--fpn) = naValue();
#ifdef DEBUG
                        Rprintf("na_set fpn-fp=%d fpo-fp=%d\n",fpn - fp, fpo-fp);
#endif
                    }
                }
                for (; ir < nr; ir++) {
                    *(--fpn) = *(--fpo);
#ifdef DEBUG
                    Rprintf("assign fpn-fp=%d fpo-fp=%d\n",fpn - fp, fpo-fp);
#endif
                }
                if (ic == 0) break;
            }
        }
    }
    else if (nr < onr) {
        /* number of rows has decreased.  We need to move
         * the data around, then reallocate.
         */
        fp =  getDataPtr();

        fpn = fp + nr;
        for (size_t ic = 1; ic < onc; ic++) {
            fpo = fp + ic * onr; 
            for (size_t ir = 0; ir < nr; ir++) *fpn++ = *fpo++;
        }

        if (_pindx >= 0)
            REPROTECT(_obj = Rf_lengthgets(_obj,nr*onc),_pindx);
        else
            PROTECT_WITH_INDEX(_obj = Rf_lengthgets(_obj,nr*onc),&_pindx);

        SEXP dimobj = PROTECT(Rf_allocVector(INTSXP,2));
        INTEGER(dimobj)[0] = nr;
        INTEGER(dimobj)[1] = onc;
#ifdef USE_DIMGETS
        Rf_dimgets(_obj,dimobj);
#else
        Rf_setAttrib(_obj,R_DimSymbol,dimobj);
#endif
        UNPROTECT(1);

#ifdef DEBUG
        Rprintf("R_Matrix _length=%d\n",nr*onc);
#endif
    }

    /* changing the number of columns is easy, just let R
     * reallocate and copy the old data. It does the appropriate
     * NA fill.
     */
    if (onc != nc) {
        if (_pindx >= 0)
            REPROTECT(_obj = Rf_lengthgets(_obj,nr*nc),_pindx);
        else
            PROTECT_WITH_INDEX(_obj = Rf_lengthgets(_obj,nr*nc),&_pindx);

        SEXP dimobj = PROTECT(Rf_allocVector(INTSXP,2));
        INTEGER(dimobj)[0] = nr;
        INTEGER(dimobj)[1] = nc;
#ifdef USE_DIMGETS
        Rf_dimgets(_obj,dimobj);
#else
        Rf_setAttrib(_obj,R_DimSymbol,dimobj);
#endif
        UNPROTECT(1);
#ifdef DEBUG
        Rprintf("R_Matrix _length=%d\n",nr*nc);
#endif
    }

    _dims[0] = nr;
    _dims[1] = nc;
    _length = nr * nc;
}

/*
 * transpose a nr x nc matrix at m1 to m2.
 * sufficient space has been allocated at m2
 */
template<class T1, class T2>
void transpose(T1 *m1,int nr, int nc, T2 *m2) {
    for (int i = 0; i < nc; i++)
        for (int j = 0; j < nr; j++)
            *m2++ = (T2)m1[j * nc + i];
}

}   // namespace eolts

#endif
