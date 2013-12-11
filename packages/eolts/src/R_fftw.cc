// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
//
//              Copyright (C) by UCAR
//

#ifdef HAVE_FFTW3

#include <R.h>
#include <Rinternals.h>

#include <R_Matrix.h>

/* FFTW header */
#include <fftw3.h>

using eolts::R_Matrix;

extern "C" {
    SEXP R_cfftw(SEXP nrp,SEXP ncp, SEXP cmatp, SEXP invp);
    SEXP R_rfftw(SEXP nrp,SEXP ncp, SEXP dmatp, SEXP invp);
}

/*
 * From the FFTW tutorial http://www.fftw.org/doc, about rfftw_one:
 * Note also that we use the standard "in-order" output ordering--
 * the k-th output corresponds to the frequency k/n (or k/T, where T
 * is your total sampling period). For those who like to think in terms
 * of positive and negative frequencies, this means that the positive
 * frequencies are stored in the first half of the output and the
 * negative frequencies are stored in backwards order in the second
 * half of the output. (The frequency -k/n is the same as the frequency
 * (n-k)/n.)
 */

SEXP R_cfftw(SEXP nrp,SEXP ncp, SEXP cmatp, SEXP invp)
{

    /*
     * In R, Rcomplex is struct { double r; double i; };
     * In fftw3, fftw_complex is double[2], with the real part in [0].
     * Therefore they are identical in memory.
     * Check the size just to be paranoid.
     */
    if (sizeof(Rcomplex) != sizeof(fftw_complex))
        Rf_error("incompatible complex types");

    if (TYPEOF(nrp) != INTSXP || Rf_length(nrp) != 1)
        Rf_error("nrow parameter is object is not type integer, length 1");
    int nr = INTEGER(nrp)[0];

    if (TYPEOF(ncp) != INTSXP || Rf_length(ncp) != 1)
        Rf_error("ncol parameter is object is not type integer, length 1");
    size_t nc = (size_t)INTEGER(ncp)[0];

    if (TYPEOF(cmatp) != CPLXSXP)
        Rf_error("data parameter is not of type complex");
    R_Matrix<Rcomplex> cmat(CPLXSXP,cmatp);

    if (TYPEOF(invp) != LGLSXP || Rf_length(invp) != 1)
        Rf_error("inverse parameter is object is not type logical, length 1");
    bool inverse = (bool)LOGICAL(invp)[0];

    int sign = (inverse ? FFTW_BACKWARD :  FFTW_FORWARD);

    fftw_complex* cpin = (fftw_complex*) cmat.getDataPtr();

    R_Matrix<Rcomplex> cmatout(CPLXSXP,nr,nc);
    fftw_complex* cpout = (fftw_complex*) cmatout.getDataPtr();

    /*
     * rank=1
     * n=&nr
     * howmany=nc
     * inembed=NULL
     * istride=1
     * idist=nr
     * onembed=NULL
     * ostride=1
     * odist=nr
     */
    /*
     * With flags=FFTW_ESTIMATE, the input/output arrays are not
     * overwritten during planning, which is important.
     * We don't want to change the input array, dpin.
     */
    fftw_plan plan = fftw_plan_many_dft(1,&nr,nc,
            cpin,NULL,1,nr,
            cpout,NULL,1,nr,
            sign,FFTW_ESTIMATE);
    fftw_execute(plan);
    fftw_destroy_plan(plan);

    return cmatout.getRObject();
}

/*
 * From the FFTW tutorial http://www.fftw.org/doc, about rfftw_one:
 *
 * Depending upon the direction of the plan, either the input or the output
 * array is halfcomplex, and is stored in the following format:
 *
 * r0, r1, r2, ..., rn/2, i(n+1)/2-1, ..., i2, i1
 *
 * Here, rk is the real part of the kth output, and ik is the imaginary part.
 * (We follow here the C convention that integer division is rounded down,
 * e.g. 7 / 2 = 3.) For a halfcomplex array hc[], the kth component has its
 * real part in hc[k] and its imaginary part in hc[n-k], with the exception
 * of k == 0 or n/2 (the latter only if n is even)---in these two cases, the
 * imaginary part is zero due to symmetries of the real-complex transform,
 * and is not stored. 
 */
SEXP R_rfftw(SEXP nrp,SEXP ncp, SEXP dmatp, SEXP invp)
{
    if (TYPEOF(nrp) != INTSXP || Rf_length(nrp) != 1)
        Rf_error("nrow parameter is object is not type integer, length 1");
    int nr = INTEGER(nrp)[0];

    if (TYPEOF(ncp) != INTSXP || Rf_length(ncp) != 1)
        Rf_error("ncol parameter is object is not type integer, length 1");
    size_t nc = (size_t)INTEGER(ncp)[0];

    if (TYPEOF(dmatp) != REALSXP)
        Rf_error("data parameter is not of type real");
    R_Matrix<double> dmat(REALSXP,dmatp);

    if (TYPEOF(invp) != LGLSXP || Rf_length(invp) != 1)
        Rf_error("inverse parameter is object is not type logical, length 1");
    bool inverse = (bool)LOGICAL(invp)[0];

    fftw_r2r_kind kind = (inverse ? FFTW_HC2R :  FFTW_R2HC);
    // Rprintf("R_fftw: nr=%zu,nc=%zu,inverse=%d\n",nr,nc,inverse);

    R_Matrix<double> dmatout(REALSXP,nr,nc);
    double* dpout = dmatout.getDataPtr();

    double* dpin = dmat.getDataPtr();

    /*
     * rank=1
     * n=&nr
     * howmany=nc
     * inembed=NULL
     * istride=1
     * idist=nr
     * onembed=NULL
     * ostride=1
     * odist=nr
     */
    /*
     * With flags=FFTW_ESTIMATE, the input/output arrays are not
     * overwritten during planning, which is important.
     * We don't want to change the input array, dpin.
     */
    fftw_plan plan = fftw_plan_many_r2r(1,&nr,nc,
            dpin,NULL,1,nr,
            dpout,NULL,1,nr,
            &kind,FFTW_ESTIMATE);
    fftw_execute(plan);
    fftw_destroy_plan(plan);

    return dmatout.getRObject();
}
#endif  // HAVE_FFTW3
