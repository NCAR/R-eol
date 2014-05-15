// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 * 2013,2014, Copyright University Corporation for Atmospheric Research
 * 
 * This file is part of the "eolts" package for the R software environment.
 * The license and distribution terms for this file may be found in the
 * file LICENSE in this package.
 */

#include <deque>
#include <list>
#include <algorithm>
#include <limits>
#include <memory>
#include <sstream>

#include <math.h>

#include "NtsMath.h"

using std::vector;
using std::deque;
using std::list;
using std::string;

using namespace eolts;

namespace {     // anonymous namespace
    /**
     * Compute median of values in a container C using the nth_element
     * algorithm in stdc++ lib. Note that this re-arranges the contents
     * of the container. nth_element requires that the container supports
     * a random access iterator, like std::vector and std::deque.
     */
    template<class C>
        typename C::value_type medianOfC(C &v)
        {
            typename C::size_type n = v.size();
            if (n == 0) return std::numeric_limits<typename C::value_type>::quiet_NaN();
            if (n % 2) {    // odd
                n /= 2;
                std::nth_element(v.begin(), v.begin()+n, v.end());
                return (typename C::value_type)v[n];
            }
            else {
                n /= 2;
                std::nth_element(v.begin(), v.begin()+n, v.end());
                typename C::value_type f1 = (typename C::value_type)v[n];
                n--;
                std::nth_element(v.begin(), v.begin()+n, v.end());
                typename C::value_type f2 = (typename C::value_type) v[n];
                return (f1 + f2) / 2.0;
            }
        }
}

SEXP ntsaverage(SEXP obj,SEXP avgperiod, SEXP outinterval, SEXP useWeights)
{

    R_nts nts(obj);
    NtsMath crunch;

    double avg = REAL(avgperiod)[0];
    double out = REAL(outinterval)[0];
    bool wts = LOGICAL(useWeights)[0];

    return crunch.averageMean(nts,avg,out,wts);
}

SEXP ntsaverage_median(SEXP obj,SEXP avgperiod, SEXP outinterval, SEXP outWeights) {

    R_nts nts(obj);
    NtsMath crunch;

    double avg = REAL(avgperiod)[0];
    double out = REAL(outinterval)[0];
    bool wts = LOGICAL(outWeights)[0];

    return crunch.averageMedian(nts,avg,out,wts);
}

NtsMath::NtsMath() {}
NtsMath::~NtsMath() {}

SEXP NtsMath::averageMean(R_nts& ntsin, double avgint,
        double outint, char useWeights)
{

    int k;
    unsigned int ic,ir;

    int nsum = (int) rint(avgint / outint);

    if (fabs(avgint/outint - nsum) > 1.e-5 || nsum < 1) {
        std::ostringstream ost;
        ost <<  "average interval=" << avgint << " is not a multiple of the output interval=" << outint;
        Rf_error(ost.str().c_str());
    }

    R_Matrix<double> matin(REALSXP,ntsin.getMatrix());
    size_t nr = matin.getNrows();
    size_t nc = matin.getNcols();
    double *din = matin.getDataPtr();
    vector<string> colNames = matin.getColumnNames();

    R_utime posin(ntsin.getPositions());

    bool inWeights = useWeights;

    vector<int> wmapin = ntsin.getWeightMap();
    if (wmapin.size() == 0) inWeights = 0;

    std::auto_ptr<R_Matrix<double> > wtsin;
    double *win = 0;
    if (inWeights) {
        wtsin.reset(new R_Matrix<double>(REALSXP,ntsin.getWeights()));
        win = wtsin->getDataPtr();
    }

    int is=0,ns=0;
    int nout,navg = 0;

    double tsum,tsum0;
    double sum;
    double cnt;

    double tt;
    double d;

    bool inWeightCol;
    double weight;

    double t0 = posin.getTime(0);
    tsum0 = ceil(t0 / outint) * outint;
    if (tsum0 == t0) tsum0 += outint;

    vector<double> sums(nsum);
    vector<double> nsums(nsum);
    for (k = 0; k < nsum; k++) {
        sums[k] = 0.;
        nsums[k] = 0;
    }

    nout = (int) ceil((posin.getTime(nr-1) - tsum0) / outint) + 1;
    if (nout < 1) {
        double ttmp = posin.getTime(nr-1);
        std::ostringstream ost;
        ost << "not enough data to average,nout=" << nout <<
            ",dt=" << ttmp-tsum0 <<
            ",nsum=" << nsum << ",tsum0=" << tsum0;
        Rf_error(ost.str().c_str());
    }

    if (inWeights && wmapin.size() != nc) {
        std::ostringstream ost;
        ost << "invalid weightmap, length=" << wmapin.size() <<
            ",ncols=" << nc;
        Rf_error(ost.str().c_str());
    }

    R_nts ntsout;

    R_Matrix<double> matout(REALSXP,nout,nc);
    double *dout0 = matout.getDataPtr();
    double *dout;

    R_utime posout;
    posout.setLength(nout);

    R_Matrix<double> wtsout(REALSXP,nout,nc);
    vector<int> wmapout(nc);

    double *wout0 = wtsout.getDataPtr();
    double *wout;

    int sc,wc = 0;

    for (ic = 0; ic < nc; ic++) {
        dout = dout0 + ic * nout;
        wout = wout0 + ic * nout;

        wmapout[ic] = ic + 1;
        tsum = tsum0;
        is = ns = navg = 0;
        sums[is] = 0.;
        nsums[is] = 0;
        weight = 1;
        inWeightCol = false;

        sc = ic * nr;        /* index of head of column */
        if (inWeights && wmapin[ic] > 0) {
            wc = (wmapin[ic] - 1) * nr;
            inWeightCol = true;
        }
        /* fprintf(stderr,"useWeights=%d, ic=%d, wc=%d\n",(int)useWeights,ic,wc); */

        for (ir = 0; ir < nr; ir++) {
            tt = posin.getTime(ir);

            while (tt > tsum) {       /* sum is finished */
                ns++;
                if (ns >= (nsum-1)/2+1) {
                    sum = 0.;
                    cnt = 0;
                    for (k = 0; k < std::min(nsum,ns); k++) {
                        sum += sums[k];
                        cnt += nsums[k];
                    }

                    // check for data overrun
                    if (navg == nout) {
                        double ttmp = posin.getTime(nr-1);
                        std::ostringstream ost;
                        ost << "navg=" << navg << ",nout=" << nout <<
                            ", tdiff=" << ttmp-tsum0 << ", outint=" << outint <<
                            ",nsum=" << nsum;
                        Rf_error(ost.str().c_str());
                    }
                    if (ic == 0) posout.setTime(navg,tsum - avgint * .5);

                    if (cnt > 0) *dout = sum / cnt;
                    else *dout = NA_REAL;
                    *wout = cnt;
                    dout++;
                    wout++;
                    navg++;
                }

                tsum = tsum0 + outint * ns;
                is = ns % nsum;
                sums[is] = 0.;
                nsums[is] = 0;
            }
            d = din[sc + ir];
            if (inWeightCol) weight = win[wc+ir];

            if (!ISNAN(d))
                { sums[is] += d * weight; nsums[is] += weight; }
        }

        for (int i = 0; i < (nsum-1)/2+1; i++) {
            /* last partial sums */
            ns++;

            sum = 0.;
            cnt = 0;
            for (k = 0; k < std::min(nsum,ns); k++) {
                sum += sums[k];
                cnt += nsums[k];
            }
            // check for data overrun
            if (navg == nout) {
                double ttmp = posin.getTime(nr-1);
                std::ostringstream ost;
                ost << "navg=" << navg << ",nout=" << nout <<
                    ", tdiff=" << ttmp-tsum0 << ", outint=" << outint <<
                    ",nsum=" << nsum;
                Rf_error(ost.str().c_str());
            }
            if (ic == 0) posout.setTime(navg,tsum - avgint * .5);

            if (cnt > 0) *dout = sum / cnt;
            else *dout = NA_REAL;
            *wout = cnt;

            dout++;
            wout++;
            navg++;
            tsum = tsum0 + outint * ns;
            is = ns % nsum;
            sums[is] = 0.;
            nsums[is] = 0;
        }
    }
#ifdef DEBUG
    Rprintf("navg=%d, nout=%d\n",navg,nout);
#endif
    if (navg != nout) {
        matout.setDims(navg,nc);
        wtsout.setDims(navg,nc);
        posout.setLength(navg);
    }
    matout.setColumnNames(colNames);
    wtsout.setColumnNames(colNames);

    ntsout.setMatrix(matout.getRObject());

    ntsout.setPositions(posout.getRObject());
    ntsout.setUnits(ntsin.getUnits());

    ntsout.setStations(ntsin.getStationNames(),ntsin.getStationNumbers());
    ntsout.setWeights(wtsout.getRObject());
    ntsout.setWeightMap(wmapout);
    ntsout.setTimeFormat(ntsin.getTimeFormat());
    ntsout.setTimeZone(ntsin.getTimeZone());

    return ntsout.getRObject();
}

SEXP NtsMath::averageMedian(R_nts& ntsin, double avgint,
        double outint, char outWeights)
{

    int k;
    size_t ic,ir;

    int nint = (int) rint(avgint / outint);

    if (fabs(avgint/outint - nint) > 1.e-5 || nint < 1) {
        std::ostringstream ost;
        ost <<  "average interval=" << avgint << " is not a multiple of the output interval=" << outint;
        Rf_error(ost.str().c_str());
    }

    R_Matrix<double> matin(REALSXP,ntsin.getMatrix());
    size_t nr = matin.getNrows();
    size_t nc = matin.getNcols();
    double *din = matin.getDataPtr();
    vector<string> colNames = matin.getColumnNames();

    R_utime posin(ntsin.getPositions());

    int is,ns=0;
    int nout,navg = 0;

    double tint,tint0;

    double tt;
    double d;

    double t0 = posin.getTime(0);
    tint0 = ceil(t0 / outint) * outint;
    if (tint0 == t0) tint0 += outint;

    double inputres = 0.;
    if (nr > 1) inputres = posin.getTime(1) - posin.getTime(0);
    if (inputres <= 0.) inputres = 1.;
    int neachalloc = (int) (avgint / inputres);
    if (neachalloc < 5) neachalloc = 5;

    nout = (int) ceil((posin.getTime(nr-1) - tint0) / outint) + 1;
    if (nout < 1) {
        double ttmp = posin.getTime(nr-1);
        std::ostringstream ost;
        ost << "not enough data to average,nout=" << nout <<
            ",dt=" << ttmp-tint0 <<
            ",nint=" << nint <<
            ",tint0=" << tint0;
        Rf_error(ost.str().c_str());
    }


    R_Matrix<double> matout(REALSXP,nout,nc);
    double *dout0 = matout.getDataPtr();
    double *dout;

    R_utime posout;
    posout.setLength(nout);

    R_Matrix<double> wtsout(REALSXP,outWeights ? nout: 0,nc);
    vector<int> wmapout(nc);

    double *wout0 = 0;
    if (outWeights) wout0 = wtsout.getDataPtr();
    double *wout;

    int sc;
    vector<int> npoints(nint);   /* how many points in each output interval */

    for (ic = 0; ic < nc; ic++) {

        dout = dout0 + ic * nout;
        wout = wout0 + ic * nout;

        wmapout[ic] = ic + 1;
        tint = tint0;
        is = ns = navg = 0;

        sc = ic * nr;        /* index of head of column */

        deque<double> sortBuffer;   /* data buffer for sorting */
        list<double> dataBuffer;    /* data points in the original time order */
        double lastmedian = 0.0;
        for (k = 0; k < nint; k++)
            npoints[k] = 0;

        for (ir = 0; ir < nr; ir++) {
            tt = posin.getTime(ir);

            while (tt > tint) {       /* at end of interval */
                ns++;
                if (ns >= (nint-1)/2+1) {
                    // check for data overrun
                    if (navg == nout) {
                        double ttmp = posin.getTime(nr-1);
                        std::ostringstream ost;
                        ost << "navg=" << navg << ",nout=" << nout <<
                            ", tdiff=" << ttmp-tint0 << ", outint=" << outint <<
                            ",nint" << nint;
                        Rf_error(ost.str().c_str());
                    }

                    if (sortBuffer.size() > 0) 
                        *dout = lastmedian = medianOfC(sortBuffer);
                    else
                        *dout = NA_REAL;

                    if (outWeights) *wout = sortBuffer.size();

                    if (ic == 0) posout.setTime(navg,tint - avgint * .5);
                    dout++;
                    wout++;
                    navg++;
                    is = (is + 1) % nint;
                    // age out the old points from sortBuffer
                    for (k = 0; k < npoints[is]; k++) {
                        double d0 = dataBuffer.front();
                        dataBuffer.pop_front();
                        deque<double>::iterator di = find(sortBuffer.begin(),sortBuffer.end(),d0);
                        sortBuffer.erase(di);
                    }
                    npoints[is] = 0;
                }
                tint = tint0 + outint * ns;
            }
            d = din[sc + ir];

            if (!ISNAN(d)) {
                if (d > lastmedian) sortBuffer.push_back(d);
                else sortBuffer.push_front(d);
                dataBuffer.push_back(d);
                npoints[is]++;
            }
        }
        for (int i = 0; i < (nint-1)/2+1; i++) {
            ns++;
            /* last partial intervals */

            // check for data overrun
            if (navg == nout) {
                double ttmp = posin.getTime(nr-1);
                std::ostringstream ost;
                ost << "navg=" << navg << ",nout=" << nout <<
                    ", tdiff=" << ttmp-tint0 << ", outint=" << outint <<
                    ",nint" << nint;
                Rf_error(ost.str().c_str());
            }

            if (sortBuffer.size() > 0) 
                *dout = medianOfC(sortBuffer);
            else
                *dout = NA_REAL;
            if (outWeights) *wout = sortBuffer.size();

            if (ic == 0) posout.setTime(navg,tint - avgint * .5);
            dout++;
            wout++;
            navg++;
            is = (is + 1) % nint;
            for (k = 0; k < npoints[is]; k++) {
                double d0 = dataBuffer.front();
                dataBuffer.pop_front();
                deque<double>::iterator di = find(sortBuffer.begin(),sortBuffer.end(),d0);
                sortBuffer.erase(di);
            }
            npoints[is] = 0;
            tint = tint0 + outint * ns;
        }
    }
    // fprintf(stderr,"navg=%d, nout=%d\n",navg,nout);

    if (navg != nout) {
        matout.setDims(navg,nc);
        if (outWeights) wtsout.setDims(navg,nc);
        posout.setLength(navg);
    }

    matout.setColumnNames(colNames);
    if (outWeights) wtsout.setColumnNames(colNames);

    R_nts ntsout;

    ntsout.setMatrix(matout.getRObject());

    ntsout.setPositions(posout.getRObject());

    ntsout.setUnits(ntsin.getUnits());
    ntsout.setStations(ntsin.getStationNames(),ntsin.getStationNumbers());
    if (outWeights) {
        ntsout.setWeights(wtsout.getRObject());
        ntsout.setWeightMap(wmapout);
    }
    ntsout.setTimeFormat(ntsin.getTimeFormat());
    ntsout.setTimeZone(ntsin.getTimeZone());

    return ntsout.getRObject();
}
