// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 * 2013,2014, Copyright University Corporation for Atmospheric Research
 * 
 * This file is part of the "eolts" package for the R software environment.
 * The license and distribution terms for this file may be found in the
 * file LICENSE in this package.
 */

#ifndef NTS_MATH_H
#define NTS_MATH_H

#include <R.h>
#include <Rinternals.h>

#include "R_nts.h"

extern "C" {
    SEXP ntsaverage(SEXP nts,SEXP avgperiod, SEXP outperiod,
            SEXP useWeights);
    SEXP ntsaverage_median(SEXP nts,SEXP avgperiod, SEXP outperiod,
            SEXP useWeights);
}

namespace eolts {

class NtsMath {
public:
  NtsMath();
  ~NtsMath();

  SEXP averageMean(R_nts& , double avgint, double outint, char useWeights);
  SEXP averageMedian(R_nts& , double avgint, double outint, char useWeights);

};

}   // namespace eolts

#endif
