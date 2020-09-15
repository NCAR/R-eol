// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 * 2013,2014, Copyright University Corporation for Atmospheric Research
 * 
 * This file is part of the "eolts" package for the R software environment.
 * The license and distribution terms for this file may be found in the
 * file LICENSE in this package.
 */

#ifndef R_UTIME_H
#define R_UTIME_H

#include <string>

#include <R.h>
#include <Rinternals.h>

namespace eolts {

class R_utime {
protected:

    SEXP _obj;
    size_t _length;
    PROTECT_INDEX _pindx;
    static SEXP _classDef;

public:
    R_utime();

    /**
     * Create instance from another instance of utime.
     * The SEXP must be PROTECTed by the caller for the
     * lifetime of the R_utime.
     */
    R_utime(SEXP);

    ~R_utime();

    SEXP getRObject() { return _obj; }

    void setTime(size_t index,double t);
    double getTime(size_t index);

    double* getTimePtr();

    void setLength(size_t nr);
    size_t getLength();

    static double parse(const std::string& tstr,const std::string& format,const std::string& tz);

    static std::string format(double,const std::string& format,const std::string& tz);

    // static SEXP dotDataSlotName;

private:
    // declared private to prevent copying and assignment
    R_utime(const R_utime &);
    R_utime &operator=(const R_utime &);
};

}   // namespace eolts

#endif
