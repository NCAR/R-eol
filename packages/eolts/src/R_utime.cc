// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 * 2013,2014, Copyright University Corporation for Atmospheric Research
 * 
 * This file is part of the "eolts" package for the R software environment.
 * The license and distribution terms for this file may be found in the
 * file LICENSE in this package.
 */

#include "R_utime.h"

#include <R_ext/Parse.h>

#include <sstream>

using std::string;

using namespace eolts;

// SEXP R_utime::_dotDataSlotName;
#ifdef STATIC_CLASSDEF
SEXP R_utime::_classDef = 0;
#endif

R_utime::R_utime() : _obj(0),_length(1),_pindx(-1)
#ifndef STATIC_CLASSDEF
                     ,_classDef(0)
#endif
{

    // Seems like a good idea to protect _classDev, though we've run this
    // package with no errors for many years as a static member
    // without PROTECTing it. Perhaps the pointer to the the class is saved
    // in the object, and so doesn't need to be protected.
    //
    // If _classDev is static and PROTECTed then we see
    // "Warning: stack imbalance in '<-', 2 then 3" from R, because
    // it isn't (ever) UNPROTECTed. So if it is static, then don't
    // PROTECT it.
    //
    // We'll make it non-static and PROTECT it, and UNPROTECT it and _obj
    // in the destructor.

#ifdef STATIC_CLASSDEF
    if (!_classDef) _classDef =  R_do_MAKE_CLASS("utime");
#else
    _classDef =  PROTECT(R_do_MAKE_CLASS("utime"));
#endif

    PROTECT_WITH_INDEX(_obj = R_do_new_object(_classDef),&_pindx);

#ifdef DEBUG
    Rprintf("pindx=%d\n", _pindx);
    Rprintf("R_utime::R_utime(), TYPEOF(_obj)=%d\n",TYPEOF(_obj));
    Rprintf("R_utime::R_utime(), IS_S4_OBJECT(_obj)=%d\n",IS_S4_OBJECT(_obj));
#endif

    _length = Rf_xlength(_obj);
}

R_utime::R_utime(SEXP obj) : _obj(obj),_length(0),_pindx(-1)
{
    // Assumes obj has been protected.
#ifdef DEBUG
    Rprintf("R_utime::R_utime(SEXP), TYPEOF(obj)=%d\n",TYPEOF(obj));
#endif

    _length = Rf_xlength(_obj);

#ifdef DEBUG
    Rprintf("R_utime::R_utime(SEXP), _length=%d, val[0]=%f\n",
        _length,getTime(0));
#endif
}

R_utime::~R_utime()
{
#ifdef DEBUG
    Rprintf("~R_utime, _pindx=%d\n",_pindx);
#endif
#ifdef STATIC_CLASSDEF
    if (_pindx >= 0) UNPROTECT(1);
#else
    if (_pindx >= 0) UNPROTECT(2);
#endif
}

void R_utime::setLength(size_t val)
{
    if (_length != val) {
        if (_pindx >= 0)
            REPROTECT(_obj = Rf_xlengthgets(_obj,val),_pindx);
        else
            PROTECT_WITH_INDEX(_obj = Rf_xlengthgets(_obj,val),&_pindx);
        _length = val;
    }
}

size_t R_utime::getLength() { return _length; }

void R_utime::setTime(size_t index,double val)
{
    if (index >= _length) return;
    REAL(_obj)[index] = val;
}

double R_utime::getTime(size_t index)
{
#ifdef DEBUG
    Rprintf("getTime, index=%zd\n",index);
#endif
    if (index >= _length) return NA_REAL;
    return REAL(_obj)[index];
}

double* R_utime::getTimePtr()
{
    return REAL(_obj);
}

/* static */
double R_utime::parse(const std::string& tstr,
        const std::string& format,const std::string& tz)
{

    int nunprot;

#ifdef DO_R_PARSE

    // for future information, here's how to parse and evaluate
    // an R expression in a string.
    string cmd = string("utime(\"" + tstr + "\",in.format=\"" + format +
        "\",time.zone=\"" + tz + "\")");
    SEXP cmdSexp = PROTECT(Rf_allocVector(STRSXP,1));
    SET_STRING_ELT(cmdSexp,0,Rf_mkChar(cmd.c_str()));
    ParseStatus status;
    SEXP cmdexpr = PROTECT(R_ParseVector(cmdSexp,-1,&status,R_NilValue));
    if (status != PARSE_OK) {
        UNPROTECT(2);
        throw string("cannot parse ") + cmd;
    }
    if (Rf_length(cmdexpr) != 1) {
        UNPROTECT(2);
        throw string("unexpected return from ") + cmd;
    }

    SEXP res = PROTECT(Rf_eval(VECTOR_ELT(cmdexpr,0),R_GlobalEnv));
    nunprot = 3;

#else
    // build an expression pairlist and evaluate it.
    SEXP s, t;
    t = s = PROTECT(Rf_allocList(4));
    SET_TYPEOF(s, LANGSXP);

    SETCAR(t, Rf_install("utime")); t = CDR(t);

    SETCAR(t, Rf_mkString(tstr.c_str())); t = CDR(t);

    SETCAR(t, Rf_mkString(format.c_str()));
    SET_TAG(t, Rf_install("in.format")); t = CDR(t);

    SETCAR(t, Rf_mkString(tz.c_str()));
    SET_TAG(t, Rf_install("time.zone")); t = CDR(t);

    SEXP res = PROTECT(Rf_eval(s, R_GlobalEnv));

    nunprot = 2;
#endif

#ifdef DEBUG
    Rprintf("res %d, TYPEOF=%d,length=%d\n",
        i,TYPEOF(res),Rf_length(res));
#endif

    double val = NA_REAL;
    if (TYPEOF(res) == REALSXP && Rf_length(res) == 1) {
        val = REAL(res)[0];
        if (ISNAN(val)) {
            UNPROTECT(nunprot);
            throw string("cannot parse: \"") + tstr +
                "\" with format \"" + format + "\", time zone=" + tz;
        }
    }
    UNPROTECT(nunprot);
    return val;
}

/* static */
string R_utime::format(double time,
        const std::string& format,const std::string& tz)
{
    R_utime utime;
    utime.setLength(1);
    utime.setTime(0,time);

    SEXP s, t;

    t = s = PROTECT(Rf_allocList(4));

    SET_TYPEOF(s, LANGSXP);

    SETCAR(t, Rf_install("format_utime")); t = CDR(t);

    // this results in the numeric part of a utime being string-ified,
    // and so the format method will be called for numeric.
    // So we have to define a format_time method to convert the
    // numeric to a utime and format it.
    SETCAR(t, utime.getRObject());
    SET_TAG(t, Rf_install("x")); t = CDR(t);

    SETCAR(t, Rf_mkString(format.c_str()));
    SET_TAG(t, Rf_install("format")); t = CDR(t);

    SETCAR(t, Rf_mkString(tz.c_str()));
    SET_TAG(t, Rf_install("time.zone")); t = CDR(t);

    SEXP res = PROTECT(Rf_eval(s, R_GlobalEnv));

    if (Rf_length(res) != 1 || TYPEOF(res) != STRSXP) {
        std::ostringstream ost;
        ost << "format result is not string of length 1, length=" << Rf_length(res);
        UNPROTECT(2);
        throw(ost.str());
    }

    string str(CHAR(STRING_ELT(res,0)));
    UNPROTECT(2);
    return str;
}
