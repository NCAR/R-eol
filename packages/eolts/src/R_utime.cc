// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
//
#include "R_utime.h"

#include <R_ext/Parse.h>

#include <sstream>

using std::string;

using namespace eolts;

SEXP R_utime::classDef;
SEXP R_utime::dotDataSlotName;

R_utime::R_utime() : _obj(0),_length(1),_pindx(-1)
{

    if (!classDef) classDef = R_do_MAKE_CLASS("utime");

    PROTECT_WITH_INDEX(_obj = R_do_new_object(classDef),&_pindx);

    SEXP cls = PROTECT(Rf_allocVector(STRSXP,1));
    SET_STRING_ELT(cls,0,Rf_mkChar("utime"));
    Rf_classgets(_obj,cls);
    UNPROTECT(1);

#ifdef DEBUG
    Rprintf("R_utime::R_utime(), TYPEOF(_obj)=%d\n",TYPEOF(_obj));
    Rprintf("R_utime::R_utime(), IS_S4_OBJECT(_obj)=%d\n",IS_S4_OBJECT(_obj));
#endif

    _length = Rf_xlength(_obj);
}

R_utime::R_utime(SEXP obj) : _obj(obj),_length(0),_pindx(-1)
{
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
    if (_pindx >= 0) UNPROTECT(1);
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
        const std::string& format,const std::string& tz) throw(std::string)
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
        const std::string& format,const std::string& tz) throw(std::string)
{
    R_utime utime;
    utime.setLength(1);
    utime.setTime(0,time);

    SEXP s, t;

    t = s = PROTECT(Rf_allocList(4));

    SET_TYPEOF(s, LANGSXP);
    SETCAR(t, Rf_install("format")); t = CDR(t);

    SETCAR(t, utime.getRObject()); t = CDR(t);

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
