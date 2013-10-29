// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
//
#include "R_utime.h"

#include <cassert>

using std::string;

using namespace eolts;

SEXP R_utime::classDef;
SEXP R_utime::dotDataSlotName;

R_utime::R_utime() : _obj(0),_length(1),_pindx(-1)
{

    if (!classDef) classDef = R_do_MAKE_CLASS("utime");
    _obj = R_do_new_object(classDef);
    PROTECT_WITH_INDEX(_obj,&_pindx);

#ifdef DEBUG
    Rprintf("R_utime::R_utime(), TYPEOF(_obj)=%d\n",TYPEOF(_obj));
#endif

    assert(TYPEOF(_obj) == REALSXP);
    _length = Rf_length(_obj);
}

R_utime::R_utime(SEXP obj) : _obj(obj),_length(0),_pindx(-1)
{
#ifdef DEBUG
    Rprintf("R_utime::R_utime(SEXP), TYPEOF(obj)=%d\n",TYPEOF(obj));
#endif
    assert(TYPEOF(obj) == REALSXP);

    _length = Rf_length(_obj);

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
        _obj = Rf_lengthgets(_obj,val);
        if (_pindx >= 0) UNPROTECT(1);
        PROTECT_WITH_INDEX(_obj,&_pindx);
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

