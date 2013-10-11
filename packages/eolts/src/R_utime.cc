// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
//
#include "R_utime.h"
#include <cstring>

using std::string;

SEXP R_utime::classDef;
SEXP R_utime::dotDataSlotName;
SEXP R_utime::formatSlotName;
SEXP R_utime::timezoneSlotName;

R_utime::R_utime() : _obj(0),_data(0),_length(1),_pindx(-1)
{

    if (!classDef) classDef = R_do_MAKE_CLASS("utime");
    _obj = R_do_new_object(classDef);

    PROTECT_WITH_INDEX(_obj,&_pindx);

    _data = PROTECT(allocVector(REALSXP,_length));
    for (unsigned int i = 0; i < _length; i++)
        REAL(_data)[i] = 0;

    setAttrib(_obj,dotDataSlotName,_data);
    UNPROTECT(1);   // _data

    // format
    SEXP sobj = PROTECT(allocVector(STRSXP,1));
    SET_STRING_ELT(sobj,0,mkChar("%Y %m %d %02H:%02M%02S.%03N"));
    setAttrib(_obj,formatSlotName,sobj);
    UNPROTECT(1);   // sobj

    sobj = PROTECT(allocVector(STRSXP,1));
    SET_STRING_ELT(sobj,0,mkChar("GMT"));
    setAttrib(_obj,timezoneSlotName,sobj);
    UNPROTECT(1);   // sobj
}

R_utime::R_utime(SEXP obj) : _obj(obj)
{
}

R_utime::~R_utime()
{
    if (_pindx >= 0) UNPROTECT(1);
}

void R_utime::setLength(size_t val)
{
    if (_length != val) {
        _data = PROTECT(lengthgets(_data,val));
        _length = val;
        setAttrib(_obj,dotDataSlotName,_data);
        UNPROTECT(1);
    }
}

size_t R_utime::getLength() { return _length; }

void R_utime::setTime(size_t index,double val)
{
    if (index >= _length) return;
    REAL(_data)[index] = val;
}

double R_utime::getTime(size_t index)
{
    if (index >= _length) return NA_REAL;
    return REAL(_data)[index];
}

void R_utime::setFormat(const string &val)
{
    SEXP sobj = getAttrib(_obj,formatSlotName);
    if (length(sobj) != 1) {
        sobj = PROTECT(lengthgets(sobj,1));
        setAttrib(_obj,formatSlotName,sobj);
        UNPROTECT(1);
    }
    SET_STRING_ELT(sobj,0,mkChar(val.c_str()));
}

string R_utime::getFormat(void)
{
    SEXP sobj = getAttrib(_obj,formatSlotName);
    if (length(sobj) != 1) return "";
    return CHAR(STRING_ELT(sobj,0));
}
void R_utime::setTimeZone(const string &val)
{
    SEXP sobj = getAttrib(_obj,timezoneSlotName);
    if (length(sobj) != 1) {
        sobj = PROTECT(lengthgets(sobj,1));
        setAttrib(_obj,timezoneSlotName,sobj);
        UNPROTECT(1);
    }
    SET_STRING_ELT(sobj,0,mkChar(val.c_str()));
}

string R_utime::getTimeZone(void)
{
    SEXP sobj = getAttrib(_obj,timezoneSlotName);
    if (length(sobj) != 1) return "";
    return CHAR(STRING_ELT(sobj,0));
}
