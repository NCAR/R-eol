// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.8 $
 *  $Date: 2009/04/01 01:35:53 $
 * 
 *  Description:
 *    A class that allows creating an Splus named vector from c++.
 * 
 */
#include "R_NamedVectorBase.h"
#include "util.h"

// #include <string.h>

using std::vector;
using std::string;

using namespace eolts;

/*
 * R "named" class
 */

R_NamedVectorBase::R_NamedVectorBase(int type, size_t length):
    _obj(0),_type(type),_length(length),_pindx(-1),_names(0)
{
    _obj = allocVector(type,_length);
    PROTECT_WITH_INDEX(_obj,&_pindx);
    _names = allocVector(STRSXP,_length);
    setAttrib(_obj,R_NamesSymbol,_names);
}

R_NamedVectorBase::R_NamedVectorBase(int type, SEXP obj):
    _obj(obj),_type(type),_length(0),_pindx(-1),_names(0)
{

    if (TYPEOF(obj) != type) {
        _obj = coerceVector(_obj,type);
        PROTECT_WITH_INDEX(_obj,&_pindx);
    }

    _length = length(_obj);
    _names = getAttrib(_obj,R_NamesSymbol);
    if (!_names || !isString(_names)) {
        _names = allocVector(STRSXP,_length);
        setAttrib(_obj,R_NamesSymbol,_names);
    }

    if ((unsigned)length(_names) != _length) {
        warning("length of names of R_NamedVector is not equal to length of vector");
        _names = lengthgets(_names,_length);
        setAttrib(_obj,R_NamesSymbol,_names);
    }
}

R_NamedVectorBase::~R_NamedVectorBase() {
#ifdef DEBUG
    Rprintf("~R_NamedVectorBase\n");
#endif
    if (_pindx >= 0) UNPROTECT(1);

}

void R_NamedVectorBase::setLength(size_t length)
{
    if (_length != length) {
        _obj = lengthgets(_obj,length);
        if (_pindx >= 0)
            REPROTECT(_obj,_pindx);
        else
            PROTECT_WITH_INDEX(_obj,&_pindx);
        _length = length;

        _names = lengthgets(_names,_length);
        setAttrib(_obj,R_NamesSymbol,_names);
    }
}

vector<string> R_NamedVectorBase::getNames()
{
    vector<string> names;
    for (size_t i = 0; i < (unsigned)length(_names); i++) {
        SEXP dn = STRING_ELT(_names,i);
        names.push_back(CHAR(dn));
    }
    return names;
}
void R_NamedVectorBase::setNames(const vector<string>& names)
{
    if ((unsigned)length(_names) != _length) {
        Rprintf("length(_names)=%d, _length=%d\n",
                (unsigned)length(_names),_length);
        error("bad length in R_NamedVectorBase::setNames");
    }

    if (_length != names.size())
        warning("bad length in R_NamedVectorBase::setNames");

    size_t i;
    for (i = 0; i < names.size() && i < _length; i++)
        SET_STRING_ELT(_names,i,mkChar(names[i].c_str()));
    for ( ; i < _length; i++)
        SET_STRING_ELT(_names,i,NA_STRING);
}

