// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 * 2013,2014, Copyright University Corporation for Atmospheric Research
 * 
 * This file is part of the "eolts" package for the R software environment.
 * The license and distribution terms for this file may be found in the
 * file LICENSE in this package.
 */
/*
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
    PROTECT_WITH_INDEX(_obj = Rf_allocVector(type,_length),&_pindx);
    _names = Rf_allocVector(STRSXP,_length);
    Rf_setAttrib(_obj,R_NamesSymbol,_names);
}

R_NamedVectorBase::R_NamedVectorBase(int type, SEXP obj):
    _obj(obj),_type(type),_length(0),_pindx(-1),_names(0)
{

    if (TYPEOF(obj) != type)
        PROTECT_WITH_INDEX(_obj = Rf_coerceVector(_obj,type),&_pindx);

    _length = Rf_xlength(_obj);
    _names = Rf_getAttrib(_obj,R_NamesSymbol);
    if (!_names || !Rf_isString(_names)) {
        _names = Rf_allocVector(STRSXP,_length);
        Rf_setAttrib(_obj,R_NamesSymbol,_names);
    }

    if ((size_t)Rf_xlength(_names) != _length) {
        Rf_warning("length of names of R_NamedVector is not equal to length of vector");
        _names = Rf_xlengthgets(_names,_length);
        Rf_setAttrib(_obj,R_NamesSymbol,_names);
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
        if (_pindx >= 0)
            REPROTECT(_obj = Rf_xlengthgets(_obj,length),_pindx);
        else
            PROTECT_WITH_INDEX(_obj = Rf_xlengthgets(_obj,length),&_pindx);
        _length = length;

        _names = Rf_xlengthgets(_names,_length);
        Rf_setAttrib(_obj,R_NamesSymbol,_names);
    }
}

vector<string> R_NamedVectorBase::getNames()
{
    vector<string> names;
    for (size_t i = 0; i < (size_t)Rf_xlength(_names); i++) {
        SEXP dn = STRING_ELT(_names,i);
        names.push_back(CHAR(dn));
    }
    return names;
}
void R_NamedVectorBase::setNames(const vector<string>& names)
{
    if ((size_t)Rf_xlength(_names) != _length) {
        Rprintf("Rf_xlength(_names)=%td, _length=%zd\n",
                Rf_xlength(_names),_length);
        Rf_error("bad length in R_NamedVectorBase::setNames");
    }

    if (_length != names.size())
        Rf_warning("bad length in R_NamedVectorBase::setNames");

    size_t i;
    for (i = 0; i < names.size() && i < _length; i++)
        SET_STRING_ELT(_names,i,Rf_mkChar(names[i].c_str()));
    for ( ; i < _length; i++)
        SET_STRING_ELT(_names,i,NA_STRING);
}

