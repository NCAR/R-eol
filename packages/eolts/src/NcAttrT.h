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
 *    A NetCDF attribute with a vector of typed values.
 * 
 */
#ifndef NCATTRT_H
#define NCATTRT_H

#include <string>
#include <map>

#include "NcAttr.h"

namespace eolts {

/**
 * Class providing necessary information about a NetCDF variable.
 */
template <class T> class NcAttrT : public NcAttr {
protected:
    std::vector<T> _value;

public:

    NcAttrT(std::string name, nc_type nctype) :
        NcAttr(name,nctype),_value() {}
    ~NcAttrT() {}

    void setValue(std::vector<T> val) { _value = val; }
    const std::vector<T>& getValue() const { return _value; }

    size_t getLength() const { return _value.size(); }

    T getValue(int i) const { return _value[i]; }

    double getNumericValue(int i) const;

    std::string getStringValue(int i) const;

};

}   // namespace eolts

#endif
