// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.2 $
 *  $Date: 2008/09/09 23:42:36 $
 * 
 *  Description:
 *    A NetCDF attribute with a vector of typed values.
 * 
 */
#ifndef NCATTRT_H
#define NCATTRT_H

#include <string>
#include <map>

#include "NcAttr.h"

/**
 * Class providing necessary information about a NetCDF variable.
 */
template <class T> class NcAttrT : public NcAttr {
protected:
    std::vector<T> _value;

public:

    NcAttrT(std::string name, nc_type nctype) :
        NcAttr(name,nctype) {}
    ~NcAttrT() {}

    void setValue(std::vector<T> val) { _value = val; }
    const std::vector<T>& getValue() const { return _value; }

    size_t getLength() const { return _value.size(); }

    T getValue(int i) const { return _value[i]; }

    double getNumericValue(int i) const { return 0.0; }

};

#endif
