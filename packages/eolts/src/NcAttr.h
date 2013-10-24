// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.3 $
 *  $Date: 2008/09/09 23:42:36 $
 * 
 *  Description:
 *    Baseclass for a NetCDF attribute.
 * 
 */
#ifndef NCATTR_H
#define NCATTR_H

#include <string>

#include "NcVar.h"
// #include "NcException.h"
//


namespace eolts {

/**
 * Class providing necessary information about a NetCDF variable.
 */
class NcAttr {
protected:
    std::string _name;
    nc_type _nctype;

public:

    static NcAttr *createNcAttr(NcVar *var,
            int attnum) throw(NcException);

    NcAttr(std::string name,nc_type type) : _name(name),_nctype(type) {}
    virtual ~NcAttr();

    const std::string& getName() const { return _name; }

    nc_type getNcType() const { return _nctype; }

    virtual size_t getLength() const = 0;

    virtual double getNumericValue(int i) const = 0;

};

}   // namespace eolts

#endif
