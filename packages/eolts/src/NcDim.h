// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.3 $
 *  $Date: 2008/09/09 23:42:36 $
 * 
 *  Description:
 *    A NetCDF dimension.
 * 
 */
#ifndef NCDIM_H
#define NCDIM_H

#include <string>

#include "NcFile.h"

namespace eolts {
/**
 * Class providing necessary information about a NetCDF dimension.
 */
class NcDim {
protected:
    int _dimid;
    std::string _name;
    size_t _length;
    // bool _valid;
public:

    NcDim(NcFile *nch,int dimid) throw(NcException);
    NcDim(const std::string name, size_t length):_dimid(-1),_name(name),_length(length){}

    int getId() const { return _dimid; }
    const std::string& getName() const { return _name; }
    size_t getLength() const { return _length; }

    void readLength(NcFile*) throw(NcException);

};

}   // namespace eolts

#endif
