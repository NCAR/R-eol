// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.1 $
 *  $Date: 2004/02/11 17:15:23 $
 * 
 *  Description:
 *    A class that allows creating an instance of an Splus NetcdfVariable
 *    object.
 * 
 */
#ifndef R_NETCDFVARIABLE_H
#define R_NETCDFVARIABLE_H

#include <R.h>

#include "R_netcdf.h"

namespace eolts {

class R_NetcdfVariable {

public:
    /**
     * for creating an R_NetcdfVariable object from scratch.
     */
    R_NetcdfVariable(R_netcdf *,NcVar* var);

    /**
     */
    ~R_NetcdfVariable();

    SEXP getRObject() { return _obj; }

    const std::string& getName() const { return _name; }

    R_netcdf *getConnection() const { return _connection; }

    void setMode(const std::string& mode);
    void setNcType(const std::string& type);
    void setDimensions(const std::vector<const NcDim*>&);
    void setAttributes(const std::vector<const NcAttr*>&);

    static SEXP classDef;
    static SEXP nameSlotName;
    static SEXP modeSlotName;
    static SEXP nctypeSlotName;
    static SEXP dimensionSlotName;
    static SEXP attributeSlotName;

private:

    SEXP _obj;

    std::string _name;

    R_netcdf* _connection;

    std::string _type;	// "character","single","integer"

    // declared private to prevent copying and assignment
    R_NetcdfVariable(const R_NetcdfVariable &);
    R_NetcdfVariable &operator=(const R_NetcdfVariable &);

};

}   // namespace eolts

#endif
