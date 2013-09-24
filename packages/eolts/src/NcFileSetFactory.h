// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.2 $
 *  $Date: 2006/03/29 10:51:57 $
 * 
 *  Description:
 *    A factory base class for creating NcFiles and NcFileSets.
 * 
 */
#ifndef NCFILESETFACTORY_H
#define NCFILESETFACTORY_H

#include <vector>
#include <string>

#include "NcFileSet.h"

/*
 * Create a default NcFileSet
 */
class NcFileSetFactory {

public:
    virtual ~NcFileSetFactory() {}

    virtual NcFileSet* createNcFileSet(const std::vector<std::string>& names) {
        return new NcFileSet(names,this);
    }
    virtual NcFile* createNcFile(const std::string& name) {
        return new NcFile(name);
    }
};

#endif
