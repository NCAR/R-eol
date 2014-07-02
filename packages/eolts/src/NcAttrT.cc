// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 * 2013,2014, Copyright University Corporation for Atmospheric Research
 * 
 * This file is part of the "eolts" package for the R software environment.
 * The license and distribution terms for this file may be found in the
 * file LICENSE in this package.
 */

#include <sstream>
#include "NcAttrT.h"

namespace eolts {

template<>
double NcAttrT<double>::getNumericValue(int i) const
{
    return getValue(i);
}

template<>
double NcAttrT<float>::getNumericValue(int i) const
{
    return getValue(i);
}

template<>
double NcAttrT<int>::getNumericValue(int i) const
{
    return (double) getValue(i);
}

template<>
double NcAttrT<std::string>::getNumericValue(int i) const
{
    return 0.0;
}

template<>
std::string NcAttrT<double>::getStringValue(int i) const
{
    std::ostringstream ost;
    ost << getValue(i);
    return ost.str();
}

template<>
std::string NcAttrT<float>::getStringValue(int i) const
{
    std::ostringstream ost;
    ost << getValue(i);
    return ost.str();
}

template<>
std::string NcAttrT<int>::getStringValue(int i) const
{
    std::ostringstream ost;
    ost << getValue(i);
    return ost.str();
}

template<>
std::string NcAttrT<std::string>::getStringValue(int i) const
{
    return getValue(i);
}

}   // namespace eolts
