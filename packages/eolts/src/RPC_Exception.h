// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 * 2013,2014, Copyright University Corporation for Atmospheric Research
 * 
 * This file is part of the "eolts" package for the R software environment.
 * The license and distribution terms for this file may be found in the
 * file LICENSE in this package.
 */

#ifndef RPC_EXCEPTION_H
#define RPC_EXCEPTION_H

#include <string>

#ifndef NOEXCEPT
#if __cplusplus >= 201103L  // We are using C++11 or a later version
#define NOEXCEPT noexcept
#else
#define NOEXCEPT throw()
#endif
#endif


namespace eolts {

class RPC_Exception : public std::exception {

public:
    RPC_Exception(const std::string& msg):_what(msg) {}

    virtual ~RPC_Exception() NOEXCEPT {}

    const std::string& toString() const { return _what; }

    const char* what() const NOEXCEPT { return _what.c_str(); }

protected:
    std::string _what;

};

}   // namespace eolts

#endif

