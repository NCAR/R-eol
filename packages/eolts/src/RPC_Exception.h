// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 */

#ifndef RPC_EXCEPTION_H
#define RPC_EXCEPTION_H

#include <string>

namespace eolts {

class RPC_Exception : public std::exception {

public:
    RPC_Exception(const std::string& msg):_what(msg) {}

    virtual ~RPC_Exception() throw() {}

    const std::string& toString() const { return _what; }

    const char* what() const throw() { return _what.c_str(); }

protected:
    std::string _what;

};

}   // namespace eolts

#endif

