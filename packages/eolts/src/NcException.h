// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.2 $
 *  $Date: 2006/03/29 03:28:16 $
 * 
 *  Description:
 *    General exception for errors encountered while accessing a NetCDF file.
 * 
 */

#ifndef NCEXCEPTION_H
#define NCEXCEPTION_H

#include <string>

class NcFile;
class NcVar;

class NcException : public std::exception {

public:
    NcException();
    NcException(const std::string& filename, const std::string& msg);
    NcException(const std::string& op,const std::string& fname, int status);
    NcException(const std::string& op,const std::string& fname,
            const std::string& vname, int status);

    virtual ~NcException() throw() {}

    const std::string& toString() const { return _what; }

    const char* what() const throw() { return _what.c_str(); }

    int getStatus() const { return _status; }

protected:
    std::string _what;
    int _status;

};

#endif

