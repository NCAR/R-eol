// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.5 $
 *  $Date: 2004/02/11 17:15:23 $
 * 
 *  Description:
 *    A class that allows creating an Splus named vector from c++.
 * 
 */
#ifndef R_NAMEDVECTORBASE_H
#define R_NAMEDVECTORBASE_H

#include <R.h>
#include <Rinternals.h>

#include <vector>
#include <string>

namespace eolts {

class R_NamedVectorBase {
protected:
    SEXP _obj;
    int _type;
    size_t _length;
    PROTECT_INDEX _pindx;
    SEXP _names;

public:

    R_NamedVectorBase(int type,size_t length);
    R_NamedVectorBase(int type,SEXP);
    virtual ~R_NamedVectorBase();

    SEXP getRObject() { return _obj; }

    int getType() const
    {
        return _type;
    }

    virtual int getSizeOfT() = 0;

    void setNames(const std::vector<std::string>&);
    std::vector<std::string> getNames();

    size_t getLength() const
    {
        return _length;
    }

    void setLength(size_t);

private:
    // declared private to prevent copying and assignment
    R_NamedVectorBase(const R_NamedVectorBase &);
    R_NamedVectorBase &operator=(const R_NamedVectorBase &);


};

}   // namespace eolts

#endif
