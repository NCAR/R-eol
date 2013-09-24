// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.2 $
 *  $Date: 2004/02/12 01:46:45 $
 * 
 *  Description:
 *    A class that allows creating an Splus array from c++.
 * 
 */
#ifndef R_ARRAYBASE_H
#define R_ARRAYBASE_H

#include <vector>
#include <string>

#include <R.h>
#include <Rinternals.h>

/*
*/
class R_ArrayBase {
protected:

    SEXP _obj;
    int _type;
    std::vector<size_t> _dims;
    size_t _length;
    int _pindx;
    SEXP _dnobj;

public:
    R_ArrayBase(int type, const std::vector<size_t>& dims);
    R_ArrayBase(int type, SEXP obj);
    virtual ~R_ArrayBase();

    SEXP getRObject() const { return _obj; }

    int getType() const
    {
        return _type;
    }

    std::vector<size_t> getDims(void);
    virtual void setDims(const std::vector<size_t>& _dims) = 0;

    std::vector<std::string> getDimNames(unsigned int idim);
    void setDimNames(unsigned int idim,const std::vector<std::string>& names);

    size_t getLength() const { return _length; }

    virtual int getSizeOfT() = 0;

private:
    // declared private to prevent copying and assignment
    R_ArrayBase(const R_ArrayBase &);
    R_ArrayBase &operator=(const R_ArrayBase &);

};

#endif
