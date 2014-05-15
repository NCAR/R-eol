// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 * 2013,2014, Copyright University Corporation for Atmospheric Research
 * 
 * This file is part of the "eolts" package for the R software environment.
 * The license and distribution terms for this file may be found in the
 * file LICENSE in this package.
 */
/*
 *  Description:
 *    A class that allows creating an Splus matrix from c++.
 * 
 */
#ifndef R_MATRIXBASE_H
#define R_MATRIXBASE_H

#include <vector>
#include <string>

#include <R.h>
#include <Rinternals.h>

namespace eolts {

/*
*/
class R_MatrixBase {
public:
    R_MatrixBase(int type,size_t nrow,size_t ncol);
    R_MatrixBase(int type, SEXP obj);
    virtual ~R_MatrixBase();

    SEXP getRObject() { return _obj; }

    int getType() const
    {
        return _type;
    }

    virtual void setDims(size_t,size_t) = 0;

    std::vector<std::string> getDimNames(size_t idim);
    std::vector<std::string> getRowNames();
    std::vector<std::string> getColumnNames();

    void setDimNames(size_t idim, const std::vector<std::string>& names);
    void setRowNames(const std::vector<std::string>& names);
    void setColumnNames(const std::vector<std::string>& names);

    size_t getNrows() const { return _dims[0]; }
    size_t getNcols() const { return _dims[1]; }
    size_t getLength() const { return _length; }

    virtual unsigned int getSizeOfT() const = 0;

protected:

    SEXP _obj;

    int _type;

    size_t _dims[2];

    size_t _length;

    PROTECT_INDEX _pindx;

    /**
     * Dim names.
     */
    SEXP _dnobj;

private:
    // declared private to prevent copying and assignment
    R_MatrixBase(const R_MatrixBase &);
    R_MatrixBase &operator=(const R_MatrixBase &);

};

}   // namespace eolts

#endif
