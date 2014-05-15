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
 *    Useful functions.
 * 
 */
#include <vector>
#include <string>

#include <stdlib.h>

namespace eolts {

std::string dimToString(const size_t* d, size_t nd);
std::string dimToString(const ssize_t* d, size_t nd);

std::string dimToString(const std::vector<size_t>&d);
std::string dimToString(const std::vector<ssize_t>&d);

std::string rModeToString(int rmode);

int sizeOfRMode(int mode);

}   // namespace eolts

