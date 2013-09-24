// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.2 $
 *  $Date: 2004/02/11 17:15:24 $
 * 
 *  Description:
 *    Useful functions.
 * 
 */
#include <vector>
#include <string>

#include <R/Rinternals.h>

std::string dimToString(const size_t* d, int nd);

std::string dimToString(const std::vector<size_t>&d);

