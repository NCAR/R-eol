# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsdng" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

check.YTYPE <-
function(YTYPE="height")
{
  YTYPE = tolower(YTYPE)
  if (YTYPE != "pressure" & YTYPE != "height")
    stop("YTYPE CAN ONLY BE pressure OR height !!!!")

  return(YTYPE)
}
