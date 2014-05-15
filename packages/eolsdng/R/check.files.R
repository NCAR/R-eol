# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsdng" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

check.files <-
function(fdir)
{
  if (is.null(fdir))
      stop("PLEASE SPECIFY THE LOCATION OF REQUIRED SOUNDING FILES!!!")
  files = system(paste("cd ", fdir, "; ls D*", sep="/"), TRUE)
  if (length(files) == 0)
    stop(paste("There is NO required sounding files under directory", fdir))
  return(files)
}
