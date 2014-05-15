# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsdng" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

readin.file <-
function(ftype="qc", file=NULL)
{
  if (is.null(file))
    stop("PLEASE SPECIFY file NAME AND LOCATION !!!")
  ftype = tolower(ftype)

  if (ftype=="qc")
    data = readQCFile(file)

  if (ftype=="raw")
    data = readDFile(file)

  if (nrow(data) <= 1)
    print(paste(file, "has NO data !!!"))

  return(data)
}
