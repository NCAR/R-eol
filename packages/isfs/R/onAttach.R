#
#               Copyright (C) by UCAR
# 

.onAttach = function(libname,pkgname)
{
    # cat(paste("hello from libname=",libname,", pkgname=",pkgname," .onAttach\n"))
    options(dcache=TRUE)
}
