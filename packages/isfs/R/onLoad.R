#
#               Copyright (C) by UCAR
# 

.onLoad = function(libname,pkgname)
{
    # cat(paste("hello from libname=",libname,", pkgname=",pkgname," .onLoad\n"))
    options(dcache=TRUE)
}
