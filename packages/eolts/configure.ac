#-------------------------------------------------------------------------------#
#  Initialize                                                    		#
#-------------------------------------------------------------------------------#

AC_INIT([eolts], 0.0.-1, [maclean@ucar.edu])

PKG_PROG_PKG_CONFIG([0])

#-------------------------------------------------------------------------------#
#  Find the compiler and compiler options to use   	                 	#
#-------------------------------------------------------------------------------#

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
    echo "Could not determine R_HOME"
    exit 1
fi
CC=`${R_HOME}/bin/R CMD config CC`
CFLAGS=`${R_HOME}/bin/R CMD config CFLAGS`
CPPFLAGS=`${R_HOME}/bin/R CMD config CPPFLAGS`
LDFLAGS=`${R_HOME}/bin/R CMD config LDFLAGS`
AC_LANG(C++)

#-------------------------------------------------------------------------------#
#  Optional include paths for NetCDF and/or UDUNITS libraries    		#
#-------------------------------------------------------------------------------#

#-- NetCDF include and library path --------------------------------------------#
AC_ARG_WITH([netcdf-include],
    AC_HELP_STRING([--with-netcdf-include=DIR],
                   [location of netcdf header files]),
    [netcdf_include_path=$withval])
if test [ -n "$netcdf_include_path" ] ; then
   AC_SUBST([CPPFLAGS],["-I${netcdf_include_path} ${CPPFLAGS}"])
fi

AC_ARG_WITH([netcdf-lib],
    AC_HELP_STRING([--with-netcdf-lib=DIR],
                   [location of netcdf libraries]),
    [netcdf_lib_path=$withval])
if test [ -n "$netcdf_lib_path" ] ; then
   AC_SUBST([LIBS],["-L${netcdf_lib_path} ${LIBS}"])
fi

#-- Check for NetCDF and UDUNITS library ---------------------------------------#
AC_CHECK_LIB(netcdf, nc_open, [],
[
    AC_CHECK_LIB(netcdf, nc_create, [have_netcdf_hdf5=TRUE],
                 AC_MSG_ERROR(netcdf library not found),
                 [-lhdf5 -lhdf5_hl -lcurl -lz])
    if test "${have_netcdf_hdf5}" = TRUE; then
        LIBS="${LIBS} -lnetcdf -lhdf5 -lhdf5_hl -lcurl -lz"
    fi
])

#-- Check for netcdf.h and udunits.h -------------------------------------------#
AC_CHECK_HEADERS(netcdf.h, [], AC_MSG_ERROR(netcdf header netcdf.h not found))

#-- fftw3 include and library path ---------------------------------------------#
AC_ARG_WITH([fftw3-include],
    AC_HELP_STRING([--with-fftw3-include=DIR],
                   [location of fftw3 headers]),
    [fftw3_header_path=$withval]
)

AC_ARG_WITH([fftw3-lib],
    AC_HELP_STRING([--with-fftw3-lib=DIR],
                   [location of fftw3 library]),
    [fftw3_lib_path=$withval]
)

AS_IF([test -n "$fftw3_header_path"], 
    AC_SUBST([FFTW3_CFLAGS],["-I${fftw3_header_path}"])
)

AS_IF([test -n "$fftw3_lib_path"], 
    AC_SUBST([FFTW3_LIBS],["-L${fftw3_lib_path} -lfftw3 -lm"]),
    [PKG_CHECK_MODULES([FFTW3],[fftw3],,FFTW3_LIBS="-lfftw3 -lm")]
)


#-------------------------------------------------------------------------------#
#  Do substitution                               	                 	#
#-------------------------------------------------------------------------------#

AC_SUBST(CPPFLAGS)
AC_SUBST(LIBS)
AC_OUTPUT(src/Makevars)

AC_SUBST([FFTW3_CFLAGS])
AC_SUBST([FFTW3_LIBS])

#-------------------------------------------------------------------------------#

