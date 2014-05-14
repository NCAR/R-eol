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

AS_IF([test -n "$netcdf_include_path"], 
   [CPPFLAGS+=" -I${netcdf_include_path}"]
)

AC_ARG_WITH([netcdf-lib],
    AC_HELP_STRING([--with-netcdf-lib=DIR],
                   [location of netcdf libraries]),
    [netcdf_lib_path=$withval])

AS_IF([test -n "$netcdf_lib_path"], 
    [LDFLAGS+=" -L${netcdf_lib_path}"]
)

#-- Check for NetCDF library ---------------------------------------#
AC_CHECK_LIB(netcdf, nc_open, [],
[
    AC_CHECK_LIB(netcdf, nc_create, [have_netcdf_hdf5=TRUE],
                 AC_MSG_ERROR(netcdf library not found),
                 [-lhdf5 -lhdf5_hl -lcurl -lz])
    if test "${have_netcdf_hdf5}" = TRUE; then
        LIBS="-lnetcdf -lhdf5 -lhdf5_hl -lcurl -lz ${LIBS}"
    fi
])

AC_CHECK_HEADER([netcdf.h], [], AC_MSG_ERROR(netcdf header netcdf.h not found))

#-- fftw3 include and library path ---------------------------------------------#
AC_ARG_WITH([fftw3-include],
    AC_HELP_STRING([--with-fftw3-include=DIR],
                   [location of fftw3 headers]),
    [fftw3_header_path=$withval]
)

AS_IF([test -n "$fftw3_header_path"], 
    [CPPFLAGS+=" -I${fftw3_header_path}"]
)

AC_CHECK_HEADER([fftw3.h],,
    [
        PKG_CHECK_MODULES([FFTW3],[fftw3],
            [
                CPPFLAGS+=" ${FFTW3_CFLAGS}"
            ]
        )
    ]
)

AC_ARG_WITH([fftw3-lib],
    AC_HELP_STRING([--with-fftw3-lib=DIR],
                   [location of fftw3 library]),
    [fftw3_lib_path=$withval]
)

AS_IF([test -n "$fftw3_lib_path"], 
    [LDFLAGS+=" -L${fftw3_lib_path}"]
)

AC_CHECK_LIB([fftw3],[fftw_execute],
    [
        HAVE_FFTW3=TRUE
        CPPFLAGS+=" -DHAVE_FFTW3"
        LIBS="-lfftw3 -lm ${LIBS}"
    ],
    [
        HAVE_FFTW3=FALSE
    ],
    [-lm]
)

#-- nc_server_rpc include and library path ---------------------------------#

AC_ARG_WITH([nc_server-include],
    AC_HELP_STRING([--with-nc_server-include=DIR],
                   [location of nc_server headers]),
    [nc_server_header_path=$withval]
)

AS_IF([test -n "$nc_server_header_path"],
    [
        CPPFLAGS+=" -I${nc_server_header_path}"
    ],
    [
        if test "$PKG_CONFIG" != "" && $PKG_CONFIG --exists nc_server 2>/dev/null; then
            PKG_CHECK_MODULES([NC_SERVER],[nc_server],
            [
                CPPFLAGS+=" ${NC_SERVER_CFLAGS}"
            ]
            )
        fi
    ]
)

AC_CHECK_HEADER([nc_server_rpc.h])

AC_ARG_WITH([nc_server-lib],
    AC_HELP_STRING([--with-nc_server-lib=DIR],
                   [location of nc_server library]),
    [nc_server_lib_path=$withval]
)

AS_IF([test -n "$nc_server_lib_path"],
    [
        LDFLAGS+=" -L${nc_server_lib_path}"
    ]
)

if test "$PKG_CONFIG" != "" && $PKG_CONFIG --exists nc_server 2>/dev/null; then
    PKG_CHECK_MODULES([NC_SERVER],[nc_server],
        [
            LIBS="${NC_SERVER_LIBS} ${LIBS}"
        ]
    )
fi

# As of R 3.0 one can add a "Biarch: yes" in DESCRIPTION to
# force a build of both architectures (i386 and x64) under windows.
# Without that, the existence of configure.win would cause
# builds for only i386, since configure.win is only run once.
# This made configure.win somewhat unusable, especially when
# wanting to build packages for both architectures on
# win-builder.r-project.org, where one cannot specify --force-biarch.
# As a result,  R/netdcdfWrite.R had to be correct for windows, without being
# edited in the configure step. This configure.ac then issues a sed command
# to change the value of HAVE_NC_SERVER in R/netcdfWrite.R, instead of
# using the AC_CONFIG_FILES macro to generate R/netcdfWrite.R from
# R/netcdfWrite.R.in, which wouldn't be compatible with windows without
# a configure.win.
#
# Now that configure.win works for both windows architectures,
# it could be changed to do for windows what AC_CONFIG_FILES does here.
# That has not been done, and we instead issue a sed command to
# set the correct value in R/netcdfWrite.R.
AC_CHECK_LIB([nc_server_rpc],[writedatarec_float_1],
    [
        HAVE_NC_SERVER=TRUE
        CPPFLAGS+=" -DHAVE_NC_SERVER"
        AC_OUTPUT_COMMANDS([${SED} -i -e 's/HAVE_NC_SERVER <- FALSE/HAVE_NC_SERVER <- TRUE/' R/netcdfWrite.R])
    ],
    [
        HAVE_NC_SERVER=FALSE
    ]
)

#-------------------------------------------------------------------------------#
#  Do substitution                               	                 	#
#-------------------------------------------------------------------------------#

# To substitute @HAVE_NC_SERVER@ when generating netcdfWrite.R from netcdfWrite.R.in
# AC_CONFIG_FILES([R/netcdfWrite.R],[chmod -w R/netcdfWrite.R])
# AC_SUBST([HAVE_NC_SERVER])

AC_SUBST([CPPFLAGS])
AC_SUBST([LDFLAGS])
AC_SUBST([LIBS])

AC_OUTPUT(src/Makevars)

#-------------------------------------------------------------------------------#

