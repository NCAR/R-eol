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

AC_CHECK_LIB([nc_server_rpc],[writedatarec_float_1],
    [
        HAVE_NC_SERVER=TRUE
        CPPFLAGS+=" -DHAVE_NC_SERVER"
    ],
    [
        HAVE_NC_SERVER=FALSE
    ]
)

#-------------------------------------------------------------------------------#
#  Do substitution                               	                 	#
#-------------------------------------------------------------------------------#

AC_CONFIG_FILES([R/netcdfWrite.R],[chmod -w R/netcdfWrite.R])

AC_SUBST([HAVE_FFTW3])
AC_SUBST([HAVE_NC_SERVER])

AC_SUBST([CPPFLAGS])
AC_SUBST([LDFLAGS])
AC_SUBST([LIBS])

AC_OUTPUT(src/Makevars)

#-------------------------------------------------------------------------------#

