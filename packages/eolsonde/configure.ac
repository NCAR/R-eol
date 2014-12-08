#-------------------------------------------------------------------------------#
#  Initialize                                                    		#
#-------------------------------------------------------------------------------#

AC_INIT([eolsonde], 0.0.-1, [maclean@ucar.edu])

AC_LANG(C++)

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

#-- GeographicLib include and library path ---------------------------------------------#
AC_ARG_WITH([geographiclib-include],
    AC_HELP_STRING([--with-geographiclib-include=DIR],
                   [location of GeographicLib headers]),
    [geographiclib_header_path=$withval]
)

AS_IF([test -n "$geographic_header_path"], 
    [CPPFLAGS+=" -I${geographic_header_path}"]
)

AC_CHECK_HEADER([GeographicLib/Geoid.hpp],
    [ LIBS=-lGeographic ${LIBS} ],
    [
        PKG_CHECK_MODULES([GEOGRAPHICLIB],[Geographic],
            [
                CPPFLAGS+=" ${GEOGRAPHIC_CFLAGS}"
                LIBS+=" ${GEOGRAPHIC_LIBS}"
            ]
        )
    ]
)

AC_ARG_WITH([geographiclib],
    AC_HELP_STRING([--with-geographiclib=DIR],
                   [location of GeographicLib library]),
    [geographiclib_path=$withval]
)

AS_IF([test -n "$geographiclib_path"], 
    [LDFLAGS+=" -L${geographiclib_path}"]
)

AC_TRY_LINK([#include <GeographicLib/Geoid.hpp>],
            [GeographicLib::Geoid g("");],
    [
        HAVE_GEOGRAPHICLIB=TRUE
        CPPFLAGS+=" -DHAVE_GEOGRAPHICLIB"
        AC_OUTPUT_COMMANDS([${SED} -i -e 's/HAVE_GEOGRAPHICLIB <- FALSE/HAVE_GEOGRAPHICLIB <- TRUE/' R/geoid.R])
    ],
    [
        HAVE_GEOGRAPHICLIB=FALSE
    ]
)


#-------------------------------------------------------------------------------#
#  Do substitution                               	                 	#
#-------------------------------------------------------------------------------#

AC_SUBST([CPPFLAGS])
AC_SUBST([LDFLAGS])
AC_SUBST([LIBS])

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

#-------------------------------------------------------------------------------#

