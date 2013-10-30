#-------------------------------------------------------------------------------#
#  Initialize                                                    		#
#-------------------------------------------------------------------------------#

AC_INIT([isfs], 0.0.-1, [maclean@ucar.edu])

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

# AC_EGREP_CPP(yes,[#ifdef __linux yes #endif],AC_DEFINE([HAS_PREP]))

AC_CHECK_PROG([HAVE_PREP],[prep],[TRUE],[FALSE],[$PATH:/opt/nidas/bin])
#-------------------------------------------------------------------------------#
#  Optional include paths
#-------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------#
#  Do substitution                               	                 	#
#-------------------------------------------------------------------------------#

AC_SUBST(CPPFLAGS)
AC_ARG_VAR(HAVE_PREP)
AC_CONFIG_FILES([R/prep.R],[chmod -w R/prep.R])
AC_SUBST(LIBS)
AC_OUTPUT(src/Makevars)

#-------------------------------------------------------------------------------#

