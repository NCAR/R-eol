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
CPPFLAGS=`${R_HOME}/bin/R CMD config CPPFLAGS`
CFLAGS=`${R_HOME}/bin/R CMD config CFLAGS`
CXXFLAGS=`${R_HOME}/bin/R CMD config CXXFLAGS`
LDFLAGS=`${R_HOME}/bin/R CMD config LDFLAGS`
AC_LANG(C++)

# AC_EGREP_CPP(yes,[#ifdef __linux yes #endif],AC_DEFINE([HAS_PREP]))

AC_CHECK_PROG([HAVE_PREP],[prep],[TRUE],[FALSE],[$PATH:/opt/nidas/bin])

PKG_LIBS=:
OBJECTS="init.o"

# -I.../eolts/include is added to CPPFLAGS by R already, apparently because
# of "LinkingTo: eolts" in DESCRIPTION.

if test "$HAVE_PREP" == TRUE; then
    CPPFLAGS+=" -DHAVE_PREP"
    OBJECTS+=" R_prep.o"
    PKG_LIBS='$(R_HOME)/bin/Rscript -e "eolts:::LdFlags()"'
    # As of R 3.0 one can add a "Biarch: yes" in DESCRIPTION to
    # force a build of both architectures (i386 and x64) under windows.
    # Without that, the existence of configure.win would cause
    # builds for only i386, since configure.win is only run once.
    # This made configure.win somewhat unusable, especially when
    # wanting to build packages for both architectures on
    # win-builder.r-project.org, where one cannot specify --force-biarch.
    # As a result,  R/prep.R had to be correct for windows, without being
    # edited in the configure step. This configure.ac then issues a sed command
    # to change the value of HAVE_PREP in R/prep.R, instead of using the
    # AC_CONFIG_FILES macro to substitute a value for @HAVE_PREP@ when
    # generating R/prep.R from R/prep.R.in, which wouldn't be compatible
    # with windows without a configure.win.
    #
    # Now that configure.win works for both windows architectures,
    # it could be changed to do for windows what AC_CONFIG_FILES does here.
    # That has not been done, and we instead issue a sed command to
    # set the correct value in R/prep.R.
    #
    AC_OUTPUT_COMMANDS([${SED} -i -e 's/HAVE_PREP <- FALSE/HAVE_PREP <- TRUE/' R/prep.R])
fi
#-------------------------------------------------------------------------------#
#  Optional include paths
#-------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------#
#  Do substitution                               	                 	#
#-------------------------------------------------------------------------------#


AC_SUBST(CPPFLAGS)
AC_SUBST(OBJECTS)
AC_SUBST(PKG_LIBS)

AC_ARG_VAR(HAVE_PREP)
# AC_CONFIG_FILES([R/prep.R],[chmod -w R/prep.R])

AC_OUTPUT(src/Makevars)

#-------------------------------------------------------------------------------#

