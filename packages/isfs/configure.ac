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

PKG_LIBS=:
OBJECTS="init.o"

# -I.../eolts/include is added to CPPFLAGS by R already, apparently because
# of "LinkingTo: eolts" in DESCRIPTION.

if test "$HAVE_PREP" == TRUE; then
    CPPFLAGS+=" -DHAVE_PREP"
    OBJECTS+=" R_prep.o"
    PKG_LIBS='$(R_HOME)/bin/Rscript -e "eolts:::LdFlags()"'
    # Change HAVE_PREP variable in R/prep.R from FALSE to TRUE rather
    # than doing the usual generation of R/prep.R from R/prep.R.in
    # with a substitution of @HAVE_PREP@ by the AC_CONFIG_FILES macro.
    # Windows systems run configure.win and don't have autoconf and configure.ac.
    # configure.win is run only once on a build. Since it then generally
    # cannot configure things for both windows architectures, i386 and x64,
    # it just generates a i386 package. So it isn't really a workable
    # configuration script under windows.
    # Therefore the unedited prep.R must be correct R code for a windows package,
    # with "HAVE_PREP <- FALSE".  For unix-likes, issue commands here
    # to set it to TRUE in R/prep.R if needed.
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
# AC_SUBST(LIBS)

AC_ARG_VAR(HAVE_PREP)
# AC_CONFIG_FILES([R/prep.R],[chmod -w R/prep.R])

AC_OUTPUT(src/Makevars)

#-------------------------------------------------------------------------------#

