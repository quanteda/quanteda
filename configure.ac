
# require at least autoconf 2.69
AC_PREREQ([2.69])

# Process this file with autoconf to produce a configure script.
AC_INIT([quanteda],[4.0.0])

# Ensure C++ is set up as R expects
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
    AC_MSG_ERROR([Could not determine R_HOME.])
fi
CXX=`"${R_HOME}/bin/R" CMD config CXX`
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXXFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
LDFLAGS=`"${R_HOME}/bin/R" CMD config LDFLAGS`
AC_LANG(C++)
AC_REQUIRE_CPP
AC_PROG_CC
AC_PROG_CXX

# Default to no Intel TBB
TBBFLAGS=
TBBLIBS=

# If tbb.h and libtbb are found, define TBB and add -ltbb
AC_CHECK_HEADER([tbb/tbb.h],,
    [AC_MSG_WARN([parallel computing is disabled because tbb/tbb.h (Intel TBB) is not found])])
AC_MSG_CHECKING([libtbb available for linking])
AC_LANG(C++)
SAVED_LDFLAGS=$LDFLAGS
LDFLAGS="$LDFLAGS -ltbb"
HAVE_TBB=no
AC_LINK_IFELSE(
    [AC_LANG_PROGRAM([#include <tbb/tbb.h>],
      [major_version()])]
    [TBBFLAGS=-DTBB;TBBLIBS=-ltbb] [HAVE_TBB=yes],
    [AC_MSG_WARN([parallel computing is disabled because libtbb (Intel TBB devel package) is absent])])
if test ${HAVE_TBB} = no ; then
    AC_MSG_RESULT(no)
else
    AC_MSG_RESULT(yes)
fi
LDFLAGS=$SAVED_LDFLAGS
# Now substitute these variables in src/Makevars.in to create src/Makevars
AC_SUBST(TBB_CFLAGS, ${TBBFLAGS})
AC_SUBST(TBB_LIBS, ${TBBLIBS})
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
