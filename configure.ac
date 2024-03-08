
# require at least autoconf 2.69
AC_PREREQ([2.69])

# Process this file with autoconf to produce a configure script.
AC_INIT([corels],[0.0.4])

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
TBBFLAG=
TBBLIBS=

# If tbb.h is found, define TBB
AC_CHECK_HEADER([tbb.h],
   [TBBFLAG=-DTBB;TBBLIBS=-ltbb],
   [AC_MSG_WARN([tbb.h not found, so skipping TBB resulting in slower code.])])

# Now substitute these variables in src/Makevars.in to create src/Makevars
AC_SUBST(TBB_CFLAGS, ${TBBFLAG})
AC_SUBST(TBB_LIBS, ${TBBLIBS})
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
