
# require at least autoconf 2.69
AC_PREREQ([2.69])

# Process this file with autoconf to produce a configure script.
AC_INIT([quanteda],[4.0.2])

# Ensure C++ is set up as R expects
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
    AC_MSG_ERROR([Could not determine R_HOME.])
fi
CXX=`"${R_HOME}/bin/R" CMD config CXX`
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXXFLAGS`
CXXFLAGS="${CXXFLAGS} -g0"  # Disable debug information
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
LDFLAGS=`"${R_HOME}/bin/R" CMD config LDFLAGS`
AC_LANG(C++)
AC_REQUIRE_CPP
AC_PROG_CC
AC_PROG_CXX

# Default to no Intel TBB
TBBFLAGS=
TBBLIBS=

# Configure argument to avoid TBB
WITH_TBB="yes"
AC_ARG_ENABLE([tbb], [AS_HELP_STRING([--enable-tbb],["to choose to disable Intel TBB even if installed, use --disable-tbb"])], 
[if test "${enableval}" = no; then
  want_tbb=no
elif test "${enableval}" = yes; then
  want_tbb=yes
else
  want_tbb=yes
fi],
[want_tbb=yes])

if test "$want_tbb" = no ; then
  AC_MSG_NOTICE([Intel TBB detection disabled])
else
# If tbb/tbb.h and libtbb are found, define TBB and add -ltbb
  AC_MSG_CHECKING([Intel TBB existence:])
  TBB_EXISTS=no
  pkg-config --exists tbb >/dev/null 2>&1
  SH_TBB_EXISTS=`echo $?`
  if test ${SH_TBB_EXISTS} = 0 ; then
    TBB_EXISTS=yes
    AC_MSG_RESULT(yes)
  else 
    if test ${SH_TBB_EXISTS} = 1 ; then
      TBB_EXISTS=no
      AC_MSG_RESULT(no)
      AC_MSG_WARN([Intel TBB not installed; install TBB devel package for parallel processing or update PKG_CONFIG_PATH environment variable])
    else
      TBB_EXISTS=no
      AC_MSG_RESULT(no)
      AC_MSG_WARN([pkg-config not found; install pkg-config to auto-detect Intel TBB; assumed absent])
    fi
  fi

  if test ${TBB_EXISTS} = yes ; then
    AC_SUBST(TBBFLAGS, ["-DTBB $(pkg-config --cflags tbb)"])
    AC_SUBST(TBBLIBS, ["$(pkg-config --libs tbb)"])

    AC_MSG_CHECKING([tbb available for compiling and linking:])
[cat > libtbb_test.cpp <<_EOCONF
#include <tbb/tbb.h>

int main() { 
  int H = 1000;
  tbb::concurrent_vector<bool> v(H, false);
  tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
    for (int h = r.begin(); h < r.end(); ++h) {
        v[h] = true;
    }
  });
  if (std::find(v.begin(), v.end(), false) == v.end()) { // all true
    return 0;
  } else {
    return 1;
  }
}
_EOCONF]
    ${CXX} ${CXXFLAGS} ${TBBFLAGS} -o libtbb_test libtbb_test.cpp ${TBBLIBS} 2> errors.txt

    if test `echo $?` -ne 0 ; then
      AC_MSG_RESULT(no)
      AC_MSG_WARN([parallel computing is disabled because the Intel TBB devel package is absent])
      AC_SUBST(TBBFLAGS, [""])
      AC_SUBST(TBBLIBS, [""])
    else
      ./libtbb_test >/dev/null 2>&1
      if test `echo $?` -ne 0 ; then
        AC_MSG_RESULT(no)
        AC_MSG_WARN([parallel computing is disabled because Intel TBB did not load])
        AC_SUBST(TBBFLAGS, [""])
        AC_SUBST(TBBLIBS, [""])
      else
        AC_MSG_RESULT(yes)
      fi
    fi
    rm -f libtbb_test.cpp libtbb_test errors.txt
  fi
fi
# Now substitute these variables in src/Makevars.in to create src/Makevars
AC_SUBST(TBB_CFLAGS, ${TBBFLAGS})
AC_SUBST(TBB_LIBS, ${TBBLIBS})

AC_MSG_NOTICE([Package CPP flags: ${TBB_CFLAGS}])
AC_MSG_NOTICE([Package LIBS: ${TBB_LIBS}])

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
