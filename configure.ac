# dnl Process this file with autoconf to produce a configure script.
AC_INIT(rgdal, 0.7-10, Roger.Bivand@nhh.no)
AC_CONFIG_SRCDIR(src/gdal-bindings.cpp)

# find R home and set correct compiler + flags
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  AC_MSG_ERROR([cannot determine R_HOME. Make sure you use R CMD INSTALL!])
fi
RBIN="${R_HOME}/bin/R"

# pick all flags for testing from R
: ${CC=`"${RBIN}" CMD config CC`}
: ${CXX=`"${RBIN}" CMD config CXX`}
: ${CPP=`"${RBIN}" CMD config CPP`}
: ${CFLAGS=`"${RBIN}" CMD config CFLAGS`}
: ${CPPFLAGS=`"${RBIN}" CMD config CPPFLAGS`}
: ${LDFLAGS=`"${RBIN}" CMD config LDFLAGS`}

#GDAL

GDAL_CONFIG="gdal-config"

AC_ARG_WITH([gdal-config],
    AC_HELP_STRING([--with-gdal-config=GDAL_CONFIG],
           [the location of gdal-config]),
           [gdal_config=$withval])
if test [ -n "$gdal_config" ] ; then
    AC_SUBST([GDAL_CONFIG],["${gdal_config}"])
fi

AC_MSG_NOTICE([gdal-config: ${GDAL_CONFIG}])

AC_MSG_CHECKING(gdal-config usability)
if test `${GDAL_CONFIG} --version`;
then

	GDAL_CPPFLAGS=`${GDAL_CONFIG} --cflags`
	GDAL_LIBS=`${GDAL_CONFIG} --libs`
        GDAL_VERSION=`${GDAL_CONFIG} --version`
        GDAL_DEP_LIBS=`${GDAL_CONFIG} --dep-libs`
        GDAL_DATADIR=`${GDAL_CONFIG} --datadir`
        AC_MSG_RESULT(yes)
else
        AC_MSG_RESULT(no)
	echo "Error: gdal-config not found"
	echo "The gdal-config script distributed with GDAL could not be found."
	echo "If you have not installed the GDAL libraries, you can"
	echo "download the source from  http://www.gdal.org/"
	echo "If you have installed the GDAL libraries, then make sure that"
	echo "gdal-config is in your path. Try typing gdal-config at a"
	echo "shell prompt and see if it runs. If not, use:"
	echo " --configure-args='--with-gdal-config=/usr/local/bin/gdal-config'"
	echo "with appropriate values for your installation."
	echo ""

	exit 1

fi

AC_MSG_NOTICE([GDAL: ${GDAL_VERSION}])
AC_MSG_CHECKING([GDAL version > 1.5.4])
GDAL_VER_DOT=`echo $GDAL_VERSION | tr -d "."`
if test ${GDAL_VER_DOT} -lt 160 ; then
  AC_MSG_RESULT(no)
  AC_MSG_ERROR([upgrade GDAL to 1.6.0 or later])
else
  AC_MSG_RESULT(yes)
fi

INLIBS="${LIBS}"
INCPPFLAGS="${CPPFLAGS}"
INPKG_CPPFLAGS="${PKG_CPPFLAGS}"
INPKG_LIBS="${PKG_LIBS}"

AC_SUBST([PKG_CPPFLAGS], ["${INPKG_CPPFLAGS} ${GDAL_CPPFLAGS}"])
AC_SUBST([PKG_LIBS], ["${INPKG_LIBS} ${GDAL_LIBS}"])

# honor PKG_xx overrides
# for CPPFLAGS we will superfluously double R's flags
# since we'll set PKG_CPPFLAGS with this, but that shouldn't hurt
CPPFLAGS="${INCPPFLAGS} ${PKG_CPPFLAGS}"

gdalok=yes
AC_CHECK_HEADERS(gdal.h,,gdalok=no)
if test "${gdalok}" = no; then
   AC_MSG_ERROR([gdal.h not found in given locations.])
fi

NEED_DEPS=no
LIBS="${INLIBS} ${PKG_LIBS}"
[cat > gdal_test.c <<_EOCONF
#include <gdal.h>
#ifdef __cplusplus
extern "C" {
#endif
main() {
GDALAllRegister();
}
#ifdef __cplusplus
}
#endif
_EOCONF]

AC_MSG_CHECKING(gdal: linking with --libs only)
${CXX} ${CPPFLAGS} -o gdal_test gdal_test.c ${LIBS} 2> errors.txt
if test `echo $?` -ne 0 ; then
gdalok=no
AC_MSG_RESULT(no)
else
AC_MSG_RESULT(yes)
fi

if test "${gdalok}" = no; then
AC_MSG_CHECKING(gdal: linking with --libs and --dep-libs)
LIBS="${LIBS} ${GDAL_DEP_LIBS}"
gdalok=yes
${CXX} ${CPPFLAGS} -o gdal_test gdal_test.c ${LIBS} 2>> errors.txt
if test `echo $?` -ne 0 ; then
gdalok=no
fi
if test "${gdalok}" = yes; then
    NEED_DEPS=yes
    AC_MSG_RESULT(yes)
else
    AC_MSG_RESULT(no)
fi
fi

if test "${gdalok}" = no; then
   AC_MSG_ERROR([GDALAllRegister not found in libgdal.])
fi

rm -f gdal_test errors.txt gdal_test.c


GDAL_DATA_TEST_FILE="${GDAL_DATADIR}/pcs.csv"
AC_MSG_CHECKING(GDAL: ${GDAL_DATADIR}/pcs.csv readable)
if test -r "${GDAL_DATA_TEST_FILE}" ; then
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
  AC_MSG_ERROR([pcs.csv not found in GDAL data directory.])
fi

# PROJ.4

AC_ARG_WITH([proj-include],
    AC_HELP_STRING([--with-proj-include=DIR],
                   [location of proj header files]),
    [proj_include_path=$withval])
if test [ -n "$proj_include_path" ] ; then
   AC_SUBST([PKG_CPPFLAGS],["-I${proj_include_path} ${INPKG_CPPFLAGS}"])
fi

# honor PKG_xx overrides
# for CPPFLAGS we will superfluously double R's flags
# since we'll set PKG_CPPFLAGS with this, but that shouldn't hurt
CPPFLAGS="${INCPPFLAGS} ${PKG_CPPFLAGS}"


proj4ok=yes
AC_CHECK_HEADERS(proj_api.h,,proj4ok=no)
if test "${proj4ok}" = no; then
   AC_MSG_ERROR([proj_api.h not found in standard or given locations.])
fi

          
# dnl ditto for a library path
AC_ARG_WITH([proj-lib],
    AC_HELP_STRING([--with-proj-lib=LIB_PATH],
               [the location of proj libraries]),
               [proj_lib_path=$withval])
if test [ -n "$proj_lib_path" ] ; then
    AC_SUBST([PKG_LIBS], ["-L${proj_lib_path} ${INPKG_LIBS}"])
fi

LIBS="${INLIBS} ${PKG_LIBS}"

proj4ok=yes
AC_CHECK_LIB(proj,pj_init_plus,,proj4ok=no)
if test "${proj4ok}" = no; then
   AC_MSG_ERROR([libproj not found in standard or given locations.])
fi


PKG_LIBS="${PKG_LIBS} -lproj"

[cat > proj_conf_test.c <<_EOCONF
#include <stdio.h>
#include <proj_api.h>

int main() {
    if (PJ_VERSION >= 480) exit(1);
    exit(0);
}
_EOCONF]

${CC} ${CFLAGS} ${PKG_CPPFLAGS} -o proj_conf_test proj_conf_test.c ${PKG_LIBS}
./proj_conf_test

proj_version=`echo $?`

AC_MSG_CHECKING(PROJ.4 Version 4.7 or earlier)
if test ${proj_version} -eq 1; then
  AC_MSG_RESULT(no)
  AC_SUBST([PKG_CPPFLAGS],["-DP4CTX=${proj_version} ${PKG_CPPFLAGS}"])
else
  AC_MSG_RESULT(yes)
fi

if test ${proj_version} -eq 1; then
[cat > proj_conf_test.c <<_EOCONF
#include <stdio.h>
#include <proj_api.h>
FILE *pj_open_lib(projCtx, const char *, const char *);

int main() {
    FILE *fp;
    fp = pj_open_lib(pj_get_default_ctx(), "epsg", "rb");
    if (fp == NULL) exit(1);
    fclose(fp);
    exit(0);
}
_EOCONF]
else
[cat > proj_conf_test.c <<_EOCONF
#include <stdio.h>
#include <proj_api.h>
FILE *pj_open_lib(const char *, const char *);

int main() {
    FILE *fp;
    fp = pj_open_lib("epsg", "rb");
    if (fp == NULL) exit(1);
    fclose(fp);
    exit(0);
}
_EOCONF]
fi

${CC} ${CFLAGS} ${PKG_CPPFLAGS} -o proj_conf_test proj_conf_test.c ${PKG_LIBS}
./proj_conf_test

proj_share=`echo $?`
AC_MSG_CHECKING(PROJ.4: epsg found and readable)
if test ${proj_share} -eq 1 ; then
    AC_MSG_RESULT(no)
    STOP="stop"
else
    AC_MSG_RESULT(yes)
fi

rm -f proj_conf_test.c proj_conf_test

if test "$STOP" = "stop" ; then
    echo "Error: proj/epsg not found"
    echo "Either install missing proj support files, for example"
    echo "the proj-nad and proj-epsg RPMs on systems using RPMs,"
    echo "or if installed but not autodetected, set PROJ_LIB to the"
    echo "correct path."
    exit 1
fi


if test ${proj_version} -eq 1; then
[cat > proj_conf_test.c <<_EOCONF
#include <stdio.h>
#include <proj_api.h>
FILE *pj_open_lib(projCtx, const char *, const char *);

int main() {
    FILE *fp;
    fp = pj_open_lib(pj_get_default_ctx(), "conus", "rb");
    if (fp == NULL) exit(1);
    fclose(fp);
    exit(0);
}
_EOCONF]
else
[cat > proj_conf_test.c <<_EOCONF
#include <stdio.h>
#include <proj_api.h>
FILE *pj_open_lib(const char *, const char *);

int main() {
    FILE *fp;
    fp = pj_open_lib("conus", "rb");
    if (fp == NULL) exit(1);
    fclose(fp);
    exit(0);
}
_EOCONF]
fi

${CC} ${CFLAGS} ${PKG_CPPFLAGS} -o proj_conf_test proj_conf_test.c ${PKG_LIBS}
./proj_conf_test

proj_share=`echo $?`
AC_MSG_CHECKING(PROJ.4: conus found and readable)
if test ${proj_share} -eq 1 ; then
    WARN="warn"
    AC_MSG_RESULT(no)
else
    AC_MSG_RESULT(yes)
fi

rm -f proj_conf_test.c proj_conf_test

if test "$WARN" = "warn" ; then
    echo "Note: proj/conus not found"
    echo "No support available in PROJ4 for NAD grid datum transformations"
    echo "If required, consider re-installing from source with the contents"
    echo "of proj-datumgrid-1.<latest>.zip from http://download.osgeo.org/proj/ in nad/."
fi

# Optional local copy of GDAL datadir and PROJ_LIB

data_copy=no
AC_ARG_WITH([data-copy],
    AC_HELP_STRING([--with-data-copy=yes/no],
               [local copy of data directories in package, default no]),
               [data_copy=$withval])
if test "${data_copy}" = "yes" ; then
AC_MSG_NOTICE([Copy data for:])
  proj_lib0="${PROJ_LIB}"
  AC_ARG_WITH([proj-data],
    AC_HELP_STRING([--with-proj-data=DIR],
                   [location of PROJ.4 data directory]),
    [proj_lib1=$withval])
  if test -n "${proj_lib0}" ; then
    proj_lib="${proj_lib0}"
  else
    proj_lib="${proj_lib1}"
  fi
  if test -n "${proj_lib}" ; then
    if test -d "${proj_lib}" ; then
      cp -r "${proj_lib}" "${R_PACKAGE_DIR}"
      AC_MSG_NOTICE([  PROJ.4: ${proj_lib}])
    else
      AC_MSG_ERROR([PROJ.4 data files not found; set environment variable PROJ_LIB=DIR or --with-proj-data=DIR.])
    fi
  else
      AC_MSG_ERROR([PROJ.4 data files not found; set environment variable PROJ_LIB=DIR or --with-proj-data=DIR.])
  fi

  if test -d "${GDAL_DATADIR}" ; then
    cp -r "${GDAL_DATADIR}" "${R_PACKAGE_DIR}"
    AC_MSG_NOTICE([  GDAL: ${GDAL_DATADIR}])
  else
    AC_MSG_ERROR([GDAL data files not found.])
  fi
fi


# concluding substitution

AC_SUBST([PKG_CPPFLAGS], ["${INPKG_CPPFLAGS} ${GDAL_CPPFLAGS}"])
if test [ -n "$proj_include_path" ] ; then
AC_SUBST([PKG_CPPFLAGS], ["${PKG_CPPFLAGS} -I${proj_include_path}"])
fi
if test "${proj_version}" -eq 1; then
AC_SUBST([PKG_CPPFLAGS], ["${PKG_CPPFLAGS} -DP4CTX=${proj_version}"]) 
fi

AC_SUBST([PKG_LIBS], ["${INPKG_LIBS} ${GDAL_LIBS}"])
if test "${NEED_DEPS}" = yes; then
   AC_SUBST([PKG_LIBS], ["${PKG_LIBS} ${GDAL_DEP_LIBS}"])
fi
if test [ -n "$proj_lib_path" ] ; then
AC_SUBST([PKG_LIBS], ["${PKG_LIBS} -L${proj_lib_path}"])
fi
AC_SUBST([PKG_LIBS], ["${PKG_LIBS} -lproj"])

AC_MSG_NOTICE([Package CPP flags: ${PKG_CPPFLAGS}])
AC_MSG_NOTICE([Package LIBS: ${PKG_LIBS}])

AC_CONFIG_FILES(src/Makevars)
AC_OUTPUT

