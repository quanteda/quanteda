# for rJava and the Arabic stemmer
#
.onLoad <- function(libname, pkgname) {
    .jpackage(pkgname, lib.loc = libname)
}