#
# Copyright (C) 2013 UCAR
#
# Much below was heavily borrowed from the Rcpp package for R.
# Copyright (C) 2010 - 2012 Dirk Eddelbuettel and Romain Francois
#

## make sure system.file returns an absolute path
eolts.system.file <- function(...)
{
    tools::file_path_as_absolute(base::system.file(...,package="eolts" ) )
}

## identifies if the default linking on the platform should be static
## or dynamic. We'll use dynamic linking on unix variants, which include
## mac osx.
staticLinking <- function() {
    ! grepl( "^unix", .Platform$OS.type )
}

## Use R's internal knowledge of path settings to find the lib/ directory
## plus optinally an arch-specific directory on system building multi-arch
eoltsLdPath <- function(static) {
    if (static || substring(.Platform$pkgType,1,10) == "mac.binary")
        dir = "lib"
    else
        dir = "libs"

    if (nzchar(.Platform$r_arch)) {	## eg amd64, ia64, mips
        path <- eolts.system.file(dir,.Platform$r_arch)
    } else {
        path <- eolts.system.file(dir)
    }
    path
}

## Provide linker flags -- i.e. -L/path/to/eolts.so -- as well as an
## optional rpath call needed to tell the Linux dynamic linker about the
## location.  This is not needed on OS X where we encode this as library
## built time (see src/Makevars) or Windows where we use a static library
## Updated Jan 2010:  We now default to static linking but allow the use
##                    of rpath on Linux if static==FALSE has been chosen
##                    Note that this is probably being called from LdFlags()
eoltsLdFlags <- function(static=staticLinking()) {
    lddir <- eoltsLdPath(static)
    if (static) {                               # static is default on Windows and OS X
        flags <- paste(lddir, "/libeolts.a", sep="")
        if (.Platform$OS.type=="windows") {
            flags <- asBuildPath(flags)
        }
    } else {					# else for dynamic linking
        # use  -l:filename option, so that the leading "lib" is not appended
        # when searching for the library. In this way we can use the
        # same library that is loaded by the eolts package.
        if (substring(.Platform$pkgType,1,10) == "mac.binary")
            flags <- paste("-L", lddir, " -leolts", sep="") # baseline setting
        else
            flags <- paste("-L", lddir, " -l:eolts.so", sep="") # baseline setting

        if ((.Platform$OS.type == "unix") &&    # on Linux, we can use rpath to encode path
            (length(grep("^linux",R.version$os)))) {
            flags <- paste(flags, " -Wl,-rpath,", lddir, sep="")
        }
    }
    invisible(flags)
}

## LdFlags defaults to static linking on the non-Linux platforms Windows and OS X
LdFlags <- function(static=staticLinking()) {
    cat(eoltsLdFlags(static=static))
}

# Transform a path for passing to the build system on the command line.
# Leave paths alone for posix. For Windows, mirror the behavior of the 
# R package build system by starting with the fully resolved absolute path,
# transforming it to a short path name if it contains spaces, and then 
# converting backslashes to forward slashes
asBuildPath <- function(path) {

    if (.Platform$OS.type == "windows") {
        path <- normalizePath(path)
        if (grepl(' ', path, fixed=TRUE))
            path <- utils::shortPathName(path)
        path <- gsub("\\\\", "/", path)
    }

    return(path)
}

