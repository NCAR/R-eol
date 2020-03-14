#!/bin/sh

sdir=$(dirname $0)
# echo $sdir

# Download R source packages from $R_REPO, then build binary packages
# Meant to be used on a mac

repo=${R_REPO:?}

# Possible paths to pdflatex, used by check step
PATH=$PATH:/Library/TeX/texbin:/usr/texbin

# cleanup old stuff
rm -rf *.tar.gz *.tgz

rargs="--vanilla"

hide_shlibs() {
    for libdir in ${fix_lib_dirs[*]}; do
	$sdir/hide-shared-libs.py --hide --dir /usr/local/opt/$libdir/lib
    done
}

restore_shlibs() {
    for libdir in ${fix_lib_dirs[*]}; do
	$sdir/hide-shared-libs.py --restore --dir /usr/local/opt/$libdir/lib
    done
}

# 
export R_LIBS_USER=$(mktemp -d $WORKSPACE/Rtemp_XXXXXX)
trap "{ rm -rf $R_LIBS_USER; }" EXIT

# download the source package tar-balls
R $rargs -e "download.packages(c('eolts','isfs','eolsonde'),repos='$repo',type='source',destdir='.')"

fix_lib_dirs=(fftw netcdf hdf5 szip)
hide_shlibs
trap "restore_shlibs > /dev/null;" EXIT

for pkg in eolts isfs eolsonde; do

    # check, then build binary tar-ball
    # R $rargs CMD check -l $R_LIBS_USER ${pkg}_*.tar.gz || exit 1
    R $rargs CMD INSTALL --build -l $R_LIBS_USER ${pkg}_*.tar.gz || exit 1

    # Warn if the package has dependencies on /usr/local
    if R $rargs CMD otool -L $R_LIBS_USER/$pkg/libs/$pkg.so | fgrep /usr/local; then
        echo "Warning: otool -L $R_LIBS_USER/$pkg/libs/$pkg.so indicates it is using a shareable library on /usr/local"
    fi
    
done

# save the value of contrib.url for the .Platform$pkgType to a file
R $rargs -e 'cat(contrib.url(repos=".",type=.Platform$pkgType),file="contrib_path.txt")'


