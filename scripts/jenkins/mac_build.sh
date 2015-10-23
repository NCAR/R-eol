#!/bin/sh

# Download R source packages from $R_REPO, then build binary packages
# Meant to be used on a mac

repo=${R_REPO:?}

# path to pdflatex, used by check step
PATH=$PATH:/usr/texbin

# cleanup old stuff
rm -rf *.tar.gz *.tgz

rargs="--vanilla"

# 
export R_LIBS_USER=$(mktemp -d $WORKSPACE/Rtemp_XXXXXX)
trap "{ rm -rf $R_LIBS_USER; }" EXIT

# download the source package tar-balls
R $rargs -e "download.packages(c('eolts','isfs','eolsonde'),repos='$repo',type='source',destdir='.')"

for pkg in eolts isfs eolsonde; do

    # check, then build binary tar-ball
    R $rargs CMD check -l $R_LIBS_USER ${pkg}_*.tar.gz || exit 1
    R $rargs CMD INSTALL --build -l $R_LIBS_USER ${pkg}_*.tar.gz || exit 1

    # Check that the package does not have dependencies on /usr/local/lib
    if R $rargs CMD otool -L $R_LIBS_USER/$pkg/libs/$pkg.so | fgrep /usr/local/lib; then
        echo "otool -L $R_LIBS_USER/$pkg/libs/$pkg.so indicates it is using a shareable library on /usr/local/lib"
        exit 1
    fi
    
done

# save the value of contrib.url for the .Platform$pkgType to a file
R $rargs -e 'cat(contrib.url(repos=".",type=.Platform$pkgType),file="contrib_path.txt")'


