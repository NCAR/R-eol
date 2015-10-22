#!/bin/sh

# revision will be the commit counts on the HEAD
revision=$(git rev-list HEAD | wc -l)

rargs="--vanilla"

export R_LIBS_USER=$(mktemp -d $WORKSPACE/Rtemp_XXXXXX)
trap "{ rm -rf $R_LIBS_USER; }" EXIT

cd packages

rm -f *.tar.gz

# build eolts first. isfs and eolsonde depend on it.
for pkg in eolts isfs eolsonde; do

    sed -ri "s/^Version: *([0-9]+)\.([0-9]+)-.*/Version: \1.\2-$revision/" $pkg/DESCRIPTION

    cd $pkg; autoreconf; cd -

    # build the source tar-ball
    R $rargs CMD build $pkg || exit 1
    
    # The check step leaves the package installed in R_LIBS_USER.
    # It can then be found in R_LIBS_USER by checks of other packages
    # which may depend on it.
    R $rargs CMD check -l $R_LIBS_USER ${pkg}_*.tar.gz || exit 1
    
done
