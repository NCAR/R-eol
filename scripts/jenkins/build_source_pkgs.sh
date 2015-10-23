#!/bin/sh

# Build and check R source packages from contents of R-eol working tree

# Revision info from output of git describe based on a tag of the form vX.Y
if ! gitdesc=$(git describe --match "v[0-9]*"); then
    echo "git describe failed, looking for a tag vX.Y"
    exit 1
fi
#  v1.2-14-gabcdef123
gitdesc=${gitdesc/#v}   # remove v: 1.2-14-gabcdef123
version=${gitdesc%-*}   # remove trailing -*: 1.2-14

[ $gitdesc == "$version" ] && version=${gitdesc}-0  # if no commits since tag

rargs="--vanilla"

export R_LIBS_USER=$(mktemp -d $WORKSPACE/Rtemp_XXXXXX)
trap "{ rm -rf $R_LIBS_USER; }" EXIT

cd packages

rm -f *.tar.gz

# build eolts first. isfs and eolsonde depend on it.
for pkg in eolts isfs eolsonde; do

    sed -ri "s/^Version:.*/Version: $version/" $pkg/DESCRIPTION

    cd $pkg; autoreconf; cd -

    # build the source tar-ball
    R $rargs CMD build $pkg || exit 1
    
    # The check step leaves the package installed in R_LIBS_USER.
    # It can then be found in R_LIBS_USER by checks of other packages
    # which may depend on it.
    R $rargs CMD check -l $R_LIBS_USER ${pkg}_*.tar.gz || exit 1
    
done
