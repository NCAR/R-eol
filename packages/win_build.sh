#!/bin/sh

# Script to send a source package to win-builder.r-project.org to be built for windows.

if [ $# -gt 0 ]; then
    pkg=$1
    shift
fi

if [ $# -gt 0 ]; then
    buildver=$1
    shift
fi

rver=4.1

ftphost=win-builder.r-project.org

# Revision info from output of git describe based on a tag of the form vX.Y
if ! gitdesc=$(git describe --match "v[0-9]*"); then
    echo "git describe failed, looking for a tag vX.Y"
    exit 1
fi
#  v1.2-14-gabcdef123
gitdesc=${gitdesc/#v}   # remove v: 1.2-14-gabcdef123
version=${gitdesc%-*}   # remove trailing -*: 1.2-14

[ $gitdesc == "$version" ] && version=${gitdesc}-0  # if no commits since tag

tmpdesc=$(mktemp /tmp/${0##*/}_XXXXXX)
trap "{ rm -f $tmpdesc; }" EXIT

if [ -z "$pkg" ]; then

    echo "You usually must send the eolts package first to win-builder.r-project.org.
Then if that build is successful, send isfs, and when that is successful, send eolsonde.

isfs depends on eolts.  eolsonde depends on isfs and eolts.

Any package dependencies must be present on win-builder for a
build to succeed.  win-builder keeps packages around for a few days,
so after eolts is successfully built there, then builds of isfs should work.
"

    packages=(eolts isfs eolsonde)

    oldps3=$PS3
    PS3="Choose a package to build, by number: "
    select pkg in ${packages[*]}; do
        break
    done
    PS3=$oldps3
fi

if [ -z "$buildver" ]; then
    echo "Browse to http://$ftphost to see what versions of R are available for builds"
    bvers=(R-release R-oldrelease R-devel)
    oldps3=$PS3
    PS3="Choose an R version for building on $ftphost: "
    select buildver in ${bvers[*]}; do
        break
    done
    PS3=$oldps3
fi

rm -f ${pkg}_*.tar.gz

cp $pkg/DESCRIPTION $tmpdesc

if [ $(uname) == Darwin ]; then
    sed -i "" -E "s/^Version:.*/Version: $version/" $pkg/DESCRIPTION
else
    sed -ri "s/^Version:.*/Version: $version/" $pkg/DESCRIPTION
fi

R --vanilla CMD build $pkg

bstatus=$?

cp $tmpdesc $pkg/DESCRIPTION
[ $bstatus -ne 0 ] && exit $bstatus

pkgz=$(echo ${pkg}_[0-9].[0-9]-*.tar.gz)

[ -f $pkgz ] || exit 1

trap "{ rm -f $tmpdesc $pkgz; }" EXIT

maintainer=$(grep "^Maintainer:" $pkg/DESCRIPTION | sed "s/^Maintainer://")

echo "ftp-ing $pkgz to $buildver directory on $ftphost "

ftp -p -n $ftphost << EOD || exit 1
user anonymous $USER@ucar.edu
binary
cd $buildver
put $pkgz
quit
EOD

echo "Done.

The maintainer of this package is listed as $maintainer in $pkg/DESCRIPTION.

Usually within 15 minutes the maintainer should receive an email from winbuilder,
with a link to a URL containing the build results.  If the build succeeds,
download the .zip file and copy it to
/net/www/docs/software/R/bin/windows/contrib/\$rver, where \$rver
is the version of R that was used to do the build on win-build,
which can be determined by looking at 00check.log on the results page.
Then, to update the PACKAGES file on the R repository: run:
R --vanilla -e 'tools::write_PACKAGES(dir=\"/net/www/docs/software/R/bin/windows/contrib/\$rver\",type=\"win.binary\")'
"

