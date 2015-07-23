#!/bin/sh

# Script to send a source package to win-builder.r-project.org to be built for windows.

ftphost=win-builder.r-project.org

revision=$(( $(git rev-list HEAD | wc -l) ))
[ $revision -eq 0 ] && revision=1

tmpdesc=$(mktemp /tmp/${0##*/}_XXXXXX)
trap "{ rm -f $tmpdesc; }" EXIT

if [ $# -gt 0 ]; then
    pkg=$1
else

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

rm -f ${pkg}_*.tar.gz

cp $pkg/DESCRIPTION $tmpdesc

if [ $(uname) == Darwin ]; then
    sed -i "" -E "s/^Version: *([0-9]+)\.([0-9]+)-.*/Version: \1.\2-$revision/" $pkg/DESCRIPTION
else
    sed -i -r "s/^Version: *([0-9]+)\.([0-9]+)-.*/Version: \1.\2-$revision/" $pkg/DESCRIPTION
fi

R --vanilla CMD build $pkg

bstatus=$?

cp $tmpdesc $pkg/DESCRIPTION
[ $bstatus -ne 0 ] && exit $bstatus

pkgz=$(echo ${pkg}_[0-9].[0-9]-*.tar.gz)

[ -f $pkgz ] || exit 1

trap "{ rm -f $tmpdesc $pkgz; }" EXIT

maintainer=$(grep "^Maintainer:" $pkg/DESCRIPTION | sed "s/^Maintainer://")

echo "ftp-ing $pkgz to R-release directory on $ftphost "

ftp -p -n $ftphost << EOD || exit 1
user anonymous $USER@ucar.edu
binary
cd R-release
put $pkgz
quit
EOD

echo "Done.

The maintainer of this package is listed as $maintainer in $pkg/DESCRIPTION.

Usually within 15 minutes the maintainer should receive an email from winbuilder,
with a link to a URL containing the build results.  If the build succeeds,
download the .zip file and copy it to
/net/www/docs/software/R/bin/windows/contrib/<Rversion>, where <Rversion>
is the version of R that was used to do the build on win-build,
which can be determined by looking at 00check.log on the results page.
Then, to update the PACKAGES file on the R repository: run:
R --vanilla -e 'tools::write_PACKAGES(dir=\"/net/www/docs/software/R/bin/windows/contrib/<Rversion>\",type=\"win.binary\")'
"

