#!/bin/sh
# From working directory, copy .tgz mac.binary R packages to a
# local R package repository

umask 0002

# contrib_path.txt was written by mac_build.sh, 
repo=${R_REPO:?}/$(<contrib_path.txt)

if ! [ -d $repo ]; then
    mkdir -p $repo || exit 1
    chmod g+ws $repo
fi

rsync -a *.tgz $repo
rm -f *.tgz

R --vanilla -e "tryCatch(tools::write_PACKAGES(dir='$repo',type='mac.binary'),error=function(e)q(status=1))"

for pkg in eolts isfs eolsonde; do
    rm -f $(ls -rt $repo/${pkg}_*.tgz | sort | head -n -2)
    chmod g+w $repo/${pkg}_*.tgz
done
