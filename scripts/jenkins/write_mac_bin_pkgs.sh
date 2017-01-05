#!/bin/sh

umask 002

# From working directory, copy .tgz mac.binary R packages to a
# local R package repository

# contrib_path.txt was written by mac_build.sh, 
repo=${R_REPO:?}/$(<contrib_path.txt)

[ -d $repo ] || mkdir -p $repo

rsync -a *.tgz $repo

rm -f *.tgz

for pkg in eolts isfs eolsonde; do
    rm -f $(ls $repo/${pkg}_*.tgz | sort | head -n -2)
    chmod g+w $repo/${pkg}_*.tgz
done

R --vanilla -e "tryCatch(tools::write_PACKAGES(dir='$repo',type='mac.binary'),error=function(e)q(status=1))"


