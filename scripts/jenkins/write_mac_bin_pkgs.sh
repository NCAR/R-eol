#!/bin/sh

# From working directory, copy .tgz mac.binary R packages to a
# local R package repository

# contrib_path.txt was written by mac_build.sh, 
repo=${R_REPO:?}/$(<contrib_path.txt)

[ -d $repo ] || mkdir -p $repo

rm -f $repo/eolts_*.tgz $repo/isfs_*.tgz $repo/eolsonde_*.tgz

rsync -a *.tgz $repo

rm -f *.tgz

R --vanilla -e "tryCatch(tools::write_PACKAGES(dir='$repo',type='mac.binary'),error=function(e)q(status=1))"


