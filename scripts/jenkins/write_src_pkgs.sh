#!/bin/sh
# From working directory, copy source R packages to a
# local R package repository

umask 0002

repo=${R_REPO:?}/src/contrib

if ! [ -d $repo ]; then
    mkdir -p $repo || exit 1
    chmod g+ws $repo
fi

rsync -a *.tar.gz $repo

R --vanilla -e "tryCatch(tools::write_PACKAGES(dir='$repo',type='source'),error=function(e)q(status=1))"

for pkg in eolts isfs eolsonde; do
    rm -f $(ls -rt $repo/${pkg}_*.tar.gz | head -n -2)
    chmod g+w $repo/${pkg}_*.tar.gz
done

