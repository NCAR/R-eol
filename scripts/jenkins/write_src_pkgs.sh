#!/bin/sh

# From working directory, copy source R packages to a
# local R package repository

repo=${R_REPO:?}/src/contrib

[ -d $repo ] || mkdir -p $repo || exit 1

rsync -a *.tar.gz $repo

for pkg in eolts isfs eolsonde; do
    rm -f $(ls $repo/${pkg}_*.tar.gz | sort | head -n -2)
    chmod g+w $repo/${pkg}_*.tar.gz
done

R --vanilla -e "tryCatch(tools::write_PACKAGES(dir='$repo',type='source'),error=function(e)q(status=1))"

