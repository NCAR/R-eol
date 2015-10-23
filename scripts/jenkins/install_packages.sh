#!/bin/sh

# Build R packages from source tar files on the current directory,
# install to the first site library, .Library.site[1].

for pkg in eolts isfs eolsonde; do
    pkgf=$(echo ${pkg}_*.tar.gz)    # force pathname expansion
    if [ -f $pkgf ]; then
        [ -n "$pkgs" ] && pkgs=$pkgs,
        pkgs+="'$pkgf'"
    fi
done

pkgs="c("$pkgs")"

rlib=$(R --vanilla --slave -e 'cat(.Library.site[1])')

if [ ! -d $rlib ]; then
    echo "$rlib not found"
    exit 1
fi

R --vanilla -e "tryCatch(install.packages(pkgs=$pkgs,type='source',repos=NULL,lib='$rlib'),error=function(e)q(status=1))"

