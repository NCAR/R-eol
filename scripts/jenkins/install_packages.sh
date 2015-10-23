#!/bin/sh

# Build R packages from source form, install to the first site
# library, .Library.site[1].

shopt -s nullglob

while [ $# -gt 0 ]; do
    case $1 in
    *)
        pkg=$1.tar.gz
        if [ -f $pkg ]; then
            [ -n "$pkgs" ] && pkgs=$pkgs,
            pkgs+="'$pkg'"
        fi
        ;;
    esac
    shift
done

pkgs="c("$pkgs")"

rlib=$(R --vanilla --slave -e 'cat(.Library.site[1])')

if [ ! -d $rlib ]; then
    echo "$rlib not found"
    exit 1
fi


R --vanilla -e "tryCatch(install.packages(pkgs=$pkgs,type='source',lib='$rlib'),error=function(e)q(status=1))"

