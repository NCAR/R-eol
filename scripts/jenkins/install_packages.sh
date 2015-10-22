#!/bin/sh

http=false

repo=http://www.eol.ucar.edu/software/R
rdir=/net/www/docs/software/R/src/contrib

# If $rdir is accessible, fetch source file from it, otherwise use http
[ -d $rdir ] || http=true

shopt -s nullglob

while [ $# -gt 0 ]; do
    case $1 in
    *)
        # [ -n "$pkgs" ] && pkgs=$pkgs,

        [ -n "$pkgs" ] && pkgs=$pkgs,

        if $http; then
            pkgs+=\'$1\'
        else
            pkg=$(echo $rdir/${1}_*.tar.gz)

            if [ -n "$pkg" ]; then
                pkgs+=\'$pkg\'
            fi
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


if $http; then

    R --vanilla -e "tryCatch(install.packages(pkgs=$pkgs,type='source',repos='$repo',lib='$rlib'),error=function(e)q(status=1))"

else

    R --vanilla -e "tryCatch(install.packages(pkgs=$pkgs,type='source',repos=NULL,lib='$rlib'),error=function(e)q(status=1))"
fi

