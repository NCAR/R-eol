#!/bin/sh

# Build R packages from source tar files on the EOL repo
# install to the first site library, .Library.site[1].

pkgs="c('eolts','isfs','eolsonde')"

rlib=$(R --vanilla --slave -e 'cat(.Library.site[1])')

if [ ! -d $rlib ]; then
    echo "$rlib not found"
    exit 1
fi

R --vanilla -e "tryCatch(install.packages(pkgs=$pkgs,type='source',repos='http://www.eol.ucar.edu/software/R',lib='$rlib'),error=function(e)q(status=1))"

