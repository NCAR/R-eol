#!/bin/sh

ver=$(R -q --version | sed -nr 's/^R version ([0-9]+\.[0-9]+).*$/\1/p')
plat=$(R -q --version | sed -nr 's/^Platform: +([^ ]+).*$/\1/p')

rlib=$HOME/R/${plat}-library/$ver
echo "rlib=$rlib"

[ -d $rlib ] | mkdir -p $rlib

# rm -rf /home/maclean/R/x86_64-redhat-linux-gnu-library/3.0/eolts
# rm -rf /home/maclean/R/x86_64-redhat-linux-gnu-library/3.0/isfs

# build package tar ball
# R CMD build eolts

cd eolts
autoconf
cd -

# build package tar ball and install it into $rlib
# -l library
# R --vanilla CMD INSTALL --preclean --build --configure-args=--with-netcdf-include=/tmp/bozo eolts || exit $?
R --vanilla CMD INSTALL --preclean --build eolts || exit $?
# R CMD INSTALL --clean eolts

# echo "building isfs"
R --vanilla CMD INSTALL --preclean --build isfs || exit $?

R --vanilla CMD build eolts || exit ?
R --vanilla CMD build isfs || exit ?
