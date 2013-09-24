#!/bin/sh

ver=$(R -q --version | sed -nr 's/^R version ([0-9]+\.[0-9]+).*$/\1/p')
plat=$(R -q --version | sed -nr 's/^Platform: +([^ ]+).*$/\1/p')

rlib=$HOME/R/${plat}-library/$ver
echo "rlib=$rlib"

[ -d $rlib ] | mkdir -p $rlib

# build package tar ball
# R CMD build eolts

# build package tar ball and install it into $rlib
# -l library
R CMD INSTALL --preclean --build eolts
# R CMD INSTALL --clean eolts
