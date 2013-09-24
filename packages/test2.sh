#!/bin/sh

while [ $# -gt 0 ]; do
    case $1 in
    -v)
        valops="-d valgrind"
        ;;
    esac
    shift
done

R --vanilla $valops << \EOD

library("eolts")

con = netcdf(file=c("flossii.021120.nc","flossii.021121.nc"),dir="/home/maclean/git/R-eol/packages/eolts/tests")

vars = variables(con)

x = readnc(con,"w_15m")

close(con)


q()
EOD
