#!/bin/sh

# Command used to extract sample data file

dir=$ISFS/projects/VERTEX/ISFS/netcdf/noqc_instrument

# Want station 15, it has h2o, co2
ncks -a -h -d time,15,25 -d station,12,15 $dir/isfs_20160912.nc data/test_20160912.nc
