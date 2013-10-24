#!/bin/sh

ftp -p -n win-builder.r-project.org << EOD
user anonymous maclean@ucar.edu
binary
cd R-release
put eolts_0.0-1.tar.gz
quit
EOD

