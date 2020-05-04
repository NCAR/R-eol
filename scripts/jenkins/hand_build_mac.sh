#!/bin/sh

# set -v

# script to build macOS binary packages without using jenkins

# returns binary path portion of the repo URL, e.g.
#	/bin/macosx/el-capitan/contrib/3.6 
repopath=$(R --vanilla --slave -e "cat(contrib.url('',type='binary'),'\n')")

repopath=${repopath%% }		# remove any trailing space!!!
rver=${repopath##*/}		# trailing portion

# echo "${repopath}x"
if [ -d /tmp$repopath ]; then
	cd /tmp$repopath/.. || exit 1
	rm -rf $rver
	cd -
fi

export R_REPO=https://archive.eol.ucar.edu/software/R
export WORKSPACE=/tmp

./mac_build.sh

export R_REPO=/tmp
./write_mac_bin_pkgs.sh

cd /tmp$repopath
tar cvzf /tmp/mac_${rver}.tar.gz *

cat << EOD

Copy /tmp/mac_${rver}.tar.gz to EOL server (barolo).

On server:

# if necessary
mkdir /net/www/docs/software/R$repopath

cd /net/www/docs/software/R$repopath

tar xvzf /tmp/mac_${rver}.tar.gz

EOD
