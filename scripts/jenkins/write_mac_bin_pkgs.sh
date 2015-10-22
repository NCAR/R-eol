#!/bin/sh

repo=/net/www/docs/software/R

contrib=$(</scr/tmp/maclean/jenkins/R-eol/mac/contrib_path.txt)

repo=$repo/$contrib

[ -d $repo ] || mkdir -p $repo

rm -f $repo/eolts_*.tgz $repo/isfs_*.tgz $repo/eolsonde_*.tgz

rsync -a /scr/tmp/maclean/jenkins/R-eol/mac/*.tgz $repo

rm -f /scr/tmp/maclean/jenkins/R-eol/mac/*.tgz

R --vanilla -e "tryCatch(tools::write_PACKAGES(dir='$repo',type='mac.binary'),error=function(e)q(status=1))"


