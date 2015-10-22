#!/bin/sh

jscr=/scr/tmp/$USER/jenkins/R-eol/packages
[ -d $jscr ] || exit 1

repo=/net/www/docs/software/R/src/contrib

tmpfile1=$(mktemp /tmp/$(basename $0)_XXXXXX)
tmpfile2=$(mktemp /tmp/$(basename $0)_XXXXXX)
trap "{ rm -f $tmpfile1 $tmpfile2; }" EXIT

ls $repo/eolts_*.tar.gz $repo/isfs_*.tar.gz $repo/eolsonde_*.tar.gz | sort > $tmpfile1

rsync --remove-source-files -a $jscr/*.tar.gz $repo

R --vanilla -e "tryCatch(tools::write_PACKAGES(dir='$repo',type='source'),error=function(e)q(status=1))"

ls $repo/eolts_*.tar.gz $repo/isfs_*.tar.gz $repo/eolsonde_*.tar.gz | sort > $tmpfile2

# number of new files on $repo
nnew=$(comm -13 $tmpfile1 $tmpfile2 | wc -l)

# If there are new files delete old ones.
# The idea is not to delete a package until it is no
# longer referenced in the PACKAGES file on the repository
if [ $nnew -ge 2 ]; then
    rm -f $(comm -2 $tmpfile1 $tmpfile2)
fi
