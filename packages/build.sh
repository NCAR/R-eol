#!/bin/sh

if [ $# -eq 0 ]; then
    echo "Usage: ${0##*/} [-e] [-c] [-i] [-I]"
    exit 1
fi

do_eolts=false
do_check=false
do_isfs=false
do_install=false

while [ $# -gt 0 ]; do
    case $1 in
    -e)
        do_eolts=true
        ;;
    -c)
        do_check=true
        ;;
    -i)
        do_isfs=true
        ;;
    -I)
        do_install=true
        ;;
    *)
        echo "huh?"
        exit 1
        ;;
    esac
    shift
done

# ver=$(R -q --version | sed -nr 's/^R version ([0-9]+\.[0-9]+).*$/\1/p')
# plat=$(R -q --version | sed -nr 's/^Platform: +([^ ]+).*$/\1/p')
# rlib=$HOME/R/${plat}-library/$ver
# [ -d $rlib ] | mkdir -p $rlib
# rm -rf /home/maclean/R/x86_64-redhat-linux-gnu-library/3.0/eolts
# rm -rf /home/maclean/R/x86_64-redhat-linux-gnu-library/3.0/isfs

rlib=$(R --vanilla --slave -e 'cat(.Library.site[1],"\n")' )
# rlib=$(R RHOME)/site-library

# --vanilla does --no-environ so we have to set R_LIBS_SITE ourselves
# export R_LIBS_SITE=$rlib
rargs="--vanilla"

revision=$(git rev-list HEAD | wc -l)
[ $revision -eq 0 ] && revision=1

if $do_eolts; then

    cd eolts
    autoreconf
    cd -

    rm -f eolts_*.tar.gz

    # make a backup of the eolts/DESCRIPTION file before changing the Version field
    tmpdesc=$(mktemp /tmp/${0##*/}_XXXXXX)
    cp eolts/DESCRIPTION $tmpdesc

    sed -ri "s/^Version: *([0-9]+)\.([0-9]+)-.*/Version: \1.\2-$revision/" eolts/DESCRIPTION

    R $rargs CMD build eolts
    bstatus=$?
    cp $tmpdesc eolts/DESCRIPTION
    rm -f $tmpdesc
    [ $bstatus -ne 0 ] && exit $bstatus

    R $rargs CMD INSTALL -l $rlib eolts_[0-9].[0-9]-*.tar.gz || exit $?

    if $do_check; then
        R $rargs CMD check -o /tmp eolts_[0-9].[0-9]-*.tar.gz || exit $?
        # R --vanilla --environ CMD check --use-valgrind -o /tmp eolts_*.tar.gz || exit $?
    fi
fi

if $do_isfs; then

    cd isfs
    autoreconf
    cd -

    rm -f isfs_*.tar.gz

    # make a backup of the isfs/DESCRIPTION file before changing the Version field
    tmpdesc=$(mktemp /tmp/${0##*/}_XXXXXX)
    cp isfs/DESCRIPTION $tmpdesc

    sed -ri "s/^Version: *([0-9]+)\.([0-9]+)-.*/Version: \1.\2-$revision/" isfs/DESCRIPTION

    R $rargs CMD build isfs
    bstatus=$?
    cp $tmpdesc isfs/DESCRIPTION
    rm -f $tmpdesc
    [ $bstatus -ne 0 ] && exit $bstatus


    R $rargs CMD INSTALL -l $rlib isfs_[0-9].[0-9]-*.tar.gz || exit $?

    if $do_check; then
        R $rargs CMD check -o /tmp isfs_[0-9].[0-9]*.tar.gz || exit $?
        # R --vanilla --environ CMD check --use-valgrind -o /tmp isfs_*.tar.gz || exit $?
    fi
fi

if $do_install; then
    scp eolts_[0-9]*.[0-9]*-*.tar.gz isfs_[0-9]*.[0-9]*-*.tar.gz porter2:/net/www/docs/software/R/src/contrib
fi

