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

rlib=$(R RHOME)/site-library

# --vanilla does --no-environ so we have to set R_LIBS_SITE ourselves
# export R_LIBS_SITE=$rlib
rargs="--vanilla"

if $do_eolts; then

    cd eolts
    autoreconf
    cd -


    R $rargs CMD build eolts || exit ?
    R $rargs CMD INSTALL -l $rlib eolts_[0-9].[0-9]-[0-9].tar.gz || exit $?

    if $do_check; then
        R $rargs CMD check -o /tmp eolts_[0-9].[0-9]-[0-9].tar.gz || exit $?
        # R --vanilla --environ CMD check --use-valgrind -o /tmp eolts_*.tar.gz || exit $?
    fi
fi

if $do_isfs; then

    cd isfs
    autoreconf
    cd -

    R $rargs CMD build isfs || exit ?
    R $rargs CMD INSTALL -l $rlib isfs_[0-9].[0-9]-[0-9].tar.gz || exit $?

    if $do_check; then
        R $rargs CMD check -o /tmp isfs_[0-9].[0-9]-[0-9].tar.gz || exit $?
        # R --vanilla --environ CMD check --use-valgrind -o /tmp eolts_*.tar.gz || exit $?
    fi
fi

if $do_install; then
    scp eolts_0.0-1.tar.gz isfs_0.0-1.tar.gz porter2:/net/www/docs/software/R/src/contrib
fi

