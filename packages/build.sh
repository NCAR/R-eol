#!/bin/sh

# build and install EOL R packages on local system

if [ $# -eq 0 ]; then
    echo "Usage: ${0##*/} [-e] [-c] [-i] [-s] [-t]
-c: do R CMD check after builds
-e: eolts (R CMD build, R CMD INSTALL)
-i: isfs (R CMD build, R CMD INSTALL)
-s: eolsonde (R CMD build, R CMD INSTALL)
-t: run isfs:runTests() after isfs build, quicker, less thorough than -c"
    exit 1
fi

do_eolts=false
do_check=false
do_quick_test=false
do_isfs=false
do_eolsonde=false

is_mac=false
if [ $(uname) == Darwin ]; then
	is_mac=true
else
	is_mac=false
fi

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
    -s)
        do_eolsonde=true
        ;;
    -t)
        do_quick_test=true
        ;;
    *)
        echo "huh?"
        exit 1
        ;;
    esac
    shift
done

# --vanilla implies --no-environ, which means $R_ENVIRON (defaulting
# to $R_HOME/etc/Renviron.site) is not sourced.
# But the default $R_HOME/etc/Renviron on Linux does set R_LIBS_SITE.
# We generally don't use an Renviron.site anyway.

if $is_mac; then
	rlib=$(R --vanilla --slave -e 'cat(.Library[1])' )
else
	rlib=$(R --vanilla --slave -e 'cat(.Library.site[1])' )
fi

rargs="--vanilla"

# Revision info from output of git describe based on a tag of the form vX.Y
if ! gitdesc=$(git describe --match "v[0-9]*"); then
    echo "git describe failed, looking for a tag vX.Y"
    exit 1
fi
#  v1.2-14-gabcdef123
gitdesc=${gitdesc/#v}   # remove v: 1.2-14-gabcdef123
version=${gitdesc%-*}   # remove trailing -*: 1.2-14

[ $gitdesc == "$version" ] && version=${gitdesc}-0  # if no commits since tag

if $do_eolts; then

    pkg=eolts
    cd $pkg
    autoreconf
    cd -

    rm -f ${pkg}_*.tar.gz

    # make a backup of the $pkg/DESCRIPTION file before changing the Version field
    tmpdesc=$(mktemp /tmp/${0##*/}_XXXXXX)
    cp $pkg/DESCRIPTION $tmpdesc

    if $is_mac; then
        sed -i "" -E "s/^Version:.*/Version: $version/" $pkg/DESCRIPTION
    else
        sed -ri "s/^Version:.*/Version: $version/" $pkg/DESCRIPTION
    fi

    R $rargs CMD build ${pkg}
    bstatus=$?
    cp $tmpdesc ${pkg}/DESCRIPTION
    rm -f $tmpdesc
    [ $bstatus -ne 0 ] && exit $bstatus

    R $rargs CMD INSTALL -l $rlib ${pkg}_[0-9].[0-9]-*.tar.gz || exit $?

    if $is_mac; then
        # Check that the package does not have dependencies on /usr/local/lib
        if R $rargs CMD otool -L $rlib/${pkg}/libs/eolts.so | fgrep -q /usr/local/lib; then
            echo "otool -L $rlib/${pkg}/libs/eolts.so indicates it is using a shareable library on /usr/local/lib"
            exit 1
        fi
    fi

    if $do_check; then
        R $rargs CMD check -o /tmp ${pkg}_[0-9].[0-9]-*.tar.gz || exit $?
        # R --vanilla --environ CMD check --use-valgrind -o /tmp ${pkg}_*.tar.gz || exit $?
    fi
fi

if $do_isfs; then

    pkg=isfs
    cd $pkg
    autoreconf
    cd -

    rm -f ${pkg}_*.tar.gz

    # make a backup of the isfs/DESCRIPTION file before changing the Version field
    tmpdesc=$(mktemp /tmp/${0##*/}_XXXXXX)
    cp ${pkg}/DESCRIPTION $tmpdesc

    if $is_mac; then
        sed -i "" -E "s/^Version:.*/Version: $version/" $pkg/DESCRIPTION
    else
        sed -ri "s/^Version:.*/Version: $version/" $pkg/DESCRIPTION
    fi

    R $rargs CMD build ${pkg}
    bstatus=$?
    cp $tmpdesc ${pkg}/DESCRIPTION
    rm -f $tmpdesc
    [ $bstatus -ne 0 ] && exit $bstatus


    R $rargs CMD INSTALL -l $rlib ${pkg}_[0-9].[0-9]-*.tar.gz || exit $?

    if $is_mac; then
        # Check that the package does not have dependencies on /usr/local/lib
        if R $rargs CMD otool -L $rlib/${pkg}/libs/isfs.so | fgrep -q /usr/local/lib; then
            echo "otool -L $rlib/${pkg}/libs/isfs.so indicates it is using a shareable library on /usr/local/lib"
            exit 1
        fi
    fi

    if $do_check; then
        R $rargs CMD check -o /tmp ${pkg}_[0-9].[0-9]*.tar.gz || exit $?
        # R --vanilla --environ CMD check --use-valgrind -o /tmp ${pkg}_*.tar.gz || exit $?
    fi
    if $do_quick_test; then
        R --vanilla <<-EOD || exit 1
            library(isfs)
            isfs::runTests()
EOD
    fi
fi

if $do_eolsonde; then

    pkg=eolsonde
    cd $pkg
    autoreconf
    cd -

    rm -f ${pkg}_*.tar.gz

    # make a backup of the eolsonde/DESCRIPTION file before changing the Version field
    tmpdesc=$(mktemp /tmp/${0##*/}_XXXXXX)
    cp ${pkg}/DESCRIPTION $tmpdesc

    if $is_mac; then
        sed -i "" -E "s/^Version:.*/Version: $version/" $pkg/DESCRIPTION
    else
        sed -ri "s/^Version:.*/Version: $version/" $pkg/DESCRIPTION
    fi

    R $rargs CMD build ${pkg}
    bstatus=$?
    cp $tmpdesc ${pkg}/DESCRIPTION
    rm -f $tmpdesc
    [ $bstatus -ne 0 ] && exit $bstatus

    R $rargs CMD INSTALL -l $rlib ${pkg}_[0-9].[0-9]-*.tar.gz || exit $?

    if $is_mac; then
        # Check that the package does not have dependencies on /usr/local/lib
        if R $rargs CMD otool -L $rlib/${pkg}/libs/eolsonde | fgrep -q /usr/local/lib; then
            echo "otool -L $rlib/${pkg}/libs/eolsonde indicates it is using a shareable library on /usr/local/lib"
            exit 1
        fi
    fi

    if $do_check; then
        R $rargs CMD check -o /tmp ${pkg}_[0-9].[0-9]-*.tar.gz || exit $?
        # R --vanilla --environ CMD check --use-valgrind -o /tmp ${pkg}_*.tar.gz || exit $?
    fi
fi

