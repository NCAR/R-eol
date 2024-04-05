#!/bin/sh

# build and install EOL R packages on local system

if [ $# -eq 0 ]; then
    echo "Usage: ${0##*/} [-e] [-c] [-i] [-s] [-t]
-d: install dependencies
-c: do R CMD check after builds
-e: eolts (R CMD build, R CMD INSTALL)
-i: isfs (R CMD build, R CMD INSTALL)
-s: eolsonde (R CMD build, R CMD INSTALL)
-t: run isfs:runTests() after isfs build, quicker, less thorough than -c"
    exit 1
fi

do_depends=false
do_eolts=false
do_check=false
do_quick_test=false
do_isfs=false
do_eolsonde=false
r_version=
no_echo=--no-echo


is_mac=false
if [ $(uname) == Darwin ]; then
	is_mac=true
	# possible paths to pdflatex
	PATH=$PATH:/Library/TeX/texbin:/usr/texbin
	# geographiclib doesn't have a static library
	fix_lib_dirs=(fftw netcdf hdf5 szip geographiclib)
else
	is_mac=false
fi


get_r_version() {
    vline=`R --vanilla --version | head -1`
    case "$vline" in
        R\ version\ 3*)
            r_version=3 ;;
        R\ version\ 4*)
            r_version=4 ;;
    esac
    test -n "$r_version" || (echo "could not determine R version"; exit 1)
    if [ "$r_version" == 3 ]; then
        no_echo="--slave"
    fi
}

get_r_version


# Unfortunate hack...  Seems the only way on MacOSx to force linking against a
# static library is to make sure the dynlib doesn't exist. Should be another way...
hide_shlibs() {
    for libdir in ${fix_lib_dirs[*]}; do
	./hide-shared-libs.py --hide --dir /usr/local/opt/$libdir/lib
    done
}

restore_shlibs() {
    for libdir in ${fix_lib_dirs[*]}; do
	./hide-shared-libs.py --restore --dir /usr/local/opt/$libdir/lib
    done
}

while [ $# -gt 0 ]; do
    case $1 in
    -d)
        do_depends=true
        ;;
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
    rlib=$(R --vanilla $no_echo -e 'cat(.Library[1])' )

    hide_shlibs
    trap "restore_shlibs > /dev/null;" EXIT

elif [ -n "${R_LIBS_USER}" ]; then
    rlib="${R_LIBS_USER}"
else
    rlib=$(R --vanilla $no_echo -e 'cat(.Library.site[1])' )
    if [ -z "$rlib" -o "$rlib" == "NA" ]; then
        echo "first rlib query failed: $rlib"
        rlib=$(R --vanilla $no_echo -e 'cat(.Library)' )
    fi
fi
echo "using rlib: $rlib"
rargs="--vanilla --quiet"

# Revision info from output of git describe based on a tag of the form vX.Y
if ! gitdesc=$(git describe --match "v[0-9]*"); then
    echo "git describe failed, looking for a tag vX.Y"
    exit 1
fi
#  v1.2-14-gabcdef123
gitdesc=${gitdesc/#v}   # remove v: 1.2-14-gabcdef123
version=${gitdesc%-*}   # remove trailing -*: 1.2-14

[ $gitdesc == "$version" ] && version=${gitdesc}-0  # if no commits since tag



do_pkg() {
    local pkg=$1
    cd $pkg
    rm -f inst/.RData inst/.Rhistory
    autoreconf
    cd -
    rm -f ${pkg}_*.tar.gz

    # make a backup of the $pkg/DESCRIPTION file before changing the Version field
    local tmpdesc=$(mktemp /tmp/${0##*/}_XXXXXX)
    cp $pkg/DESCRIPTION $tmpdesc

    if $is_mac; then
        sed -i "" -E "s/^Version:.*/Version: $version/" $pkg/DESCRIPTION
    else
        sed -ri "s/^Version:.*/Version: $version/" $pkg/DESCRIPTION
    fi

    R $rargs CMD build ${pkg}
    local bstatus=$?
    cp $tmpdesc ${pkg}/DESCRIPTION
    rm -f $tmpdesc
    [ $bstatus -ne 0 ] && exit $bstatus

    if $do_check; then
        R $rargs CMD check -l $rlib -o /tmp ${pkg}_[0-9].[0-9]-*.tar.gz || exit $?
        # R --vanilla --environ CMD check --use-valgrind -o /tmp ${pkg}_*.tar.gz || exit $?
    else
	R $rargs CMD INSTALL -l $rlib ${pkg}_[0-9].[0-9]-*.tar.gz || exit $?
    fi

    if $is_mac; then
        # Check that the package does not have dependencies on /usr/local/lib
	echo "Checking that dependencies of $rlib/${pkg}/libs/${pkg}.so are static"
        if R $rargs CMD otool -L $rlib/${pkg}/libs/${pkg}.so | fgrep -e /usr/local; then
            echo "Error: otool -L $rlib/${pkg}/libs/${pkg}.so indicates it is using a shareable library on /usr/local"
            exit 1
        fi
    fi

    if $do_quick_test; then
        R --vanilla <<-EOD || exit 1
        library($pkg)
        runTests("$pkg")
EOD
    fi
}


stdsrc="https://cran.r-project.org/src/contrib/Archive/splusTimeDate/splusTimeDate_2.5.4.tar.gz"
stssrc="https://cran.r-project.org/src/contrib/Archive/splusTimeSeries/splusTimeSeries_1.5.5.tar.gz"

install_depends() {
    packages='"gWidgets2", "quantreg", "maps", "Rcpp", "RUnit"'
    R $rargs --vanilla << EOD
options(repos=c("http://cran.us.r-project.org"))
install.packages(c($packages))
install.packages("${stdsrc}", repo=NULL, type="source")
install.packages("${stssrc}", repo=NULL, type="source")
EOD
    # R CMD INSTALL "$stdsrc"
    # R CMD INSTALL "$stssrc"
}


if $do_depends; then
    install_depends
fi

if $do_eolts; then
    do_pkg eolts
fi

if $do_isfs; then
    do_pkg isfs
fi

if $do_eolsonde; then
    do_pkg eolsonde
fi

