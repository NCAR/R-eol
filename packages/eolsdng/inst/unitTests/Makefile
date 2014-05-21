PKG=eolsdng
TOP=../..
SUITE=doRUnit.R
R=R

all: test

dtest: inst dutest

test:
	cd ${TOP}/..;\
	export R_LIBS_USER=`mktemp -d /tmp/Rtest_XXXXXX`;\
	${R} --vanilla CMD INSTALL ${PKG};\
	export RCMDCHECK=FALSE;\
	cd ${PKG}/tests;\
	${R} --vanilla --slave < ${SUITE};\
	rm -rf $R_LIBS_USER

dutest:
	export RCMDCHECK=FALSE;\
	cd ${TOP}/tests;\
	${R} --vanilla --slave --debugger=valgrind < ${SUITE}
