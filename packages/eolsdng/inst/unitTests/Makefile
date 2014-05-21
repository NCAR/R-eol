PKG=eolsdng
TOP=../..
SUITE=doRUnit.R
R=R

all: inst test

dtest: inst dutest

inst: # First must install package -- but where ?? -- will that be in R_LIBS ?
	cd ${TOP}/..;\
	export R_LIBS_USER=$(mktemp -d /tmp/Rtest_XXXXXX)
	${R} --vanilla CMD INSTALL ${PKG}

test: # Run unit tests
	export RCMDCHECK=FALSE;\
	cd ${TOP}/tests;\
	${R} --vanilla --slave < ${SUITE}

dutest:
	export RCMDCHECK=FALSE;\
	cd ${TOP}/tests;\
	${R} --vanilla --slave --debugger=valgrind < ${SUITE}
