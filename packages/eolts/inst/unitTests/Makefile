PKG=eolts
TOP=../..
SUITE=doRUnit.R
R=R

all: inst test

inst: # First must install package -- but where ?? -- will that be in R_LIBS ?
	cd ${TOP}/..;\
	${R} --vanilla CMD INSTALL ${PKG}

test: # Run unit tests
	export RCMDCHECK=FALSE;\
	cd ${TOP}/tests;\
	${R} --vanilla --slave --debugger=valgrind < ${SUITE}
