// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.14 $
 *  $Date: 2010/01/02 21:39:03 $
 * 
 *  Description:
 *    A connection to the NIDAS prep application which
 *    provides high rate time series.
 * 
 */

#ifdef HAVE_PREP

#include <sys/types.h>
#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <sys/wait.h>
#include <assert.h>
#include <cstdio>
#include <fcntl.h>

#include "R_prep.h"
#include <R_Matrix.h>
#include <R_utime.h>
#include <R_nts.h>

#include <Rinternals.h>

#include <sstream>
#include <limits>

using namespace std;
using namespace isfs;

using eolts::R_utime;
using eolts::R_nts;
using eolts::R_Matrix;

set<R_prep *> R_prep::_openSet;

char **R_prep::_environ = 0;

int R_prep::_lenv = 0;

SEXP R_prep::variablesSlotName;

SEXP R_prep::rateSlotName;

SEXP R_prep::cppSlotName;

#ifndef __linux
extern "C" {
    char **environ;
}
#endif

SEXP open_prep(SEXP con)
{
    new R_prep(con);
    return con;
}

SEXP prep_pid(SEXP con)
{

    R_prep *prep = R_prep::getR_prep(con);

    int pid = -1;
    if (prep) pid = prep->getPid();
    
    SEXP ans = PROTECT(Rf_allocVector(INTSXP,1));
    INTEGER(ans)[0] = pid;
    UNPROTECT(1);
    return ans;
}

SEXP prep_get_period(SEXP con)
{
    R_prep *prep = R_prep::getR_prep(con);
    if (!prep)
        Rf_error("prep object is not open. Has it already been closed? You must reopen with prep(...)");

    SEXP ans = PROTECT(Rf_allocVector(REALSXP,1));
    REAL(ans)[0] = prep->getPeriod();
    UNPROTECT(1);
    return ans;
}

SEXP prep_set_period(SEXP con,SEXP val)
{
    R_prep *prep = R_prep::getR_prep(con);
    if (!prep)
        Rf_error("prep object is not open. Has it already been closed? You must reopen with prep(...)");

    if (Rf_length(val) != 1) {
        std::ostringstream ost;
        ost << "prep_set_period error: length of period argument is " <<
            Rf_length(val) << " It should be 1";
        Rf_error(ost.str().c_str());
    }
    prep->setPeriod(REAL(val)[0]);
    return val;
}

SEXP prep_get_nrows(SEXP con)
{

    R_prep *prep = R_prep::getR_prep(con);
    if (!prep)
        Rf_error("prep object is not open. Has it already been closed? You must reopen with prep(...)");

    SEXP ans = PROTECT(Rf_allocVector(INTSXP,1));
    INTEGER(ans)[0] = prep->getNrows();
    UNPROTECT(1);
    return ans;
}

SEXP prep_set_nrows(SEXP con,SEXP val)
{
    R_prep *prep = R_prep::getR_prep(con);
    if (!prep)
        Rf_error("prep object is not open. Has it already been closed? You must reopen with prep(...)");

    if (Rf_length(val) != 1) {
        std::ostringstream ost;
        ost << "prep_set_nrows error: length of nrows argument is " <<
            Rf_length(val) << " It should be 1";
        Rf_error(ost.str().c_str());
    }
    prep->setNrows(INTEGER(val)[0]);
    return val;
}

SEXP start_prep(SEXP con, SEXP prog,SEXP argsp, SEXP envp, SEXP unitsp)
{
    R_prep *prep = R_prep::getR_prep(con);
    if (!prep)
        Rf_error("prep object is not open. Has it already been closed? You must reopen with prep(...)");

    if (TYPEOF(prog) != STRSXP || Rf_length(prog) != 1)
        Rf_error("error: prog argument is not a character vector of length 1");

    string progName(CHAR(STRING_ELT(prog,0)));

    if (TYPEOF(argsp) != STRSXP)
        Rf_error("error: args argument is not a character vector");

    vector<string> args;
    for (size_t i = 0; i < (size_t)Rf_xlength(argsp); i++) {
        SEXP str = STRING_ELT(argsp,i);
        args.push_back(CHAR(str));
    }

    if (TYPEOF(envp) != STRSXP)
        Rf_error("error: env argument is not a character vector");

    vector<string> env;
    for (size_t i = 0; i < (size_t)Rf_xlength(envp); i++) {
        SEXP str = STRING_ELT(envp,i);
        env.push_back(CHAR(str));
    }

    if (TYPEOF(unitsp) != STRSXP)
        Rf_error("error: units argument is not a character vector");

    vector<string> units;
    for (size_t i = 0; i < (size_t)Rf_xlength(unitsp); i++) {
        SEXP str = STRING_ELT(unitsp,i);
        units.push_back(CHAR(str));
    }

    prep->start(progName,args,env,units);

    SEXP result = PROTECT(Rf_allocVector(INTSXP,1));
    INTEGER(result)[0] = prep->getPid();
    UNPROTECT(1);
    return result;
}

SEXP read_prep(SEXP con,SEXP beginp, SEXP endp, SEXP nrowsp)
{

    R_prep *prep = R_prep::getR_prep(con);
    if (!prep)
        Rf_error("prep object is not open. Has it already been closed? You must reopen with prep(...)");

    R_utime begin(beginp);
    R_utime end(endp);

    if (TYPEOF(nrowsp) != INTSXP && Rf_length(nrowsp) != 1)
        Rf_error("error: nrows argument is not an integer of length 1");

    size_t nrows = INTEGER(nrowsp)[0];

    return prep->read(begin,end,nrows);
}

SEXP close_prep(SEXP con)
{
    R_prep *prep = R_prep::getR_prep(con);
    if (!prep)
        Rf_error("prep object is not open. Has it already been closed?");

    int status = prep->send_cont();
    status = prep->close();
    delete prep;

    SEXP result = PROTECT(Rf_allocVector(INTSXP,1));
    INTEGER(result)[0] = status;
    UNPROTECT(1);
    return result;
}

SEXP stop_sig_prep(SEXP con)
{
    R_prep *prep = R_prep::getR_prep(con);
    if (!prep)
        Rf_error("prep object is not open. Has it already been closed? You must reopen with prep(...)");

    int status = prep->send_stop();

    SEXP result = PROTECT(Rf_allocVector(INTSXP,1));
    INTEGER(result)[0] = status;
    UNPROTECT(1);
    return result;
}

SEXP cont_sig_prep(SEXP con)
{
    R_prep *prep = R_prep::getR_prep(con);
    if (!prep)
        Rf_error("prep object is not open. Has it already been closed? You must reopen with prep(...)");

    int status = prep->send_cont();

    SEXP result = PROTECT(Rf_allocVector(INTSXP,1));
    INTEGER(result)[0] = status;
    UNPROTECT(1);
    return result;
}

/**
 * Get pointer to C++ R_prep object from R object.
 * Verify that it is valid pointer.
 */
R_prep *R_prep::getR_prep(SEXP obj)
{
    SEXP cslot = Rf_getAttrib(obj,R_prep::cppSlotName);
    R_prep *prep = *(R_prep **)RAW(cslot);

    if (!findConnection(prep)) prep = 0;
    return prep;

}

R_prep::R_prep(SEXP obj): _obj(obj), _pid(-1),_inputfp(0),
    _variables(),_units(),_progName(),_savedTime(std::numeric_limits<double>::min()),
    _period(-1.0),_nrows(0),_haveSavedTime(0)
{

    // store a pointer to this C++ object in the _cxxObjectOffset slot
    SEXP slot = Rf_getAttrib(obj,cppSlotName);

    if (TYPEOF(slot) != RAWSXP || Rf_length(slot) != 8)
        Rf_error("cppPtr slot of object is not of type \"raw\", length 8");

    *((R_prep **)RAW(slot)) = this;

    addConnection(this);

    slot = Rf_getAttrib(obj,variablesSlotName);

    for (size_t i = 0; i < (size_t)Rf_xlength(slot); i++) {
        SEXP str = STRING_ELT(slot,i);
        _variables.push_back(CHAR(str));
    }
}

R_prep::~R_prep()
{
#ifdef DEBUG
    REprintf("~R_prep\n",this);
#endif

    if (removeConnection(this) == 0) {
        Rf_error("programming error, can't find object in open list");
    }
    close();

    return;
}

bool R_prep::findConnection(R_prep *con)
{
    // these accesses of _openSet should be protected with a mutex...
    set<R_prep*>::const_iterator si = _openSet.find(con);
    return si != _openSet.end();
}

bool R_prep::addConnection(R_prep *con) {

    // these accesses of _openSet should be protected with a mutex...
    if (findConnection(con)) return false;
    _openSet.insert(con);
    return true;
}

bool R_prep::removeConnection(R_prep *con)
{
    // these accesses of _openSet should be protected with a mutex...
    set<R_prep*>::const_iterator si = _openSet.find(con);
    if (si != _openSet.end()) {
        _openSet.erase(si);
        return true;
    }
    return false;
}

int R_prep::close()
{
#ifdef DEBUG
    REprintf("R_prep::close()\n");
#endif
    int ret = 0;

    if (_inputfp) fclose(_inputfp);
    _inputfp = 0;

    if (_pid > 0)  {

        int status;
        int sig = SIGINT,stat;
        int i,waitres;
        int nwait = 5;

        waitres = 0;
        for (i = 0; i < nwait && waitres == 0; i++) {
            if (i == nwait - 1) sig = SIGKILL;
            /* ::kill(_pid,0) checks to see if process exists */
            if (::kill(_pid,0) == 0 && ::kill(_pid,sig) < 0) {
                std::ostringstream ost;
                ost << "error sending signal " << strsignal(sig) << " to prep: kill(" << 
                    _pid << ',' << sig << "): " << strerror(errno);
                Rf_warning(ost.str().c_str());
                break;
            }
            sleep(1);

            if ((waitres = waitpid(_pid,&status,WNOHANG)) < 0) {
                std::ostringstream ost;
                ost << "prep error in waitpid(" << _pid << ",...) : " <<
                        strerror(errno);
                Rf_warning(ost.str().c_str());
                break;
            }
        }
        if (waitres > 0) {
            _pid = -1;
            if (WIFEXITED(status)) {
                stat = WEXITSTATUS(status);
                /* Return program status */
                ret = stat;
            }
            else if (WIFSIGNALED(status)) {
                stat = WTERMSIG(status);
                ret = (stat == sig || stat == SIGPIPE ) ? 0 : stat;
            }
        }
        else if (waitres == 0) {
            std::ostringstream ost;
            ost <<  "wait timeout waiting for prep, pid=" << _pid;
            Rf_warning(ost.str().c_str());
            ret = -1;
        }
    }
    return ret;
}

int R_prep::send_stop() {
    int ret = 0;

    if (_pid > 0)  {
        int sig = SIGSTOP;
        /* kill(_pid,0) checks to see if process exists */
        if (::kill(_pid,0) == 0 && ::kill(_pid,sig) < 0) {
            std::ostringstream ost;
            ost << "error sending signal " << strsignal(sig) << " to prep: kill(" << 
                _pid << ',' << sig << "): " << strerror(errno);
            Rf_warning(ost.str().c_str());
            ret = -1;
        }
    }
    return ret;
}

int R_prep::send_cont() {
    int ret = 0;

    if (_pid > 0)  {
        int sig = SIGCONT;
        /* kill(_pid,0) checks to see if process exists */
        if (::kill(_pid,0) == 0 && ::kill(_pid,sig) < 0) {
            std::ostringstream ost;
            ost << "error sending signal " << strsignal(sig) << " to prep: ::kill(" << 
                _pid << ',' << sig << "): " << strerror(errno);
            Rf_warning(ost.str().c_str());
            ret = -1;
        }
    }
    return ret;
}
void R_prep::start(const string& prog,
        const vector<string>& args,
        const vector<string>& env,
        const vector<string>& units)
{
    _progName = prog;


    int fd[2];
    if (::pipe(fd) < 0) {
        std::ostringstream ost;
        ost << "prep start error: pipe(fd) failure: " << strerror(errno);
        Rf_error(ost.str().c_str());
    }

    switch (_pid = ::vfork()) {
    case 0:
        /* child */
        {
            unsigned int i;
            const char **newargs = new const char*[args.size()+2];

            newargs[0] = _progName.c_str();
            for (i = 0; i < args.size(); i++)
                newargs[i+1] = args[i].c_str();
            newargs[i+1] = 0;

#ifdef DEBUG
            for (i = 0; newargs[i]; i++)
                REprintf("argv[%d]=%s\n",i,newargs[i]);
#endif

#ifdef __linux
            unsigned int ne;
            for (ne = 0; environ[ne]; ne++);

            const char **newenv = new const char*[ne+env.size()+1];

            ::memcpy(newenv,environ,ne*sizeof(newenv[0]));

            for (i = 0; i < env.size(); i++) newenv[ne++] = env[i].c_str();
            newenv[ne] = 0;
#endif

#ifdef DEBUG
            for (i = 0; newenv[i]; i++)
                REprintf("newenv[%d]=%s\n",i,newenv[i]);
#endif

            int res = ::nice(4);
            if (res < 0) REprintf("nice: %s\n",strerror(errno));

            ::close(1);
            res = ::dup(fd[1]);
            ::close(fd[0]);
            ::close(fd[1]);

#ifdef __linux
            ::execvpe(_progName.c_str(),(char *const *)newargs,
                    (char * const*)newenv);
#else
            for (i = 0; i < env.size(); i++) putenv(env[i].c_str());
            ::execvp(_progName.c_str(),(char *const *)newargs);
#endif

            // shouldn't return
            REprintf("exec returned! %s\n",strerror(errno));
            _exit(1);
        }
    case -1:
        {
            std::ostringstream ost;
            ost << "prep start error: vfork() failure: " << strerror(errno);
            Rf_error(ost.str().c_str());
        }
    default:	// parent
        ::close(fd[1]);
        if ((_inputfp = ::fdopen(fd[0],"r")) == NULL) {
            std::ostringstream ost;
            ost << "prep start error: fdopen() failure: " << strerror(errno);
            Rf_error(ost.str().c_str());
        }
        _units.clear();
        if (units.empty()) {
            int c;
            // skip first line containing variable names
            while ((c = ::fgetc(_inputfp)) != EOF && c != '\n');
            if (c == EOF && ferror(_inputfp)) {
                Rf_error(strerror(errno));
            }
            Rprintf("units=");
            unsigned int n;
            for (n = 0; c != EOF; n++) {
                string unitstr;
                // If you try to use fscanf, you get the compile error:
                // cannot convert `R_newio_FILE*' to `FILE*'
                // So we'll use fgetc
                while ((c = ::fgetc(_inputfp)) != EOF && (c == ' ' || c == '\t'));
                if (c == EOF && ferror(_inputfp)) {
                    Rf_error(strerror(errno));
                }
                if (c == EOF || c != '"') break;
                for ( ; (c = ::fgetc(_inputfp)) != EOF && c != '"'; ) unitstr += c;
                if (c == EOF && ferror(_inputfp)) {
                    Rf_error(strerror(errno));
                }
                Rprintf("\"%s\" ",unitstr.c_str());
                _units.push_back(unitstr);
                if (c == EOF || c != '"') break;
            }
            Rprintf("\n");
        }
        else for (unsigned int i = 0; i < units.size(); i++)
            _units.push_back(string(units[i]));
    }
}

SEXP R_prep::read(R_utime &begin, R_utime &end, size_t nrows)
{

    double beginTime = begin.getTime(0);
    double endTime = end.getTime(0);

#ifdef DEBUG
    Rprintf("beginTime=%f, endtime=%f, nrows=%zu\n",beginTime,endTime,nrows);
#endif

    size_t nc = _variables.size();

    R_Matrix<double> matrix(REALSXP,0,nc);
    double *dptr = matrix.getDataPtr();

    R_utime times;
    double trec;

    /*
     * if user wants to read a given number of rows, but
     * there is a data gap, don't read more than 2% past
     * the end time.
     */
    double maxendtime = endTime + (endTime - beginTime) * .02;

    size_t irow;
    size_t nalloc = 0;
    size_t n = 0;
    size_t res;

    vector<float> drec(nc);

    for (irow = 0; nrows <= 0 || irow < nrows; ) {
        if (irow == nalloc) {
            if (nrows > 0) n = nrows;
            else if (irow > 1) {
                double recsPerSec =
                    ((irow-1) / (times.getTime(irow-1) - times.getTime(0)));
                n = (size_t)((endTime - beginTime) * recsPerSec + 0.5);
                if (n < nalloc + 10) n = nalloc + 10;
                else if (n > 200000) n = nalloc + 2000;
            }
            else n = 6000;

            matrix.setDims(n,nc);
            dptr = matrix.getDataPtr();

            times.setLength(n);
            nalloc = n;
#ifdef DEBUG
            REprintf("nalloc=%d, irow=%d, nrows=%d\n",nalloc,irow,nrows);
#endif
        }
        if (_haveSavedTime)  {
            trec = _savedTime;
            _haveSavedTime = 0;
        }
        else if ((res = ::fread((char *)&trec,sizeof(double),1,_inputfp)) != 1) {
            if (ferror(_inputfp)) Rf_error(strerror(errno));
            break;
        }

        /*
         * Check to see if time exceeds requested time
         */
        if (nrows > 0) {
            if (trec >= maxendtime) {
                _savedTime = trec;
                _haveSavedTime = 1;
                break;
            }
        }
        else if (trec >= endTime) {
            _savedTime = trec;
            _haveSavedTime = 1;
            break;
        }

        if ((res = ::fread((char *)&drec.front(),sizeof(float),nc,_inputfp)) != nc)
        {
            if (ferror(_inputfp)) Rf_error(strerror(errno));
            break;
        }

        if (trec >= beginTime) {
            times.setTime(irow,trec);
            for (size_t i = 0; i < nc; i++) dptr[i * n + irow] = drec[i];
            irow++;
        }
#ifdef DEBUG
        REprintf("irow=%d,trec=%f\n",irow,trec);
#endif
    }
    matrix.setDims(irow,nc);
    matrix.setColumnNames(_variables);
    times.setLength(irow);
    R_nts nts;

    nts.setMatrix(matrix.getRObject());
    nts.setPositions(times.getRObject());

    nts.setUnits(_units);

    return nts.getRObject();
}

void R_prep::putenv(const char *env)
{
    for (int ii=0; environ[ii]; ii++)
        if (!::strcmp(env,environ[ii])) return;       // already set

    char* prevenv;
    ::putenv((char *)dupenv(env,&prevenv));
    delete [] prevenv;
}

const char *R_prep::dupenv(const char *env,char** prevenv)
{
    const char *e1,*e2;
    int ii;
    e1 = ::strchr(env,'=');
    if (e1 == (char *)0) e1 = env + strlen(env);
    for (ii=0; ii < _lenv; ii++) {
        if ((e2 = ::strchr(_environ[ii],'=')) != (char *)0 &&
                (e1 - env == e2 - _environ[ii]) &&
                !::strncmp(env,_environ[ii],e1-env))  break;
    }

    // Did not find env variable with same name in my static collection.
    // Grow the collection.
    if (ii == _lenv) {
        char **newenv = new char*[_lenv+1];
        for (ii = 0; ii < _lenv; ii++) newenv[ii] = _environ[ii];
        newenv[ii] = 0;
        delete [] _environ;
        _environ = newenv;
        _lenv++;
    }

    // if no environment variable of this name and value
    // then add it to our static array of environment variables.
    // prevenv should be delete[]d after doing a ::putenv of the
    // new value.
    *prevenv = 0;
    if (_environ[ii] == 0 || strcmp(env,_environ[ii])) {
        *prevenv = _environ[ii];            // save to be delete[]d
        _environ[ii] = new char[strlen(env)+1];
        strcpy(_environ[ii],env);
    }
    return _environ[ii];
}
#endif  // HAVE_PREP
