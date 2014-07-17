// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 * 2013,2014, Copyright University Corporation for Atmospheric Research
 * 
 * This file is part of the "isfs" package for the R software environment.
 * The license and distribution terms for this file may be found in the
 * file LICENSE in this package.
 */
/*
 *  Description:
 *    A connection to the NCAR/ATD/ISFF prep application which
 *    provides high rate time series.
 * 
 */

#ifdef HAVE_PREP

#ifndef R_PREP_H
#define R_PREP_H

#include <R.h>

#include "R_utime.h"
#include <vector>
#include <set>
#include <string>

extern "C" {
    SEXP open_prep(SEXP);
    SEXP prep_pid(SEXP);
    SEXP prep_get_period(SEXP);
    SEXP prep_set_period(SEXP,SEXP);
    SEXP prep_get_nrows(SEXP);
    SEXP prep_set_nrows(SEXP,SEXP);
    SEXP start_prep(SEXP con,SEXP prog,SEXP args,SEXP env,SEXP units);
    SEXP read_prep(SEXP con,SEXP begin, SEXP end, SEXP nrows);
    SEXP close_prep(SEXP con);
    SEXP stop_sig_prep(SEXP con);
    SEXP cont_sig_prep(SEXP con);
}

namespace isfs {

class R_prep {
public:

    R_prep(SEXP);

    ~R_prep();

    SEXP getRObject() { return _obj; }

    int close();

    int getPid() const { return _pid; }

    double getPeriod() const { return _period; }
    void setPeriod(double v) { _period =v ; }

    size_t getNrows() const { return _nrows; }
    void setNrows(size_t v) { _nrows =v ; }

    void start(const std::string& prog,
            const std::vector<std::string>& args,
            const std::vector<std::string>& env,
            const std::vector<std::string>& units);

    SEXP read(eolts::R_utime &begin, eolts::R_utime &end, size_t nrows);

    int send_cont();

    int send_stop();

    static void putenv(const char *env);

    static bool addConnection(R_prep *);

    static bool removeConnection(R_prep *);

    static bool findConnection(R_prep *);

    static R_prep *getR_prep(SEXP);

    static SEXP variablesSlotName;

    static SEXP rateSlotName;

    static SEXP cppSlotName;

private:

    static const char *dupenv(const char *env,char** prevenv);

    static std::set<R_prep*> _openSet;

    static char ** _environ;

    static int _lenv;

    SEXP _obj;

    int _pid;

    FILE *_inputfp;

    std::vector<std::string> _variables;

    std::vector<std::string> _units;

    std::string _progName;

    /**
     * Last time read from previous read
     */
    double _savedTime;

    /**
     * The length of time in seconds of the previous request.
     */
    double _period;

    /**
     * The number of records returned in the previous request.
     */
    size_t _nrows;

    char _haveSavedTime;

    // declared private to prevent copying and assignment
    R_prep(const R_prep&);
    R_prep& operator = (const R_prep&) const;

};

}   // namespace isfs

#endif

#endif  // HAVE_PREP
