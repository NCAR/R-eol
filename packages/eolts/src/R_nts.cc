// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 * 2013,2014, Copyright University Corporation for Atmospheric Research
 * 
 * This file is part of the "eolts" package for the R software environment.
 * The license and distribution terms for this file may be found in the
 * file LICENSE in this package.
 */

#include "R_nts.h"
#include "R_NamedVector.h"

#include <sstream>
#include <limits>

using std::vector;
using std::string;

using namespace eolts;

SEXP R_nts::classDef;
SEXP R_nts::dataSlotName;
SEXP R_nts::posSlotName;
SEXP R_nts::unitsSlotName;
SEXP R_nts::weightsSlotName;
SEXP R_nts::weightMapSlotName;
SEXP R_nts::stationsSlotName;
SEXP R_nts::longNamesSlotName;
SEXP R_nts::startposSlotName;
SEXP R_nts::endposSlotName;
SEXP R_nts::timeFormatSlotName;
SEXP R_nts::timeZoneSlotName;

R_nts::R_nts(): _obj(0),_pindx(-1)
{
    if (!classDef) classDef = R_do_MAKE_CLASS("nts");
    PROTECT_WITH_INDEX(_obj = R_do_new_object(classDef),&_pindx);
}

R_nts::R_nts(SEXP obj):_obj(obj),_pindx(-1)
{
}

R_nts::~R_nts() {
#ifdef DEBUG
    Rprintf("~R_nts, _pindx=%d\n",_pindx);
#endif
    if (_pindx >= 0) UNPROTECT(1);
}

void R_nts::setMatrix(SEXP val)
{
    Rf_setAttrib(_obj,dataSlotName,val);
}

SEXP R_nts::getMatrix()
{
    return Rf_getAttrib(_obj,dataSlotName);
}

void R_nts::setPositions(SEXP val)
{
    Rf_setAttrib(_obj,posSlotName,val);
}

SEXP R_nts::getPositions()
{
    return Rf_getAttrib(_obj,posSlotName);
}

void R_nts::setStations(const vector<string>& names,const vector<int>& numbers)
{
    R_NamedVector<int> stns(INTSXP,names.size());

    int *stnptr = stns.getDataPtr();

    for (unsigned int i = 0; i < names.size(); i++) stnptr[i] = numbers[i];

    stns.setNames(names);

    Rf_setAttrib(_obj,stationsSlotName,stns.getRObject());
}

vector<int> R_nts::getStationNumbers() const
{
    SEXP obj = Rf_getAttrib(_obj,stationsSlotName);
    R_NamedVector<int> stns(INTSXP,obj);

    size_t n = stns.getLength();

    int *iptr = stns.getDataPtr();
    vector<int> nums(n);

    for (unsigned int i = 0; i < n; i++) nums[i] = iptr[i];
    return nums;
}

vector<string> R_nts::getStationNames() const
{
    SEXP obj = Rf_getAttrib(_obj,stationsSlotName);
    R_NamedVector<int> stns(INTSXP,obj);
    return stns.getNames();
}

void R_nts::setUnits(const vector<string>& units)
{
    SEXP cobj = PROTECT(Rf_allocVector(STRSXP,units.size()));

    for (unsigned int i = 0; i < units.size(); i++)
        SET_STRING_ELT(cobj,i,Rf_mkChar(units[i].c_str()));

    Rf_setAttrib(_obj,unitsSlotName,cobj);
    UNPROTECT(1);
}

vector<string> R_nts::getUnits() const
{
    SEXP cobj = Rf_getAttrib(_obj,unitsSlotName);

    vector<string> units;
    for (int i = 0; i < Rf_length(cobj); i++) 
        units.push_back(string(CHAR(STRING_ELT(cobj,i))));

    return units;
}

void R_nts::setLongNames(const vector<string>& long_names)
{
    SEXP cobj = PROTECT(Rf_allocVector(STRSXP,long_names.size()));

    for (unsigned int i = 0; i < long_names.size(); i++)
        SET_STRING_ELT(cobj,i,Rf_mkChar(long_names[i].c_str()));

    Rf_setAttrib(_obj,longNamesSlotName,cobj);
    UNPROTECT(1);
}

vector<string> R_nts::getLongNames() const
{
    SEXP cobj = Rf_getAttrib(_obj,longNamesSlotName);

    vector<string> longNames;
    for (int i = 0; i < Rf_length(cobj); i++) 
        longNames.push_back(string(CHAR(STRING_ELT(cobj,i))));

    return longNames;
}
void R_nts::setTimeFormat(const string& val)
{
    SEXP cobj = Rf_getAttrib(_obj,timeFormatSlotName);
    if (TYPEOF(cobj) != STRSXP) {
#ifdef DEBUG
        std::ostringstream ost;
        ost << "time.format slot is " << Rf_type2char(TYPEOF(cobj)) << " not STRSXP";
        Rf_warning(ost.str().c_str());
#endif
        cobj = PROTECT(Rf_allocVector(STRSXP,1));
        Rf_setAttrib(_obj,timeFormatSlotName,cobj);
        UNPROTECT(1);
    }
    if (Rf_length(cobj) != 1) {
        cobj = PROTECT(Rf_lengthgets(cobj,1));
        Rf_setAttrib(_obj,timeFormatSlotName,cobj);
        UNPROTECT(1);
    }
    SET_STRING_ELT(cobj,0,Rf_mkChar(val.c_str()));
}

string R_nts::getTimeFormat() const
{
    SEXP cobj = Rf_getAttrib(_obj,timeFormatSlotName);
    if (TYPEOF(cobj) != STRSXP) {
#ifdef DEBUG
        std::ostringstream ost;
        ost << "time.format slot is " << Rf_type2char(TYPEOF(cobj)) << " not STRSXP";
        Rf_warning(ost.str().c_str());
#endif
        cobj = PROTECT(Rf_allocVector(STRSXP,1));
        Rf_setAttrib(_obj,timeFormatSlotName,cobj);
        UNPROTECT(1);
    }
    if (Rf_length(cobj) != 1) return "";
    return CHAR(STRING_ELT(cobj,0));
}

void R_nts::setTimeZone(const string& val)
{
    SEXP cobj = Rf_getAttrib(_obj,timeZoneSlotName);
    if (TYPEOF(cobj) != STRSXP) {
#ifdef DEBUG
        std::ostringstream ost;
        ost << "time.zone slot is " << Rf_type2char(TYPEOF(cobj)) << " not STRSXP";
        Rf_warning(ost.str().c_str());
#endif
        cobj = PROTECT(Rf_allocVector(STRSXP,1));
        Rf_setAttrib(_obj,timeZoneSlotName,cobj);
        UNPROTECT(1);
    }
    if (Rf_length(cobj) != 1) {
        cobj = PROTECT(Rf_lengthgets(cobj,1));
        Rf_setAttrib(_obj,timeZoneSlotName,cobj);
        UNPROTECT(1);
    }
    SET_STRING_ELT(cobj,0,Rf_mkChar(val.c_str()));
}

string R_nts::getTimeZone() const
{
    SEXP cobj = Rf_getAttrib(_obj,timeZoneSlotName);
    if (TYPEOF(cobj) != STRSXP) {
#ifdef DEBUG
        std::ostringstream ost;
        ost << "time.zone slot is " << Rf_type2char(TYPEOF(cobj)) << " not STRSXP";
        Rf_warning(ost.str().c_str());
#endif
        cobj = PROTECT(Rf_allocVector(STRSXP,1));
        Rf_setAttrib(_obj,timeZoneSlotName,cobj);
        UNPROTECT(1);
    }
    if (Rf_length(cobj) != 1) return "";
    return CHAR(STRING_ELT(cobj,0));
}

void R_nts::setWeights(SEXP val)
{
    Rf_setAttrib(_obj,weightsSlotName,val);
}

SEXP R_nts::getWeights()
{
    return Rf_getAttrib(_obj,weightsSlotName);
}

void R_nts::setWeightMap(vector<int> wmap)
{
    R_NamedVector<int> wtmap(INTSXP,wmap.size());
    int *iptr = wtmap.getDataPtr();

    for (unsigned int i = 0; i < wmap.size(); i++) iptr[i] = wmap[i];

    Rf_setAttrib(_obj,weightMapSlotName,wtmap.getRObject());
}

vector<int> R_nts::getWeightMap() const
{
    vector<int> wmap;

    SEXP obj = Rf_getAttrib(_obj,weightMapSlotName);
    R_NamedVector<int> wtmap(INTSXP,obj);

    int *iptr = wtmap.getDataPtr();

    for (unsigned int i = 0; i < wtmap.getLength(); i++) wmap.push_back(iptr[i]);
    return wmap;
}

extern "C" {

    /* 
     * Do a match within a delta.
     * Returns an error if it detects that the sequences are not ordered.
     */
    SEXP match_within(SEXP t1p, SEXP t2p, SEXP dtp, SEXP typep, SEXP dupokp)
    {
        int nprot = 0;

        if (TYPEOF(t1p) != REALSXP) {
            PROTECT(t1p = Rf_coerceVector(t1p,REALSXP));
            nprot++;
        }
        double* t1 = REAL(t1p);
        size_t l1 = Rf_xlength(t1p);

        if (TYPEOF(t2p) != REALSXP) {
            PROTECT(t2p = Rf_coerceVector(t2p,REALSXP));
            nprot++;
        }
        double* t2 = REAL(t2p);
        size_t l2 = Rf_xlength(t2p);

        double dt;
        if (Rf_length(dtp) != 1) {
            UNPROTECT(nprot);
            Rf_error("delta argument is not length 1");
        }
        if (TYPEOF(dtp) == REALSXP)
            dt = REAL(dtp)[0];
        else if (TYPEOF(dtp) == INTSXP)
            dt = INTEGER(dtp)[0];
        else {
            UNPROTECT(nprot);
            Rf_error("delta argument is not numeric");
        }

        if (TYPEOF(typep) != INTSXP || Rf_length(typep) != 1) {
            UNPROTECT(nprot);
            Rf_error("match type is not integer, length 1");
        }
        int type = INTEGER(typep)[0];

        if (TYPEOF(dupokp) != INTSXP || Rf_length(dupokp) != 1) {
            UNPROTECT(nprot);
            Rf_error("duplicate type is not integer, length 1");
        }
        int dupok = INTEGER(dupokp)[0];

        double t1l = -std::numeric_limits<double>::max();
        double t2l = -std::numeric_limits<double>::max();

        SEXP res = PROTECT(Rf_allocVector(INTSXP,l1));
        nprot++;
        int* match = INTEGER(res);

        size_t i2 = 0;
        for (size_t i1 = 0; i1 < l1; i1++) {
            match[i1] = 0;

            double x1 = t1[i1];
            if (x1 < t1l) {
                UNPROTECT(nprot);
                Rf_error("first series is not ordered");
            }
            t1l = x1;

            double xc = x1 - dt;
            /*
             * Find first t2[i2] such that t2[i2] >= t1[i1]-dt
             */
            for (; i2 < l2; i2++) {
                if (t2[i2] < t2l) {
                    UNPROTECT(nprot);
                    Rf_error("second series is not ordered");
                }
                t2l = t2[i2];
                if (t2l >= xc) break;
            }
            /*
             * At this point:
             * if i2 == l2:
             *	t2[i2-1] < t1[i2]-dt (no match)
             * else
             *  t2[i2] >= t1[i1]-dt (possible match)
             */
            if (i2 == l2) continue;

            /*
             * Now check if t2[i2] <= t1[i1] + dt
             */
            xc = x1 + dt;
            if (t2[i2] <= xc) {
                if (type == 1) {
                    /* possibly more than one match, look for closest one */
                    double d,dmin;
                    double t2lx = t2l;
                    dmin = fabs(t2[i2] - x1);
                    for (size_t j2 = i2 + 1; j2 < l2; j2++) {
                        if (t2[j2] < t2lx) {
                            UNPROTECT(nprot);
                            Rf_error("second series is not ordered");
                        }
                        t2lx = t2[j2];
                        if (t2lx > xc) break;
                        if ((d = fabs(t2[j2] - x1)) < dmin) {
                            dmin = d;
                            i2 = j2;
                            t2l = t2lx;
                        }
                        else break;
                    }
                }
                // assert(i1 >= 0);
                // assert(i1 < l1);
                // assert(i2 < l2);
                match[i1] = i2 + 1;	/* 1 based indexing */
                if (!dupok) i2++;       /* no duplicates */
            }
        }
        UNPROTECT(nprot);
        return res;
    }
}
