#include "R_nts.h"
#include "R_NamedVector.h"

// #include <string.h>
// #include <math.h>

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
SEXP R_nts::startposSlotName;
SEXP R_nts::endposSlotName;
SEXP R_nts::timeFormatSlotName;
SEXP R_nts::timeZoneSlotName;

R_nts::R_nts(): _obj(0),_pindx(-1)
{
    if (!classDef) classDef = R_do_MAKE_CLASS("nts");
    _obj = R_do_new_object(classDef);
    PROTECT_WITH_INDEX(_obj,&_pindx);
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

void R_nts::setMatrix(R_MatrixBase *val)
{
    Rf_setAttrib(_obj,dataSlotName,val->getRObject());
}

R_Matrix<double> *R_nts::getRealMatrix()
{
    SEXP obj = Rf_getAttrib(_obj,dataSlotName);
    R_Matrix<double> *mat = new R_Matrix<double>(REALSXP,obj);
    return mat;		// owned by caller
}

R_Matrix<int> *R_nts::getIntMatrix()
{
    SEXP obj = Rf_getAttrib(_obj,dataSlotName);
    R_Matrix<int> *mat = new R_Matrix<int>(INTSXP,obj);
    return mat;		// owned by caller
}

void R_nts::setPositions(R_utime *val)
{
    Rf_setAttrib(_obj,posSlotName,val->getRObject());
}

R_utime *R_nts::getPositions()
{
    SEXP obj = Rf_getAttrib(_obj,posSlotName);
    R_utime *pos = new R_utime(obj);
    return pos;		// owned by caller
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

void R_nts::setTimeFormat(const string& val)
{
    SEXP cobj = Rf_getAttrib(_obj,timeFormatSlotName);
    if (Rf_length(cobj) != 1) {
        cobj = PROTECT(Rf_lengthgets(cobj,1));
        Rf_setAttrib(_obj,timeFormatSlotName,cobj);
        UNPROTECT(1);
    }
    SET_STRING_ELT(cobj,0,Rf_mkChar(val.c_str()));
    Rf_setAttrib(_obj,timeFormatSlotName,cobj);
}

string R_nts::getTimeFormat() const
{
    SEXP cobj = Rf_getAttrib(_obj,timeFormatSlotName);
    if (Rf_length(cobj) != 1) return "";
    return CHAR(STRING_ELT(cobj,0));
}

void R_nts::setTimeZone(const string& val)
{
    SEXP cobj = Rf_getAttrib(_obj,timeZoneSlotName);
    if (Rf_length(cobj) != 1) {
        cobj = PROTECT(Rf_lengthgets(cobj,1));
        Rf_setAttrib(_obj,timeZoneSlotName,cobj);
        UNPROTECT(1);
    }
    SET_STRING_ELT(cobj,0,Rf_mkChar(val.c_str()));
    Rf_setAttrib(_obj,timeZoneSlotName,cobj);
}

string R_nts::getTimeZone() const
{
    SEXP cobj = Rf_getAttrib(_obj,timeZoneSlotName);
    if (Rf_length(cobj) != 1) return "";
    return CHAR(STRING_ELT(cobj,0));
}

void R_nts::setWeights(SEXP weights)
{
    Rf_setAttrib(_obj,weightsSlotName,weights);
}

R_Matrix<int> *R_nts::getWeights()
{
    SEXP obj = Rf_getAttrib(_obj,weightsSlotName);
    return new R_Matrix<int>(INTSXP,obj);	// owned by caller
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
     * This assumes that both sequences are ordered!
     */
    void match_within(
            double *t1,		/* vector of numeric values */
            long *l1p,		/* length of t1 */
            double *t2,		/* look for t1 in t2 */
            long *l2p,		/* length of t2 */
            double *dtp,	/* closeness */
            long *matchType,	/* type: 1=nearest, 0=first */
            long *match)	/* returned match */
    {

        int i1,i2,j2;
        int l1 = *l1p;
        int l2 = *l2p;
        double dt = *dtp;
        double x1,xc;
        int type = *matchType;

        i2 = 0;
        for (i1 = 0; i1 < l1; i1++) {
            match[i1] = 0;

            x1 = t1[i1];
            xc = x1 - dt;
            /*
             * Find first t2[i2] such that t2[i2] >= t1[i1]-dt
             */
            for (; i2 < l2 && t2[i2] < xc; i2++);
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
                    dmin = fabs(t2[i2] - x1);
                    for (j2 = i2 + 1; j2 < l2 && t2[j2] <= xc; j2++) {
                        if ((d = fabs(t2[j2] - x1)) < dmin) {
                            dmin = d;
                            i2 = j2;
                        }
                    }
                }
                // assert(i1 >= 0);
                // assert(i1 < l1);
                // assert(i2 < l2);
                match[i1] = i2 + 1;	/* 1 based indexing */
            }
        }
    }
}
