// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.34 $
 *  $Date: 2009/04/01 01:35:53 $
 * 
 *  Description:
 *    Implements a read of a NcFileSet.
 * 
 */

// include <R.h> after <sstream>, otherwise you'll get a compile error on Mac OS X:
// /usr/include/c++/4.2.1/bits/codecvt.h: error: macro "length" passed 4 arguments, but takes just 1
#include <sstream>
#include <memory>   // auto_ptr

#include "NetcdfReader.h"

#include <assert.h>

#include "NcFileSetSummary.h"
#include "NcVar.h"
#include "NcFile.h"
#include "NcAttrT.h"
#include "util.h"	// for dimToString

#include "R_utime.h"
#include "R_nts.h"

using std::string;
using std::vector;
using std::map;
using std::set;

using namespace eolts;

// #define DEBUG

namespace {
    template <typename T>
    void setToNA(T *dp,size_t nr,size_t nd,size_t nc, T na_val)
    {
        unsigned int i,j;

        T *dp1;
        for (i = 0; i < nr; i++) { 
            dp1 = dp;
            for (j = 0; j < nd; j++) { 
                *dp1++ = na_val;
            }
            dp += nc;
        }
    }     

    template <typename T>
    void setTo0(T *dp,size_t nr,size_t nd,size_t nc)
    {
        size_t i,j;

        T *dp1;
        for (i = 0; i < nr; i++) { 
            dp1 = dp;
            for (j = 0; j < nd; j++) *dp1++ = 0;
            dp += nc;
        }
    }     

    template <typename T>
    void setFillToNA(T *dp,size_t nr,size_t nd,size_t nc,T fill, T na_val)
    {
        unsigned int i,j;

        T *dp1;
        for (i = 0; i < nr; i++) { 
            dp1 = dp;
            for (j = 0; j < nd; j++,dp1++) { 
                if (*dp1 == fill) *dp1 = na_val;
            }
            dp += nc;
        }
    }     

    double parseCFTimeString(const string& ustr,double unitsMult) throw(string)
    {
        if (ustr.find("second") != string::npos) unitsMult = 1.0;
        else if (ustr.find("day") != string::npos) unitsMult = 86400.0;
        else if (ustr.find("hour") != string::npos) unitsMult = 3600.0;
        else if (ustr.find("minute") != string::npos) unitsMult = 60.0;
        else throw string("cannot parse time units: ") + ustr;

        string::size_type ss = ustr.find("since");
        if (ss == string::npos)
            throw string("cannot find \"since\": ") + ustr;

        const char *sp1;
        struct tm tm;

// #define TEST_STRPTIME
#ifdef TEST_STRPTIME
        {
            // strptime on Mac OSX is quite finiky.
            // In order to parse:  " 2011-11-15 21:23:00" with "%Y-%m-%d %H:%M:%S"
            // Seems that:
            // 1. must have a space before the %Y descriptor, or move the char
            //    pointer to point to the first non-space in the date string.
            //    Otherwise, strptime with "%Y" by itself returns non-NULL but
            //    a bad year is in the tm struct.  "%Y-%m" returns NULL.
            // 2. Similarly %H must have a space before it, "%H" alone doesn't
            //    remove spaces
            const char* fmts[] = {"%Y"," %Y","%Y-%m"," %Y-%m",
                " %Y-%m-%d"," %Y-%m-%d %H",
                " %Y-%m-%d%H"," %Y-%m-%d %H:%M"," %Y-%m-%d %H:%M:%S"};

            sp1 = " 2011-11-15 21:23:00";

            // while(*sp1 && ::isspace(*sp1)) sp1++;
            
            for (unsigned int i = 0; i < sizeof(fmts)/sizeof(fmts[0]); i++) {
                const char* sp = ::strptime(sp1,fmts[i],&tm);
                if (!sp) {
                    Rprintf("cannot parse with strptime: \"%s\", format: \"%s\"\n",
                        sp1,fmts[i]);
                }
                else {
                    Rprintf("successful parse with strptime: \"%s\", format: \"%s\"\n",
                        sp1,fmts[i]);
                    if (tm.tm_year != 111) Rprintf("bad year after parse: %d\n",
                            tm.tm_year);

                }
            }
        }
#endif

        string str = ustr.substr(ss+5);
        sp1 = str.c_str();
        while(*sp1 && ::isspace(*sp1)) sp1++;

        const char* fmt = "%Y-%m-%d %H:%M:%S";
        const char* sp = ::strptime(sp1,fmt,&tm);
        if (!sp) {
            Rprintf("cannot parse with strptime: \"%s\", format: \"%s\"\n",
                    sp1,fmt);
            throw string("cannot parse with strptime: \"") + sp1 +
            "\", format: \"" + fmt +"\"";
        }
        tm.tm_isdst = 0;
        time_t tval = ::mktime(&tm);    // treats tm as local time
        if (tval == -1) throw string("cannot convert with mktime: ") + sp1;

#define CHECK_TIMEZONE_GLOBAL
#ifdef USE_TIMEZONE_GLOBAL
        tval -= timezone;
        // Rprintf("timezone=%d\n",timezone);
#else
        // compute difference between UTC and local time
        gmtime_r(&tval,&tm);   // convert tval to time fields in GMT
        tm.tm_isdst = 0;
        time_t tval2 = ::mktime(&tm);
#ifdef CHECK_TIMEZONE_GLOBAL
        // Rprintf("timezone=%d, tval2-tval=%d\n",timezone,tval2-tval);
        if (tval2 - tval != timezone)
            Rprintf("timezone discrepancy: timezone=%d, tval2-tval=%d\n",
                    timezone,tval2-tval);
#endif
        tval -= (tval2 - tval);
#endif

        float fsec = 0.0;
        if (*sp == '.' && ::sscanf(sp++,"%f",&fsec) == 1)
            while(*sp && ::isdigit(*sp)) sp++;

        while(*sp && ::isspace(*sp)) sp++;
        if (!*sp) return (double)tval + fsec;

        /* parse time zone spec:
         * [+-]H:M  where H and M can be one or two digits
         * [+-]H    one digit hour
         * [+-]HH   two digit hour
         * [+-]HMM  one digit hour, two digit minutes
         * [+-]HHMM two digit hour, two digit minutes
         */

        bool pos = true;
        if (*sp == '+') sp++;
        else if (*sp == '-') {
            pos = false;
            sp++;
        }

        int hr,mn = 0;
        int toff = 0;
        if (::strchr(sp,':')) {
            if (::sscanf(sp,"%d:%d",&hr,&mn) != 2)
                throw string("cannot parse time zone in: ") + sp1;
            toff = hr * 3600 + mn * 60;
        }
        else {
            int sl = ::strlen(sp);
            if (sl < 3) {
                if (::sscanf(sp,"%d",&hr) != 1)
                    throw string("cannot parse time zone in: ") + sp1;
                toff = hr * 3600;
            }
            else if (sl == 3) {
                if (::sscanf(sp,"%1d%d",&hr,&mn) != 2) 
                    throw string("cannot parse time zone in: ") + sp1;
                toff = hr * 3600 + mn * 60;
            }
            else {
                if (::sscanf(sp,"%2d%d",&hr,&mn) != 2)
                    throw string("cannot parse time zone in: ") + sp1;
                toff = hr * 3600 + mn * 60;
            }
        }
        if (!pos) toff = -toff;
        return (double)tval + fsec + toff;
    }
}     

NetcdfReader::NetcdfReader(R_netcdf* conn):
    _connection(conn)
{
}

NetcdfReader::~NetcdfReader()
{
#ifdef DEBUG
    Rprintf("~NetcdfReader\n");
#endif

#ifdef DEBUG
    Rprintf("~NetcdfReader done\n");
#endif
}

int NetcdfReader::rMode(nc_type type) {
    switch (type) {
    case NC_BYTE:
    case NC_SHORT:
    case NC_INT:
        return INTSXP;
    case NC_FLOAT:
    case NC_DOUBLE:
        return REALSXP;
    case NC_CHAR:
        return STRSXP;
    default:
        return NILSXP;		// problem - should be caught
    }
}

/**
 * Return: 0=OK, 1=WARNING, -1=INCOMPATIBLE
 */
int NetcdfReader::checkVarTypeCompatibility(int rmode1, int rmode2) {
    int result = 0;
    switch (rmode1) {
    case INTSXP:
        switch (rmode2) {
        case INTSXP:
            break;
        case REALSXP:
            result = 1;
            break;
        default:
            result = -1;
        }
        break;
    case REALSXP:
        switch (rmode2) {
        case INTSXP:
            result = 1;
            break;
        case REALSXP:
            break;
        default:
            result = -1;
        }
        break;
    case STRSXP:
        switch (rmode2) {
        case INTSXP:
        case REALSXP:
            result = -1;
            break;
        case STRSXP:
            break;
        default:
            result = -1;
        }
        break;
    }
    return result;
}

/* 
 * Read data from netcdf variables into an array.
 * Netcdf data is stored "by row", i.e. the last indice varies
 * most rapidly. R data is stored by column, first indice varies
 * most rapidly.
 */
SEXP NetcdfReader::read(const vector<string> & vnames,
        const vector<size_t>& startarg, const vector<size_t>&countarg)
    throw(NcException)
{
    NcFileSet *fileset = _connection->getFileSet();

    // #define DEBUG
#ifdef DEBUG
    Rprintf("fileset=0x%x\n",fileset);
#endif

    int nfiles = fileset->getNFiles();
#ifdef DEBUG
    Rprintf("nfiles=%d\n",nfiles);
#endif

    SEXP result = PROTECT(allocVector(VECSXP,vnames.size()));
    SEXP resnames = PROTECT(allocVector(STRSXP,vnames.size()));

    vector<size_t> startreq = startarg;
    vector<size_t> countreq = countarg;

    for (int ivar = 0; ivar < (signed)vnames.size(); ivar++) {

        std::auto_ptr<R_ArrayBase> array;

        vector<size_t> finalRdims;
        size_t lenRead = 0;

        double dFillValue = 0.0;
        int  iFillValue = 0;
        bool hasFillValue = false;
        /*
         * Pointer to first instance of the variable in the file set,
         * for checking type and dimensions of that variable in subsequent files.
         */
        NcVar* var1 = 0;
        const size_t* vedges1 = 0;
        unsigned int ndims1 = 0;
        int rmode = 0;
        int sizeOfT = 0;

        for (int ifile = 0; ifile < nfiles; ifile++) {
            NcFile* ncf = fileset->getNcFile(ifile);
            if (!ncf) continue;
            if (!ncf->isOpen()) continue;
            int ncid = ncf->getNcid();

            NcVar* var = ncf->getVariable(vnames[ivar]);

            if (!var) {
#ifdef VAR_NOT_FOUND_WARNING
                std::ostringstream ost;
                ost << "variable \"" << vnames[ivar] << "\" not found in " <<
                    ncf->getName();
                warning(ost.str().c_str());
#else
                Rprintf("variable \"%s\" not found in file %s\n",
                        vnames[ivar].c_str(),ncf->getName().c_str());
#endif
                continue;
            }

            unsigned int ndims = var->getNumDimensions();
            int vmode = rMode(var->getNcType());
            const size_t *vedges = var->getEdges();
            size_t i;

            // first check consistency of variable types and dimensions between files
            if (!var1) {
                var1 = var;
                vedges1 = vedges;
                ndims1 = ndims;
                rmode = vmode;
                sizeOfT = sizeOfRMode(rmode);
#ifdef DEBUG
                Rprintf("var=%s,sizeOfT=%d\n",var->getName().c_str(),sizeOfT);
#endif
            }
            else {
                if (rmode != vmode) {
                    switch (checkVarTypeCompatibility(rmode,vmode)) {
                    case 0: break;
                    case 1:
                        {
                            std::ostringstream ost;
                            ost << "Possible loss of precision or range: " <<
                                "variable "<< var1->getName() << " in file " << 
                                var1->getFileName() << " is of type " << var1->typeToString() <<
                                ", variable " << var->getName() << " in file " <<
                                var->getFileName() << " is of type " << var->typeToString();
                            warning(ost.str().c_str());
                        }
                        break;
                    case -1:
                        {
                            std::ostringstream ost;
                            ost << "Incompatible variable types: " <<
                                "variable "<< var1->getName() << " in file " << 
                                var1->getFileName() << " is of type " << var1->typeToString() <<
                                ", variable " << var->getName() << " in file " <<
                                var->getFileName() << " is of type " << var->typeToString();
                            error(ost.str().c_str());
                        }
                        break;
                    }
                }

                // first index doesn't have to match
                if (ndims1 != ndims ||
                        (ndims > 1 && !std::equal(vedges+1,vedges+ndims,vedges1+1))) {
                    std::ostringstream ost;
                    ost << "netcdf read error: " <<
                        "variable " << var1->getName() << '(' << dimToString(vedges1,ndims1) << ')' <<
                        " in file " << var1->getFileName() << " has different dimensions than " <<
                        "variable " << var->getName() << '(' << dimToString(vedges,ndims) << ')' <<
                        " in file " << var->getFileName();
                    error(ost.str().c_str());
                }
            }

            unsigned int nrdims = ndims;
            if (rmode == STRSXP) nrdims--;
            if (nrdims == 0) nrdims = 1;

            vector <size_t> start;
            vector <size_t> count;

            if (startreq.size() > 0 && ndims > 0 && startreq[0] > vedges[0]) {
                startreq[0] -= vedges[0];
                continue;		// read next file
            }
            for (i = 0; i < startreq.size() && i < ndims; i++)
                start.push_back(startreq[i]);
            for (; i < ndims; i++) start.push_back(0);

            for (i = 0; i < countreq.size() && i < ndims; i++)
                count.push_back(countreq[i]);
            for (; i < ndims; i++) count.push_back(vedges[i]-start[i]);

            if (countreq.size() > 0 && ndims > 0) {
                // asking to read next file
                if (countreq[0] > vedges[0]-start[0]) {
                    count[0] = vedges[0]-start[0];
                    countreq[0] -= count[0];
                    startreq[0] = 0;
                }
            }

            for (i = 0; i < ndims; i++) {
#ifdef DEBUG
                Rprintf("start[%d]=%d, count[%d]=%d\n",
                        i,start[i],i,count[i]);
#endif
                if (start[i] > vedges[i]) {
                    std::ostringstream ost;
                    ost << "netcdf read error: start=c(" << dimToString(start) << ')' <<
                        " exceeds dimensions of variable " << vnames[ivar] << '(' << 
                        dimToString(vedges,ndims) << ')' << " in file " << ncf->getShortName();
                    error(ost.str().c_str());
                }
                if (start[i]+count[i] > vedges[i]) {
                    std::ostringstream ost;
                    ost << "netcdf read error: start=c(" << dimToString(start) << ')' <<
                        " + count=c(" << dimToString(count) << ')' <<
                        " exceeds dimensions of variable " << vnames[ivar] << '(' << 
                        dimToString(vedges,ndims) << ')' << " in file " << ncf->getShortName();
                    error(ost.str().c_str());
                }
            }

            size_t rlen = 1;
            vector<size_t> rdims;
            for (i = 0; i < nrdims; i++) {
                // first R index is last netcdf index
                size_t ic = nrdims - i - 1;
                size_t len;
#ifdef DEBUG
                Rprintf("%s ic=%d,nrdims=%d,ndims=%d\n",var->getName().c_str(),
                        ic,nrdims,ndims);
#endif
                if (ic == count.size() || (i == 0 && rmode == STRSXP && ndims == 1)) len = 1;
                else len = count[ic];
                rdims.push_back(len);
                rlen *= len;
            }
#ifdef DEBUG
            Rprintf("%s rdims=%s,dims=%s,rlen=%d\n",var->getName().c_str(),
                    dimToString(rdims).c_str(),dimToString(vedges,ndims).c_str(),rlen);
#endif

            char *dataPtr = 0;

            switch (rmode) {
            case REALSXP:
                {
                    R_Array<double>* darray;
                    if (!array.get()) {
                        darray = new R_Array<double>(rmode,rdims);
                        array.reset(darray);
                        finalRdims = rdims;
                        try {
                            const NcAttr* attr = var->getAttribute("_FillValue");
                            if (attr && attr->getLength() == 1) {
                                hasFillValue = true;
                                dFillValue = attr->getNumericValue(0);
                            }
                        }
                        catch(const NcException& nce) {}

                        // this doesn't work. The attribute is somewhere stripped
                        // and is not seen in the return value.
                        // Apparently it is only used when passing arguments
                        // from the .C and .Fortran functions to native code
#ifdef DO_CSINGLE_ATTR
                        if (var->getNcType() == NC_FLOAT) {
                            Rprintf("setting Csingle attribute\n");
                            setAttrib(array.get()->getRObject(),install("Csingle"),ScalarLogical(1));
                        }
#endif
                    }
                    else {
                        // increment last index on successive files
                        darray = dynamic_cast<R_Array<double>*>(array.get());
                        finalRdims[nrdims-1] += rdims[nrdims-1];
                        array->setDims(finalRdims);
                    }
                    dataPtr = (char*) darray->getDataPtr();
#ifdef DEBUG
                    Rprintf("%s rdims=%s,dims=%s,finalRdims=%s,rlen=%d,lenRead=%d\n",var->getName().c_str(),
                        dimToString(rdims).c_str(),
                        dimToString(vedges,ndims).c_str(),
                        dimToString(finalRdims).c_str(),
                        rlen,lenRead);
#endif
                }
                break;
            case INTSXP:
                {
                    R_Array<int>* iarray;
                    if (!array.get()) {
                        iarray = new R_Array<int>(rmode,rdims);
                        array.reset(iarray);
                        finalRdims = rdims;
                        try {
                            const NcAttr* attr = var->getAttribute("_FillValue");
                            if (attr && attr->getLength() == 1) {
                                hasFillValue = true;
                                iFillValue = attr->getNumericValue(0);
                            }
                        }
                        catch(const NcException& nce) {}
                    }
                    else {
                        // increment last index on successive files
                        iarray = dynamic_cast<R_Array<int>*>(array.get());
                        finalRdims[nrdims-1] += rdims[nrdims-1];
                        array->setDims(finalRdims);
                    }
                    dataPtr = (char*) iarray->getDataPtr();
                }
                break;
            case STRSXP:
                {
                    R_Array<char*>* sarray;
                    if (!array.get()) {
                        sarray = new R_Array<char*>(rmode,rdims);
                        array.reset(sarray);
                        finalRdims = rdims;
                        array.reset(sarray);
                    }
                    else {
                        // increment last index on successive files
                        sarray = dynamic_cast<R_Array<char*>*>(array.get());
                        finalRdims[nrdims-1] += rdims[nrdims-1];
                        array->setDims(finalRdims);
                    }
                }
                break;
            }

            char *workPtr = dataPtr + lenRead * sizeOfT;
            int varid = var->getId();

#ifdef DEBUG
            Rprintf("nrdims=%d,ndims=%d,rlen=%d,lenRead=%d,start=%s,count=%s\n",
                    nrdims,ndims,rlen,lenRead,
                    dimToString(start).c_str(),dimToString(count).c_str());
#endif

            int status;
            switch (rmode) {
            case REALSXP:
                // this function will do type conversion if necessary
                status = nc_get_vara_double(ncid,varid,&start.front(),&count.front(),
                        (double*)workPtr);
                if (status != NC_NOERR) {
                    throw NcException("nc_get_vara_double",
                            var->getFileName(),var->getName(),status);
                }
                if (hasFillValue) {
                    double *fp = (double*)workPtr;
                    double *fpend = fp + rlen;
                    for ( ; fp < fpend; fp++) if (*fp == dFillValue) *fp = NA_REAL;
                }
                break;
            case INTSXP:
                status = nc_get_vara_int(ncid,varid,&start.front(),&count.front(),
                        (int*)workPtr);
                if (status != NC_NOERR) {
                    throw NcException("nc_get_vara_int",
                            var->getFileName(),var->getName(),status);
                }
                if (hasFillValue) {
                    int *fp = (int*)workPtr;
                    int *fpend = fp + rlen;
                    for ( ; fp < fpend; fp++) if (*fp == iFillValue) *fp = NA_INTEGER;
                }
                break;
            case STRSXP:
                {
                    int nreads = 1;
                    size_t slen = count[ndims-1];
                    size_t idim;
                    vector<size_t> tstart = start;
                    vector<size_t> tcount = count;
                    for (idim = 0; idim < ndims-1; idim++) {
                        nreads *= count[idim];
                        tcount[idim] = 1;
                    }

                    char tmpstr[slen];

                    for (int iread = 0; iread < nreads; iread++) {
#ifdef DEBUG
                        Rprintf("iread=%d,nread=%d,nrdims=%d,ndims=%d,slen=%d,start=%s,count=%s,tstart=%s,tcount=%s\n",
                                iread,nreads,nrdims,ndims,slen,
                                dimToString(start).c_str(),dimToString(count).c_str(),
                                dimToString(tstart).c_str(),dimToString(tcount).c_str());
#endif

                        status = nc_get_vara_text(ncid,varid,&tstart.front(),
                                &tcount.front(),tmpstr);
                        if (status != NC_NOERR) {
                            throw NcException("nc_get_vara_text",
                                    var->getFileName(),var->getName(),status);
                        }
                        SET_STRING_ELT(array->getRObject(),lenRead + iread,mkCharLen(tmpstr,slen));

                        tstart[nrdims-1]++;		// read next
                        for (idim = nrdims-1; tstart[idim] == (start[idim] + count[idim]); ) {
                            tstart[idim] = start[idim];
                            if (idim == 0) break;
                            tstart[--idim]++;
                        }
                        if (tstart[idim] == (start[idim] + count[idim])) {
                            assert(++iread == nreads);
                            break;
                        }
                    }
                    rlen = nreads;
                }
                break;
            }
            lenRead += rlen;
        }
        if (array.get()) SET_VECTOR_ELT(result,ivar,array->getRObject());
        if (var1) SET_STRING_ELT(resnames,ivar,mkChar(var1->getName().c_str()));
    }
    setAttrib(result,R_NamesSymbol,resnames);
    UNPROTECT(2);   // resnames,result
    return result;
}

size_t NetcdfReader::searchTime(NcVar* var,double stime,NetcdfReader::timeTests test,double timeMult)
    throw(NcException)
{
    /*
     * Test:      GE:  find first record >= search time
     *            GT:  find first record > search time
     *            LE:  find last record <= search time
     *            LT:  find last record < search time
     */

    /* variable shape */
    int ndims = var->getNumDimensions();
    const size_t *dims = var->getEdges();

    size_t nrecs = dims[0];
    if (nrecs == 0) return 0;

    vector<size_t> vindex(ndims);
    for (int i = 0; i < ndims; i++) vindex[i] = 0;

    /*
     * Number of reads necessary to do a binary search
     * The file is halved until one record remains:
     *  nrecs / (2 ^ nreads) <= 1. Use this to avoid
     * an infinite loop if the times are not monotonically
     * increasing.
     */
    int nreads = (int)(log10((double) nrecs) / .30103) + 1;
    size_t n1 = 0;
    size_t n2 = nrecs - 1; // nrecs is > 0
    int nread = 0;
    size_t n;
    double dtime;
    int ncid = var->getNcid();
    int varid = var->getId();
    int status;

    while (n2 - n1 > 1) {
        n = (n1 + n2) / 2;
        vindex[0] = n;
        status = nc_get_var1_double(ncid,varid,&vindex.front(),&dtime);
        if (status != NC_NOERR) {
            throw NcException("nc_get_var1_double",
                    var->getFileName(),var->getName(),status);
        }
        dtime *= timeMult;
        switch (test) {
        case GE:
        case LT:
            if (dtime < stime) n1 = n;
            else n2 = n;
            break;
        case GT:
        case LE:
            if (stime < dtime) n2 = n;
            else n1 = n;
            break;
        }
        if (nread++ > nreads) {
            Rprintf("Error in binary search of variable %s in file %s, nreads=%d,nread=%d",
                    var->getName().c_str(),var->getFileName().c_str(),nreads,nread);
            std::ostringstream ost;
            ost << "Variable " << var->getName() << " in file " <<
                    var->getFileName() << " does not seem to be ordered";
            warning(ost.str().c_str());
            n2 = 0;
            break;
        }
    }
    /* Check if no time in file is >= stime, if so return record after last */
    if (n2 == nrecs - 1) {
        vindex[0] = n2;
        status = nc_get_var1_double(ncid,varid,&vindex.front(),&dtime);
        if (status != NC_NOERR) {
            throw NcException("nc_get_var1_double",
                    var->getFileName(),var->getName(),status);
        }
        dtime *= timeMult;
        switch (test) {
        case GE:
        case LT:
            if (dtime < stime) n2 = nrecs;
            break;
        case GT:
        case LE:
            if (dtime <= stime) n2 = nrecs;
            break;
        }
    }
    /* Check if all times in file are >= stime, if so, return 0 record num */
    if (n1 == 0) {
        vindex[0] = n1;
        status = nc_get_var1_double(ncid,varid,&vindex.front(),&dtime);
        if (status != NC_NOERR) {
            throw NcException("nc_get_var1_double",
                    var->getFileName(),var->getName(),status);
        }
        dtime *= timeMult;
        switch (test) {
        case GE:
        case LT:
            if (dtime >= stime) n2 = 0;
            break;
        case GT:
        case LE:
            if (dtime > stime) n2 = 0;
            break;

        }
    }
    switch (test) {
    case LT:
    case LE:
        if (n2 > 0) n2--;
        break;
    case GE:
    case GT:
        break;
    }
    return n2;
}

SEXP NetcdfReader::read(const vector<string> &vnames,
        double startTime, double endTime,
        const vector<int> &stations,
        const vector<string> &timeVarNames,
        const string &baseTimeName,
        const string& timezone,bool readCountsOnly) throw(NcException)
{

    int ifile,ivar;
    size_t ui,uj;

    NcFileSet* fileset = _connection->getFileSet();

#ifdef DEBUG
    for(ui = 0; ui < vnames.size(); ui++)
        Rprintf("NetcdfReader::read, vnames[%d]=%s,nfiles=%d\n",
                ui,vnames[ui].c_str(),fileset->getNFiles());
#endif

    int nfiles = fileset->getNFiles();

#ifdef DEBUG
    Rprintf("stations.size()=%zu\n",stations.size());
#endif
    NcFileSetSummary vs(fileset,vnames,stations,readCountsOnly);

    // create start array, with enough dimensions for all input variables
    int maxdim = 3;	// time,sample,station
    vector<size_t> start;
    vector<size_t> count;
    vector<ptrdiff_t> stride;
    vector<ptrdiff_t> imap;

    for (int i = 0; i < maxdim; i++) {
        start.push_back(0);
        count.push_back(0);
        // all values of stride are 1. No NetCDF data is skipped.
        stride.push_back(1);
        imap.push_back(1);
    }

    unsigned int ncols = vs.getNumOutputColumns();
    size_t maxSampleDim = vs.getMaxSampleDimension();

    // variable	imap
    // x(time)
    //		ncol * maxSampleDim
    // x(time,station)
    //		ncol * maxSampleDim,1
    // x(time,sample)
    //		ncol * maxSampleDim,ncol*maxSampleDim/sampleDim[ivar]
    // x(time,sample,station)
    //		ncol * maxSampleDim,ncol*maxSampleDim/sampleDim[ivar],1
    //
#ifdef DEBUG
    Rprintf("ncols=%u,getMaxSampleDimension()=%u\n",ncols,maxSampleDim);
#endif

    // mapping for those variables without a sample dimension
    imap[0] = ncols * maxSampleDim;

#ifdef DEBUG
    Rprintf("startTime=%f, endTime=%f\n",startTime,endTime);
#endif

    size_t nrecAlloc = 0;
    size_t nrecs = 0;

    int rmode = rMode(vs.getOutType());

    vector<double> ddata;
    double* ddataPtr0 = 0;
    double* ddataPtr = 0;

    vector<int> idata;
    int* idataPtr0 = 0;
    int* idataPtr = 0;

    vector<double> times;
    double*  timesPtr = &times.front();

    vector <string> colNames;
    vector <string> unitsNames;

    vector <string> stationNames;
    vector <int> stationNumbers;

    // vector <string> countsNames;
    map <string, int> countsCols;
    vector <int> countsMap;
    int ncountsCols = 0;

    for (unsigned int i = 0; i < ncols; i++) {
        string tmp;
        colNames.push_back(tmp);
        unitsNames.push_back(tmp);
        stationNames.push_back(tmp);
        stationNumbers.push_back(-1);
        countsMap.push_back(0);
    }
    int status;

    // mapping of station number to name, where station 0 is the "non" station.
    map<int,string> filesetStations = fileset->getStations();

    // The name of the first variable which uses a
    // given counts variable becomes the countsGroupName.
    map <string, string> countsGroupNameByVariableName;

    // The beginning counts column for each group
    map <string, int> countsColByGroupName;
    // counts group names, in order
    vector <string> countsGroupNames;

    const set<string>& timeDimensionNames = fileset->getTimeDimensionNames();

    for (ifile = 0; ifile < nfiles; ifile++) {

        double interval = 0.0;
        bool timesAreMidpoints = false;

        NcFile* ncf = fileset->getNcFile(ifile);
        if (!ncf || !ncf->isOpen()) continue;

        const NcDim* timeDim = ncf->getTimeDimension(timeDimensionNames);
        if (!timeDim) {
            std::ostringstream ost;
            ost << "error reading file " << ncf->getName() <<
                ", cannot find a time dimension";
            warning(ost.str().c_str());
            continue;
        }

        int ncid = ncf->getNcid();
        NcVar* btvar = ncf->getVariable(baseTimeName);	// OK to fail

        NcVar* tvar = 0;
        for (int i = 0; i < (signed)timeVarNames.size(); i++) {
            tvar = ncf->getTimeSeriesVariable(timeVarNames[i],timeDim);
            if (tvar) break;
        }
        if (!tvar) {
            std::ostringstream ost;
            ost << "error reading file " << ncf->getName() <<
                ", cannot find time variables";
            warning(ost.str().c_str());
            continue;
        }

        double basetime = 0.0;
        size_t nrec = 0;

        if (btvar) {
            status = nc_get_var1_double(ncid,btvar->getId(),&nrec,&basetime);
            if (status != NC_NOERR) {
                throw NcException("nc_get_var1_double",
                        btvar->getFileName(),btvar->getName(),status);
            }
            const NcAttr* unitsAttr = btvar->getAttribute("units");
            if (unitsAttr->getLength() == 1 && unitsAttr->getNcType() == NC_CHAR) {
                const NcAttrT<string>* sattr;
                if ((sattr = dynamic_cast<const NcAttrT<string>*>(unitsAttr))) {
                    string tunits = sattr->getValue(0);
                    try {
                        double unitsMult = 1.0;
                        double toff = parseCFTimeString(tunits,unitsMult);
                        basetime *= unitsMult;
                        basetime += toff;
                    }
                    catch(const string& e) {
                        warning(e.c_str());
                    }
                }
            }
        }

        double timemult = 1.0;

        const NcAttr* unitsAttr = tvar->getAttribute("units");
        if (unitsAttr->getLength() == 1 && unitsAttr->getNcType() == NC_CHAR) {
            const NcAttrT<string>* sattr;
            if ((sattr = dynamic_cast<const NcAttrT<string>*>(unitsAttr))) {
                string tunits = sattr->getValue(0);
                try {
                    basetime = parseCFTimeString(tunits,timemult);
                }
                catch(const string& e) {
                    warning(e.c_str());
                }
            }
        }

        const NcAttr* intervalAttr = tvar->getAttribute("interval");
        if (intervalAttr && intervalAttr->getLength() == 1) {
            const NcAttrT<double>* dattr;
            const NcAttrT<float>* fattr;
            const NcAttrT<long>* lattr;
            if ((dattr = dynamic_cast<const NcAttrT<double>*>(intervalAttr)))
                interval = dattr->getValue(0);
            else if ((fattr = dynamic_cast<const NcAttrT<float>*>(intervalAttr)))
                interval = fattr->getValue(0);
            else if ((lattr = dynamic_cast<const NcAttrT<long>*>(intervalAttr)))
                interval = lattr->getValue(0);
            Rprintf("%s interval attribute= %f\n",ncf->getName().c_str(),interval);
        }

        size_t n1 = searchTime(tvar,std::max(startTime-basetime,0.0),GE,timemult);
        size_t n2 = searchTime(tvar,endTime-basetime,GT,timemult);

#ifdef DEBUG
        Rprintf("%s basetime=%f, t1=%f, t2=%f, n1=%zd, n2=%zd\n",
                ncf->getName().c_str(),
                basetime,startTime-basetime, endTime-basetime, n1,n2);
#endif
        nrec = n2 - n1;

        // nothing to read in this file.
        if (nrec < 1) {
            Rprintf("Requested times not found in file: %s\n",
                    ncf->getShortName().c_str());
            continue;
        }

        double t1,t2;
        status = nc_get_var1_double(ncid,tvar->getId(),&n1,&t1);
        if (status != NC_NOERR) {
            throw NcException("nc_get_var1_double",
                    tvar->getFileName(),tvar->getName(),status);
        }
        t1 *= timemult;
        size_t n = n2 - 1;
        status = nc_get_var1_double(ncid,tvar->getId(),&n,&t2);
        if (status != NC_NOERR) {
            throw NcException("nc_get_var1_double",
                    tvar->getFileName(),tvar->getName(),status);
        }
        t2 *= timemult;
        double recsPerSec;
        if (t2 - t1 > 0.0)
            recsPerSec = (nrec-1) / (t2 - t1);
        else
            recsPerSec = nrec / (endTime - startTime);

        if (interval == 0.0 && recsPerSec != 0.0) interval = 1.0 / recsPerSec;
        // Rprintf("%s interval= %f\n",ncf->getName().c_str(),interval);

        // Is t1 in the middle of an interval? if so, the time variable
        // are the middle times.
        timesAreMidpoints = interval > 0.0 &&
            (fabs(fmod(t1, interval) - interval * .5) < interval * 1.e-3);
        // Rprintf("%s timesAreMidpoints= %d\n",ncf->getName().c_str(),timesAreMidpoints);

        // check if we need to allocate more space
        // Rprintf("%s nrecs=%d, nrec=%d, nrecAlloc=%d\n",
        //     ncf->getName().c_str(),nrecs,nrec,nrecAlloc);
        if (nrecs + nrec > nrecAlloc) {

            n = (size_t) (recsPerSec * (endTime - startTime) + 1.5);
#ifdef DEBUG
            Rprintf("t1,t2=%f,%f,recsPerSec=%f, n=%d\n",t1,t2,recsPerSec,n);
#endif
            /* screen absurd requests */
            if (n < (nrecAlloc + nrec)) n = nrecAlloc + nrec;
            if (n > (nrecAlloc + 100 * nrec)) n = nrecAlloc + 100 * nrec;

            if (nrecAlloc > 0)
                Rprintf("Reallocating: %zd records, previous=%zd\n", n,nrecAlloc);


#ifdef DEBUG
            Rprintf("realloc, n=%d, ncols=%d, nrecAlloc=%d,tot=%d\n",
                    n,ncols,nrecAlloc,n * ncols);
#endif
            size_t nalloc = n * ncols * maxSampleDim;

            switch (rmode) {
            case INTSXP:
                {
                    ptrdiff_t di = idataPtr - idataPtr0;
                    idata.resize(nalloc);
                    idataPtr0 = &idata.front();
                    idataPtr = idataPtr0 + di;
                }
                break;
            case REALSXP:
                {
                    ptrdiff_t di = ddataPtr - ddataPtr0;
                    ddata.resize(nalloc);
                    ddataPtr0 = &ddata.front();
                    ddataPtr = ddataPtr0 + di;
                }
                break;
            }

            if (!readCountsOnly) {
                ptrdiff_t di = timesPtr - &times.front();
                size_t nalloc = n * maxSampleDim;
                times.resize(nalloc);
                timesPtr = &times.front() + di;
            }
            nrecAlloc = n;
        }

        count[0] = nrec;
        start[0] = n1;
        size_t offset;

        if (!readCountsOnly) {
            ptrdiff_t tmap = maxSampleDim;

            if (timesAreMidpoints) {
                offset = (maxSampleDim - 1) / 2;

                status = nc_get_varm_double(ncid,tvar->getId(),&start.front(),
                        &count.front(),0,&tmap,timesPtr+offset);
                if (status != NC_NOERR) throw NcException("nc_get_varm_double",
                        tvar->getFileName(),tvar->getName(),status);
                if (maxSampleDim > 1) {
                    double dt = 1;	// if nrec == 1, dt is undefined

                    if (timemult != 1.0)
                        for (ui = offset; ui < nrec * maxSampleDim; ui += maxSampleDim)
                            timesPtr[ui] *= timemult;

                    // interpolate times if getMaxSampleDimension() > 1
                    // This is symmetrical only if getMaxSampleDimension is odd
                    for (ui = offset; ui < (nrec-1) * maxSampleDim; ui += maxSampleDim) {
                        dt = (timesPtr[ui+maxSampleDim] - timesPtr[ui]) / maxSampleDim;
                        if (ui == offset) 
                            for (uj = 1; uj <= offset; uj++)
                                timesPtr[ui - uj] = timesPtr[ui] - dt * uj;
                        for (uj = 1; uj < maxSampleDim; uj++)
                            timesPtr[ui + uj] = timesPtr[ui] + dt * uj;
                    }
                    for (uj = 1; uj < maxSampleDim - offset; uj++)
                        timesPtr[ui + uj] = timesPtr[ui] + dt * uj;
                }
            }
            else {
                status = nc_get_varm_double(ncid,tvar->getId(),&start.front(),
                        &count.front(),0,&tmap,timesPtr);
                if (status != NC_NOERR) throw NcException("nc_get_varm_double",
                        tvar->getFileName(),tvar->getName(),status);
                if (timemult != 1.0)
                    for (ui = 0; ui < nrec * maxSampleDim; ui += maxSampleDim)
                        timesPtr[ui] *= timemult;

                if (maxSampleDim > 1) {
                    double dt = 1;	// if nrec == 1, dt is undefined
                    // interpolate times if getMaxSampleDimension() > 1
                    for (ui = 0; ui < nrec * maxSampleDim; ui += maxSampleDim) {
                        if (ui + maxSampleDim < nrec * maxSampleDim)
                            dt = (timesPtr[ui+maxSampleDim] - timesPtr[ui]) / maxSampleDim;
                        for (uj = 1; uj < maxSampleDim; uj++)
                            timesPtr[ui + uj] = timesPtr[ui] + dt * uj;
                    }
                }
            }

            for (ui = 0; ui < nrec * maxSampleDim; ui++) {
                *timesPtr++ += basetime;
            }
        }

        unsigned int colNum = 0;
        int stnDimNum;
        int sampleDimNum;

        // for each counts variable in a file, the countsGroupName
        map <string, string> countsGroupNameByCountsVariable;

        for (ivar = 0; ivar < (signed)vnames.size(); ivar++) {

            const vector<size_t> &varDims = vs.getVariableDimensions(ivar);

            // variable not found in any file
            if (varDims.size() == 0) continue;

            // value of station dimension for a variable. 1 if no station dim
            size_t stationDim = vs.getStationDimension(ivar);
#ifdef DEBUG
            Rprintf("var=%s, stationDim=%u\n",vnames[ivar].c_str(),stationDim);
#endif
            // variable does not match stations requested
            if (stationDim == 0) continue;

            size_t sampleDim = vs.getSampleDimension(ivar);

            NcVar* var = 0;

            if (readCountsOnly)
                var = ncf->getTimeSeriesCountsVariable(vnames[ivar],timeDim);
            else
                var = ncf->getTimeSeriesVariable(vnames[ivar],timeDim);

            // found this variable in this file
            if (var) {
                offset = 0;
                int sampleRatio = maxSampleDim / sampleDim;

                if ((maxSampleDim % sampleDim)) {
                    std::ostringstream ost;
                    ost << "Sample rate of " << vnames[ivar] << " is " << sampleDim <<
                        " does not divide evenly into maximum sample rate of requested variables, which is " <<
                    maxSampleDim << ". Timetags of " << vnames[ivar] <<
                    " will be approximate";
                    warning(ost.str().c_str());
                }
                else if (!(sampleRatio % 2)) {
                    std::ostringstream ost;
                    ost << "Sample rate of " << vnames[ivar] << " is " << sampleDim <<
                        ". Maximum sample rate of requested variables is " <<
                        maxSampleDim << ". Since " << maxSampleDim << "/" << sampleDim << "=" <<
                        sampleRatio << " is not odd, timetags of " << vnames[ivar] << " will be approximate";
                    warning(ost.str().c_str());
                }

                sampleDimNum = -1;
                for (unsigned int i = 1; i < var->getNumDimensions(); i++) {
                    const NcDim* dim = var->getDimension(i);
                    if (dim->getName().substr(0,6) == "sample") {
                        if (sampleDim != dim->getLength()) {
                            std::ostringstream ost;
                            ost << "Error " << var->getFileName() << ": " <<
                                var->getName() << ": dimension " <<
                                dim->getName() << " has length " <<
                                dim->getLength() << " should be " << sampleDim;
                            error(ost.str().c_str());
                        }
                        sampleDimNum = i;
                    }
                }

                if (sampleDimNum >= 0) {
                    start[sampleDimNum] = 0;
                    count[sampleDimNum] = var->getEdge(sampleDimNum);
                    imap[sampleDimNum] = ncols * sampleRatio;
                }

                stnDimNum = var->whichDimension("station");
                if (stnDimNum >= 0) {
                    start[stnDimNum] = vs.getStationStart();
                    count[stnDimNum] = vs.getStationCount();
                    imap[stnDimNum] = 1;
                }

                // if sampleRatio > 1 then NA fill block before reading data
                // since not all records will have data
                if (sampleRatio > 1) {
                    // offset into output block of first sample
                    offset = ((sampleRatio - 1) / 2) * ncols;
                    switch (rmode) {
                    case INTSXP:
                        setToNA<int>(idataPtr,nrec * maxSampleDim,
                                stationDim,ncols,NA_INTEGER);
                        break;
                    case REALSXP:
                        setToNA<double>(ddataPtr,nrec * maxSampleDim,
                                stationDim,ncols,NA_REAL);
                        break;
                    }
                }

                Rprintf("Reading %s, %s: ... start=(%s), count=(%s)\n",
                    ncf->getShortName().c_str(),
                    var->getName().c_str(),
                    dimToString(&start.front(),varDims.size()).c_str(),
                    dimToString(&count.front(),varDims.size()).c_str());


                int varid = var->getId();
                const NcAttr* attr;

                switch (rmode) {
                case REALSXP:

#ifdef DEBUG
                    Rprintf("Reading %s %s into %s, readCountsOnly=%d\n",
                        ncf->getShortName().c_str(),var->getName().c_str(),NcVar::typeToString(vs.getOutType()).c_str(),readCountsOnly);
                Rprintf("ndim=%d, dims=%s, start=%s, count=%s, imap=%s, offset=%d ptrdiff=%d\n",
                        varDims.size(),
                        dimToString(varDims).c_str(),
                        dimToString(start).c_str(),
                        dimToString(count).c_str(),
                        dimToString(imap).c_str(),
                        offset,ddataPtr + offset - ddataPtr0);
#endif
                    status = nc_get_varm_double(ncid,varid,&start.front(),&count.front(),
                            &stride.front(),&imap.front(), ddataPtr + offset);
                    if (status != NC_NOERR) {
                        throw NcException("nc_get_varm_double",
                                var->getFileName(),var->getName(),status);
                    }
                    attr = var->getAttribute("_FillValue");
                    if (attr && attr->getLength() == 1) {
                        double fill = attr->getNumericValue(0);
                        double *dp = ddataPtr + offset;
                        setFillToNA(dp,nrec * maxSampleDim,
                                stationDim,ncols,fill,NA_REAL);
                    }
                    break;
                case INTSXP:
#ifdef DEBUG
                    Rprintf("Reading %s %s into %s, readCountsOnly=%d\n",
                        ncf->getShortName().c_str(),var->getName().c_str(),NcVar::typeToString(vs.getOutType()).c_str(),readCountsOnly);
                Rprintf("ndim=%d, dims=%s, start=%s, count=%s, imap=%s, offset=%d ptrdiff=%d\n",
                        varDims.size(),
                        dimToString(varDims).c_str(),
                        dimToString(start).c_str(),
                        dimToString(count).c_str(),
                        dimToString(imap).c_str(),
                        offset,idataPtr + offset - idataPtr0);
#endif
                    status = nc_get_varm_int(ncid,varid,&start.front(),&count.front(),
                            &stride.front(),&imap.front(), idataPtr + offset);
                    if (status != NC_NOERR) {
                        throw NcException("nc_get_varm_long",
                                var->getFileName(),var->getName(),status);
                    }
                    attr = var->getAttribute("_FillValue");
                    if (attr && attr->getLength() == 1) {
                        int fill = (int)attr->getNumericValue(0);
                        int *dp = idataPtr + offset;
                        setFillToNA(dp,nrec * maxSampleDim,
                                stationDim,ncols,fill,NA_INTEGER);
                    }
                    break;
                default:
                    break;
                }
            }
            else {
#ifdef VAR_NOT_FOUND_WARNING
                std::ostringstream ost;
                ost << "variable \"" << vnames[ivar] << "\" not found in " <<
                    ncf->getName();
                warning(ost.str().c_str());
#else
                Rprintf("variable \"%s\" not found in %s\n",
                        vnames[ivar].c_str(),ncf->getShortName().c_str());
#endif
                switch (rmode) {
                case INTSXP:
                    setToNA(idataPtr,nrec * maxSampleDim,
                            stationDim,ncols,NA_INTEGER);
                    break;
                case REALSXP:
                    setToNA(ddataPtr,nrec * maxSampleDim,
                            stationDim,ncols,NA_REAL);
                    break;
                }
            }

            assert(countsMap.size() >= colNum + stationDim);
            if (var) {
                if (countsMap[colNum] == 0) {
                    string cname = var->getCharAttribute("counts");

#undef length
                    if (cname.length() > 0) {
                        string groupName;
                        map<string,string>::iterator vi =
                            countsGroupNameByVariableName.find(vnames[ivar]);
                        assert(vi == countsGroupNameByVariableName.end());

                        vi = countsGroupNameByCountsVariable.find(cname);
                        if (vi == countsGroupNameByCountsVariable.end()) {
                            groupName = vnames[ivar];
                            countsGroupNameByCountsVariable[cname] = groupName;
                            countsColByGroupName[groupName] = ncountsCols;
                            ncountsCols += stationDim;
                            countsGroupNames.push_back(groupName);
                        }
                        else groupName = vi->second;
                        countsGroupNameByVariableName[vnames[ivar]] = groupName;

#ifdef DEBUG
                        Rprintf("vname=%s, groupName=%s, countsMap=%d countsCol=%d\n",
                                vnames[ivar].c_str(),groupName.c_str(),
                                countsMap[colNum],countsColByGroupName[groupName]);
#endif

                        int countsCol = countsColByGroupName[groupName];
                        for (uj = 0; uj < stationDim; uj++)
                            countsMap[colNum + uj] = countsCol + uj + 1;
                    }
                }
                else {
                    string groupName = countsGroupNameByVariableName[vnames[ivar]];
#ifdef DEBUG
                    Rprintf("vname=%s, groupName=%s, countsMap=%d countsCol=%d\n",
                            vnames[ivar].c_str(),groupName.c_str(),
                            countsMap[colNum],countsColByGroupName[groupName]);
#endif
                    assert(countsMap[colNum] == countsColByGroupName[groupName] + 1);
                }
            }

            for (unsigned int istn = 0; istn < stationDim; istn++,colNum++) {
#ifdef DEBUG
                Rprintf("var=%s, colNum=%u\n",vnames[ivar].c_str(),colNum);
#endif
                assert(colNames.size() > colNum);
                if (colNames[colNum].size() == 0)
                    colNames[colNum] = vnames[ivar];

                assert(unitsNames.size() > colNum);
                if (unitsNames[colNum].size() == 0)
                    unitsNames[colNum] = vs.getUnitsName(ivar);

                assert(stationNames.size() > colNum);
                assert(stationNumbers.size() > colNum);

                if (stationNumbers[colNum] < 0) {
                    int stnnum;
                    if (vs.isStationVariable(ivar)) 
                        stnnum = vs.getStationStart() + istn + 1;
                    else stnnum = 0;

                    stationNumbers[colNum] = stnnum;

                    map<int,string>::iterator sitr = filesetStations.find(stnnum);
                    if (sitr != filesetStations.end())
                        stationNames[colNum] = sitr->second;
                    else 
                        stationNames[colNum] = string("");
                }

            }
            idataPtr += stationDim;
            ddataPtr += stationDim;
        }
        assert(colNum == ncols);

        // if (colNum != ncols) Rprintf("Warning: colNum=%d, ncols=%d\n",colNum,ncols);
        nrecs += nrec;
        idataPtr = idataPtr0 + (ncols * nrecs * maxSampleDim);
        ddataPtr = ddataPtr0 + (ncols * nrecs * maxSampleDim);

#ifdef DEBUG
        Rprintf("done with %s\n",ncf->getName().c_str());
#endif
    }

    /*
     * R stores matrices by column, where the first (row) index varies most
     * rapidly. NetCDF variables with an unlimited time dimension are
     * stored by row (time), where the first dimension varies the slowest.
     *
     * If we pre-scanned the files we could read with an imap above that
     * would eliminate the need for this transpose.
     * For now we'll do it this way.
     */
    std::auto_ptr<R_MatrixBase> matrix;

    switch (rmode) {
    case REALSXP:
        {
            assert((ddataPtr - ddataPtr0) == nrecs * ncols * maxSampleDim);
            R_Matrix<double>* matrixt =
                new R_Matrix<double>(rmode,nrecs*maxSampleDim,ncols);
            matrix.reset(matrixt);
            transpose(ddataPtr0,
                    nrecs * maxSampleDim,ncols,matrixt->getDataPtr());
        }
        break;
    case INTSXP:
        {
            assert((idataPtr - idataPtr0) == nrecs * ncols * maxSampleDim);
            R_Matrix<int>* matrixt =
                new R_Matrix<int>(rmode,nrecs*maxSampleDim,ncols);
            matrix.reset(matrixt);
            transpose(idataPtr0,
                    nrecs * maxSampleDim,ncols,matrixt->getDataPtr());
        }
        break;
    default:
        break;
    }

    assert(colNames.size() == ncols);
    assert(unitsNames.size() == ncols);
    assert(stationNames.size() == ncols);
    assert(stationNumbers.size() == ncols);
    assert(countsMap.size() == ncols);

    matrix->setColumnNames(colNames);

    if (!readCountsOnly) {
        assert((timesPtr - &times.front()) == nrecs * maxSampleDim);
        R_utime positions;
#ifdef DEBUG
        Rprintf("setting length on positions to %zd\n",
                nrecs * maxSampleDim);
#endif
        positions.setLength(nrecs * maxSampleDim);

        for (ui = 0; ui < nrecs * maxSampleDim; ui++) {
#ifdef DEBUG
            if (!(ui % 100)) Rprintf("setTime, ui=%zd, t=%f\n",ui,times[ui]);
#endif
            positions.setTime(ui,times[ui]);
        }

        R_nts nts;
        nts.setMatrix(matrix.get());
        nts.setPositions(&positions);

        nts.setStations(stationNames,stationNumbers);
        nts.setUnits(unitsNames);

#ifdef DEBUG
        Rprintf("ncountsCols=%d, countsGroupNames.size=%d\n",
                ncountsCols,countsGroupNames.size());
#endif

        if (ncountsCols > 0) {
            NetcdfReader rdr(_connection); 
            SEXP counts = rdr.read(countsGroupNames,
                    startTime, endTime, stations,timeVarNames, baseTimeName,timezone,true);
            nts.setWeights(counts);
            nts.setWeightMap(countsMap);
        }
        return nts.getRObject();
    }
    return matrix->getRObject();
}
