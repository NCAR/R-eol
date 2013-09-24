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

#include <sstream>
#include <memory>   // auto_ptr

#include <string.h>
#include <assert.h>

#include "NetcdfReader.h"
#include "NcVar.h"
#include "NcFile.h"
#include "NcAttr.h"
#include "util.h"	// for dimToString

// #include <algorithm>

using std::vector;
using std::string;

NetcdfReader::NetcdfReader(R_NetcdfConnection* conn):
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

/* 
 * Read data from netcdf variables into an array.
 * Netcdf data is stored "by row", i.e. the last indice varies
 * most rapidly. R data is stored by column, first indice varies
 * most rapidly.
 */
SEXP NetcdfReader::read(const vector<string> & vnames,
        const vector<size_t>& startarg, const vector<size_t>&countarg)
    NCEXCEPTION_CLAUSE
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
                PROBLEM "variable \"%s\" not found in file %s",
                        vnames[ivar].c_str(),ncf->getName().c_str()
                            WARNING(NULL_ENTRY);
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
#ifdef THROW_NCEXCEPTION
                    throw NcException("nc_get_vara_double",
                            var->getFileName(),var->getName(),status);
#else
                    std::ostringstream ost;
                    ost<<  "Error in file " << var->getFileName() <<
                        " variable " << var->getName() << ": nc_get_vara_double: " <<
                            ::nc_strerror(status);
                    error(ost.str().c_str());
#endif
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
#ifdef THROW_NCEXCEPTION
                    throw NcException("nc_get_vara_int",
                            var->getFileName(),var->getName(),status);
#else
                    std::ostringstream ost;
                    ost<<  "Error in file " << var->getFileName() <<
                        " variable " << var->getName() << ": nc_get_vara_int: " <<
                            ::nc_strerror(status);
                    error(ost.str().c_str());
#endif
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
#ifdef THROW_NCEXCEPTION
                            throw NcException("nc_get_vara_text",
                                    var->getFileName(),var->getName(),status);
#else
                            std::ostringstream ost;
                            ost<<  "Error in file " << var->getFileName() <<
                                " variable " << var->getName() << ": nc_get_vara_text: " <<
                                    ::nc_strerror(status);
                            error(ost.str().c_str());
#endif
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

string NetcdfReader::rModeToString(int rmode) {
    switch (rmode) {
    case INTSXP:
        return "integer";
    case REALSXP:
        return "double";
    case STRSXP:
        return "character";
    default:
        return "unknown";
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

int NetcdfReader::sizeOfRMode(int mode)
{
    switch(mode) {
    case INTSXP:
        return sizeof(int);
    case REALSXP:
        return sizeof(double);
    case STRSXP:
        return sizeof(char *);
    default:
        PROBLEM "NetcdfReader::sizeOfRMode, programmer's error, unknown type"
            RECOVER(NULL_ENTRY);
        return 0;
    }
}
#ifdef IF_NEEDED
void NetcdfReader::setToNA(char *dp,size_t c0,size_t c1,ptrdiff_t m0, int rmode) {
    unsigned int i,j;
    int size = sizeOfRMode(rmode);

    char *dp1;
    for (i = 0; i < c0; i++) { 
        dp1 = dp;
        for (j = 0; j < c1; j++) { 
            na_set(dp1,rmode);
            dp1 += size;
        }
        dp += (m0 * size);
    }
}     

void NetcdfReader::setTo0(char *dp,size_t c0,size_t c1,ptrdiff_t m0, int rmode) {
    unsigned int i,j;
    int size = sizeOfRMode(rmode);

    char *dp1;
    for (i = 0; i < c0; i++) { 
        dp1 = dp;
        for (j = 0; j < c1; j++) { 
            memset(dp1,0,size);
            dp1 += size;
        }
        dp += (m0 * size);
    }
}     

#endif
