// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.4 $
 *  $Date: 2009/04/01 01:35:53 $
 * 
 *  Description:
 *    A class that allows creating an instance of an Splus NetcdfVariable
 *    object.
 * 
 */

// #include <string.h>

#include <sstream>
#include <cassert>

#include "R_NetcdfVariable.h"
#include "NetcdfReader.h"
#include "R_NamedVector.h"
#include "NcAttrT.h"
#include "util.h"

using std::vector;
using std::string;

using namespace eolts;

SEXP R_NetcdfVariable::classDef;
SEXP R_NetcdfVariable::nameSlotName;
SEXP R_NetcdfVariable::modeSlotName;
SEXP R_NetcdfVariable::nctypeSlotName;
SEXP R_NetcdfVariable::dimensionSlotName;
SEXP R_NetcdfVariable::attributeSlotName;

R_NetcdfVariable::R_NetcdfVariable(R_netcdf *con,NcVar *var):
    _obj(0),_name(var->getName()),_connection(con),_type("unknown")
{
    if (!classDef) classDef = R_do_MAKE_CLASS("netcdfVariable");
    _obj = PROTECT(R_do_new_object(classDef));

    SEXP cobj = PROTECT(allocVector(STRSXP,1));
    SET_STRING_ELT(cobj,0,mkChar(_name.c_str()));
    setAttrib(_obj,nameSlotName,cobj);
    UNPROTECT(1);

    int rmode = NetcdfReader::rMode(var->getNcType());
    if (rmode == NILSXP) {
        std::ostringstream ost;
        ost << "NetcdfReader::rMode, unsupported nctype " <<
            var->typeToString() <<" for variable " << var->getName();
        error(ost.str().c_str());
    }
    setMode(rModeToString(rmode));
    setNcType(var->typeToString());

    vector<const NcDim *> dims = var->getDimensions();
    if (var->getNcType() == NC_CHAR) dims.erase(dims.end()-1);
    setDimensions(dims);

    setAttributes(var->getAttributes());

#ifdef DEBUG
    SEXP tmp = getAttrib(_obj,nameSlotName);
    Rprintf("R_NetcdfVariable ctor, name slot=%s\n",
            CHAR(STRING_ELT(tmp,0)));
#endif
}

R_NetcdfVariable::~R_NetcdfVariable() {
#ifdef DEBUG
    Rprintf("~R_NetcdfVariable\n");
#endif
    if (_obj) UNPROTECT(1);
}

void R_NetcdfVariable::setMode(const string& mode)
{
    SEXP cobj = PROTECT(allocVector(STRSXP,1));
    SET_STRING_ELT(cobj,0,mkChar(mode.c_str()));
    setAttrib(_obj,R_NetcdfVariable::modeSlotName,cobj);
    UNPROTECT(1);
}

void R_NetcdfVariable::setNcType(const string& type)
{
    SEXP cobj = PROTECT(allocVector(STRSXP,1));
    SET_STRING_ELT(cobj,0,mkChar(type.c_str()));
    setAttrib(_obj,R_NetcdfVariable::nctypeSlotName,cobj);
    UNPROTECT(1);
}

void R_NetcdfVariable::setDimensions(const vector<const NcDim*>& dims)
{
    if (dims.size() > 0) {
        R_NamedVector<int> rdims(INTSXP,dims.size());
        int *vals = rdims.getDataPtr();
        vector<string> dimnames;
        for (unsigned int i = 0; i < dims.size(); i++) {
            vals[i] = (signed) dims[i]->getLength();
            dimnames.push_back(dims[i]->getName());
        }
        rdims.setNames(dimnames);
        setAttrib(_obj,R_NetcdfVariable::dimensionSlotName,rdims.getRObject());
    }
    else {
        SEXP dobj = PROTECT(allocVector(INTSXP,0));
        setAttrib(_obj,R_NetcdfVariable::dimensionSlotName,dobj);
        UNPROTECT(1);
    }
}

void R_NetcdfVariable::setAttributes(const vector<const NcAttr*>& attrs)
{
    SEXP dobj = PROTECT(allocVector(VECSXP,attrs.size()));

    // no attributes, create a list of length 0
    if (attrs.size() == 0) {
        setAttrib(_obj,R_NetcdfVariable::attributeSlotName,dobj);
        UNPROTECT(1);   // dobj
        return;
    }

    SEXP names = getAttrib(dobj,R_NamesSymbol);
    if (!names || !isString(names) || length(names) != length(dobj)) {
        names = allocVector(STRSXP,attrs.size());
        setAttrib(dobj,R_NamesSymbol,names);
    }

    for (unsigned int i = 0; i < attrs.size(); i++) {
        SET_STRING_ELT(names,i,mkChar(attrs[i]->getName().c_str()));

        switch (attrs[i]->getNcType()) {
        case NC_CHAR:
            {
                const NcAttrT<string>* attrt = dynamic_cast<const NcAttrT<string>*>(attrs[i]); // the ugly cast
                assert(attrt);
                SEXP sobj = PROTECT(allocVector(STRSXP,attrt->getLength()));

#ifdef DEBUG
                Rprintf("char attr=%s,length=%d,val=%s\n",
                        attrt->getName().c_str(),attrt->getLength(),attrt->getValue(0).c_str());
#endif
                for (unsigned int j = 0; j < attrt->getLength(); j++) {
                    string str = attrt->getValue(j);
                    SET_STRING_ELT(sobj,j,mkChar(str.c_str()));
                }
                SET_VECTOR_ELT(dobj,i,sobj);
                UNPROTECT(1);   // sobj
            }
            break;
        case NC_DOUBLE:
            {
                const NcAttrT<double>* attrt = dynamic_cast<const NcAttrT<double>*>(attrs[i]); // the ugly cast
                assert(attrt);
                SEXP sobj = PROTECT(allocVector(REALSXP,attrt->getLength()));

#ifdef DEBUG
                Rprintf("double attr=%s,length=%d,val=%g\n",
                        attrt->getName().c_str(),attrt->getLength(),attrt->getValue(0));
#endif

                for (unsigned int j = 0; j < attrt->getLength(); j++)
                    REAL(sobj)[j] = attrt->getValue(j);
                SET_VECTOR_ELT(dobj,i,sobj);
                UNPROTECT(1);   // sobj
            }
            break;
        case NC_FLOAT:
            {
                const NcAttrT<float>* attrt = dynamic_cast<const NcAttrT<float>*>(attrs[i]); // the ugly cast
                assert(attrt);
                SEXP sobj = PROTECT(allocVector(REALSXP,attrt->getLength()));

#ifdef DEBUG
                Rprintf("float attr=%s,length=%d,val=%g\n",
                        attrt->getName().c_str(),attrt->getLength(),attrt->getValue(0));
#endif

                for (unsigned int j = 0; j < attrt->getLength(); j++)
                    REAL(sobj)[j] = attrt->getValue(j);
                SET_VECTOR_ELT(dobj,i,sobj);
                UNPROTECT(1);   // sobj
            }
            break;
        case NC_BYTE:
        case NC_SHORT:
        case NC_INT:
            {
                const NcAttrT<int>* attrt = dynamic_cast<const NcAttrT<int>*>(attrs[i]); // the ugly cast
                assert(attrt);
                SEXP sobj = PROTECT(allocVector(INTSXP,attrt->getLength()));

#ifdef DEBUG
                Rprintf("int attr=%s,length=%d,val=%d\n",
                        attrt->getName().c_str(),attrt->getLength(),attrt->getValue(0));
#endif
                for (unsigned int j = 0; j < attrt->getLength(); j++)
                    INTEGER(sobj)[j] = attrt->getValue(j);
                SET_VECTOR_ELT(dobj,i,sobj);
                UNPROTECT(1);   // sobj
            }
            break;
        default:
            break;
        }
    }
    setAttrib(_obj,attributeSlotName,dobj);
    UNPROTECT(1);   // dobj
}
