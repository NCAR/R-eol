// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 *               Copyright (C) by UCAR
 * 
 *  $Revision: 1.2 $
 *  $Date: 2006/03/29 03:28:16 $
 * 
 *  Description:
 *	A NetCDF attribute.
 */
#include <vector>

#include "NcAttrT.h"

using std::string;
using std::vector;

using namespace eolts;

NcAttr* NcAttr::createNcAttr(NcVar* var,int attnum) throw(NcException)
{

    char name[NC_MAX_NAME];
    nc_type nctype;
    size_t len;

    int status;
    status = nc_inq_attname(var->getNcid(),var->getId(),attnum,name);
    if (status != NC_NOERR)  {
        throw NcException("nc_inq_attname",var->getFileName(),
                var->getName(),status);
    }

    status = nc_inq_att(var->getNcid(),var->getId(),name,&nctype,&len);
    if (status != NC_NOERR) {
        throw NcException("nc_inq_att",var->getFileName(),
                var->getName(),status);
    }

    NcAttr *attr = 0;
    switch (nctype) {
    case NC_CHAR:
        {
            char *val = new char[len+1];
            status = nc_get_att_text(var->getNcid(),var->getId(),name,val);
            if (status != NC_NOERR) {
                throw NcException("nc_get_att_text",var->getFileName(),
                        var->getName(),status);
                delete [] val;
                return 0;
            }
            val[len] = '\0';
#ifdef DEBUG
            Rprintf("attribute %s,len=%d,val=%s\n",name,len,val);
#endif
            std::string sval(val);
            delete [] val;
            std::vector<std::string> vval;
            vval.push_back(sval);
            NcAttrT<std::string> *attrt;
            attr = attrt = new NcAttrT<std::string>(name,nctype);
            attrt->setValue(vval);
        }
        break;
    case NC_BYTE:
    case NC_SHORT:
    case NC_INT:
        {
            int *val = new int[len];
            status = nc_get_att_int(var->getNcid(),var->getId(),name,val);
            if (status != NC_NOERR) {
                throw NcException("nc_get_att_int",var->getFileName(),
                        var->getName(),status);
                delete [] val;
                return 0;
            }

            std::vector<int> vval(val,val+len);
            delete [] val;
            NcAttrT<int> *attrt;
            attr = attrt = new NcAttrT<int>(name,nctype);
            attrt->setValue(vval);
        }
        break;
    case NC_FLOAT:
        {
            float *val = new float[len];
            status = nc_get_att_float(var->getNcid(),var->getId(),name,val);
            if (status != NC_NOERR) {
                throw NcException("nc_get_att_float", var->getFileName(),
                        var->getName(),status);
                delete [] val;
                return 0;
            }

            std::vector<float> vval(val,val+len);
            delete [] val;
            NcAttrT<float> *attrt;
            attr = attrt = new NcAttrT<float>(name,nctype);
            attrt->setValue(vval);
        }
        break;
    case NC_DOUBLE:
        {
            double *val = new double[len];
            status = nc_get_att_double(var->getNcid(),var->getId(),name,val);
            if (status != NC_NOERR) {
                throw NcException("nc_get_att_double", var->getFileName(),
                        var->getName(),status);
                delete [] val;
                return 0;
            }

            std::vector<double> vval(val,val+len);
            delete [] val;
            NcAttrT<double> *attrt;
            attr = attrt = new NcAttrT<double>(name,nctype);
            attrt->setValue(vval);
        }
        break;
    default:
        throw NcException(var->getFileName(),
                string("unknown type for attribute ") + name);
    }
    return attr;
}

NcAttr::~NcAttr() {}
