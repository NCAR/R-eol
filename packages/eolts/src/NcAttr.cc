// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 * 2013,2014, Copyright University Corporation for Atmospheric Research
 * 
 * This file is part of the "eolts" package for the R software environment.
 * The license and distribution terms for this file may be found in the
 * file LICENSE in this package.
 */
/*
 *  Description:
 *	A NetCDF attribute.
 */
#include <vector>

#include "NcAttrT.h"

using std::string;
using std::vector;

using namespace eolts;

NcAttr* NcAttr::readNcAttr(NcVar* var,int attnum)
{
    return readNcAttr(var->getNcid(),var->getFileName(),
            var->getId(),var->getName(),attnum);
}

NcAttr* NcAttr::readNcAttr(NcFile* file,int attnum)
{
    return readNcAttr(file->getNcid(),file->getName(), NC_GLOBAL,"NC_GLOBAL",attnum);
}

NcAttr* NcAttr::readNcAttr(int ncid, const string& filename,
        int varid, const string& varname, int attnum)
{

    char name[NC_MAX_NAME];
    nc_type nctype;
    size_t len;

    int status;
    status = nc_inq_attname(ncid,varid,attnum,name);
    if (status != NC_NOERR)  {
        throw NcException("nc_inq_attname",filename,
                varname,status);
    }

    status = nc_inq_att(ncid,varid,name,&nctype,&len);
    if (status != NC_NOERR) {
        throw NcException("nc_inq_att",filename,
                varname,status);
    }

    NcAttr *attr = 0;
    switch (nctype) {
    case NC_CHAR:
        {
            char *val = new char[len+1];
            status = nc_get_att_text(ncid,varid,name,val);
            if (status != NC_NOERR) {
                throw NcException("nc_get_att_text",filename,
                        varname,status);
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
            status = nc_get_att_int(ncid,varid,name,val);
            if (status != NC_NOERR) {
                throw NcException("nc_get_att_int",filename,
                        varname,status);
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
            status = nc_get_att_float(ncid,varid,name,val);
            if (status != NC_NOERR) {
                throw NcException("nc_get_att_float", filename,
                        varname,status);
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
            status = nc_get_att_double(ncid,varid,name,val);
            if (status != NC_NOERR) {
                throw NcException("nc_get_att_double", filename,
                        varname,status);
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
        throw NcException(filename,
                string("unknown type for attribute ") + name);
    }
    return attr;
}

NcAttr::~NcAttr() {}
