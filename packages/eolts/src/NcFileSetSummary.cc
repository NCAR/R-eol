// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:

#include <sstream>

#include <vector>
#include <set>
#include <string>
#include <limits>

#include "NcFileSetSummary.h"
#include "NcFile.h"
#include "util.h"

#include <R.h>  // warning

// #define DEBUG

using std::vector;
using std::set;
using std::string;

using namespace eolts;

// #define DEBUG

NcFileSetSummary::NcFileSetSummary(NcFileSet *fs,
        const vector<string> &varnames,const vector<int> &stations,
        bool getCounts) throw(NcException) :
    _fileset(fs), _vnames(varnames),_nvars(varnames.size()),
    _firstFile(-1),
    _outType(NC_NAT),   // not a type
    _dims(vector<vector<size_t> >(_nvars)),
    _unitsNames(vector<string>(_nvars)),
    _firstDims(),
    _sampleDims(vector<size_t>(_nvars)),
    _stationDims(vector<size_t>(_nvars)),
    _stationVar(vector<bool>(_nvars)),
    _nfoundvars(0),_firstVar(-1),_maxSampleDim(1),
    _ncolOut(0),_stationStart(0),_stationCount(0)
{
    int nfiles = _fileset->getNFiles();

    int ifile;

    /*
     * determine start and count for station dimension.
     * If the stations vector is empty, then user
     * wants data from all stations.
     *
     * onlyStationVarsRequested==true means the user only
     * wants data from variables with a station dimension.
     */
    bool onlyStationVarsRequested = !stations.empty();
    int minStation = std::numeric_limits<int>::max();
    int maxStation = 0;

    // convert to C numbering: -1 is the non-station, 0 is the first station number
    for (unsigned int i = 0; i < stations.size(); i++) {
        int s = stations[i] - 1;
        if (s < 0) onlyStationVarsRequested = false;
        else {
            if (s < minStation) minStation = s;
            if (s > maxStation) maxStation = s;
        }
    }

    _stationStart = minStation;
    if (maxStation >= minStation) _stationCount = maxStation - minStation + 1;

    // If stations.empty() or only non-station asked for,
    // set _stationStart to other than std::numeric_limits<int>::max()
    if (_stationCount == 0) _stationStart = 0;
    
#ifdef DEBUG
    Rprintf("_stationCount=%d\n",_stationCount);
#endif

    /*
     * scan over dimensions of all requested variables.
     * Time dimension must be first, followed by optional sample
     * dimension, followed by optional station dimension.
     */

    vector<bool> varChecked(_nvars);
    std::fill(varChecked.begin(),varChecked.end(),false);

    const set<string>& timeDimensionNames = _fileset->getTimeDimensionNames();

    for (ifile = 0; ifile < nfiles; ifile++) {
        NcFile* ncf = (NcFile*) _fileset->getNcFile(ifile);
        if (ncf == 0 || !ncf->isOpen()) continue;
#ifdef DEBUG
        Rprintf("NcFileSetSummary file=%s\n",
                ncf->getName().c_str());
#endif

        const NcDim* timeDimension = ncf->getTimeDimension(timeDimensionNames);
#ifdef DEBUG
        Rprintf("NcFileSetSummary file=%s, timDim=%d\n",
                ncf->getName().c_str(),timeDimension->getLength());
#endif
        if (!timeDimension) continue;

        int sampleid = -1;
        int stationid = -1;

        for (int ivar = 0; ivar < _nvars; ivar++) {

#ifdef DEBUG
            Rprintf("NcFileSetSummary ctor ifile=%d,ivar=%d %s %s, getCounts=%d\n",
                    ifile,ivar,ncf->getName().c_str(),_vnames[ivar].c_str(),getCounts);
#endif
            NcVar* var = 0;
            try {
                if (getCounts)
                    var = ncf->getTimeSeriesCountsVariable(_vnames[ivar],timeDimension);
                else
                    var = ncf->getTimeSeriesVariable(_vnames[ivar],timeDimension);
                if (!var) {
                    std::ostringstream ost;
                    ost << "variable \"" << _vnames[ivar] <<
                        "\" is not a time series variable in in file " << ncf->getName();
                    Rprintf("%s\n",ost.str().c_str());
                    continue;
                }
            }
            catch(const NcException& nce) {
                std::ostringstream ost;
                ost << "time series variable \"" << _vnames[ivar] <<
                    "\" not found in file " << ncf->getName();
                // Rf_warning(ost.str().c_str());
                Rprintf("%s\n",ost.str().c_str());
                continue;
            }

#ifdef DEBUG
            Rprintf("NcFileSetSummary ctor ts var ifile=%d,ivar=%d %s\n",
                    ifile,ivar,_vnames[ivar].c_str());
#endif
            int ndim = var->getNumDimensions();
            const size_t* vedges = var->getEdges();

            if (!varChecked[ivar]) {	// haven't encountered this var
                if (_firstVar < 0) _firstVar = ivar;	// first variable found
                if (_firstFile < 0) _firstFile = ifile;
                checkType(var);
                _dims[ivar] = vector<size_t>(vedges,vedges+ndim);
                _sampleDims[ivar] = 1;
                _stationDims[ivar] = 1;
                _stationVar[ivar] = false;
            }
            else {
                if (_dims[ivar].size() == 0) continue;	// we've excluded this variable
                bool ok = ndim == (signed)_dims[ivar].size();
                for (int i = 1; ok && i < ndim; i++)
                    ok = vedges[i] == _dims[ivar][i];

                if (!ok) {
                    std::ostringstream ost;
                    ost << "error in file " << ncf->getName() <<
                        ", dimensions of variable \"" << var->getName() <<
                        "\"(" << dimToString(vedges,ndim) <<
                        ") are different than dimensions in previous file: " <<
                        dimToString(_dims[ivar]) << " Skipping variable";
                    Rf_warning(ost.str().c_str());
                    _dims[ivar].resize(0);
                }
            }
            if (_dims[ivar].size() > 0 && _unitsNames[ivar].size() == 0)
                _unitsNames[ivar] = var->getUnits();

            // We've checked this variable against its dimensions in
            // a previous file, so we're done.
            if (varChecked[ivar]) continue;
            varChecked[ivar] = true;

            const NcDim* sampleDim = 0;
            for (int i = 1; i < ndim; i++) {
                const NcDim* dim = var->getDimension(i);
                // Rprintf("dim name=%s\n",dim->getName().c_str());
                if (dim->getName().substr(0,6) == "sample")
                    sampleDim = dim;
                else if (dim->getName().substr(0,3) == "sps")
                    sampleDim = dim;
            }

            if (sampleDim) {
                sampleid = sampleDim->getId();
                const NcDim* dim1 = var->getDimension(1);
                if (!dim1 || sampleDim->getId() != dim1->getId()) {
                    std::ostringstream ost;
                    ost << "error in file " << ncf->getName() <<
                        ", variable \"" << var->getName() << "\": sample dimension with length " <<
                        sampleDim->getLength() <<
                        " is not the second dimension. I don't know what to do! Skipping variable";
                    Rf_warning(ost.str().c_str());
                    _dims[ivar].resize(0);
                    continue;
                }
                _sampleDims[ivar] = sampleDim->getLength();
                if (_sampleDims[ivar] > _maxSampleDim) _maxSampleDim = _sampleDims[ivar];
            }

            const NcDim* stationDim = var->getDimension("station");
            if (stationDim) {
                if (stations.empty() && _stationCount == 0)
                    _stationCount = stationDim->getLength();

                stationid = stationDim->getId();
                if (_stationCount > 0) {
                    if (stationDim->getLength() < _stationStart + _stationCount) {
                        std::ostringstream ost;
                        ost << "error in file " << ncf->getName() <<
                            ", variable \"" << var->getName() << "\": station dimension with length " <<
                        stationDim->getLength() <<
                        " is less than requested station=%d. Skipping variable";
                        Rf_warning(ost.str().c_str());
                        _dims[ivar].resize(0);
                        continue;
                    }
#ifdef DEBUG
                    Rprintf("var=%s, ivar=%d,_stationCount=%u,_stationDims=%u,_ncolOut=%d\n",
                            var->getName().c_str(),ivar,_stationCount,_stationDims[ivar],_ncolOut);
#endif
                }
                _stationVar[ivar] = true;
                _stationDims[ivar] = _stationCount;
            }
            else {
                // This variable doesn't have a station dimension, and user
                // asked only for vars with station dim.
                if (onlyStationVarsRequested) _stationDims[ivar] = 0;
            }

            // check other dimensions
            for (int i = 1; i < ndim; i++) {
                const NcDim* dim = var->getDimension(i);
                int id = dim->getId();
                if (id != stationid && id != sampleid) {
                    std::ostringstream ost;
                    ost << "error in file " << ncf->getName() <<
                        ", variable \"" << var->getName() << "\": don't know how to handle dimension \"" <<
                        dim->getName() << " of length " << dim->getLength() <<
                        ". Skipping variable";

                    Rf_warning(ost.str().c_str());
                    _dims[ivar].resize(0);
                }
            }
            if (_dims[ivar].size() == 0) continue;
            _ncolOut += _stationDims[ivar];
            _nfoundvars++;
        }
        if (_nfoundvars == _nvars) break;
    }
    resolveOutType();
}

NcFileSetSummary::~NcFileSetSummary()
{
}

void NcFileSetSummary::checkType(NcVar*var)
{

    nc_type type = var->getNcType();
    /*
     * decide on best output type if we will try to merge
     * all data into a matrix.
     * S supports these types: NC_CHAR, NC_DOUBLE, NC_FLOAT, NC_INT
     *
     * Rules:
     *    one or more NC_CHAR and one or more numeric: must output a list
     *	rather than a matrix
     *    otherwise one or more NC_DOUBLE: output matrix is double
     *    otherwise if one or more NC_FLOAT:
     *	and one or more longs, output matrix is double
     *	otherwise output matrix is float (this may promote shorts
     *		and bytes to floats)
     *    otherwise output matrix is longs
     */

    /*
     * If any input column is character, then they all must be character.
     */
    if (_outType != NC_NAT && (_outType == NC_CHAR || type == NC_CHAR) && _outType != type) {
        Rf_error("error: cannot mix character and numeric variables");
    }

    switch (type) {
    case NC_CHAR:
    case NC_DOUBLE:
        _outType = type;
        break;
    case NC_FLOAT:
        _outType = NC_DOUBLE;
        break;
    case NC_INT:
        switch (_outType) {
        case NC_NAT:
            _outType = type;
            break;
        case NC_CHAR:	// caught above
        case NC_DOUBLE:	// leave as double
        case NC_FLOAT:	// convert to double
            _outType = NC_DOUBLE;
            break;
        case NC_INT:	// leave as int
            break;
        case NC_SHORT:	// convert to int
        case NC_BYTE:	// convert to int
            _outType = NC_INT;
            break;
        }
        break;
    case NC_SHORT:
    case NC_BYTE:
        /*
         * If _outType hasn't been set, set it to NC_SHORT or NC_BYTE.
         * After passing through all variables, and _outType is
         * NC_SHORT or NC_BYTE, set it to NC_INT.
         */
        if (_outType == NC_NAT) _outType = type;
        break;
    case NC_NAT:
        {
            std::ostringstream ost;
            ost << "unknown type for variable \"" << var->getName() <<
                "\" in file " << var->getFileName();
            Rf_error(ost.str().c_str());
        }
        break;
    }

#ifdef DEBUG
    Rprintf("var=%s type=%s _outType=%s\n",
            var->getName().c_str(),var->getNcTypeString().c_str(),NcVar::typeToString(_outType).c_str());
#endif
}

void NcFileSetSummary::resolveOutType() {

    if (_outType == NC_NAT) {
        std::ostringstream ost;
        ost << "error: no requested variables found";
        Rf_error(ost.str().c_str());
    }

    if (_outType == NC_SHORT || _outType == NC_BYTE) _outType = NC_INT;
}

