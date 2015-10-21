// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:
/*
 * 2013,2014, Copyright University Corporation for Atmospheric Research
 * 
 * This file is part of the "eolts" package for the R software environment.
 * The license and distribution terms for this file may be found in the
 * file LICENSE in this package.
 */

#ifndef R_NTS_H
#define R_NTS_H

#include "R_Matrix.h"
#include "R_utime.h"

namespace eolts {

class R_nts {

public:

    /**
     * for creating an R_nts object from scratch.
     */
    R_nts();

    /**
     * for creating an R_nts object from an nts R object.
     */
    R_nts(SEXP obj);

    /**
     * Does not free up any allocated R memory of the object.
     */
    ~R_nts();

    SEXP getRObject() { return _obj; }

    /**
     * Set the .Data slot.
     */
    void setMatrix(SEXP val);

    SEXP getMatrix();

    /**
     * Set the positions slot.
     */
    void setPositions(SEXP val);

    SEXP getPositions();

    void setStations(const std::vector<std::string>& names,
        const std::vector<int>& numbers);
    std::vector<std::string> getStationNames() const;
    std::vector<int> getStationNumbers() const;

    void setUnits(const std::vector<std::string>& names);
    std::vector<std::string> getUnits() const;

    void setLongNames(const std::vector<std::string>& names);
    std::vector<std::string> getLongNames() const;

    /**
     * Set the weights slot.
     */
    void setWeights(SEXP val);

    SEXP getWeights();

    void setWeightMap(std::vector<int>);
    std::vector<int> getWeightMap() const;

    void setTimeFormat(const std::string& val);
    std::string getTimeFormat() const;

    void setTimeZone(const std::string& val);
    std::string getTimeZone() const;

    static SEXP classDef;

    static SEXP dataSlotName;
    static SEXP posSlotName;
    static SEXP unitsSlotName;
    static SEXP weightsSlotName;
    static SEXP weightMapSlotName;
    static SEXP stationsSlotName;
    static SEXP longNamesSlotName;
    static SEXP startposSlotName;
    static SEXP endposSlotName;
    static SEXP timeFormatSlotName;
    static SEXP timeZoneSlotName;

private:
    // declared private to prevent copying and assignment
    R_nts(const R_nts &);
    R_nts &operator=(const R_nts &) const;

    SEXP _obj;

    PROTECT_INDEX _pindx;

};

}   // namespace eolts

#endif
