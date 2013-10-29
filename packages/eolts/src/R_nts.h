#ifndef R_NTS_H
#define R_NTS_H

#include "R_Matrix.h"
#include "R_utime.h"

namespace eolts {

class R_nts {

protected:

    SEXP _obj;

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

    void setMatrix(R_MatrixBase *val);
    R_Matrix<double> *getRealMatrix();
    R_Matrix<int> *getIntMatrix();

    void setPositions(R_utime *val);
    R_utime *getPositions();

    void setStations(const std::vector<std::string>& names,
        const std::vector<int>& numbers);
    std::vector<std::string> getStationNames() const;
    std::vector<int> getStationNumbers() const;

    void setUnits(const std::vector<std::string>& names);
    std::vector<std::string> getUnits() const;

    void setWeights(SEXP);
    R_Matrix<int> *getWeights();

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
    static SEXP startposSlotName;
    static SEXP endposSlotName;
    static SEXP timeFormatSlotName;
    static SEXP timeZoneSlotName;

private:
    // declared private to prevent copying and assignment
    R_nts(const R_nts &);
    R_nts &operator=(const R_nts &) const;

    int _pindx;

};

}   // namespace eolts

#endif
