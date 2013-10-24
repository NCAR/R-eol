#ifndef R_UTIME_H
#define R_UTIME_H

#include <string>

#include <R.h>
#include <Rinternals.h>

namespace eolts {

class R_utime {
protected:

    SEXP _obj;
    size_t _length;
    int _pindx;

public:
    R_utime();

    R_utime(SEXP);

    ~R_utime();

    SEXP getRObject() { return _obj; }

    void setTime(size_t index,double t);
    double getTime(size_t index);

    double* getTimePtr();

    void setLength(size_t nr);
    size_t getLength();

    static SEXP classDef;
    static SEXP dotDataSlotName;

private:
    // declared private to prevent copying and assignment
    R_utime(const R_utime &);
    R_utime &operator=(const R_utime &);
};

}   // namespace eolts

#endif
