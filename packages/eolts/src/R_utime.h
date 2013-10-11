#ifndef R_TIME_DATE_H
#define R_TIME_DATE_H

/* In Splus 6.0 Release 1 (using Solaris SUNWspro CC) we get a warning
 * if we don't put S.h within extern "C":
"/net/csoft_sol/splus6/include/S_extern.h", line 292: Warning (Anachronism): S_newio_MemCheckFile was declared before with a different language.
*/

#include <R.h>
#include <Rinternals.h>

#include <string>

class R_utime {
protected:

  SEXP _obj;
  SEXP _data;
  size_t _length;
  int _pindx;

public:
  R_utime();
  R_utime(SEXP);
  ~R_utime();

  SEXP getRObject() { return _obj; }

  void setTime(size_t index,double t);
  double getTime(size_t index);

  void setLength(size_t nr);
  size_t getLength();

  void setFormat(const std::string& val);
  std::string getFormat(void);

  void setTimeZone(const std::string& val);
  std::string getTimeZone(void);

  static SEXP classDef;
  static SEXP dotDataSlotName;
  static SEXP formatSlotName;
  static SEXP timezoneSlotName;

private:
  // declared private to prevent copying and assignment
  R_utime(const R_utime &);
  R_utime &operator=(const R_utime &);
};

#endif
