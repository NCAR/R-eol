#include "NcAttrT.h"

template<>
double NcAttrT<double>::getNumericValue(int i) const
{
    return getValue(i);
}

template<>
double NcAttrT<float>::getNumericValue(int i) const
{
    return getValue(i);
}

template<>
double NcAttrT<int>::getNumericValue(int i) const
{
    return (double) getValue(i);
}

template<>
double NcAttrT<std::string>::getNumericValue(int i) const
{
    return 0.0;
}
