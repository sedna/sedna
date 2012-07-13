#ifndef _CPP_CAST_H_
#define _CPP_CAST_H_

#include <sstream>

template <typename Source>
inline static
std::string cast_to_string(const Source & x) {
    std::stringstream stream;
    stream << x;
    return stream.str();
}

/*
template <>
inline static
std::string cast_to_string<std::string>(const std::string & x) {
    return x;
}
*/

template <typename Target>
class cast_exception : public std::exception { };

template <typename Target>
inline static
Target cast_string(const std::string & s) {
    Target result;

    if (!(std::istringstream(s) >> result)) {
        throw cast_exception<Target>();
    };

    return result;
}

#endif /* _CPP_CAST_H_ */

