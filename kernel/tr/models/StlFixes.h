#ifndef _STL_FIXES_
#define _STL_FIXES_

#include <stdint.h>

#include <set>

namespace std {

template<typename Set>
typename Set::iterator set_erase(Set & s, typename Set::iterator & it)
{
    typename Set::iterator result = it;
    ++result;
    s.erase(it);
    return result;
};

template<typename Set1, typename Set2>
static
size_t intersection_size(const Set1 &set1, const Set2 &set2)
{
    if(set1.empty() || set2.empty()) return 0;

    typename Set1::const_iterator it1 = set1.begin(), it1End = set1.end();
    typename Set2::const_iterator it2 = set2.begin(), it2End = set2.end();

    if (*it1 > *set2.rbegin() || *it2 > *set1.rbegin()) return 0;

    size_t result = 0;

    while(it1 != it1End && it2 != it2End)
    {
        if (*it1 == *it2) {
            result++;
            it1++;
            it2++;
        }

        if (*it1 < *it2) {
            it1++;
        } else {
            it2++;
        }
    }

    return result;
}

};

#endif /* _STL_FIXES_ */
