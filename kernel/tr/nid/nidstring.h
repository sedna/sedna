#ifndef _NID_STRING_H_
#define _NID_STRING_H_

#include "tr/nid/numb_scheme.h"
#include "tr/nid/nidalloc.h"

#include <string>

class NidString : private std::basic_string<unsigned char> {
public:
    NidString(xptr node) {
        t_prefix prefix = nid_get_prefix(nid_get_nid(node));
        this->assign(prefix.prefix, prefix.size);
        nid_free(prefix.prefix);
    };

    int compare(const NidString & b) const {
        const NidString & a = *this;

        int result = memcmp(a.data(), b.data(), std::min(a.size(), b.size()));

        if (result != 0) {
            return sign(result);
        } else {
            if (a.size() == b.size()) {
                return 0;
            } else if (a.size() > b.size()) {
                return (a[b.size()] == ALPHABET_SIZE) ? 1 : 2;
            } else {
                return (b[a.size()] == ALPHABET_SIZE) ? -1 : -2;
            }
        }
    };
};

#endif /* _NID_STRING_H_ */
