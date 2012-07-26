#ifndef _COMPARISON_H_
#define _COMPARISON_H_

#include <string>
#include "tr/executor/por2qep/scheme_tree.h"

class CollationHandler;

extern const int reverseComparisonMap[];

namespace opt {

struct Comparison {
    enum comp_t {
        invalid = 0,
        g_eq = 0x01, g_gt = 0x02, g_ge = 0x03, g_lt = 0x04, g_le = 0x05,
        do_before = 0x11, do_after = 0x12,
    } op;

    CollationHandler * collation;

    Comparison() : op(invalid), collation(NULL) {};

    explicit Comparison(enum comp_t _op, CollationHandler * _collation)
      : op(_op), collation(_collation) {};

    Comparison(const Comparison & x)
      : op(x.op), collation(x.collation) {};

    Comparison(const scheme_list * lst);

    bool inversable() const { return op == g_eq; };

    Comparison inverse() const {
        if (op == g_eq) {
            return *this;
        };

        return Comparison();
    };

    std::string toString() const;
    std::string toLRString() const;
};

};

#endif /* _COMPARISON_H_ */
