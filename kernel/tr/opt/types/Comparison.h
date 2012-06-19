#ifndef _COMPARISON_H_
#define _COMPARISON_H_

#include <string>
#include "tr/executor/por2qep/scheme_tree.h"

extern const int reverseComparisonMap[];

namespace opt {

struct Comparison {
    enum comp_t {
        invalid = 0,
        g_eq = 0x01, g_gt = 0x02, g_ge = 0x03, g_lt = 0x04, g_le = 0x05,
        do_before = 0x11, do_after = 0x12,
    } op;

    Comparison() : op(invalid) {};
    explicit Comparison(enum comp_t _op) : op(_op) {};
    Comparison(const Comparison & x) : op(x.op) {};
    Comparison(const scheme_list * lst);

    bool inversable() const { return op == g_eq; };

    Comparison inverse() const {
        if (op == g_eq) {
            return *this;
        };

        return Comparison(invalid);
    };

    std::string toString() const;
    std::string toLRString() const;
};

};

#endif /* _COMPARISON_H_ */
