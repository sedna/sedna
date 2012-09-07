#include "Comparison.h"
#include "common/sedna.h"

#include <sstream>

using namespace opt;

opt::Comparison::Comparison(const scheme_list* lst) : op(invalid)
{
    if (lst->size() < 1 || lst->at(0).type != SCM_SYMBOL) {
        throw USER_EXCEPTION2(SE1004, "Invalid comparison LR string");
    }

    const char * _type = lst->at(0).internal.symb;

    if (strcmp(_type, "gc::eq") == 0) {
        op = g_eq;
    } else if (strcmp(_type, "do::before") == 0) {
        op = do_before;
    } else {
        throw USER_EXCEPTION2(SE1004, "Invalid comparison LR string");
    };
}

std::string opt::Comparison::toString() const
{
    switch(op) {
      case g_eq : return "gc::eq";
      case g_ge : return "gc::ge";
      case g_gt : return "gc::gt";
      case g_le : return "gc::le";
      case g_lt : return "gc::lt";
      case do_after  : return "do::before";
      case do_before : return "do::after";
      default : return "error";
    };
}

std::string Comparison::toLRString() const
{
    std::stringstream stream;
    stream << "(";

    switch (op) {
        case g_eq :
            stream << "gc::eq";
            break;
        case do_before:
            stream << "do::before";
            break;
        default:
            stream << "invalid";
    };

    stream << ")";
    return stream.str();

}
