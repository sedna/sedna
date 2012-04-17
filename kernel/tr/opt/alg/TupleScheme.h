/*
 * File:  TupleScheme.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _TUPLE_SCHEME_H_
#define _TUPLE_SCHEME_H_

#include <map>
#include <vector>
#include <set>
#include <string>
#include <sstream>
#include <stack>

#include "tr/opt/OptTypes.h"
#include "tr/structures/xmlns.h"

namespace rqp {
    static const TupleId nullTuple = 0;
    static const TupleId invalidTupleId = -1;
    static const TupleId worldDataTupleId = 1;

    enum operation_result_type_t {
        orTuple, orTupleCell, orTupleList, orTupleCellList
    };

    struct TupleCellType {
        xmlscm_type value_type;
    };

    struct TupleVarDescriptor {
        xmlscm_type t;
        std::string name;
    };

    struct TupleDefinition {
        TupleId tid;
        std::string name;
        TupleCellType t;

        TupleDefinition(TupleId atid, const TupleVarDescriptor * desc) : tid(atid), name(desc->name) { t.value_type = desc->t; };
        TupleDefinition(TupleId atid, const TupleCellType at) : tid(atid), t(at) {};
        TupleDefinition(TupleId atid, const std::string & aname, const TupleCellType at) : tid(atid), name(aname), t(at) {};

        std::string __debugGetVarLabel() const
        {
            std::stringstream s;

            if (name.empty()) {
                s << "#" << tid;
            } else {
                s << "$" << name;
            }

            return s.str();
        }
    };

    typedef std::map<TupleId, TupleDefinition> TupleSchemeMap;
}


#endif /* _TUPLE_SCHEME_H_ */
