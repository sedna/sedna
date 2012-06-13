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
  
enum operation_flags_t {
    ofNone = 0,
    oReturnTuple = 0x001,
    oReturnList = 0x002,
    oBlockBuilder = 0x004,
    oBlockSpecial = 0x008,
};

struct TupleDefinition {
    opt::TupleId tid;
    std::string name;
    xmlscm_type type;

    TupleDefinition(opt::TupleId atid, const std::string & aname) : tid(atid), name(aname), type(xs_anyType) {};
    TupleDefinition(opt::TupleId atid, const xmlscm_type at) : tid(atid), type(at) {};
    TupleDefinition(opt::TupleId atid, const std::string & aname, const xmlscm_type at) : tid(atid), name(aname), type(at) {};

    std::string getVarLabel() const
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

typedef std::map<opt::TupleId, TupleDefinition> TupleSchemeMap;

}


#endif /* _TUPLE_SCHEME_H_ */
