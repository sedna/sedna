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
  
static const opt::TupleId nullTuple = 0;
static const opt::TupleId invalidTupleId = -1;
static const opt::TupleId worldDataTupleId = 1;

enum operation_result_type_t {
    orTuple, orTupleCell, orTupleList, orTupleCellList
};

struct TupleVarDescriptor : opt::IPlanDisposable {
    xmlscm_type t;
    std::string name;
};

struct TupleDefinition {
    opt::TupleId tid;
    std::string name;
    xmlscm_type type;

    TupleDefinition(opt::TupleId atid, const TupleVarDescriptor * desc) : tid(atid), name(desc->name) { type = desc->t; };
    TupleDefinition(opt::TupleId atid, const xmlscm_type at) : tid(atid), type(at) {};
    TupleDefinition(opt::TupleId atid, const std::string & aname, const xmlscm_type at) : tid(atid), name(aname), type(at) {};

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

typedef std::map<opt::TupleId, TupleDefinition> TupleSchemeMap;

}


#endif /* _TUPLE_SCHEME_H_ */
