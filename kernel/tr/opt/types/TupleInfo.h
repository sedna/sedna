/*
 * File:  TupleScheme.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _TUPLE_SCHEME_H_
#define _TUPLE_SCHEME_H_

#include <map>
#include <sstream>

#include "tr/opt/OptTypes.h"
#include "tr/structures/xmlns.h"

namespace opt {

struct TupleInfo {
    TupleId id;
    std::string name;

    rqp::RPBase * definedIn;

    DataNodeSet nodes;
    OperationSet operations;

    TupleId pointsTo;

    tuple_info_t properties;

    TupleInfo(TupleId _id)
      : id(_id), definedIn(NULL), pointsTo(opt::invalidTupleId) {};

    std::string toString() const
    {
        std::stringstream s;

        if (name.empty()) {
            s << "#" << id;
        } else {
            s << "$" << name;
        }

        return s.str();
    }
};

typedef std::map<TupleId, TupleInfo> TupleInfoMap;

}


#endif /* _TUPLE_SCHEME_H_ */
