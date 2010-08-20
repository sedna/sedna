/*
 * File:  textnodes.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef TEXTNODES_H_
#define TEXTNODES_H_

#include "tr/structures/nodetypes.h"
#include "tr/structures/nodeoperations.h"
#include "tr/structures/nodeinterface.h"

class TextNodeHelper {
private:
    xptr node;
public:
    TextNodeHelper(const xptr p) : node(p) {};
    TextNodeHelper(const Node &n) : node(n.getPtr()) {};

    inline
    xptr getPtr() const { return node; };

    int comparePrefix(const std::string &s, size_t len);
    int comparePrefix(const char * s, size_t len);

    strsize_t getLength();
    size_t getLengthShort();

    inline
    std::string getString() const {
        const CommonTextNode ctn(node);
        U_ASSERT(!ctn.isPstrLong());
        const size_t size = (size_t) ctn.getTextSize();
        const xptr text = ctn.getTextPointerCP();
        return std::string((const char *) XADDR(text), size);
    }

    /* Allocates and returns string for using in tuple */
    char * getStringSe();

//    bool isPI();

    /* PI functions */
    inline
    std::string getPITarget() const {
        const size_t size = PINode(node).getPITargetSize();
        const xptr text = CommonTextNode(node).getTextPointerCP();
        return std::string((const char *) XADDR(text), size);
    }
};

#endif /* TEXTNODES_H_ */
