/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef XPATHLOOKUP_H
#define XPATHLOOKUP_H

#include "XPathTypes.h"
#include "tr/structures/nodeinterface.h"

#include <string>

class AxisHints;
class DataRoot;

namespace pe {

struct Path;

class IPathIterator {
  public:
    virtual ~IPathIterator() {};
    virtual Node next() = 0;
};

typedef counted_ptr<IPathIterator> NodeIterator;

/*
class AbsolutePathLookup {
private:
    AxisHints * evaluationInfo;
public:
    AbsolutePathLookup(const DataRoot& _root, const pe::Path& _path);

    void compile();

    NodeIterator execute();
};
*/

struct LookupInfo;

class PathLookup {
  protected:
    Path path;
  public:
    PathLookup(const Path & path);
    virtual ~PathLookup() {};

    virtual void compile() = 0;
    virtual NodeIterator execute(const Node& node) = 0;

    static PathLookup * createPathLookup(const pe::Path &path);
    static PathLookup * createStepLookup(const pe::Path &path);
};

};

#endif /* XPATHLOOKUP_H */
