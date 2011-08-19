/*
 * File:  namespaces.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef NAMESPACES_H
#define NAMESPACES_H

#include "tr/structures/xmlns.h"
#include "INamespaceMap.h"

#include <map>
#include <vector>
#include <string>

typedef std::map<std::string, xmlns_ptr> NamespaceMap;
typedef std::vector<xmlns_ptr> NamespaceStack;


struct PredefinedNamespace
{
    const char *prefix;
    const char *uri;
};

extern PredefinedNamespace predefinedNamespaces[];

xmlns_ptr generateUniquePrefix(const char * prefix, const char * uri, INamespaceMap * namespaces);

class NamespaceMapIterator : public INamespaceIterator {
  private:
    bool started;
    NamespaceMap::iterator x;
    NamespaceMap::iterator end;
  public:
    NamespaceMapIterator(const NamespaceMap::iterator &begin, const NamespaceMap::iterator &_end) :
        started(false), x(begin), end(_end) {};
    virtual ~NamespaceMapIterator() {};

    virtual bool empty() { return x != end; };
    virtual xmlns_ptr get() { return x->second; };

    virtual bool next() {
        if (started) {
            return x++ != end;
        } else {
            started = true;
            return x != end;
        }
    }
};

class NamespaceSnapshot : public INamespaceMap {
  private:
    NamespaceMap map;

  public:
    NamespaceSnapshot(const NamespaceMap &_map) : map(_map) {};

    virtual ~NamespaceSnapshot() {};
    virtual INamespaceIterator* getIterator() { return new NamespaceMapIterator(map.begin(), map.end()); };
    virtual xmlns_ptr resolvePrefix(const char* prefix);
};


class StaticallyKnownNamespaces : public INamespaceMap {
  private:
    NamespaceMap namespaceMap;
    NamespaceStack namespaceStack;
    NamespaceStack undoStack;

    typedef std::map<int, NamespaceSnapshot* > SnapshotMap;
    SnapshotMap snapshots;

    int position;
    int predefinedMark;

    void prev();
    void next();
    void rewind();
  public:
    StaticallyKnownNamespaces();
    virtual ~StaticallyKnownNamespaces();

    NamespaceSnapshot * getSnapshot();

    void clear();

    void setNamespace(xmlns_ptr ns);
    void rollbackToMark(int mark);

    int mark() { return (int) namespaceStack.size(); }
    void gotoMark(int mark);

    int getPredefinedMark() const { return predefinedMark; }

    virtual INamespaceIterator* getIterator() { return new NamespaceMapIterator(namespaceMap.begin(), namespaceMap.end()); };
    virtual xmlns_ptr resolvePrefix(const char* prefix);
};

#endif /* NAMESPACES_H */
