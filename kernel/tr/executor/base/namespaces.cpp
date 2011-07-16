/*
 * File:  namespaces.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "namespaces.h"

PredefinedNamespace predefinedNamespaces[] =
{
    {"xml", "http://www.w3.org/XML/1998/namespace"},
    {"xs",  "http://www.w3.org/2001/XMLSchema"},
    {"xsi", "http://www.w3.org/2001/XMLSchema-instance"},
    {"fn", "http://www.w3.org/2005/xpath-functions"},
    {"local", "http://www.w3.org/2005/xquery-local-functions"},
    {SEDNA_NAMESPACE_PREFIX, SEDNA_NAMESPACE_URI},
    {}
};

void StaticallyKnownNamespaces::prev()
{
    position--;
    xmlns_ptr ns = undoStack.at(position);
    namespaceMap[ns->get_prefix()] = (ns->empty_uri()) ? NULL_XMLNS : ns;
}

void StaticallyKnownNamespaces::next()
{
    xmlns_ptr ns = namespaceStack.at(position);
    namespaceMap[ns->get_prefix()] = (ns->empty_uri()) ? NULL_XMLNS : ns;
    position++;
}

void StaticallyKnownNamespaces::rewind()
{
    position = 0;
    namespaceMap.clear();

    while (position < predefinedMark) {
        next();
    }
}


StaticallyKnownNamespaces::StaticallyKnownNamespaces() : position(0) {
    clear();
}

void StaticallyKnownNamespaces::clear()
{
    position = 0;
    namespaceMap.clear();
    namespaceStack.clear();
    undoStack.clear();

    PredefinedNamespace * tmp = predefinedNamespaces;

    /* Initialize with predefined namespaces */
    while (tmp->prefix != NULL)
    {
        setNamespace(xmlns_touch(tmp->prefix, tmp->uri));
        tmp++;
    }

    predefinedMark = mark();
}


void StaticallyKnownNamespaces::gotoMark(int mark) {
    if (mark > position) {
        while (mark != position) {
            next();
        }
    } else {
        while (mark != position) {
            prev();
        }
    }
}

void StaticallyKnownNamespaces::rollbackToMark(int mark)
{
    for (int i = (int) position; i > mark; --i) {
        setNamespace(undoStack.at(i - 1));
    }
}


void StaticallyKnownNamespaces::setNamespace(xmlns_ptr ns)
{
    xmlns_ptr undoNs;

    if (ns->empty_uri()) {
        NamespaceMap::iterator it = namespaceMap.find(ns->get_prefix());

        if (it == namespaceMap.end()) {
            return;
        }

        undoNs = it->second;

        namespaceMap[ns->get_prefix()] = NULL_XMLNS;
    } else {
        NamespaceMap::iterator it = namespaceMap.find(ns->get_prefix());

        if (it == namespaceMap.end()) {
            undoNs = NULL_XMLNS;
        } else {
            undoNs = it->second;
        }

        namespaceMap[ns->get_prefix()] = ns;
    }

    namespaceStack.push_back(ns);
    position++;

    if (undoNs == NULL_XMLNS) {
        undoStack.push_back(xmlns_touch(ns->get_prefix(), ""));
    } else {
        U_ASSERT(ns->same_prefix(undoNs->get_prefix()));
        undoStack.push_back(undoNs);
    }
};

xmlns_ptr StaticallyKnownNamespaces::resolvePrefix(const char* prefix)
{
    U_ASSERT(prefix != NULL);
    NamespaceMap::const_iterator i = namespaceMap.find(prefix);
    if (i != namespaceMap.end()) {
        return i->second;
    } else {
        return NULL_XMLNS;
    }
}

xmlns_ptr NamespaceSnapshot::resolvePrefix(const char* prefix)
{
    U_ASSERT(prefix != NULL);
    NamespaceMap::const_iterator i = map.find(prefix);
    if (i != map.end()) {
        return i->second;
    } else {
        return NULL_XMLNS;
    }
}

StaticallyKnownNamespaces::~StaticallyKnownNamespaces()
{
    for (SnapshotMap::iterator x = snapshots.begin(); x != snapshots.end(); ++x) {
        delete x->second;
        x->second = NULL;
    }
}

NamespaceSnapshot* StaticallyKnownNamespaces::getSnapshot()
{
    SnapshotMap::iterator x = snapshots.find(position);

    if (x != snapshots.end()) {
        return x->second;
    } else {
        NamespaceSnapshot * result = new NamespaceSnapshot(namespaceMap);
        snapshots.insert(SnapshotMap::value_type(position, result));
        return result;
    }
}

xmlns_ptr generateUniquePrefix(const char * prefix, const char * uri, INamespaceMap * namespaces) {
    if (namespaces == NULL) {
        return generate_prefix(prefix, uri);
    }

    xmlns_ptr ns = namespaces->resolvePrefix(prefix);
    xmlns_ptr result = xmlns_touch(prefix, uri);

    while (ns != NULL_XMLNS && !ns->same_uri(uri)) {
        result = generate_prefix(result->get_prefix(), uri);
        ns = namespaces->resolvePrefix(result->get_prefix());
    };

    return result;
}
