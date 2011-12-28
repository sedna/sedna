/*
 * File:  inscns.cpp
 * Copyright (C) 2011 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "inscns.h"

void InscopeNamespaceMap::buildMap()
{
    while (iterator.next()) { };

    mapReady = true;
}


INamespaceIterator* InscopeNamespaceMap::getIterator()
{
    if (!mapReady) {
        buildMap();
    }

    return new NamespaceMapIterator(iterator.map.begin(), iterator.map.end());
}

xmlns_ptr InscopeNamespaceMap::resolvePrefix(const char* prefix)
{
    if (started) {
        buildMap();
        U_ASSERT(mapReady);
    }

    if (mapReady) {
        NamespaceMap::const_iterator x = iterator.map.find(prefix);
        if (x == iterator.map.end()) {
            return NULL_XMLNS;
        } else {
            return x->second;
        }
    } else {
        U_ASSERT(!started);
        started = true;

        while (iterator.next()) {
            xmlns_ptr x = iterator.get();
            if (x->same_prefix(prefix)) {
                return x;
            }
        };
    }

    return NULL_XMLNS;
}

bool InscopeNamespaceIterator::setCurrent(xmlns_ptr p)
{
    NamespaceMap::iterator x = map.find(p->get_prefix());

    if (x == map.end()) {
        map.insert(NamespaceMap::value_type(p->get_prefix(), p));
        current = p;

        return !p->empty_uri();
    } else {
        return false;
    }
}

bool InscopeNamespaceIterator::next()
{
    if (state == ii_predefined) {
        state = ii_self;
        setCurrent(xmlns_touch(predefinedNamespaces[0].prefix, predefinedNamespaces[0].uri));
        return true;
    }

    while (!node.isNull()) {
        schema_node_cptr scm = node.checkp().getSchemaNode();

        if (state == ii_self) {
            state = ii_namespaces;
            if (scm->get_xmlns() != NULL_XMLNS) {
                if (setCurrent(scm->get_xmlns())) {
                    return true;
                }
            } else {
                map.insert(NamespaceMap::value_type("", NULL_XMLNS));
            }
        }

        if (state == ii_namespaces) {
            do {
                if (stateNode.isNull()) {
                    stateNode = getFirstChildByType(node.getPtr(), xml_namespace);
                } else {
                    stateNode = stateNode.getNextSameSort();
                }

                if (stateNode.isNull()) {
                    state = ii_attributes;
                } else {
                    if (setCurrent(NSNode(stateNode.checkp()).getNamespaceLocal())) {
                        return true;
                    }
                }
            } while (!stateNode.isNull());
        }

        if (state == ii_attributes) {
            do {
                if (stateNode.isNull()) {
                    stateNode = getFirstChildByType(node.getPtr(), attribute);
                } else {
                    stateNode = getNextAttribute(stateNode.getPtr());
                }

                if (stateNode.isNull()) {
                    state = ii_parent;
                } else {
                    xmlns_ptr ns = stateNode.checkp().getSchemaNode()->get_xmlns();
                    if (ns != NULL_XMLNS) {
                        do {
                            if (setCurrent(ns)) {
                                return true;
                            }

                            U_ASSERT(map.find(ns->get_prefix()) != map.end());

                            if (map[ns->get_prefix()] == ns) {
                                break;
                            } else {
                                ns = generateUniquePrefix(ns->get_prefix(), ns->get_uri(), skn);
                            }
                        } while (true);
                    }
                }
            } while (!stateNode.isNull());
        }

        if (state == ii_parent) {
            state = ii_self;
            node = node.checkp().getActualParent();
        }
    }

    return false;
}
