/*
 * File: namespace_utils.cpp
 * Copyright (C) 2010 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#include <map>
#include <vector>
#include <set>

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"

typedef std::map<std::string,xmlns_ptr> nms_map;
void get_in_scope_namespaces_local(xptr node,std::vector<xmlns_ptr> &result,dynamic_context *cxt)
{
/*
    nms_map mp;
    CHECKP(node);
    schema_node_cptr scm=getSchemaNode(node);
    nms_map::iterator it;
    while (scm->type != virtual_root)
    {
        //1. namespace nodes
        xptr ns = getChildPointerXptr(node,NULL,xml_namespace,NULL_XMLNS);
        while (ns != XNULL)
        {
            CHECKP(ns);
            xmlns_ptr nsp=xmlns_touch(((ns_dsc*)XADDR(ns))->ns);
            const char* pref=nsp->prefix;
            if ((it=mp.find(pref))==mp.end())
                mp[pref]=nsp;
            ns = getNextSiblingOfSameSortXptr(ns);
        }
        //2. self namespace
        if (scm->get_xmlns()!=NULL_XMLNS)
        {
            const char* pref=scm->get_xmlns()->prefix;
            if ((it=mp.find(pref))==mp.end())
            {
                mp[pref]=scm->get_xmlns();
            }
        }
        //3. attributes
        xptr attr=getFirstAttributeChild(node);
        if (attr!=XNULL)
            CHECKP(attr);
        //3.1 filling set
        std::set<xmlns_ptr> atns;
        while (attr!=XNULL)
        {
            schema_node_cptr sca = getSchemaNode(attr);
            if (sca->get_xmlns()!=NULL)
                atns.insert(sca->get_xmlns());
            attr=getNextAttribute(attr);
        }
        //3.2 copying to map
        int ctr = 0;
        std::set<xmlns_ptr>::iterator sit = atns.begin();
        while(sit != atns.end())
        {
            const char* pref = (*sit)->prefix;
            if ((it=mp.find(pref))!=mp.end())
            {
                if (it->second!=*sit)
                {
                    xmlns_ptr new_ns=generate_pref(ctr++,(*sit)->uri,cxt);
                    mp[new_ns->prefix]=new_ns;
                }
            }
            else
            {
                mp[pref]=(*sit);
            }
            ++sit;
        }
        CHECKP(node);
        if ((GETPARENTPOINTER(node)) == XNULL) break;
        node = indirectionDereferenceCP(GETPARENTPOINTER(node));
        CHECKP(node);
        scm=getSchemaNode(node);
    }

    it = mp.begin();
    while (it!=mp.end())
    {
        result.push_back(it->second);
        ++it;
    }
    result.push_back(xmlns_touch("xml", "http://www.w3.org/XML/1998/namespace"));
*/
}

void get_in_scope_namespaces_broad(xptr node,std::vector<xmlns_ptr> &result)
{
}

void get_namespaces_for_inherit(xptr node,std::vector<xmlns_ptr> &result)
{
}
