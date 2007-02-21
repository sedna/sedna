/*
 * File:  xsd.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/base/xsd.h"
#include "tr/structures/schema.h"
#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/xs_names.h"


///
/// XML Schema Part 2 NCName Functions
///
char *xs_NCName_create(const char* value, void* (*alloc_func)(size_t))
{
    char *ncname = (char*)alloc_func(strlen(value) + 1);
    strcpy(ncname, value);
    return ncname;
}

void xs_NCName_release(char *ncname, void (*free_func)(void*))
{
    free_func(ncname);
}

void xs_NCName_print(const char *ncname, std::ostream& str)
{
    if (ncname && *ncname)
        str << ncname;
}

void  xs_NCName_print_to_lr(const char *ncname, std::ostream& str)
{
    str << "\"";
    if (ncname && *ncname)
        str << ncname;
    str << "\"";
}


///
/// XML Schema Part 2 anyURI Functions
///
char *xs_anyURI_create(const char* value, void* (*alloc_func)(size_t))
{
    char *uri = (char*)alloc_func(strlen(value) + 1);
    strcpy(uri, value);
    return uri;
}

void xs_anyURI_release(char *uri, void (*free_func)(void*))
{
    free_func(uri);
}

void xs_anyURI_print(const char *uri, std::ostream& str)
{
    if (uri && *uri)
        str << uri;
}

void xs_anyURI_print_to_lr(const char *uri, std::ostream& str)
{
    str << "\"";
    if (uri && *uri)
        str << uri;
    str << "\"";
}



///
/// XML Schema Part 2 QName (qualified name from Namespaces in XML standard) Functions
///
static void _xs_QName_encode(void *source, void *dest)
{                                                                                     
    for (int i = 0; i < sizeof(void*); i++)                                           
    {                                                                                 
        if (((unsigned char*)&source)[i] == 0xFF)                                     
        {                                                                             
            ((unsigned char*)dest)[i] = 0xF0;                                         
            ((unsigned char*)dest)[i + sizeof(void*)] = 0x0F;                         
        }                                                                             
        else                                                                          
        {                                                                             
            ((unsigned char*)dest)[i] = 0xFF;                                         
            ((unsigned char*)dest)[i + sizeof(void*)] = ~((unsigned char*)&source)[i];
        }                                                                             
    }                                                                                 
}                                                                                     
                                                                                      
static void *_xs_QName_decode(const void *source)                                                
{                                                                                     
    return (void*)(*(int*)source ^ *((int*)source + 1));                             
}                                                                                     

static int _xs_QName_separator_position(const char *prefix_and_local)
{
    int i = 0;
    
    while (prefix_and_local[i] != '\0')
        if (prefix_and_local[i] == ':') return i;
        else i++;

    return 0;
}

char *xs_QName_create(xml_ns* xmlns,
                      const char *local_part, 
                      void* (*alloc_func)(size_t))
{
    U_ASSERT(local_part);

    int lp_size = strlen(local_part);
    char *qname = (char*)alloc_func(lp_size + 2 * sizeof(void*) + 1);
    strcpy(qname + 2 * sizeof(void*), local_part);

    _xs_QName_encode(xmlns, qname);

    return qname;
}

char *xs_QName_create(const char *uri,
                      const char *prefix,
                      const char *local,
                      void* (*alloc_func)(size_t),
                      dynamic_context *cxt)
{
	xml_ns* ns = NULL;
	if (uri && *uri && prefix && *prefix)
    {
        if (!check_constraints_for_xs_NCName(prefix))
            throw USER_EXCEPTION2(XPTY0004, "Error in functions xs:QName");

		ns = cxt->st_cxt->get_ns_pair(prefix, uri);
    }

    if (!check_constraints_for_xs_NCName(local))
        throw USER_EXCEPTION2(XPTY0004, "Error in functions xs:QName");

    return xs_QName_create(ns, local, alloc_func);
}

char *xs_QName_create(const char* uri,
                      const char* prefix_and_local, 
                      void* (*alloc_func)(size_t),
                      dynamic_context *cxt)
{
    U_ASSERT(prefix_and_local);

    xml_ns* xmlns = NULL;
    const char *local = NULL;
    int pos = 0;
    if (!uri) uri = "";

    // XQuery spec doesn't say that we have to check lexical representation for uri (AF)

    // separate prefix and local name 
    pos = _xs_QName_separator_position(prefix_and_local);
    if (pos) 
        local = prefix_and_local + pos + 1;
    else 
        local = prefix_and_local;

    if (!check_constraints_for_xs_NCName(local))
        throw USER_EXCEPTION2(FOCA0002, "Error in functions fn:QName");

    if (*uri) // uri is present
    {
		if (pos)
			if (!check_constraints_for_xs_NCName(prefix_and_local, pos))
				throw USER_EXCEPTION2(FOCA0002, "Error in functions fn:QName");

        xmlns = cxt->st_cxt->get_ns_pair(std::string(prefix_and_local, pos).c_str(), uri);
    }
    else
    { // uri is empty...
        if (pos) // ... and prefix is not empty
            throw USER_EXCEPTION2(FOCA0002, "Error in functions fn:QName");
        // prefix is empty already (xmlns = NULL)
    }

    return xs_QName_create(xmlns, local, alloc_func);
}

char *xs_QName_create(const char* prefix_and_local,
                      const xptr& elem_node,
                      void* (*alloc_func)(size_t),
                      dynamic_context *cxt)
{
    U_ASSERT(prefix_and_local);

    // separate prefix and local name 
    int pos = _xs_QName_separator_position(prefix_and_local);

    const char *src_prefix = NULL;
    const char *src_local  = NULL;
    if (!pos)
    {
        src_prefix = "";
        pos = 1;
        src_local = prefix_and_local;
    }
    else
    {
        src_prefix = prefix_and_local;
        src_local  = prefix_and_local + pos + 1;

        if (!check_constraints_for_xs_NCName(src_prefix, pos))
            throw USER_EXCEPTION2(FOCA0002, "Error in functions fn:resolve-QName");
    }

    if (!check_constraints_for_xs_NCName(src_local))
        throw USER_EXCEPTION2(FOCA0002, "Error in functions fn:resolve-QName");

    std::vector<xml_ns*> xmlns;
    get_in_scope_namespaces(elem_node, xmlns, cxt);
    const char *tgt_prefix = NULL;

    for (int i = 0; i < xmlns.size(); i++)
    {
        tgt_prefix = xmlns[i]->prefix ? xmlns[i]->prefix : "";
        if (strncmp(tgt_prefix, src_prefix, pos) == 0)
        {
            return xs_QName_create(xmlns[i], src_local, alloc_func);
        }
    }

    if (src_local == prefix_and_local)
        return xs_QName_create((xml_ns*)NULL, src_local, alloc_func);

    throw USER_EXCEPTION2(FONS0004, "Error in functions fn:resolve-QName");
}



void xs_QName_release(char *qname, void (*free_func)(void*))
{

    // FIXME!!! (comment by AF)
    // We should somehow release xml_ns structure here. Releasing depends on several
    // things. If somebody is using this structure we could not release it, we should
    // rather decrease a counter... There could be some problems with log(s) Leon says,
    // but he couldn't clarify what are the problems...
    // To my concern, finally, we should get somethins like this here:
    // 
    //     xml_ns *xmlns = (xml_ns*)_xs_QName_decode(qname);
    //     xml_ns::delete_namespace_node(xmlns);
    //
    // But now we are just freeing memory
    free_func(qname);
}

const char *xs_QName_get_prefix(const char* qname)
{
    xml_ns *xmlns = (xml_ns*)_xs_QName_decode(qname);
    return xmlns ? xmlns->prefix : NULL; 
}

const char *xs_QName_get_uri(const char* qname)
{ 
    xml_ns *xmlns = (xml_ns*)_xs_QName_decode(qname);
    return xmlns ? xmlns->uri : NULL; 
}

const char *xs_QName_get_local_name(const char* qname)
{ 
    return qname + 2 * sizeof(void*); 
}

xml_ns *xs_QName_get_xmlns(const char* qname)
{
    return (xml_ns*)_xs_QName_decode(qname);
}

void xs_QName_print(const char* qname, std::ostream& str)
{
    const char *prefix = xs_QName_get_prefix(qname);
    if (prefix && *prefix)
        str << prefix << ":";
    str << xs_QName_get_local_name(qname);
}

void xs_QName_print_to_lr(const char* qname, std::ostream& str)
{
    const char *prefix = xs_QName_get_prefix(qname);
    str << "(\"";
    if (prefix && *prefix)
        str << prefix;
    str << "\" \"";
    str << xs_QName_get_local_name(qname);
    str << "\")";
}

bool _xs_QName_not_equal(const char *uri, const char *local, const xptr &node)
{
	xml_ns* node_ns = GETSCHEMENODE(XADDR(node))->xmlns;
	char* node_uri = node_ns ? node_ns->uri : NULL;
    const char *node_local = GETSCHEMENODE(XADDR(node))->name;

    return _xs_QName_not_equal(node_uri, node_local, uri, local);
}
