/*
 * File:  xsd.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "xsd.h"
#include "schema.h"


// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#include "persistent_db_data.h"
#include "PPBase.h"
xml_ns* find_or_construct_xml_ns(const char* prefix, const char* uri, bool persistent)
{
    xml_ns *xmlns = NULL;
    if (persistent)
    {
        const char *urim = uri;// or '!uri ? "" : uri'
        const char *prefixm = (prefix ? prefix : "");
        xmlns = (xml_ns*)(entry_point->nslist->find(urim, prefixm));
        if (xmlns == NULL) 
        { 
            xmlns = xml_ns::init(urim, prefixm, true);
            entry_point->nslist->put(xmlns);
        }
    }
    else
    {
        xmlns = tr_globals::st_ct.get_ns_pair(prefix, uri);
    }

    return xmlns;
}
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



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
    str << ncname;
}

void  xs_NCName_print_to_lr(const char *ncname, std::ostream& str)
{
    str << "\"" << ncname << "\"";
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

/*
char *xs_QName_create(const char* uri,
                      const char* prefix, 
                      const char *local_part, 
                      void* (*alloc_func)(size_t))
{
    int lp_size = strlen(local_part);
    char *qname = (char*)alloc_func(lp_size + 2 * sizeof(void*) + 1);
    strcpy(qname + 2 * sizeof(void*), local_part);

    xml_ns *xmlns = find_or_construct_xml_ns(prefix, uri ? uri : "", IS_PH_PTR(qname));
    _xs_QName_encode(xmlns, qname);

    return qname;
}
*/
char *xs_QName_create(xml_ns* xmlns,
                      const char *local_part, 
                      void* (*alloc_func)(size_t))
{
    int lp_size = strlen(local_part);
    char *qname = (char*)alloc_func(lp_size + 2 * sizeof(void*) + 1);
    strcpy(qname + 2 * sizeof(void*), local_part);

    _xs_QName_encode(xmlns, qname);

    return qname;
}

char *xs_QName_create(const char* prefix_and_local, 
                      void* (*alloc_func)(size_t))
{
    U_ASSERT(prefix_and_local);

    // separate prefix and local name 
    int pos = _xs_QName_separator_position(prefix_and_local);

    if (pos)
    {
        // find xmlns by calling get_xmlns_by_prefix
        xml_ns* xmlns = tr_globals::st_ct.get_xmlns_by_prefix(prefix_and_local, pos);
        return xs_QName_create(xmlns, prefix_and_local + pos + 1, alloc_func);
    }
    else
    {
        return xs_QName_create((xml_ns*)NULL, prefix_and_local, alloc_func);
    }
}

char *xs_QName_create(const char* uri,
                      const char* prefix_and_local, 
                      void* (*alloc_func)(size_t))
{
    return NULL;
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
    if (prefix && strlen(prefix) != 0)
        str << prefix << ":";
    str << xs_QName_get_local_name(qname);
}

void xs_QName_print_to_lr(const char* qname, std::ostream& str)
{
    const char *prefix = xs_QName_get_prefix(qname);
    str << "(\"";
    if (prefix && strlen(prefix) != 0)
        str << prefix;
    str << "\" \"";
    str << xs_QName_get_local_name(qname);
    str << "\")";
}


