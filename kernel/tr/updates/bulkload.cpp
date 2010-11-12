/*
 * File:  bulkload.cpp
 * Copyright (C) 2004 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#include <math.h>
#include <map>
#include <ostream>

#include "common/sedna.h"

#include "tr/structures/metadata.h"
#include "expat.h"
#include "common/xptr.h"
#include "tr/mo/mo.h"

#ifdef SE_ENABLE_FTSEARCH
#include "tr/ft/ft_index_data.h"
#include "tr/updates/updates.h"
#endif

/*
 * We parse document part by part through this buffer. Since Expat
 * methods use 'int' instead of size_t we have to cast in some places.
 * The only guarantee for us is the size of this buffer is not
 * greater than INT_MAX.
 */
#define BUFFSIZE     8192
#define SEPARATOR    '>'

static char Buff[BUFFSIZE];
static int mark;
static xptr parent;
static xptr left;
static schema_node_xptr sc_parent = XNULL;
static bool wpstrip;

static bool text_inserted;
static bool print_p = true;
static int is_ns;
static char* wptail;
static int wptailsize;
static int maxwpsize=0;
static char* nodenames;
static size_t maxnm=0;
static int nodescnt=0;
static int curcnt=0;
static int curproc=0;
static bool is_coll=false;

typedef std::pair<int,int> stat_pair;
typedef std::pair<std::string,std::string> str_pair;
typedef std::map<str_pair, xmlns_ptr> ns_map;

static ns_map  xm_nsp;
static ns_map::iterator it_map;
static std::vector<xmlns_ptr> nss;

static std::vector<int> curr_fo;
static std::map<schema_node_xptr,stat_pair> max_fo;
static std::vector<stat_pair*> curp;


static void remove_hints(schema_node_cptr nd)
{
    nd.modify()->lastnode_ind=XNULL;

    cat_list<sc_ref>::item * sc;
    for (sc = nd->children->first; sc != NULL; sc = sc->next) {
        remove_hints(sc->object.snode);
    }
}

static bool isWP(const char* data, int size )
{
    char s;
    for (int i=0;i<size;i++)
    {
        s=data[i];
        if (s!=32 && s!=9 && s!=10 && s!=13 )return false;
    }
    return true;
}

static void separateName(const char* triplet,
                         const char*& uri,
                         const char*& local,
                         const char*& prefix)
{
    const char* sec=NULL;
    const char* third=NULL;
    int sepcnt = 0;
    for (size_t i=0; i < strlen(triplet); i++)
    {
        if (triplet[i] == SEPARATOR)
        {
            if (sepcnt == 0)
                sec = triplet + i + 1;
            else
                third = triplet + i + 1;
            sepcnt++;
        }
    }
    if (sepcnt)
    {
        size_t tripsize = strlen(triplet)+1;
        if (tripsize > maxnm)
        {
            if (maxnm != 0) delete [] nodenames;
            maxnm = tripsize;
            nodenames = new char[tripsize];
        }
        memcpy(nodenames, triplet, tripsize);
        uri = nodenames;
        local = nodenames + (sec-triplet);
        nodenames[sec-triplet-1] = '\0';
        if (third != NULL)
        {
            prefix=local+(third-sec);
            nodenames[third-triplet-1]='\0';
        }
        else prefix=NULL;
    }
    else
    {
        uri=NULL;
        local=triplet;
        prefix=NULL;
    }
}

static  void analyzeWP(const char** data, int& size )
{
 const char* ds=*data;
 const char* es=*data+(size-1);
 char s;
 int i;
 for ( i=0;i<size;i++)
 {
     s=*ds;
     if (s==32 || s==9 || s==10 || s==13 )
         ds++;
     else break;
 }
 size=size-i;
 *data=ds;
 for ( i=0;i<size;i++)
 {
    if (s==32 || s==9 || s==10 || s==13 )   es--;
    else break;
 }
 size=size-i;
}

static void processWP(const char** s, int& len)
{
    const char* d=*s;
    int size=len;
    analyzeWP(&d,size);
    if (size!=0||text_inserted)
    {
        if (wptailsize>0)
        {
            xptr new_node;
            if (mark)
                new_node=insert_text(XNULL, XNULL, parent, text_source_mem(wptail, wptailsize));
            else
            {
                new_node=insert_text(left, XNULL, XNULL, text_source_mem(wptail, wptailsize));
            }
            CHECKP(new_node);
            getSchemaNode(new_node).modify()->lastnode_ind=nodeGetIndirection(new_node);
#ifdef SE_ENABLE_FTSEARCH
            if (is_coll)
                update_insert_sequence(new_node,getSchemaNode(new_node));
#endif
            mark=0;
            left=new_node;
            CHECKP(left);
            parent = nodeGetParent(left);
        }
        wptailsize=0;
    }
    else
    {
        if (wptailsize+len>maxwpsize)
        {
            char* z=se_new char[wptailsize+len];
            if (wptailsize>0) memcpy(z,wptail,wptailsize);
            memcpy(z+wptailsize,*s,len);
            wptailsize+=len;
            maxwpsize=wptailsize;
            delete [] wptail;
            wptail=z;
        }
        else
        {
            memcpy(wptail+wptailsize,*s,len);
            wptailsize+=len;
        }
        len=0;
    }
}

static void clear_text()
{
    text_inserted=false;
    wptailsize=0;
}

static void start(void *s, const char *el, const char **attr)
{
    const char* uri;
    const char* local;
    const char* prefix;
    separateName((char*)el,uri,local,prefix);
    clear_text();
    xptr new_node;
    xmlns_ptr ns = NULL_XMLNS;

    if (uri != NULL || prefix != NULL)
    {
        if (!uri) uri="";
        else if (!prefix) prefix="";
        ns = xm_nsp.find(str_pair(prefix,uri))->second;
    }
    if (mark)
        new_node=insert_element(XNULL,XNULL,parent,local,xs_untyped,ns);
    else
    {
        new_node=insert_element(left,XNULL,XNULL,local,xs_untyped,ns);
        mark = 1;
    }
    CHECKP(new_node);
    getSchemaNode(new_node).modify()->lastnode_ind=nodeGetIndirection(new_node);
#ifdef SE_ENABLE_FTSEARCH
    if (is_coll)
        update_insert_sequence(new_node, schema_node_cptr(getSchemaNode(new_node)));
    CHECKP(new_node);
#endif
    xptr par_ind=nodeGetIndirection(new_node);
    stat_pair* pr=&max_fo[getSchemaPointer(new_node)];
    curp.push_back(pr);
    sizehnt= pr;
    parent=new_node;
    xptr att=XNULL;
    if (is_ns)
    {
        std::vector<xmlns_ptr>::const_iterator it=nss.begin();
        while(it!=nss.end())
        {
            att=insert_namespace(att,XNULL,(att==XNULL)?new_node:XNULL,*it);
            it++;
        }
        is_ns=0;
        nss.clear();
    }

    for (int i = 0; attr[i]; i += 2)
    {
        separateName((char*)attr[i],uri,local,prefix);
        if (uri != NULL || prefix != NULL)
        {
            if (!uri) uri="";
            else if (!prefix) prefix="";
            ns = xm_nsp.find(str_pair(prefix,uri))->second;
        }
        else ns = NULL;
        att=insert_attribute(att,XNULL,(att==XNULL)?new_node:XNULL,local,xs_untypedAtomic,attr[i + 1],strlen(attr[i + 1]),ns);
#ifdef SE_ENABLE_FTSEARCH
        if (is_coll)
            update_insert_sequence(att, schema_node_cptr(getSchemaNode(att)));
#endif
    }

    if (att!=XNULL)
    {
        left=att;
        mark=0;
        parent=indirectionDereferenceCP(par_ind);
    }
    curcnt++;
    if ((curcnt*100.)/nodescnt>curproc)
    {
     if(print_p)
      *(se_ostream*)s << curproc << "%" << '\n';
     curproc++;
    }
}


static void end(void *s, const char *el)
{
  clear_text();
  left=parent;
  CHECKP(left);
  parent = nodeGetParent(left);
  mark=0;
  curp.pop_back();
  sizehnt=curp.back();
}



static void data(void *userData, const char *s, int len)
{
    if (wpstrip)
    {
        processWP(&s,len);
        if (len==0) return;
    }
    text_inserted=true;
    xptr new_node;
    if (mark)
        new_node=insert_text(XNULL, XNULL, parent, text_source_mem(s, len));
    else
    {
        new_node=insert_text(left, XNULL, XNULL, text_source_mem(s, len));
    }
    CHECKP(new_node);
    getSchemaNode(new_node).modify()->lastnode_ind=nodeGetIndirection(new_node);
#ifdef SE_ENABLE_FTSEARCH
    if (is_coll)
        update_insert_sequence(new_node, getSchemaNode(new_node));
#endif
    mark=0;
    left=new_node;
    CHECKP(left);
    parent= nodeGetParent(left);
}


static void sc_start(void *data, const char *el, const char **attr)
{
    const char* uri;
    const char* local;
    const char* prefix;
    separateName((char*)el,uri,local,prefix);
    xmlns_ptr ns = NULL_XMLNS;
    if (uri!=NULL || prefix!=NULL)
    {
        if (!uri) uri="";
        else if (!prefix) prefix="";
        ns=xm_nsp.find(str_pair(prefix,uri))->second;
    }
    schema_node_cptr child=sc_parent->get_first_child(ns,local,element);
    if (!child.found()) child=sc_parent->add_child(ns,local,element);

    curr_fo.back()++;
    clear_text();
    curr_fo.push_back(0);
    sc_parent=child.ptr();
    if (is_ns)
    {
        if (sc_parent->get_first_child(NULL_XMLNS,NULL,xml_namespace) == XNULL)
            sc_parent->add_child(NULL_XMLNS,NULL,xml_namespace);

        curr_fo.back()+=is_ns;

        is_ns=0;
    }
    schema_node_cptr atts = XNULL;
    nodescnt++;
    for (int i = 0; attr[i]; i += 2)
    {
        separateName((char*)attr[i],uri,local,prefix);
        if (uri!=NULL || prefix!=NULL)
        {
            if (!uri) uri="";
            else if (!prefix) prefix="";
            ns=xm_nsp.find(str_pair(prefix,uri))->second;
        }
        else
            ns=NULL_XMLNS;
        atts=sc_parent->get_first_child(ns,local,attribute);
        if (!atts.found())atts=sc_parent->add_child(ns,local,attribute);
        curr_fo.back()++;
    }
}


static void sc_end(void *data, const char *el)
{
    std::map<schema_node_xptr,stat_pair>::iterator it= max_fo.find(sc_parent);
    if (it==max_fo.end())
        max_fo[sc_parent]=stat_pair(curr_fo.back(),0);
    else
        if (it->second.first<curr_fo.back()) it->second.first=curr_fo.back();
    curr_fo.pop_back();
    sc_parent=sc_parent->parent;
    clear_text();
}

static void sc_data(void *userData, const char *s, int len)
{
    if (!text_inserted)
    {
        if (wpstrip && isWP(s,len)) return;
        schema_node_cptr xsn=sc_parent->get_first_child(NULL_XMLNS,NULL,text);
        if (!xsn.found())xsn=sc_parent->add_child(NULL_XMLNS,NULL,text);
        curr_fo.back()++;
        text_inserted=true;
    }
}

static void el_ns (void *userData, const char *prefix, const char *uri)
{
    if(prefix == NULL && uri == NULL) return;

    const char* prefixm;
    if (prefix==NULL)
        prefixm="";
    else
        prefixm=prefix;
    str_pair sp(prefixm,uri);
    it_map=xm_nsp.find(sp);
    if (it_map == xm_nsp.end())
    {
        throw SYSTEM_EXCEPTION("Error of namespace parsing");
    }
    else
        nss.push_back(it_map->second);
    is_ns++;
}

static void sc_ns (void *userData, const char *prefix, const char *uri)
{
    /*
     * This is situation xmlns="". The attribute value in a default namespace
     * declaration MAY be empty. This has the same effect, within the scope of
     * the declaration, of there being no default namespace.
     */
    if(prefix == NULL && uri == NULL) return;

    const char* prefixm;

    if (prefix==NULL) prefixm="";
    else prefixm = prefix;

    str_pair sp(prefixm,uri);
    it_map=xm_nsp.find(sp);
    if (it_map== xm_nsp.end())
    {
        xm_nsp[sp] = xmlns_touch(prefixm, uri);
    }
    is_ns++;
}


static void sc_comment (void *userData, const char *data)
{
    schema_node_cptr xsn=sc_parent->get_first_child(NULL_XMLNS,NULL,comment);
    if (!xsn.found())xsn=sc_parent->add_child(NULL_XMLNS,NULL,comment);
    curr_fo.back()++;
    clear_text();
}


static void dt_comment (void *userData, const char *data)
{
    xptr new_node;
    clear_text();
    if (mark)
        new_node=insert_comment(XNULL,XNULL,parent,data,strlen(data));
    else
    {
        new_node=insert_comment(left,XNULL,XNULL,data,strlen(data));
    }
#ifdef SE_ENABLE_FTSEARCH
    if (is_coll) {
        update_insert_sequence(new_node,getSchemaNode(new_node));
    }
#endif
    mark=0;
    left=new_node;
    CHECKP(left);
    parent=nodeGetParent(left);
}


static void sc_pi (void *userData, const char *target, const char *data)
{
    schema_node_cptr xsn=sc_parent->get_first_child(NULL_XMLNS,NULL,pr_ins);
    if (!xsn.found())xsn=sc_parent->add_child(NULL_XMLNS,NULL,pr_ins);
    clear_text();
    curr_fo.back()++;
}


static void dt_pi (void *userData, const char *target, const char *data)
{
    xptr new_node;
    clear_text();
    if (mark)
        new_node=insert_pi(XNULL,XNULL,parent,target,strlen(target),data,strlen(data));
    else
    {
        new_node=insert_pi(left,XNULL,XNULL,target,strlen(target),data,strlen(data));
    }
#ifdef SE_ENABLE_FTSEARCH
    if (is_coll)
        update_insert_sequence(new_node,getSchemaNode(new_node));
#endif
    mark=0;
    left=new_node;
    CHECKP(left);
    xptr par_ind= nodeGetParentIndirection(left);
    parent=indirectionDereferenceCP(par_ind);
}


static void parse_load(FILE* f, se_ostream &ostr)
{
    XML_Parser p = XML_ParserCreateNS(NULL,SEPARATOR);
    if (! p) throw USER_ENV_EXCEPTION("Couldn't allocate memory for parser\n",true);
    XML_SetReturnNSTriplet(p,1);
    int done;
    int len;
    XML_SetStartNamespaceDeclHandler(p,el_ns);
    XML_SetElementHandler(p, start, end);
    XML_SetCommentHandler(p, dt_comment);
    XML_SetProcessingInstructionHandler(p, dt_pi);
    XML_SetCharacterDataHandler(p, data);
    XML_SetUserData (p, &ostr);

    /*
     * We have to cast since Expat uses 'int' instead of size_t,
     * it's safe since BUFFSIZE < INT_MAX
     */
    len = (int)fread(Buff, 1, BUFFSIZE, f);

    if (ferror(f))
    {
        XML_ParserFree(p);
        throw USER_ENV_EXCEPTION("Read error",true);
    }
    done = feof(f);
    while (!done)
    {
        if (XML_Parse(p, Buff, len, done) != XML_STATUS_ERROR)
        {
            /*
             * We have to cast since Expat uses 'int' instead of size_t,
             * it's safe since BUFFSIZE < INT_MAX
             */
            len = (int)fread(Buff, 1, BUFFSIZE, f);
            if (ferror(f))
            {
                XML_ParserFree(p);
                throw USER_ENV_EXCEPTION("Read error",true);
            }
            done = feof(f);
        }
        else
        {
            char tmp[256];
            sprintf(tmp, "line %d:\n%s\n",
                XML_GetCurrentLineNumber(p),
                XML_ErrorString(XML_GetErrorCode(p)));
            XML_ParserFree(p);
            throw USER_EXCEPTION2(SE2005, tmp);
        }
    }
    if (XML_Parse(p, Buff, len, done)== XML_STATUS_ERROR)
    {
        char tmp[256];
        sprintf(tmp, "line %d:\n%s\n",
            XML_GetCurrentLineNumber(p),
            XML_ErrorString(XML_GetErrorCode(p)));
        XML_ParserFree(p);
        throw USER_EXCEPTION2(SE2005, tmp);
    }
    sizehnt=NULL;
    XML_ParserFree(p);
}

static void parse_schema(FILE* f)
{
    xm_nsp.clear();
    xm_nsp[str_pair("xml","http://www.w3.org/XML/1998/namespace")] = xmlns_touch("xml", "http://www.w3.org/XML/1998/namespace");
    curr_fo.clear();
    curr_fo.push_back(0);
    max_fo.clear();
    XML_Parser p = XML_ParserCreateNS(NULL,SEPARATOR);
    if (! p) throw USER_ENV_EXCEPTION("Couldn't allocate memory for parser",true);
    XML_SetReturnNSTriplet(p,1);
    int done;
    int len;
    is_ns=0;
    XML_SetElementHandler(p, sc_start,sc_end);
    XML_SetStartNamespaceDeclHandler(p,sc_ns);
    XML_SetCommentHandler(p, sc_comment);
    XML_SetProcessingInstructionHandler(p, sc_pi);
    XML_SetCharacterDataHandler(p, sc_data);
    /*
     * We have to cast since Expat uses 'int' instead of size_t,
     * it's safe since BUFFSIZE < INT_MAX
     */
    len = (int)fread(Buff, 1, BUFFSIZE, f);
    if (ferror(f))
    {
        XML_ParserFree(p);
        throw USER_ENV_EXCEPTION("Read error",true);
    }
    done = feof(f);
    while (!done)
    {
        if (XML_Parse(p, Buff, len, done) != XML_STATUS_ERROR)
        {
            /*
             * We have to cast since Expat uses 'int' instead of size_t,
             * it's safe since BUFFSIZE < INT_MAX
             */
            len = (int)fread(Buff, 1, BUFFSIZE, f);
            if (ferror(f))
            {
                XML_ParserFree(p);
                throw USER_ENV_EXCEPTION("Read error",true);
            }
            done = feof(f);
        }
        else
        {
            char tmp[256];
            sprintf(tmp, "line %d:\n%s\n",
                XML_GetCurrentLineNumber(p),
                XML_ErrorString(XML_GetErrorCode(p)));
            XML_ParserFree(p);
            throw USER_EXCEPTION2(SE2005, tmp);

        }
    }
    if (XML_Parse(p, Buff, len, done)== XML_STATUS_ERROR)
    {
        char tmp[256];
        sprintf(tmp, "line %d:\n%s\n",
            XML_GetCurrentLineNumber(p),
            XML_ErrorString(XML_GetErrorCode(p)));
        XML_ParserFree(p);
        throw USER_EXCEPTION2(SE2005, tmp);
    }

    std::map<schema_node_xptr,stat_pair>::iterator it= max_fo.find(sc_parent);
    if (it==max_fo.end())
        max_fo[sc_parent]=stat_pair(curr_fo.back(),0);
    else
        if (it->second.first<curr_fo.back()) it->second.first=curr_fo.back();
    curr_fo.pop_back();
    it= max_fo.begin();
    while (it!=max_fo.end())
    {
        int cnt=s_max(it->second.first,1);
        it->second.first=(int)ceil(s_max (log10(1.*cnt)/log10((double)MAX_LETTER),1.));
        it->second.second=s_min(DEF_LETTER,((int)pow((double)MAX_LETTER,it->second.first))/(2+cnt));
        it++;
    }
    curp.push_back(&max_fo[sc_parent]);
    sizehnt=curp.back();
    XML_ParserFree(p);

}

xptr loadfile(FILE* f, se_ostream &ostr, const char* uri,bool stripped, bool print_progress)
{
    //test_cnt=0;
    is_coll=false;
    if (!print_progress) print_p = print_progress;
    wpstrip=stripped;
    nid_set_proportion(fnumber());
    xptr docnode=insert_document(uri);
    parent=docnode;
    left=XNULL;
    mark=1;
    sc_parent = getSchemaPointer(docnode);

    try
    {
        parse_schema(f);
        fseek (f, 0, SEEK_SET);
        docnode = nodeGetIndirection(docnode);
        parse_load(f, ostr);

        if (print_p)
            ostr << "100%" << '\n';

        nodescnt=0;
        curcnt=0;
        curproc=0;
        CHECKP(docnode);

        remove_hints(sc_parent);
        sc_parent = XNULL;
    }
    catch (SednaUserException)
    {
        remove_hints(sc_parent);
        sc_parent = XNULL;

        delete_document(uri);
        throw;
    }

    if (!print_progress) print_p=true;

    return docnode;
}

xptr loadfile(FILE* f, se_ostream &ostr, const char* uri,const char * collection, bool stripped, bool print_progress)
{
    is_coll=true;
    if (!print_progress) print_p = print_progress;
    wpstrip=stripped;
    nid_set_proportion(fnumber());

    xptr docnode=insert_document_into_collection(collection,uri);
    parent=docnode;
    left=XNULL;
    mark=1;
    CHECKP(docnode);
    sc_parent = getSchemaNode(docnode).ptr();
#ifdef SE_ENABLE_FTSEARCH
    update_insert_sequence(docnode, schema_node_cptr(sc_parent));
#endif
    try
    {
        parse_schema(f);
        fseek(f, 0, SEEK_SET);
        docnode = nodeGetIndirection(docnode);
        parse_load (f, ostr);

        if (print_p)
            ostr << "100%" << '\n';

        nodescnt=0;
        curcnt=0;
        curproc=0;
        CHECKP(docnode);

        remove_hints(sc_parent);
        sc_parent = XNULL;
    }
    catch (SednaUserException)
    {
        remove_hints(sc_parent);
        sc_parent = XNULL;

        delete_document_from_collection(collection, uri);
        throw;
    }

    if (!print_progress) print_p=true;

#ifdef SE_ENABLE_FTSEARCH
    execute_modifications();
#endif
    return docnode;
}
