/*
* File:  PPConstructors.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include <vector>

#include "common/sedna.h"

#include "tr/executor/xqops/PPConstructors.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/xs_names.h"
#include "tr/executor/base/xsd.h"
#include "tr/executor/base/visitor/PPVisitor.h"

#include "tr/updates/updates.h"
#include "tr/crmutils/crmutils.h"
#include "tr/structures/metadata.h"
#include "tr/strings/e_string.h"
#include "tr/mo/mo.h"
#include "tr/mo/blocks.h"
#include "tr/mo/microoperations.h"

using namespace std;

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// Static helpers
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

static inline 
tuple_cell getQnameParameter(PPOpIn qname)
{
    tuple name(qname.ts);

    qname.op->next(name);
    if (name.is_eos() || name.cells_number != 1 )
        throw XQUERY_EXCEPTION2(XPTY0004, "single atomic value is expected in the name expression of an attribute/element constructor");

    tuple_cell res = atomize(name.cells[0]);
    xmlscm_type xtype = res.get_atomic_type();

    if (xtype == xs_untypedAtomic)
        res = cast(res, xs_string);
    else if (!is_derived_from_xs_string(xtype) &&
        xtype != xs_string &&
        xtype != xs_QName)
        throw XQUERY_EXCEPTION2(XPTY0004, "unexpected type in the name expression of an attribute/element constructor");

    res=tuple_cell::make_sure_light_atomic(res);

    qname.op->next(name);
    if (!(name.is_eos()))
        throw XQUERY_EXCEPTION2(XPTY0004, "single atomic value is expected in the name expression of an attribute/element constructor");

    return res;
}

static bool
getStringParameter(PPOpIn content)
{
    tuple value(content.ts);
    content.op->next(value);
    sequence at_vals(1);
    if (value.is_eos())
    {
        tr_globals::tmp_op_str_buf.clear();
        tr_globals::tmp_op_str_buf.append(EMPTY_STRING_TC);
        return true;
    }
    else
    {
        at_vals.add(value);
        content.op->next(value);
    }

    while (!(value.is_eos()))
    {
        if (!(value.cells_number==1 )) throw USER_EXCEPTION2(SE1003, "in PPConstructor");
        at_vals.add(value);
        content.op->next(value);
    }
    tr_globals::tmp_op_str_buf.clear();
    sequence::iterator it=at_vals.begin();
    do
    {
        tuple_cell res=atomize((*it).cells[0]);
        res=cast(res, xs_string);
        res=tuple_cell::make_sure_light_atomic(res);
        tr_globals::tmp_op_str_buf.append(res);
        it++;

    }
    while (it!=at_vals.end());
    return false;
}

static void
getStringWSParameter(PPOpIn content)
{
    tr_globals::tmp_op_str_buf.clear();
    tuple value(content.ts);
    content.op->next(value);
    sequence at_vals(1);
    if (value.is_eos()) return;

    while (!(value.is_eos()))
    {
        if (!(value.cells_number==1 )) throw USER_EXCEPTION2(SE1003, "in PPConstructor");
        at_vals.add(value);
        content.op->next(value);
    }
    tr_globals::tmp_op_str_buf.clear();
    sequence::iterator it=at_vals.begin();
    do
    {
        tuple_cell res=atomize((*it).cells[0]);
        res=cast(res, xs_string);
        res=tuple_cell::make_sure_light_atomic(res);
        tr_globals::tmp_op_str_buf.append(res);
        it++;

    }
    while (it!=at_vals.end());
}

static inline bool
isNameValid(const char* name, const char* prefix, const char* uri, bool check_name = true)
{
    /* It has no namespace prefix and its local name is xmlns */
    if(check_name && (prefix == NULL || strcmp(prefix,"") == 0) && my_strcmp(name,"xmlns") == 0) return false;

    if(!prefix && !uri) return true;

    /* Its namespace prefix is xmlns. */
    if(my_strcmp(prefix, "xmlns") == 0) return false;
    /* Its namespace URI is xmlns URI. */
    if(my_strcmp(uri, "http://www.w3.org/2000/xmlns/") == 0) return false;
    /* Its namespace prefix is xml and its namespace URI is not xml namespace */
    if(my_strcmp(prefix, "xml") == 0 &&
       !(uri == NULL || my_strcmp(uri, "http://www.w3.org/XML/1998/namespace") == 0)) return false;
    /* Its namespace prefix is other than xml and its namespace URI is xml namespace */
    if(prefix != NULL && strcmp(prefix, "xml") != 0 && (uri == NULL || my_strcmp(uri, "http://www.w3.org/XML/1998/namespace") == 0)) return false;
    return true;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPConstructor (carries global state shared between all constructors)
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


/*
 * Global state shared between all constructors and some classes 
 * which are also inherited from PPConstructor
 */
schema_node_cptr PPConstructor::root_schema = XNULL;
xptr PPConstructor::virt_root               = XNULL;
xptr PPConstructor::last_elem               = XNULL;
xptr PPConstructor::cont_parind             = XNULL;
xptr PPConstructor::cont_leftind            = XNULL;
int PPConstructor::conscnt                  = 0;

void PPConstructor::checkInitial()
{
    if (!root_schema.found()) 
    {
        node_info_t node_info = {XNULL, XNULL, XNULL, virtual_root};
        root_schema = doc_schema_node_object::create_virtual_root()->p;
        xptr blk = createBlock(root_schema, XNULL);
        insertNodeFirst(blk, &node_info);

        virt_root=node_info.node_xptr;
        cont_parind=XNULL;
        cont_leftind=XNULL;
        conscnt=0;
        last_elem=XNULL;
    } 
}

/* 
 * Clears global state of constructors.
 * It's called in kernel statement end in trn.
 */
void PPConstructor::clear_virtual_root()
{
    if (root_schema.found())
    {
        nid_delete(virt_root);
        root_schema->drop();
        root_schema = XNULL;
        virt_root=XNULL;
    }
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPElementConstructor
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPElementConstructor::PPElementConstructor(dynamic_context *_cxt_,
                                           operation_info _info_,
                                           PPOpIn _qname_,
                                           PPOpIn _content_,
                                           bool _deep_copy,
                                           bool _ns_inside) : PPConstructor(_cxt_, _info_, _deep_copy),
                                                              qname(_qname_),
                                                              content(_content_),
                                                              ns_inside(_ns_inside)
{
    el_name=NULL;
}

PPElementConstructor::PPElementConstructor(dynamic_context *_cxt_,
                                           operation_info _info_,
                                           const char* name,
                                           PPOpIn _content_,
                                           bool _deep_copy,
                                           bool _ns_inside): PPConstructor(_cxt_, _info_, _deep_copy),
                                                             content(_content_),
                                                             ns_inside(_ns_inside)
{
    el_name=se_new char[strlen(name)+1];
    strcpy(el_name,name);

}
PPElementConstructor::~PPElementConstructor()
{

    if (el_name!=NULL)
    {
        delete [] el_name;
    }
    else
    {
        delete qname.op;
        qname.op = NULL;
    }

    delete content.op;

    content.op = NULL;
}

void PPElementConstructor::do_open ()
{
    checkInitial();
    if (el_name==NULL) qname.op->open();
    content.op->open();
    first_time = true;
}

void PPElementConstructor::do_reopen()
{
    if (el_name==NULL)  qname.op->reopen();
    content.op->reopen();

    first_time = true;
}

void PPElementConstructor::do_close()
{
    if (el_name==NULL) qname.op->close();
    content.op->close();
}

void PPElementConstructor::do_next (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        //Name parameter
        const char* name=el_name;
        tuple_cell res;
        if (name==NULL)
        {
            res=getQnameParameter(qname);
            name=res.get_str_mem();
        }


        //context save
        xptr parind=cont_parind;
        xptr leftind=cont_leftind;
        cont_parind=XNULL;
        cont_leftind=XNULL;
        int oldcnt=conscnt;
        //crm_dbg<<"\n befory body cnt in "<<name<<" = "<<conscnt;
        //Preliminaries for static context
        vector<xmlns_ptr> ns_list;
        vector<tuple> start_seq;
        tuple cont(content.ts);
        if (ns_inside)
        {
            content.op->next(cont);
            while (true)
            {
                start_seq.push_back(cont);
                if(cont.is_eos()||cont.cells[0].is_atomic()) break;
                tuple_cell tc=cont.cells[0];
                xptr node=tc.get_node();
                CHECKP(node);
                t_item typ=GETTYPE(GETSCHEMENODE(XADDR(node)));
                if(typ!=xml_namespace)
                    break;
                //ns_list.push_back(((ns_dsc*)XADDR(node))->ns);
                content.op->next(cont);
            }
        }
        //namespace search

        char* prefix = NULL;
        xmlns_ptr ns = NULL_XMLNS;
        if (!res.is_eos()&&res.get_atomic_type()==xs_QName)
        {
            ns   = xs_QName_get_xmlns(name);
            name = xs_QName_get_local_name(name);
        }
        else
        {
            separateLocalAndPrefix(prefix, name);

            if (!check_constraints_for_xs_NCName(name)) throw XQUERY_EXCEPTION(XQDY0074);

            if (prefix != NULL)
            {
                str_counted_ptr c_ptr(prefix);
                if(strcmp(prefix, "xmlns") == 0) throw XQUERY_EXCEPTION(XQDY0096);
                if(!check_constraints_for_xs_NCName(prefix)) throw XQUERY_EXCEPTION(XQDY0074);
                ns = cxt->st_cxt->get_xmlns_by_prefix(prefix);
            }
            else
            {
                ns = cxt->st_cxt->get_xmlns_by_prefix("");
            }
        }

        /* Check constraints on full name */
        if(!isNameValid(name,
                        ns == NULL_XMLNS ? NULL : ns->prefix,
                        ns == NULL_XMLNS ? NULL : ns->uri,
                        false))
            throw XQUERY_EXCEPTION(XQDY0096);

        /* Element insertion */
        xptr new_element;
        if (parind==XNULL || deep_copy)
        {
            new_element= insert_element(removeIndirection(last_elem),XNULL,get_virtual_root(),name,xs_untyped,ns);
            last_elem=((n_dsc*)XADDR(new_element))->indir;
        }
        else
        {
            if (leftind!=XNULL)
                new_element= insert_element(removeIndirection(leftind),XNULL,XNULL,name,(cxt->st_cxt->preserve_type)?xs_anyType:xs_untyped,ns);
            else
                new_element= insert_element(XNULL,XNULL,removeIndirection(parind),name,(cxt->st_cxt->preserve_type)?xs_anyType:xs_untyped,ns);
            conscnt++;
        }
        int cnt=conscnt;

        //xptr local_last=new_element;
        xptr indir= getIndirectionSafeCP(new_element);
        //context change
        cont_parind=indir;
        cont_leftind=XNULL;
        //MAIN PART
        sequence at_vals(1);
        xptr left=XNULL;
        bool mark_attr=true;
        vector<tuple>::iterator it_st;
        tuple* cont_ptr=NULL;
        if (ns_inside)
        {
            it_st=start_seq.begin();
            cont_ptr=&(*it_st);
        }
        else
        {
            content.op->next(cont);
            it_st=start_seq.end();
            cont_ptr=&cont;
        }
        while (!cont_ptr->is_eos())
        {
            tuple_cell tc=cont_ptr->cells[0];
            if (tc.is_atomic())
            {
                at_vals.add(*cont_ptr);
                //if (val->get_size()>0) val->append(" ");
                //val->append(cont_ptr);
            }
            else
            {
                if (at_vals.size()>0)
                {
                    //normalize
                    tuple_cell tcc;
                    tr_globals::tmp_op_str_buf.clear();
                    sequence::iterator it=at_vals.begin();
                    do
                    {
                        /*if (it!=at_vals.begin())
                        {
                        str_val.append(" ");
                        }*/
                        tcc=tuple_cell::make_sure_light_atomic((*it).cells[0]);
                        tcc=cast(tcc, xs_string);
                        tr_globals::tmp_op_str_buf.append(tcc);
                        it++;

                    }
                    while (it!=at_vals.end());
                    at_vals.clear();
                    if(tr_globals::tmp_op_str_buf.get_size()>0) {
                        insert_text(removeIndirection(left),XNULL,removeIndirection(indir),tr_globals::tmp_op_str_buf.get_ptr_to_text(),tr_globals::tmp_op_str_buf.get_size(),tr_globals::tmp_op_str_buf.get_type());
                        left = get_last_mo_inderection();
                    }
                    mark_attr=false;
                }
                xptr node=tc.get_node();
                CHECKP(node);
                t_item typ=GETTYPE(GETSCHEMENODE(XADDR(node)));
                switch (typ)
                {
                case xml_namespace:ns_list.push_back(xmlns_touch(((ns_dsc*)XADDR(node))->ns));break;
                case document:
                case element:
                    {
                        mark_attr=false;
                        break;
                    }
                case text:
                    {
                        if (isTextEmpty(T_DSC(node))) {
                            if (it_st!=start_seq.end()) {
                                it_st++;
                                if (it_st==start_seq.end())
                                {
                                    content.op->next(cont);
                                    cont_ptr=&cont;
                                }
                                else cont_ptr=&(*it_st);

                            }
                            else
                                content.op->next(cont);
                            continue;
                        }
                        mark_attr=false;
                        break;
                    }
                case attribute:
                    {
                        if (!mark_attr) throw XQUERY_EXCEPTION(XQTY0024);
                    }
                }
                if (conscnt>cnt)
                {
                    left= getIndirectionSafeCP(node);
                    cnt=conscnt;
                    cont_leftind=left;
                }
                else
                {
                    if (typ==document) {
                        xptr res = copy_node_content(indir, node, left, NULL, cxt->st_cxt->preserve_type);
                        if (res != XNULL) {
                            left = res;
                            cont_leftind = left;
                        }
                    } else {
                        left = deep_copy_node_ii(left, XNULL, indir, node, NULL, cxt->st_cxt->preserve_type);
                        cont_leftind=left;
                    }

                }

            }

            if (it_st!=start_seq.end())
            {
                it_st++;
                if (it_st==start_seq.end())
                {
                    content.op->next(cont);
                    cont_ptr=&cont;
                }
                else cont_ptr=&(*it_st);

            }
            else
                content.op->next(cont);
        }
        if (at_vals.size()>0)
        {
            tr_globals::tmp_op_str_buf.clear();
            tuple_cell tcc;
            sequence::iterator it=at_vals.begin();
            do
            {
                tcc=tuple_cell::make_sure_light_atomic((*it).cells[0]);
                tcc=cast(tcc, xs_string);
                tr_globals::tmp_op_str_buf.append(tcc);
                it++;

            }
            while (it!=at_vals.end());
            at_vals.clear();
            if(tr_globals::tmp_op_str_buf.get_size() > 0) {
                insert_text(removeIndirection(left),XNULL,removeIndirection(indir),tr_globals::tmp_op_str_buf.get_ptr_to_text(),tr_globals::tmp_op_str_buf.get_size(),tr_globals::tmp_op_str_buf.get_type());
                left = get_last_mo_inderection();
            }
        }
        //Result
        /*if (last_elem==local_last)
        last_elem=removeIndirection(indir);*/
        t.copy(tuple_cell::node_indir(indir));
        //clear in-scope context deleteng local namespace declarations
        vector<xmlns_ptr>::iterator it=ns_list.begin();
        while (it!=ns_list.end())
        {
            cxt->st_cxt->remove_from_context(*it);
            it++;
        }
        //context return;
        cont_parind=parind;
        cont_leftind=XNULL;
        if (deep_copy)
            conscnt=oldcnt;
        //crm_dbg<<"\n after body cnt in "<<name<<" = "<<conscnt;
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPElementConstructor::do_copy(dynamic_context *_cxt_)
{
    PPElementConstructor *res ;
    if (el_name!=NULL)
        res = se_new PPElementConstructor(_cxt_, info, el_name,content,deep_copy,ns_inside);
    else
    {
        res = se_new PPElementConstructor(_cxt_, info, qname,content,deep_copy,ns_inside);
        res->qname.op = qname.op->copy(_cxt_);
    }
    res->content.op = content.op->copy(_cxt_);
    return res;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPAttributeConstructor
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPAttributeConstructor::PPAttributeConstructor(dynamic_context *_cxt_,
                                               operation_info _info_,
                                               PPOpIn _qname_,
                                               PPOpIn _content_,
                                               bool _deep_copy): PPConstructor(_cxt_, _info_, _deep_copy),
                                                                 qname(_qname_),
                                                                 content(_content_)
{
    at_name=NULL;
    at_value=NULL;
}

PPAttributeConstructor::PPAttributeConstructor(dynamic_context *_cxt_,
                                               operation_info _info_,
                                               const char* name,
                                               PPOpIn _content_,
                                               bool _deep_copy): PPConstructor(_cxt_, _info_, _deep_copy),
                                                                 content(_content_)
{
    at_name=se_new char[strlen(name)+1];
    strcpy(at_name,name);
    at_value=NULL;

}

PPAttributeConstructor::PPAttributeConstructor(dynamic_context *_cxt_,
                                               operation_info _info_,
                                               PPOpIn _qname_,
                                               const char* value,
                                               bool _deep_copy): PPConstructor(_cxt_, _info_, _deep_copy),
                                                                 qname(_qname_)
{
    at_name=NULL;
    at_value=se_new char[strlen(value)+1];
    strcpy(at_value,value);
}

PPAttributeConstructor::PPAttributeConstructor(dynamic_context *_cxt_,
                                               operation_info _info_,
                                               const char* name,
                                               const char* value,
                                               bool _deep_copy): PPConstructor(_cxt_, _info_, _deep_copy)
{
    at_name=se_new char[strlen(name)+1];
    strcpy(at_name,name);
    at_value=se_new char[strlen(value)+1];
    strcpy(at_value,value);
}

PPAttributeConstructor::~PPAttributeConstructor()
{

    if (at_name!=NULL)
        delete [] at_name;
    else
    {
        delete qname.op;
        qname.op = NULL;
    }
    if (at_value!=NULL)
        delete [] at_value;
    else
    {
        delete content.op;
        content.op = NULL;
    }
}

void PPAttributeConstructor::do_open ()
{
    checkInitial();
    if (at_name==NULL)  qname.op->open();
    if (at_value==NULL) content.op->open();
    first_time = true;
}

void PPAttributeConstructor::do_reopen()
{
    if (at_name==NULL)  qname.op->reopen();
    if (at_value==NULL) content.op->reopen();
    first_time = true;
}

void PPAttributeConstructor::do_close()
{
    if (at_name==NULL)  qname.op->close();
    if (at_value==NULL) content.op->close();
}

void PPAttributeConstructor::do_next (tuple &t)
{
    if (first_time)
    {
        first_time = false;
        const char* name=at_name;
        const char* value=at_value;
        xmlns_ptr ns = NULL_XMLNS;
        tuple_cell res1, res;

        /* Get attribute name */
        if (NULL == name) {
            res1 = getQnameParameter(qname);
            name = res1.get_str_mem();
        }

        /* Get full name */
        if (!res1.is_eos() && res1.get_atomic_type() == xs_QName)
        {
            ns   = xs_QName_get_xmlns(name);
            name = xs_QName_get_local_name(name);
        }
        else
        {
            char* prefix = NULL;
            separateLocalAndPrefix(prefix, name);

            if(!check_constraints_for_xs_NCName(name)) throw XQUERY_EXCEPTION(XQDY0074);

            if (prefix != NULL)
            {
                str_counted_ptr c_ptr(prefix);
                if(strcmp(prefix, "xmlns") == 0) throw XQUERY_EXCEPTION(XQDY0044);
                if(!check_constraints_for_xs_NCName(prefix)) throw XQUERY_EXCEPTION(XQDY0074);
                /* Default namespace is not applied to the attributes */
                if(my_strcmp(prefix, "") != 0)
                    ns = cxt->st_cxt->get_xmlns_by_prefix(prefix);
            }
        }

        /* Check constraints on full name */
        if(!isNameValid(name,
                        ns == NULL_XMLNS ? NULL : ns->prefix,
                        ns == NULL_XMLNS ? NULL : ns->uri))
            throw XQUERY_EXCEPTION(XQDY0044);

        /* Get attribute value */
        int size;
        if (value==NULL)
        {
            getStringWSParameter(content);
            value=(char*)tr_globals::tmp_op_str_buf.c_str();
            size=tr_globals::tmp_op_str_buf.get_size();
        }
        else
            size=strlen(value);

        /* Attribute insertion */
        xptr new_attribute;
        if (cont_parind==XNULL || deep_copy)
            new_attribute= insert_attribute(XNULL,XNULL,get_virtual_root(),name,xs_untypedAtomic,value,size,ns);
        else
        {
            if (cont_leftind!=XNULL)
            {
                xptr left_sib=removeIndirection(cont_leftind);
                CHECKP(left_sib);
                schema_node_cptr ss=GETSCHEMENODE(XADDR(left_sib));
                t_item typ=GETTYPE(ss);
                if (typ!=attribute && typ!=xml_namespace)
                {
                    if (GETTYPE(ss->parent)!=document)
                        throw XQUERY_EXCEPTION(XQTY0024);
                    else
                        throw XQUERY_EXCEPTION(XPTY0004);
                }

                new_attribute= insert_attribute(removeIndirection(cont_leftind),XNULL,XNULL,name,xs_untypedAtomic,value,size,ns);
            }
            else
                new_attribute= insert_attribute(XNULL,XNULL,removeIndirection(cont_parind),name,xs_untypedAtomic,value,size,ns);
            conscnt++;
            cont_leftind=((n_dsc*)XADDR(new_attribute))->indir;
        }

        /* Result */
        t.copy(tuple_cell::node(new_attribute));
    }
    else /* i.e. first_time == false */
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPAttributeConstructor::do_copy(dynamic_context *_cxt_)
{
    PPAttributeConstructor *res ;
    if (at_name!=NULL)
    {
        if (at_value!=NULL) res = se_new PPAttributeConstructor(_cxt_, info, at_name,at_value, deep_copy);
        else res = se_new PPAttributeConstructor(_cxt_, info, at_name,content, deep_copy);
    }
    else
    {
        if (at_value!=NULL) res = se_new PPAttributeConstructor(_cxt_, info, qname,at_value, deep_copy);
        else res = se_new PPAttributeConstructor(_cxt_, info, qname,content, deep_copy);
    }
    if (at_name==NULL)res->qname.op = qname.op->copy(_cxt_);
    if (at_value==NULL)res->content.op = content.op->copy(_cxt_);
    return res;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPNamespaceConstructor
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPNamespaceConstructor::PPNamespaceConstructor(dynamic_context *_cxt_,
                                               operation_info _info_,
                                               const char* name,
                                               PPOpIn _content_): PPConstructor(_cxt_, _info_, true),
                                                                  content(_content_)
{
    if (name!=NULL)
    {
        at_name=se_new char[strlen(name)+1];
        strcpy(at_name,name);
    }
    else
        at_name=NULL;
    at_value=NULL;
}

PPNamespaceConstructor::PPNamespaceConstructor(dynamic_context *_cxt_,
                                               operation_info _info_,
                                               const char* name,
                                               const char* value): PPConstructor(_cxt_, _info_, true)
{
    if (name!=NULL)
    {
        at_name=se_new char[strlen(name)+1];
        strcpy(at_name,name);
    }
    else
        at_name=NULL;
    at_value=se_new char[strlen(value)+1];
    strcpy(at_value,value);
}

PPNamespaceConstructor::~PPNamespaceConstructor()
{
    if (at_name!=NULL)
        delete [] at_name;
    if (at_value!=NULL)
        delete [] at_value;
    else
    {
        delete content.op;
        content.op = NULL;
    }
}

void PPNamespaceConstructor::do_open ()
{
    checkInitial();
    if (at_value==NULL) content.op->open();
    first_time = true;
}

void PPNamespaceConstructor::do_reopen()
{
    if (at_value==NULL) content.op->reopen();
    first_time = true;
}

void PPNamespaceConstructor::do_close()
{
    if (at_value==NULL) content.op->close();
}

void PPNamespaceConstructor::do_next (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        const char* prefix = at_name;
        const char* uri    = at_value;

        if (uri == NULL)
        {
            getStringParameter(content);
            uri = (char*)tr_globals::tmp_op_str_buf.c_str();
        }

        xmlns_ptr ns = cxt->st_cxt->add_to_context(prefix,uri);
        xptr new_namespace= insert_namespace(XNULL,XNULL,get_virtual_root(),ns);

        t.copy(tuple_cell::node(new_namespace));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPNamespaceConstructor::do_copy(dynamic_context *_cxt_)
{
    PPNamespaceConstructor *res ;
    if (at_value!=NULL) res = se_new PPNamespaceConstructor(_cxt_, info, at_name,at_value);
    else res = se_new PPNamespaceConstructor(_cxt_, info, at_name,content);
    if (at_value==NULL)res->content.op = content.op->copy(_cxt_);
    return res;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPCommentConstructor
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPCommentConstructor::PPCommentConstructor(dynamic_context *_cxt_,
                                           operation_info _info_,
                                           PPOpIn _content_,
                                           bool _deep_copy): PPConstructor(_cxt_, _info_, _deep_copy),
                                                             content(_content_)
{
    at_value=NULL;
    strm.add_str("--","-");

}

PPCommentConstructor::PPCommentConstructor(dynamic_context *_cxt_,
                                           operation_info _info_,
                                           const char* value,
                                           bool _deep_copy): PPConstructor(_cxt_, _info_, _deep_copy)
{
    at_value=se_new char[strlen(value)+1];
    strcpy(at_value,value);
    strm.add_str("--","-");
}

PPCommentConstructor::~PPCommentConstructor()
{
    if (at_value!=NULL)
        delete [] at_value;
    else
    {
        delete content.op;
        content.op = NULL;
    }
}

void PPCommentConstructor::do_open ()
{
    checkInitial();
    if (at_value==NULL) content.op->open();
    first_time = true;
}

void PPCommentConstructor::do_reopen()
{
    if (at_value==NULL) content.op->reopen();
    first_time = true;
}

void PPCommentConstructor::do_close()
{
    if (at_value==NULL) content.op->close();
}

void PPCommentConstructor::do_next (tuple &t)
{
    if (first_time)
    {
        first_time = false;
        const char* value=at_value;
        int size=0;
        tuple_cell res;
        if (value==NULL)
        {
            getStringParameter(content);
            value=(char*)tr_globals::tmp_op_str_buf.c_str();
            size=tr_globals::tmp_op_str_buf.get_size();
        }
        else
            size=strlen(value);

        int rst=strm.parse(value,size,NULL,NULL);
        if (rst==1||(size>0 && value[size-1]=='-'))
            throw XQUERY_EXCEPTION(XQDY0072);
        xptr newcomm;
        if (cont_parind==XNULL || deep_copy )
            newcomm= insert_comment(XNULL,XNULL,get_virtual_root(),value,size);
        else
        {
            if (cont_leftind!=XNULL)
                newcomm= insert_comment(removeIndirection(cont_leftind),XNULL,XNULL,value,size);
            else
                newcomm= insert_comment(XNULL,XNULL,removeIndirection(cont_parind),value,size);
            conscnt++;
            cont_leftind=((n_dsc*)XADDR(newcomm))->indir;
        }
        //Result
        t.copy(tuple_cell::node(newcomm));

    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPCommentConstructor::do_copy(dynamic_context *_cxt_)
{
    PPCommentConstructor *res ;
    if (at_value!=NULL) res = se_new PPCommentConstructor(_cxt_, info, at_value, deep_copy);
    else
    {
        res = se_new PPCommentConstructor(_cxt_, info, content, deep_copy);
        res->content.op = content.op->copy(_cxt_);
    }
    return res;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPPIConstructor
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPPIConstructor::PPPIConstructor(dynamic_context *_cxt_,
                                 operation_info _info_,
                                 PPOpIn _qname_,
                                 PPOpIn _content_,
                                 bool _deep_copy) : PPConstructor(_cxt_, _info_, _deep_copy),
                                                    qname(_qname_),
                                                    content(_content_)
{
    at_name=NULL;
    at_value=NULL;
    strm.add_str("?>","--");

}
PPPIConstructor::PPPIConstructor(dynamic_context *_cxt_,
                                 operation_info _info_,
                                 const char* name,
                                 PPOpIn _content_,
                                 bool _deep_copy) : PPConstructor(_cxt_, _info_, _deep_copy),
                                                    content(_content_)
{
    at_name=se_new char[strlen(name)+1];
    strcpy(at_name,name);
    at_value=NULL;
    strm.add_str("?>","--");

}
PPPIConstructor::PPPIConstructor(dynamic_context *_cxt_,
                                 operation_info _info_,
                                 PPOpIn _qname_,
                                 const char* value,
                                 bool _deep_copy) : PPConstructor(_cxt_, _info_, _deep_copy),
                                                    qname(_qname_)
{
    at_name=NULL;
    at_value=se_new char[strlen(value)+1];
    strcpy(at_value,value);
    strm.add_str("?>","--");
}
PPPIConstructor::PPPIConstructor(dynamic_context *_cxt_,
                                 operation_info _info_,
                                 const char* name,
                                 const char* value,
                                 bool _deep_copy): PPConstructor(_cxt_, _info_, _deep_copy)
{
    at_name=se_new char[strlen(name)+1];
    strcpy(at_name,name);
    at_value=se_new char[strlen(value)+1];
    strcpy(at_value,value);
    strm.add_str("?>","--");
}
PPPIConstructor::~PPPIConstructor()
{

    if (at_name!=NULL)
        delete [] at_name;
    else
    {
        delete qname.op;
        qname.op = NULL;
    }
    if (at_value!=NULL)
        delete [] at_value;
    else
    {
        delete content.op;
        content.op = NULL;

    }
}

void PPPIConstructor::do_open ()
{
    checkInitial();
    if (at_name==NULL)  qname.op->open();
    if (at_value==NULL) content.op->open();
    first_time = true;
}

void PPPIConstructor::do_reopen()
{
    if (at_name==NULL)  qname.op->reopen();
    if (at_value==NULL) content.op->reopen();
    first_time = true;
}

void PPPIConstructor::do_close()
{
    if (at_name==NULL)  qname.op->close();
    if (at_value==NULL) content.op->close();
}

void PPPIConstructor::do_next (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        //Name parameter
        const char* name=at_name;
        tuple_cell res1;
        if (name==NULL)
        {
            res1=getQnameParameter(qname);
            name=res1.get_str_mem();
        }
        char* prefix=NULL;
        if (!res1.is_eos()&&res1.get_atomic_type()==xs_QName)
        {
            prefix=(char*)xs_QName_get_prefix(name);
            name=xs_QName_get_local_name(name);
            if (prefix!=NULL)
                throw XQUERY_EXCEPTION(XQDY0041);
        }
        else
        {
            separateLocalAndPrefix(prefix,name);
            if (prefix!=NULL)
            {
                delete[] prefix;
                throw XQUERY_EXCEPTION(XQDY0041);
            }
        }
        if (!check_constraints_for_xs_NCName(name))
            throw XQUERY_EXCEPTION(XQDY0041);
        if(charset_handler->matches(name, "^(?i:xml)$"))
            throw XQUERY_EXCEPTION(XQDY0064);
        const char* value=at_value;
        tuple_cell res;
        int size;
        if (value==NULL)
        {
            getStringParameter(content);
            value=(char*)tr_globals::tmp_op_str_buf.c_str();
            size=tr_globals::tmp_op_str_buf.get_size();
        }
        else
            size=strlen(value);

        int rst=strm.parse(value,size,NULL,NULL);
        if (rst==1)
            throw XQUERY_EXCEPTION(XQDY0026);
        int wp_k=0;
        int wp_s=size;
        while (wp_k<wp_s)
        {
            char s=value[0];
            if (s!=32 && s!=9 && s!=10 && s!=13) break;
            ++value;--size;

        }
        //Attribute insertion
        xptr new_pi;
        if (cont_parind==XNULL || deep_copy)
            new_pi= insert_pi(XNULL,XNULL,get_virtual_root(),name,strlen(name),value,size);
        else
        {
            if (cont_leftind!=XNULL)
                new_pi= insert_pi(removeIndirection(cont_leftind),XNULL,XNULL,name,strlen(name),value,size);
            else
                new_pi= insert_pi(XNULL,XNULL,removeIndirection(cont_parind),name,strlen(name),value,size);
            conscnt++;
            cont_leftind=((n_dsc*)XADDR(new_pi))->indir;
        }
        //Result
        t.copy(tuple_cell::node(new_pi));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPPIConstructor::do_copy(dynamic_context *_cxt_)
{
    PPPIConstructor *res ;
    if (at_name!=NULL)
    {
        if (at_value!=NULL) res = se_new PPPIConstructor(_cxt_, info, at_name,at_value, deep_copy);
        else res = se_new PPPIConstructor(_cxt_, info, at_name,content, deep_copy);
    }
    else
    {
        if (at_value!=NULL) res = se_new PPPIConstructor(_cxt_, info, qname,at_value, deep_copy);
        else res = se_new PPPIConstructor(_cxt_, info, qname, content, deep_copy);
    }
    if (at_name==NULL)res->qname.op = qname.op->copy(_cxt_);
    if (at_value==NULL)res->content.op = content.op->copy(_cxt_);
    return res;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPTextConstructor
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPTextConstructor::PPTextConstructor(dynamic_context *_cxt_,
                                     operation_info _info_,
                                     PPOpIn _content_,
                                     bool _deep_copy) : PPConstructor(_cxt_, _info_, _deep_copy),
                                                        content(_content_)
{
    at_value=NULL;

}

PPTextConstructor::PPTextConstructor(dynamic_context *_cxt_,
                                     operation_info _info_,
                                     const char* value,
                                     bool _deep_copy) : PPConstructor(_cxt_, _info_, _deep_copy)
{
    at_value=se_new char[strlen(value)+1];
    strcpy(at_value,value);
}
PPTextConstructor::~PPTextConstructor()
{

    if (at_value!=NULL)
        delete [] at_value;
    else
    {
        delete content.op;
        content.op = NULL;
    }
}

void PPTextConstructor::do_open ()
{
    checkInitial();
    if (at_value==NULL) content.op->open();
    first_time = true;
}

void PPTextConstructor::do_reopen()
{
    if (at_value==NULL) content.op->reopen();
    first_time = true;
}

void PPTextConstructor::do_close()
{
    if (at_value==NULL) content.op->close();
}

void PPTextConstructor::do_next (tuple &t)
{
    if (first_time)
    {
        first_time = false;
        const char* value=at_value;
        int size=0;
        tuple_cell res;
        if (value==NULL)
        {
            if (getStringParameter(content))
            {
                t.set_eos();
                return;
            }
            value=(char*)tr_globals::tmp_op_str_buf.c_str();

            size=tr_globals::tmp_op_str_buf.get_size();
        }
        else
            size=strlen(value);
        xptr newcomm;
        if (cont_parind==XNULL || deep_copy || size==0)
            newcomm= insert_text(XNULL,XNULL,get_virtual_root(),value,size);
        else
        {
            if (cont_leftind!=XNULL)
                newcomm= insert_text(removeIndirection(cont_leftind),XNULL,XNULL,value,size);
            else
                newcomm= insert_text(XNULL,XNULL,removeIndirection(cont_parind),value,size);
            conscnt++;
            cont_leftind=((n_dsc*)XADDR(newcomm))->indir;
        }
        //Result
        t.copy(tuple_cell::node(newcomm));

    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPTextConstructor::do_copy(dynamic_context *_cxt_)
{
    PPTextConstructor *res ;
    if (at_value!=NULL) res = se_new PPTextConstructor(_cxt_, info, at_value, deep_copy);
    else
    {
        res = se_new PPTextConstructor(_cxt_, info, content, deep_copy);
        res->content.op = content.op->copy(_cxt_);
    }
    return res;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPDocumentConstructor
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPDocumentConstructor::PPDocumentConstructor(dynamic_context *_cxt_,
                                             operation_info _info_,
                                             PPOpIn _content_): PPConstructor(_cxt_, _info_, false),
                                                                content(_content_)
{


}

PPDocumentConstructor::~PPDocumentConstructor()
{

    delete content.op;
    content.op = NULL;
}

void PPDocumentConstructor::do_open ()
{
    content.op->open();
    first_time = true;
}

void PPDocumentConstructor::do_reopen()
{
    content.op->reopen();
    first_time = true;
}

void PPDocumentConstructor::do_close()
{
    content.op->close();
}

void PPDocumentConstructor::do_next (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        //Name parameter
        tuple_cell res;
        //document insertion insertion
        //context save
        xptr parind=cont_parind;
        xptr leftind=cont_leftind;
        int cnt=conscnt;
        int oldcnt=conscnt;
        xptr new_doc=insert_document("tmp",false);
        cxt->st_cxt->temp_docs.push_back(new_doc);
        xptr indir=((n_dsc*)XADDR(new_doc))->indir;
        cont_parind=indir;
        cont_leftind=XNULL;
        sequence at_vals(1);
        xptr lefti=XNULL;
        content.op->next(t);
        while (!t.is_eos())
        {
            tuple_cell tc=t.cells[0];
            if (tc.is_atomic())
            {
                at_vals.add(t);
                //if (val->get_size()>0) val->append(" ");
                //val->append(cont_ptr);
            }
            else
            {
                if (at_vals.size()>0)
                {
                    tuple_cell tcc;
                    tr_globals::tmp_op_str_buf.clear();
                    sequence::iterator it=at_vals.begin();
                    do
                    {
                        tcc=tuple_cell::make_sure_light_atomic((*it).cells[0]);
                        tcc=cast(tcc, xs_string);
                        tr_globals::tmp_op_str_buf.append(tcc);
                        it++;
                    }
                    while (it!=at_vals.end());
                    at_vals.clear();
                    if(tr_globals::tmp_op_str_buf.get_size()>0) {
                        insert_text(indirectionDereferenceCP(lefti), XNULL, indirectionDereferenceCP(indir), tr_globals::tmp_op_str_buf.get_ptr_to_text(), tr_globals::tmp_op_str_buf.get_size(), tr_globals::tmp_op_str_buf.get_type());
                        lefti = get_last_mo_inderection();
                    }
                }
                xptr node=tc.get_node();
                CHECKP(node);
                t_item typ=GETTYPE(GETSCHEMENODE(XADDR(node)));
                switch (typ)
                {
                case document:
                case element:
                    {
                        break;
                    }
                case text:
                    {
                        if (isTextEmpty(T_DSC(node))) {
                            content.op->next(t);
                            continue;
                        }
                        break;
                    }
                case attribute:
                    {
                        throw XQUERY_EXCEPTION(XPTY0004);
                    }
                }
                if (conscnt>cnt) {
                    lefti = getIndirectionSafeCP(node);
                    cnt = conscnt;
                } else {
                    if (typ==document) {
                        xptr res = copy_node_content(indir, node, lefti, NULL, cxt->st_cxt->preserve_type);
                        if (res != XNULL) {
                            lefti = res;
                        } else {
                            content.op->next(t);
                            continue;
                        }
                    } else {
                        lefti = deep_copy_node_ii(lefti, XNULL, indir, node, NULL, cxt->st_cxt->preserve_type);
                    }
                }
                cont_leftind = lefti;
            }

            content.op->next(t);
        }
        if (at_vals.size()>0)
        {
            tr_globals::tmp_op_str_buf.clear();
            tuple_cell tcc;
            sequence::iterator it=at_vals.begin();
            do
            {
                tcc=tuple_cell::make_sure_light_atomic((*it).cells[0]);
                tcc=cast(tcc, xs_string);
                tr_globals::tmp_op_str_buf.append(tcc);
                it++;
            }
            while (it!=at_vals.end());
            at_vals.clear();
            if(tr_globals::tmp_op_str_buf.get_size()>0)
                lefti = getIndirectionSafeCP(insert_text(indirectionDereferenceCP(lefti),XNULL,indirectionDereferenceCP(indir),tr_globals::tmp_op_str_buf.get_ptr_to_text(),tr_globals::tmp_op_str_buf.get_size(),tr_globals::tmp_op_str_buf.get_type()));
        }
        t.copy(tuple_cell::node_indir(indir));

        cont_parind=parind;
        cont_leftind=XNULL;
        conscnt=oldcnt;
    } else {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPDocumentConstructor::do_copy(dynamic_context *_cxt_)
{
    PPDocumentConstructor *res ;
    res = se_new PPDocumentConstructor(_cxt_, info, content);
    res->content.op = content.op->copy(_cxt_);
    return res;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPVisitor Acceptors
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


void PPElementConstructor::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    if (el_name==NULL) qname.op->accept(v);
    content.op->accept(v);
    v.pop();
}

void PPAttributeConstructor::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    if (at_name==NULL)  qname.op->accept(v);
    if (at_value==NULL) content.op->accept(v);
    v.pop();
}

void PPNamespaceConstructor::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    if (at_value==NULL) content.op->accept(v);
    v.pop();
}

void PPCommentConstructor::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    if (at_value==NULL) content.op->close();
    v.pop();
}

void PPPIConstructor::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    if (at_name==NULL)  qname.op->accept(v);
    if (at_value==NULL) content.op->accept(v);
    v.pop();
}

void PPDocumentConstructor::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    content.op->accept(v);
    v.pop();
}

void PPTextConstructor::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    if (at_value==NULL) content.op->close();
    v.pop();
}
