/*
 * File:  print_utils.cpp
 * Copyright (C) 2004 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#include "common/sedna.h"

#include "tr/crmutils/crmutils.h"
#include "tr/crmutils/crminternals.h"
#include "tr/structures/metadata.h"
#include "tr/structures/schema.h"
#include "tr/idx/index_data.h"
#include "tr/pstr/pstr.h"
#include "tr/pstr/pstr_long.h"
#include "tr/strings/e_string.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/base/xs_helper.h"
#include "tr/idx/btree/btstruct.h"
#include "tr/idx/btree/btree.h"
#include "tr/cat/catenum.h"

#include "tr/structures/nodeoperations.h"
#include "tr/structures/nodeutils.h"

using namespace tr_globals;

///////////////////////////////////////////////////////////////////////////////
/// Type definitions
///////////////////////////////////////////////////////////////////////////////

typedef std::pair<std::string, std::string> ns_pair;
typedef std::map<ns_pair, xmlns_ptr> nspt_map;

///////////////////////////////////////////////////////////////////////////////
/// Static helpers and variables
///////////////////////////////////////////////////////////////////////////////

static nspt_map               xm_nsp;
static bool                   def_set   =false;
static bool                   is_atomic =false;
static std::set<std::string>  nspt_pref;

static void inline print_indent(se_ostream& crmout, int indent) {
    for (int i=0;i<indent;i++) crmout << "  ";
}

static void print_namespace(xmlns_ptr nsd, se_ostream& crmout, t_print ptype)
{
    U_ASSERT(nsd->prefix);
    U_ASSERT(nsd->uri);

    if (ptype == xml)
    {
        if (strcmp(nsd->prefix, "") == 0)
            crmout << " xmlns=\"";
        else
            crmout << " xmlns:" << nsd->prefix << "=\"";
        crmout.writeattribute(nsd->uri, strlen(nsd->uri));
        crmout << "\"";
    }
}

static inline const
ns_pair pref_to_str(xmlns_ptr ns)
{
    U_ASSERT(ns->prefix);
    U_ASSERT(ns->uri);

    return ns_pair(ns->prefix, ns->uri);
}

static inline const std::string
prefix_to_str(char* prefix) {
    U_ASSERT(prefix);

    return std::string(prefix);
}

static void
print_attribute_prefix(se_ostream& crmout, schema_node_cptr scm, int indent) {

    if (scm->get_xmlns() != NULL_XMLNS)
    {
        char* prefix;
        if (!indent)
            prefix = scm->get_xmlns()->prefix;
        else
            prefix = xm_nsp[pref_to_str(scm->get_xmlns())]->prefix;

        U_ASSERT(prefix);

        crmout<<prefix<<":";
    }
}


///////////////////////////////////////////////////////////////////////////////
/// Printing functions
///////////////////////////////////////////////////////////////////////////////

static
void print_text(xptr txt, se_ostream& crmout, t_print ptype, t_item xq_type)
{
    CHECKP(txt);
    strsize_t size = CommonTextNode(txt).getTextSize();
    CHECKP(txt);

    if (size<=PSTRMAXSIZE) {
        char * data;
        xptr data_p = CommonTextNode(txt).getTextPointerCP();

        if (data_p == XNULL) {
            data = NULL;
        } else {
            CHECKP(data_p);
            data = (char *) XADDR(data_p);
        }

        if (ptype==xml)
        {
            if (xq_type!=text && xq_type!=attribute)
                crmout.write(data,size);
            else if (xq_type == attribute)
                crmout.writeattribute(data, size);
            else
                crmout.writextext(data,size);
        }
        else
        {
            crmout<<"\"";
            if (xq_type!=text && xq_type!=attribute)
                crmout.write(data,size);
            else crmout.writextext(data,size);
            crmout<<"\"";
        }
    }
    else
    {
        if (ptype!=xml)
            crmout<<"\"";
        pstr_long_writextext(txt,crmout);
        if (ptype!=xml)
            crmout<<"\"";
    }

    tr_globals::client->get_serialization_params()->stm.flush(write_func, &crmout);
}

static void
print_node_internal(xptr node,
                    se_ostream& crmout,
                    bool wi,
                    int indent,
                    t_print ptype,
                    dynamic_context *cxt)
{
    CHECK_TIMER_FLAG;

    switch(getNodeType(node))
    {

    case document: case virtual_root:
        {
            if (IS_DATA_BLOCK(node))
                crmout <<((ptype==xml)? "<?xml version=\"1.0\" standalone=\"yes\"":"(*TOP*");
            xptr child=getFirstChild(node);
            if(child==XNULL)
            {
                if (IS_DATA_BLOCK(node)) crmout << ((ptype==xml)? "?>": ")");
                return;
            }
            else CHECKP(child);
            if (getNodeType(child)==attribute)
            {
                if (ptype==sxml)  crmout << "(@";
                do
                {
                    print_node_internal(child,crmout,wi,0,ptype,cxt);
                    child=nodeGetRightSibling(child);
                    if (child==XNULL) break;
                    CHECKP(child);
                }
                while (getNodeType(child)==attribute);
                if (ptype==sxml)  crmout << ")";
            }

            if (IS_DATA_BLOCK(node))
            {
                crmout << ((ptype==xml )? "?>": "");
                if(wi) crmout << "\n";
            }
            while (child!=XNULL)
            {
                CHECKP(child);
                print_node_internal(child,crmout,wi,0,ptype,cxt);
                CHECKP(child);
                child=nodeGetRightSibling(child);
                if(child != XNULL && wi) crmout << "\n";
            }
            if (ptype==sxml)  crmout << ")";
            break;
        }

    case element:
        {
            if(wi&&indent)
            {
                crmout<< "\n";
                print_indent(crmout,indent) ;
            }
            bool curwi=wi;
            bool lit=false;
            bool def_inset=false;
            crmout <<((ptype==xml)? "<": "(");
            schema_node_cptr scn=getSchemaNode(node);
            xptr first_ns=XNULL;
            std::vector<ns_pair> *att_ns=NULL;
            std::vector<std::string> *pref_ns=NULL;

            xmlns_ptr nsptr = scn->get_xmlns();

            if (nsptr != NULL_XMLNS)
            {
                U_ASSERT(nsptr->prefix != NULL);
                if(strcmp(nsptr->prefix, "") != 0)
                    crmout << nsptr->prefix << ":";
            }

            crmout << scn->get_name();
            xptr child = getFirstChild(node);

            if(child == XNULL)
            {
                if (nsptr != NULL_XMLNS
                    && xm_nsp.find(pref_to_str(nsptr)) == xm_nsp.end()
                    && strcmp(nsptr->prefix, "xml"))
                    print_namespace(nsptr, crmout, ptype);
                if(def_set && nsptr == NULL_XMLNS)
                    crmout <<" xmlns=\"\"";
                crmout << ((ptype==xml)? "/>": ")");
                return;
            }
            else
                CHECKP(child);

            //namespaces print and add
            while (getNodeType(child) == xml_namespace)
            {
                if (first_ns == XNULL) first_ns = child;

                ns_pair str = pref_to_str(NSNode(child).getNamespaceLocal());
                nspt_map::iterator it_map = xm_nsp.find(str);

                if (it_map == xm_nsp.end())
                {
                    xmlns_ptr sns = NSNode(child).getNamespaceLocal();
                    xm_nsp[str] = sns;
                    if (!att_ns)
                        att_ns= se_new std::vector<ns_pair> ;
                    att_ns->push_back(str);

                    if (nspt_pref.find(str.first)==nspt_pref.end())
                    {
                        if (!pref_ns) pref_ns= se_new std::vector<std::string> ;
                        pref_ns->push_back(str.first);
                        nspt_pref.insert(str.first);
                    }

                    U_ASSERT(sns->prefix != NULL);

                    if (strcmp(sns->prefix, "") == 0)
                    {
                        if (!def_set)def_inset=true;
                        def_set=true;
                    }
                }
                print_node_internal(child,crmout,wi,0,ptype,cxt);
                CHECKP(child);
                child=nodeGetRightSibling(child);
                if (child==XNULL) break;
                CHECKP(child);
            }

            //self namespace
            if (scn->get_xmlns() != NULL_XMLNS &&
                xm_nsp.find(pref_to_str(scn->get_xmlns()))==xm_nsp.end() &&
                strcmp(scn->get_xmlns()->prefix,"xml"))
            {
                ns_pair str=pref_to_str(scn->get_xmlns());
                xm_nsp[str]=scn->get_xmlns();
                if (!att_ns)
                    att_ns= se_new std::vector<ns_pair>;
                att_ns->push_back(str);
                print_namespace(scn->get_xmlns(),crmout,ptype);
                std::string prf = prefix_to_str(scn->get_xmlns()->prefix);
                if (nspt_pref.find(prf) == nspt_pref.end())
                {
                    if (!pref_ns) pref_ns= se_new std::vector<std::string> ;
                    pref_ns->push_back(prf);
                    nspt_pref.insert(prf);
                }
                if (strcmp(scn->get_xmlns()->prefix, "") == 0)
                {
                    if (!def_set) def_inset=true;
                    def_set = true;
                }
            }
            else
            {
                if (def_set && scn->get_xmlns() == NULL_XMLNS)
                {
                    def_set=false;
                    def_inset=true;
                    crmout <<" xmlns=\"\"";
                }
            }

            xptr* ptr=NULL;
            sc_ref_item* sch=NULL;
            int cnt=0;
            int ctr=0;
            if (child==XNULL)
            {
                crmout << ((ptype==xml )? "/>": "))");
                goto nsfree;
            }

            //namespaces for attributes
            CHECKP(node);
            getChildList(node, ptr);
            sch = scn->children->first;
            while (sch!=NULL)
            {
                if (sch->object.get_xmlns() != NULL_XMLNS &&
                    sch->object.type==attribute &&
                    xm_nsp.find(pref_to_str(sch->object.get_xmlns())) == xm_nsp.end() &&
                    *(ptr+cnt) != XNULL )
                {
                    if (strcmpex(sch->object.get_xmlns()->prefix,"xml") != 0)
                    {
                        ns_pair str=pref_to_str(sch->object.get_xmlns());
                        xmlns_ptr xmn = NULL_XMLNS;
                        if (nspt_pref.find(str.first)==nspt_pref.end())
                        {
                            xmn=sch->object.get_xmlns();
                            if (!pref_ns)pref_ns= se_new std::vector<std::string>;
                            pref_ns->push_back(str.first);
                            nspt_pref.insert(str.first);
                        }
                        else
                        {
                            xmn = generate_prefix(ctr++,sch->object.get_xmlns()->uri,cxt);
                        }
                        xm_nsp[str]=xmn;
                        if (!att_ns)
                            att_ns= se_new std::vector<ns_pair> ;
                        att_ns->push_back(str);
                        print_namespace(xmn,crmout,ptype);
                    }
                    else
                    {
                        xmlns_ptr t=xmlns_touch("xml","http://www.w3.org/XML/1998/namespace");
                        ns_pair str=pref_to_str(t);
                        xm_nsp[str]=t;
                        if (!att_ns)
                            att_ns= se_new std::vector<ns_pair> ;
                        att_ns->push_back(str);
                    }

                }
                sch=sch->next;
                cnt++;
            }
            //attributes
            CHECKP(child);
            if (getNodeType(child)==attribute)
            {
                if (ptype==sxml)  crmout << "(@";
                do
                {
                    print_node_internal(child,crmout,wi,indent+1,ptype,cxt);
                    CHECKP(child);
                    child=nodeGetRightSibling(child);
                    if (child==XNULL)  break;
                    CHECKP(child);
                } while (getNodeType(child)==attribute);
                if (ptype==sxml )  crmout << ")";
            }

            if (child==XNULL)
            {
                crmout << ((ptype==xml )? "/>": ")");
                //return;
                goto nsfree;
            }
            else
                crmout<< ((ptype==xml )? ">": "");

            while (child!=XNULL)
            {
                CHECKP(child);
                bool cit=(getNodeType(child)==text);
                if (cit)
                    curwi=false;
                else
                {
                    if (!lit) curwi=wi;
                }
                print_node_internal(child,crmout,curwi,indent+1,ptype,cxt);

                CHECKP(child);
                child=nodeGetRightSibling(child);
                lit=cit;
            }
            if(curwi||(wi && !lit))
            {
                crmout<< "\n";
                print_indent(crmout,indent) ;
            }
            if (ptype==xml )
            {
                crmout << "</";
                if (scn->get_xmlns() != NULL && strcmp(nsptr->prefix, "") != 0)
                    crmout<<scn->get_xmlns()->prefix<<":";
                crmout<< scn->get_name() <<">";
            }
            else
                crmout <<")";
            //namespaces remove
nsfree:
            if (att_ns)
            {
                std::vector<ns_pair>::const_iterator it=att_ns->begin();
                while(it!=att_ns->end())
                {
                    xm_nsp.erase(*it);
                    it++;
                }
                delete att_ns;
            }
            if (pref_ns)
            {
                std::vector<std::string>::const_iterator it=pref_ns->begin();
                while(it!=pref_ns->end())
                {
                    nspt_pref.erase(*it);
                    it++;
                }
                delete pref_ns;
            }
            if (def_inset)
                def_set=!def_set;

            break;
        }
    case xml_namespace:
        {
            print_namespace(NSNode(node).getNamespaceLocal(),crmout,ptype);
            break;
        }

    case attribute:
        {
            schema_node_cptr scn=getSchemaNode(node);
            if (ptype==xml )
            {

                crmout <<" ";
                print_attribute_prefix(crmout,scn,indent);
                crmout<< scn->name << "=\"";
            }
            else
            {
                crmout <<" (";
                print_attribute_prefix(crmout,scn,indent);
                crmout<< scn->name <<"  ";
            }
            print_text(node,crmout,ptype,attribute);
            crmout <<((ptype==xml )?"\"":")");
            return;
        }
    case text:
        {
            if(wi&&indent)
            {
                crmout<< "\n";
                print_indent(crmout,indent) ;
            }
            print_text(node,crmout,ptype,text);
            break;
        }
    case comment:
        {
            if(wi&&indent)
            {
                crmout<< "\n";
                print_indent(crmout,indent) ;
            }
            crmout<< "<!--";
            print_text(node,crmout,ptype,comment);
            crmout<< "-->";
            break;
        }
    case pr_ins:
        {
            if(wi&&indent)
            {
                crmout<< "\n";
                print_indent(crmout,indent) ;
            }
            crmout<< "<?";
            print_text(node,crmout,ptype,pr_ins);
            crmout<< "?>";
            break;
        }
    }
}

static void print_node(xptr node, se_ostream& crmout, t_print ptype, dynamic_context *cxt)
{
    CHECKP(node);
    print_node_internal(node,crmout,false,0,ptype,cxt);
}

static void print_node_indent(xptr node, se_ostream& crmout, t_print ptype, dynamic_context *cxt)
{
    CHECKP(node);
    print_node_internal(node,crmout,true,0,ptype,cxt);
}

void
print_tuple_internal  (const tuple &tup,     /* tuple to print */
                       se_ostream& crmout,   /* output strem to print into */
                       dynamic_context *cxt, /* context to get namespaces */
                       t_print ptype,        /* xml, sxml, etc ... */
                       bool is_first,        /* is item first in result */
                       bool ind) {           /* server indents result items*/

    if (tup.is_eos()) return;
    if (is_first) is_atomic=false;
    else
        if (ind ) crmout<<"\n";
    for (int i=0;i<tup.cells_number;i++)
    {
        if (tup.cells[i].is_node())
        {
            (ind)? print_node_indent(tup.cells[i].get_node(),crmout,ptype,cxt):print_node(tup.cells[i].get_node(),crmout,ptype,cxt);
            is_atomic=false;
        }
        else
        {
            if (!ind && is_atomic) crmout<<" ";
            is_atomic=true;
            if (tup.cells[i].is_light_atomic())
            {
                if (is_fixed_size_type(tup.cells[i].get_atomic_type()))
                {
                    get_lexical_representation_for_fixed_size_atomic(executor_globals::mem_str_buf2, tup.cells[i], ptype);
                    crmout.writextext(executor_globals::mem_str_buf2,strlen(executor_globals::mem_str_buf2));
                }
                else
                {
                    if (tup.cells[i].get_atomic_type() == xs_QName)
                    {
                        const char *prefix = xs_QName_get_prefix(tup.cells[i].get_str_mem());
                        if (prefix && strlen(prefix) != 0)
                        {
                            crmout.writextext(prefix, strlen(prefix));
                            crmout.writextext(":", 1);
                        }
                        crmout.writextext((char*)xs_QName_get_local_name(tup.cells[i].get_str_mem()),
                            strlen(xs_QName_get_local_name(tup.cells[i].get_str_mem())));
                    }
                    else crmout.writextext(tup.cells[i].get_str_mem(), tup.cells[i].get_strlen_mem());
                }
            }
            else
            {
                print_tuple_cell(crmout,tup.cells[i]);
            }

            tr_globals::client->get_serialization_params()->stm.flush(write_func,&crmout);
        }

        if (ind && i<(tup.cells_number-1)) crmout<<" ,";
    }
}

void
print_tuple           (const tuple &tup,     /* tuple to print */
                       se_ostream& crmout,   /* output strem to print into */
                       dynamic_context *cxt, /* context to get namespaces */
                       t_print ptype,        /* xml, sxml, etc ... */
                       bool is_first,        /* is item first in result */
                       bool indent) {        /* server indents result items*/
    print_tuple_internal(tup, crmout, cxt, ptype, is_first, indent);
}



///////////////////////////////////////////////////////////////////////////////
/// Full text search printings
///////////////////////////////////////////////////////////////////////////////

#ifdef SE_ENABLE_FTSEARCH
void print_name_space(xmlns_ptr nsd,op_str_buf& tbuf,ft_index_type type)
{
    switch (type)
    {
    case ft_xml:
    case ft_xml_ne:
    case ft_xml_hl:
        {
            if (nsd->prefix==NULL)
                tbuf<<" xmlns=\""<< nsd->uri<<"\"";
            else
                tbuf<<" xmlns:"<<nsd->prefix<<"=\""<<nsd->uri<<"\"";
            break;
        }
    case ft_string_value:
        {
            tbuf<<  nsd->uri;
            break;
        }
    case ft_delimited_value:
        {
            tbuf<<" "<< nsd->uri;
            break;
        }
    default:
        throw USER_EXCEPTION2(SE1003, "Unexpected full text index type in print namespace");
    }
}

static StrMatcher *escape_sm = NULL;
static void make_escape_sm()
{
    //TODO: assert escape_sm == NULL
    escape_sm = se_new StrMatcher();
    escape_sm->add_str("&", "&amp;", ~pat_attribute);
    escape_sm->add_str("<", "&lt;", ~pat_attribute);
    escape_sm->add_str(">", "&gt;", ~pat_attribute);
    escape_sm->add_str("\"", "&quot;", pat_attribute);
    escape_sm->add_str("'", "&apos;", pat_attribute);
    escape_sm->add_str("\"", "\xEE\xA0\x83", pat_custom1);
}

static void tbuf_write_cb(void *param, const char *str, int len)
{
    op_str_buf* tbuf = (op_str_buf*)param;
    tbuf->append(str, len);
}

static void print_text(xptr txt, op_str_buf& tbuf, t_item xq_type, bool escapes = true)
{
    CHECKP(txt);
    if (CommonTextNode(txt).isEmpty()) { return; }
    tuple_cell tc=tuple_cell::atomic_text(txt);
    if (!escapes)
    {
        if (xq_type == attribute)
        {
            if (escape_sm == NULL)
                make_escape_sm();
            escape_sm->parse_tc(&tc, tbuf_write_cb, &tbuf, pat_custom1);
            escape_sm->flush(tbuf_write_cb, &tbuf);
        }
        else
            tbuf.append(tc);
    }
    else
    {
        if (escape_sm == NULL)
            make_escape_sm();
        if (xq_type!=text && xq_type!=attribute)
            tbuf.append(tc);
        else if (xq_type == attribute)
        {
            escape_sm->parse_tc(&tc, tbuf_write_cb, &tbuf, pat_attribute);
            escape_sm->flush(tbuf_write_cb, &tbuf);
        }
        else
        {
            escape_sm->parse_tc(&tc, tbuf_write_cb, &tbuf, pat_element);
            escape_sm->flush(tbuf_write_cb, &tbuf);
        }
    }
}

void print_node_to_buffer(xptr node,op_str_buf& tbuf,ft_index_type type,ft_custom_tree_t * custom_tree, const char *opentag, const char *closetag)
{
	CHECKP(node);
    switch(getNodeType(node))
    {
    case document: case virtual_root:
        {
            switch (type)
            {
            case ft_xml:case ft_xml_ne:case ft_xml_hl: tbuf<<opentag<<"?xml version=\"1.0\" standalone=\"yes\" encoding=\"utf-8\""; break;
            case ft_string_value:break;
            case ft_customized_value: case ft_delimited_value: tbuf<<" ";break;
            }
            CHECKP(node);
            xptr child=getFirstChild(node);
            if(child==XNULL)
            {
                if (type==ft_xml || type==ft_xml_ne || type==ft_xml_hl) tbuf<<"?"<<closetag;
                return;
            }
            else CHECKP(child);
            while (getNodeType(child)==attribute)
            {

                if (type==ft_xml || type==ft_xml_ne || type==ft_xml_hl) print_node_to_buffer(child,tbuf,type,custom_tree,opentag,closetag);
                CHECKP(child);
                child=nodeGetRightSibling(child);
                if (child==XNULL) break;
                CHECKP(child);
            }
            if (type==ft_xml || type==ft_xml_ne || type==ft_xml_hl) tbuf<<"?"<<closetag;
            while (child!=XNULL)
            {
                CHECKP(child);
                print_node_to_buffer(child,tbuf,type,custom_tree,opentag,closetag);
                CHECKP(child);
                child=nodeGetRightSibling(child);
            }
            break;
        }
    case element:
        {
            schema_node_cptr scn=getSchemaNode(node);
            if (custom_tree!=NULL)
            {
                ft_custom_tree_t::sedna_rbtree_entry* scget= custom_tree->get(scn->name,scn->get_xmlns());
                if (scget!=NULL) type=scget->obj->cm;
                else
                    if (type==ft_customized_value) type=ft_xml;
            }
            switch (type)
            {
                case ft_xml: case ft_xml_ne:tbuf<<opentag; break;
                case ft_xml_hl: tbuf<<opentag; break;
                case ft_string_value:break;
                case ft_delimited_value:tbuf<<" ";break;
                default:
                    throw USER_EXCEPTION2(SE1003, "Unexpected full text index type in print element node");
            }
            //std::vector<std::string> *att_ns=NULL;
            if (scn->get_xmlns()!=NULL_XMLNS && scn->get_xmlns()->prefix!=NULL && *(scn->get_xmlns()->prefix))
                if (type==ft_xml || type==ft_xml_ne || type==ft_xml_hl) tbuf<<scn->get_xmlns()->prefix<<":";
            if (type==ft_xml || type==ft_xml_ne) tbuf<<scn->get_name();
            if (type==ft_xml_hl) tbuf<<"a";
            CHECKP(node);
            xptr child=getFirstChild(node);
            if(child==XNULL)
            {
                if (type==ft_xml || type==ft_xml_ne) tbuf<<"/"<<closetag;
                if (type==ft_xml_hl) tbuf<<"/"<<closetag<<" ";
                return;
            }
            else
                CHECKP(child);
            while (getNodeType(child)==attribute)
            {
                if (type==ft_xml || type==ft_xml_ne || type==ft_xml_hl) print_node_to_buffer(child,tbuf,type,custom_tree,opentag,closetag);
                CHECKP(child);
                child=nodeGetRightSibling(child);
                if (child==XNULL)  break;
                CHECKP(child);
            }
            if (child==XNULL)
            {
                if (type==ft_xml || type==ft_xml_ne) tbuf<<"/"<<closetag;
                if (type==ft_xml_hl) tbuf<<"/"<<closetag<<"  ";
                return;

            }
            else
            {
                if (type==ft_xml || type==ft_xml_ne) tbuf<<closetag;
                if (type==ft_xml_hl) tbuf<<closetag<<" ";
            }
            bool cit=false;
            while (child!=XNULL)
            {
                CHECKP(child);
                cit=(getNodeType(child)==element);
                print_node_to_buffer(child,tbuf,type,custom_tree,opentag,closetag);
                CHECKP(child);
                child=nodeGetRightSibling(child);
            }
            if (type==ft_xml || type==ft_xml_ne) tbuf<<opentag<<"/";
            else
                if (type==ft_xml_hl) tbuf<<opentag<<"/";
                else
                    if (type==ft_delimited_value && !cit) tbuf<<" ";
            if (scn->get_xmlns()!=NULL_XMLNS && scn->get_xmlns()->prefix!=NULL && *(scn->get_xmlns()->prefix))
                if (type==ft_xml || type==ft_xml_ne || type==ft_xml_hl) tbuf<<scn->get_xmlns()->prefix<<":";
            if (type==ft_xml || type==ft_xml_ne) tbuf<<scn->get_name()<<closetag;
            if (type==ft_xml_hl) tbuf<<"a"<<closetag<<" ";
            break;
        }
    case xml_namespace:
        {
            print_name_space(NSNode(node).getNamespaceLocal(), tbuf, type);
            break;
        }
    case attribute:
        {
            schema_node_cptr scn=getSchemaNode(node);
            switch (type)
            {
            case ft_xml:
            case ft_xml_ne:
            case ft_xml_hl:
                {
                    if (scn->get_xmlns()!=NULL_XMLNS && scn->get_xmlns()->prefix!=NULL && *(scn->get_xmlns()->prefix))
                        tbuf <<" "<<scn->get_xmlns()->prefix<<":"<< scn->name << "=\"";
                    else
                        tbuf <<" "<< scn->name << "=\"";
                    CHECKP(node);
                    print_text(node,tbuf,attribute,type!=ft_xml_ne);
                    tbuf <<"\"";
                    break;
                }
            case ft_string_value:print_text(node,tbuf,attribute);break;
            case ft_delimited_value:tbuf<<" ";break;
            default:
                throw USER_EXCEPTION2(SE1003, "Unexpected full text index type in print attribute node");
            }
            return;
        }
    case text:
        {
            print_text(node,tbuf,text,type!=ft_xml_ne);
            break;
        }
    case comment:
        {
            switch (type)
            {
            case ft_xml:case ft_xml_ne:case ft_xml_hl: tbuf<< opentag<<"!--"; break;
            case ft_string_value:break;
            case ft_delimited_value:tbuf<<" ";break;
            default:
                throw USER_EXCEPTION2(SE1003, "Unexpected full text index type in print comment node");
            }
            CHECKP(node);
            print_text(node,tbuf,text,type!=ft_xml_ne);
            if (type==ft_xml || type==ft_xml_ne || type==ft_xml_hl) tbuf<< "--" << closetag;
            break;
        }
    case pr_ins:
        {
            switch (type)
            {
            case ft_xml:case ft_xml_ne:case ft_xml_hl: tbuf<< opentag<<"?"; break;
            case ft_string_value:break;
            case ft_delimited_value:tbuf<<" ";break;
            default:
                throw USER_EXCEPTION2(SE1003, "Unexpected full text index type in print processing instruction node");
            }
            CHECKP(node);
            print_text(node,tbuf,pr_ins);
            if (type==ft_xml || type==ft_xml_ne || type==ft_xml_hl) tbuf<< "?"<<closetag;
            break;
        }
    }
}
#endif /* SE_ENABLE_FTSEARCH */

///////////////////////////////////////////////////////////////////////////////
/// Print physical operations stack
///////////////////////////////////////////////////////////////////////////////

void print_pp_stack(se_ostream* dostr)
{
    const char* indent = "  ";
    std::ostringstream message;
    message << "<stack xmlns='" << SEDNA_NAMESPACE_URI << "'>" << std::endl;

    while(!executor_globals::pp_stack.empty()) {
        const operation_info& oi = executor_globals::pp_stack.back();
        executor_globals::pp_stack.pop_back();
        message << indent << "<operation name='" << oi.name;
        if(oi.query_line != 0)
            message << "' line='" << oi.query_line;
        if(oi.query_col != 0)
            message << "' column='" << oi.query_col;
        message << "' calls='" << oi.profile->calls;
        message << "'/>" << std::endl;
    }

    message << "</stack>" << std::endl;

    dostr->set_debug_info_type(se_QueryDebug);
    (*dostr) << message.str().c_str();
    dostr->flush();
}

///////////////////////////////////////////////////////////////////////////////
/// Legacy metadata printings (should be removed in the future releases)
///////////////////////////////////////////////////////////////////////////////

/* prints information in  schema node */
void print_descriptive(schema_node_cptr node, se_ostream& crmout, int indent)
{
    crmout << "\n";
    print_indent(crmout,indent);
    crmout << "<NODE ";// << node;
    if (node->name!=NULL)
        crmout << "local_name=\"" << node->name <<"\"";
    if (node->get_xmlns()!=NULL_XMLNS)
    {
        crmout << " prefix=\"" << node->get_xmlns()->prefix <<"\"";
        crmout << " uri=\"" << node->get_xmlns()->uri <<"\"";
    }
    crmout << " type=\"" <<type2string(node->type)<<"\"";
    crmout << " nodes_count=\"" <<node->nodecnt<<"\"";
    crmout << " block_count=\"" <<node->blockcnt<<"\"";
    crmout << " ext_nid_size=\"" <<node->extnids<<"\"";
    if (node->children->empty())
    {
        crmout << "/>";
        return;
    }
    else
        crmout << ">\n";

    sc_ref_item * sc;
    for (sc = node->children->first; sc != NULL; sc = sc->next)
    {
        print_descriptive(sc->object.snode, crmout, indent+1);
    }
    crmout << "</NODE>";
}
/* prints descriptive schema  of stand-alone document*/
void print_descriptive_schema(const char * docname, se_ostream& crmout)
{
    if (docname==NULL)
        throw USER_EXCEPTION(SE2006);
    crmout << "<?xml version=\"1.0\" standalone=\"yes\"?>";
    crmout << "\n<TREE_DESCRIPTIVE_SCHEMA document=\"";
    crmout<<docname;
    crmout<<"\">";
    //metadata_sem_down();
    schema_node_xptr sn=find_document(docname);
    //metadata_sem_up();
    if (sn!=XNULL)print_descriptive(sn,crmout,0);
    crmout << "\n</TREE_DESCRIPTIVE_SCHEMA>";

}

/* prints descriptive schema  of collection*/
void print_descriptive_schema_col(const char * colname, se_ostream& crmout)
{
    if (colname==NULL)
        throw USER_EXCEPTION(SE2003);
    crmout << "<?xml version=\"1.0\" standalone=\"yes\"?>";
    crmout << "\n<XML_DESCRIPTIVE_SCHEMA collection=\"";
    crmout<<colname;
    crmout<<"\">";
    //metadata_sem_down();
    schema_node_xptr sn=find_collection(colname);
    //metadata_sem_up();
    if (sn!=XNULL)print_descriptive(sn,crmout,0);
    crmout << "\n</XML_DESCRIPTIVE_SCHEMA>";
}

/* prints information in  schema node */
void sxml_print_descriptive(schema_node_cptr node, se_ostream& crmout, int indent)
{
    crmout << " (NODE (@";
    if (node->name!=NULL)
        crmout << " (local_name \"" << node->name <<"\")";
    if (node->get_xmlns() != NULL_XMLNS)
    {
        crmout << " (prefix \"" << node->get_xmlns()->prefix <<"\")";
        crmout << " (uri \"" << node->get_xmlns()->uri <<"\")";
    }
    crmout << " (type \"" <<type2string(node->type)<<"\")";

    crmout << ")";   // closing just the attr-list
    sc_ref_item *sc= node->children->first;
    while (sc!=NULL)
    {
        sxml_print_descriptive(sc->object.snode, crmout, indent+1);
        sc=sc->next;
    }
    crmout << ")";
}
/* prints descriptive schema  of stand-alone document*/
void sxml_print_descriptive_schema(const char * docname, se_ostream& crmout)
{
    if (docname==NULL)
        throw USER_EXCEPTION(SE2006);
    crmout << "(*TOP* (*PI* xml \"version=\\\"1.0\\\" standalone=\\\"yes\\\"\")";
    crmout << " (TREE_DESCRIPTIVE_SCHEMA (@ (document \"";
    crmout<<docname;
    crmout<<"\"))";
    //metadata_sem_down();
    schema_node_xptr sn=find_document(docname);
    //metadata_sem_up();
    if (sn!=XNULL) sxml_print_descriptive(sn,crmout,0);
    crmout << "))";  // end tag for </TREE_DESCRIPTIVE_SCHEMA> and *TOP*

}
/* prints descriptive schema  of collection*/
void sxml_print_descriptive_schema_col(const char * colname, se_ostream& crmout)
{
    if (colname==NULL)
        throw USER_EXCEPTION(SE2003);
    crmout << "(*TOP* (*PI* xml \"version=\\\"1.0\\\" standalone=\\\"yes\\\"\")";
    crmout << " (XML_DESCRIPTIVE_SCHEMA (@ (collection \"";
    crmout<<colname;
    crmout<<"\"))";   // end attr-list
    //metadata_sem_down();
    schema_node_xptr sn=find_collection(colname);
    //metadata_sem_up();
    if (sn!=XNULL) sxml_print_descriptive(sn,crmout,0);
    crmout << "))";    // end-tag for </XML_DESCRIPTIVE_SCHEMA> and *TOP*
}


/* prints the list of documents*/
void print_documents(se_ostream& crmout, bool ps)
{
    crmout << "<?xml version=\"1.0\" standalone=\"yes\"?>";
    crmout << "\n<XML_DOCUMENTS>";

    metadata_cell_cptr mdc = XNULL;
    catalog_iterator it(catobj_metadata);

    while (it.next())
    {
        mdc = it.get_object();

        if (mdc->is_doc)
        {
            crmout<<"\n<Document name=\""<<mdc->name<<"\"";
            crmout<<"/>";
        }
    }
    crmout << "\n</XML_DOCUMENTS>";
}

/* prints the list of documents in the selected collection*/
void print_documents_in_collection(se_ostream& crmout,const char* collection)
{
    crmout << "<?xml version=\"1.0\" standalone=\"yes\"?>";
    crmout << "\n<XML_DOCUMENTS_Collection collection=\"";
    crmout<<collection;
    crmout<<"\">";

    metadata_cell_cptr mdc(collection);
    if (mdc.found())
    {
        col_schema_node_cptr coll=mdc->snode;
        bt_cursor cursor=bt_lm(coll->metadata);
        if(!cursor.is_null())
        {
            do
            {
                crmout<<"\n<Document name=\""<<(char*)cursor.get_key().data()<<"\"";
                crmout<<"/>";
            }
            while (cursor.bt_next_key());
        }
    }

    crmout << "\n</XML_DOCUMENTS_Collection>";
}

/* prints the list of collections*/
void print_collections(se_ostream& crmout, bool ps)
{
    crmout << "<?xml version=\"1.0\" standalone=\"yes\"?>";
    crmout << "\n<COLLECTIONS>";

    metadata_cell_cptr mdc = XNULL;
    catalog_iterator it(catobj_metadata);

    while (it.next())
    {
        mdc = it.get_object();

        if (!mdc->is_doc)
        {
            crmout<<"\n <Collection name=\""<<mdc->name<<"\"/>";
        }
    }
    crmout << "\n</COLLECTIONS>";
}
