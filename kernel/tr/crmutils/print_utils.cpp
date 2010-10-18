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

/*
typedef std::pair<std::string, std::string> ns_pair;
typedef std::map<ns_pair, xmlns_ptr> nspt_map;
*/

///////////////////////////////////////////////////////////////////////////////
/// Static helpers and variables
///////////////////////////////////////////////////////////////////////////////

/*
static nspt_map               xm_nsp;
static bool                   def_set   =false;
static bool                   is_atomic =false;
static std::set<std::string>  nspt_pref;
*/

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
    escape_sm->add_str("&", "&amp;", ~pat_xml_attribute);
    escape_sm->add_str("<", "&lt;", ~pat_xml_attribute);
    escape_sm->add_str(">", "&gt;", ~pat_xml_attribute);
    escape_sm->add_str("\"", "&quot;", ~pat_xml_attribute);
    escape_sm->add_str("'", "&apos;", ~pat_xml_attribute);
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
            escape_sm->parse_tc(&tc, tbuf_write_cb, &tbuf, pat_xml_attribute);
            escape_sm->flush(tbuf_write_cb, &tbuf);
        }
        else
        {
            escape_sm->parse_tc(&tc, tbuf_write_cb, &tbuf, pat_xml_element);
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
//    print_indent(crmout,indent);
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
