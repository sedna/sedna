/*
 * File:  system_tables.cpp
 * Copyright (C) 2007 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <sstream>

#include "common/sedna.h"
#include "common/u/uutils.h"

#include "tr/structures/system_tables.h"
#include "tr/structures/schema.h"
#include "tr/structures/metadata.h"
#include "tr/crmutils/crmutils.h"
#include "tr/mo/micro.h"
#include "tr/locks/locks.h"
#include "tr/vmm/vmm.h"
#include "tr/idx/index_data.h"
#include "tr/structures/indirection.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/idx/btree/btstruct.h"
#include "tr/idx/btree/btree.h"
#include "tr/triggers/triggers_data.h"
#include "tr/cat/catenum.h"

#ifdef SE_ENABLE_FTSEARCH
#include "tr/ft/ft_index_data.h"
#endif

extern xptr TMPNIDBLK; /* current temporary block for nid prefixes, defined in numb_scheme.cpp */

typedef void (*system_fun)(xptr root, const char* title);
static std::vector<schema_node_xptr>* sys_schema=NULL;

inline void print_type_name(xmlscm_type keytype, char* buf)
{
    strcpy(buf, xmlscm_type2c_str(keytype));
}

xptr fill_schema(schema_node_cptr scm, xptr& node,xptr& neighb)
{
    xptr parent=insert_element(neighb,XNULL,node,type2string(scm->type),xs_untyped,NULL_XMLNS);
    xptr indir=((n_dsc*)XADDR(parent))->indir;
    xptr left =insert_attribute(XNULL,XNULL,parent,"name",xs_untypedAtomic,scm->name,(scm->name==NULL)?0:strlen(scm->name),NULL_XMLNS);
    if (scm->get_xmlns()!=NULL)
    {
        left=insert_attribute(left,XNULL,XNULL,"prefix",xs_untypedAtomic,scm->get_xmlns()->prefix,(scm->get_xmlns()->prefix==NULL)?0:strlen(scm->get_xmlns()->prefix),NULL_XMLNS);
        left=insert_attribute(left,XNULL,XNULL,"uri",xs_untypedAtomic,scm->get_xmlns()->uri,(scm->get_xmlns()->uri==NULL)?0:strlen(scm->get_xmlns()->uri),NULL_XMLNS);
    }
    char buf[20];
    u_itoa(scm->nodecnt,buf,10);
    left=insert_attribute(left,XNULL,XNULL,"total_nodes",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
    u_itoa(scm->blockcnt,buf,10);
    left=insert_attribute(left,XNULL,XNULL,"total_blocks",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
    u_itoa(scm->extnids,buf,10);
    left=insert_attribute(left,XNULL,XNULL,"total_ext_nids",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
    u_itoa(scm->indir_blk_cnt,buf,10);
    left=insert_attribute(left,XNULL,XNULL,"total_indir_blocks",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
    #ifdef WIN32
    _i64toa(scm->textcnt,buf,10);
    #else
    sprintf(buf,"%lld",scm->textcnt);
    #endif
    left=insert_attribute(left,XNULL,XNULL,"total_text",xs_untypedAtomic,buf,strlen(buf),NULL_XMLNS);
    sc_ref_item * sc= scm->children.first;
    while (sc!=NULL)
    {
        left=fill_schema(sc->object.snode,XNULL,left);
        sc=sc->next;
    }
    return removeIndirection(indir);
}

void get_schema(xptr node,const char* title)
{
    addTextValue(node,"$SCHEMA.XML",12);
    xptr parent=insert_element(XNULL,XNULL,node,"schema",xs_untyped,NULL_XMLNS);
    xptr left=XNULL;

    metadata_cell_cptr mdc = XNULL;
    catalog_iterator it(catobj_metadata);

    while (it.next())
    {
        mdc = it.get_object();

        if (!mdc.found()) {
          // This is the case when the entity is deleted in this transaction, but is pointed to from the tree
          continue;
        } 

        if (title==NULL || my_strcmp(title, mdc->name)==0)
        {
            if (left==XNULL)
            {
                left=insert_element(XNULL,XNULL,parent,(mdc->is_doc)?"document":"collection",xs_untyped,NULL_XMLNS);
            }
            else
                left=insert_element(left,XNULL,XNULL,(mdc->is_doc)?"document":"collection",xs_untyped,NULL_XMLNS);
            xptr cd=insert_attribute(XNULL,XNULL,left,"name",xs_untypedAtomic, mdc->name,
                strlen(mdc->name),NULL_XMLNS);
            fill_schema(mdc->snode,left,cd);
        }
    }
}

void get_document_full (xptr node,const char* title)
{
    char* docn=se_new char[11+strlen(title)];
    docn[0]='\0';
    strcat(docn,"$DOCUMENT_");
    strcat(docn,title);
    addTextValue(node,docn,strlen(docn));
    delete [] docn;
    xptr parent=insert_element(XNULL,XNULL,node,"document",xs_untyped,NULL_XMLNS);
    insert_attribute(XNULL,XNULL,parent,"name",xs_untypedAtomic,title,strlen(title),NULL_XMLNS);
    schema_node_xptr scn=find_document(title);  
    if (scn != XNULL) getDebugInfo(scn, parent);
}

void get_collection_full (xptr node,const char* title)
{
    char* docn=se_new char[13+strlen(title)];
    docn[0]='\0';
    strcat(docn,"$COLLECTION_");
    strcat(docn,title);
    addTextValue(node,docn,strlen(docn));
    delete [] docn;
    xptr parent=insert_element(XNULL,XNULL,node,"collection",xs_untyped,NULL_XMLNS);
    insert_attribute(XNULL,XNULL,parent,"name",xs_untypedAtomic,title,strlen(title),NULL_XMLNS);
    schema_node_xptr scn=find_collection(title);    
    if (scn != XNULL) getDebugInfo(scn, parent);
}


document_type get_document_type(const char* title, db_entity_type type)
{
    if(title == NULL || title[0] != '$') return DT_NON_SYSTEM;

    if(type == dbe_document)
    {
        if(!my_strcmp(title, "$documents"))       return DT_DOCUMENTS;
        if(!my_strcmp(title, "$collections"))     return DT_COLLECTIONS;
        if(!my_strcmp(title, "$schema"))          return DT_SCHEMA;
        if(!my_strcmp(title, "$indexes"))         return DT_INDEXES;
        if(!my_strcmp(title, "$version"))         return DT_VERSION;
#ifdef SE_ENABLE_FTSEARCH
        if(!my_strcmp(title, "$ftindexes"))       return DT_FTINDEXES;
#endif
#ifdef SE_ENABLE_TRIGGERS
        if(!my_strcmp(title, "$triggers"))       return DT_TRIGGERS;
#endif
        if(!my_strcmp(title, "$errors"))          return DT_ERRORS;
        if(strstr(title, "$collection_")==title)  return DT_COLLECTION_;
        if(strstr(title, "$document_")==title)    return DT_DOCUMENT_;
        if(strstr(title, "$schema_")==title)      return DT_SCHEMA_;
    }

    return DT_NON_SYSTEM;
}

document_type get_document_type(counted_ptr<db_entity> db_ent)
{
    return get_document_type(db_ent->name, db_ent->type);
}

void get_version(xptr node,const char* title)
{
    addTextValue(node,"$VERSION.XML",12);
    xptr parent=insert_element(XNULL,XNULL,node,"sedna",xs_untyped,NULL_XMLNS);
    insert_attribute(XNULL,XNULL,parent,"version",xs_untypedAtomic,SEDNA_VERSION,
                        strlen(SEDNA_VERSION),NULL_XMLNS);
    insert_attribute(XNULL,XNULL,parent,"build",xs_untypedAtomic,SEDNA_BUILD,
                        strlen(SEDNA_BUILD),NULL_XMLNS);
    
    
}

void get_errors(xptr node,const char* title)
{
    addTextValue(node,"$ERRORS.XML",12);
    xptr parent=insert_element(XNULL,XNULL,node,"errors",xs_untyped,NULL_XMLNS);
    xptr left=XNULL;
    for (int i=0;i<=SE5100;i++)
    {
        if (left==XNULL)
        {
            left=insert_element(XNULL,XNULL,parent,"error",xs_untyped,NULL_XMLNS);
        }
        else
            left=insert_element(left,XNULL,XNULL,"error",xs_untyped,NULL_XMLNS);
        
        insert_attribute(XNULL,XNULL,left,"code",xs_untypedAtomic,user_error_code_entries[i].code,
                        strlen(user_error_code_entries[i].code),NULL_XMLNS);
        insert_attribute(XNULL,XNULL,left,"roll_back",xs_untypedAtomic,(user_error_code_entries[i].act==ueca_ROLLBACK_TRN)?"y":"n",
                        1,NULL_XMLNS);
        insert_text(XNULL,XNULL,left,user_error_code_entries[i].descr,
                        strlen(user_error_code_entries[i].descr));

    }
}

void get_indexes (xptr node,const char* title)
{
    addTextValue(node,"$INDEXES.XML",12);
    xptr parent=insert_element(XNULL,XNULL,node,"indexes",xs_untyped,NULL_XMLNS);
    xptr left=XNULL;

    local_lock_mrg->put_lock_on_db();
    static char buf[512];

    index_cell_cptr ic = XNULL;
    catalog_iterator it(catobj_indicies);

    while (it.next())
    {
        ic = it.get_object();

        if (!ic.found()) {
          // This is the case when the entity is deleted in this transaction, but is pointed to from the tree
          continue;
        } 

        if (left==XNULL)
        {
            left=insert_element(XNULL,XNULL,parent,"index",xs_untyped,NULL_XMLNS);
        }
        else
            left=insert_element(left,XNULL,XNULL,"index",xs_untyped,NULL_XMLNS);
        
        xptr node=insert_attribute(XNULL,XNULL,left,"name",xs_untypedAtomic,ic->index_title,
                        strlen(ic->index_title),NULL_XMLNS);
        node=insert_attribute(node,XNULL,XNULL,"object_type",xs_untypedAtomic,(ic->is_doc)?"document":"collection", 
                        (ic->is_doc)?8:10,NULL_XMLNS);
        node=insert_attribute(node,XNULL,XNULL,"object_name",xs_untypedAtomic,ic->doc_name,
            strlen(ic->doc_name),NULL_XMLNS);
        print_type_name(ic->keytype,buf);
        node=insert_attribute(node,XNULL,XNULL,"as_type",xs_untypedAtomic,buf, strlen(buf),NULL_XMLNS);
        std::ostringstream str1, str2;
        ic->object->print(str1);
        node=insert_attribute(node,XNULL,XNULL,"on_path",xs_untypedAtomic,str1.str().c_str(), strlen(str1.str().c_str()),NULL_XMLNS);
        ic->key->print(str2);
        node=insert_attribute(node,XNULL,XNULL,"by_path",xs_untypedAtomic,str2.str().c_str(),
            strlen(str2.str().c_str()),NULL_XMLNS);
    }
}

#ifdef SE_ENABLE_TRIGGERS
void get_triggers (xptr node,const char* title)
{
    addTextValue(node,"$TRIGGERS.XML",13);
    xptr parent=insert_element(XNULL,XNULL,node,"triggers",xs_untyped,NULL_XMLNS);
    xptr left=XNULL;
    local_lock_mrg->put_lock_on_db();

    trigger_cell_cptr tc = XNULL;
    catalog_iterator it(catobj_triggers);

    while (it.next())
    {
        tc = it.get_object();

        if (!tc.found()) {
          // This is the case when the entity is deleted in this transaction, but is pointed to from the tree
          continue;
        } 

        if (left==XNULL) {
            left=insert_element(XNULL,XNULL,parent,"trigger",xs_untyped,NULL_XMLNS);
        } else 
            left=insert_element(left,XNULL,XNULL,"trigger",xs_untyped,NULL_XMLNS);

        xptr node=insert_attribute(XNULL,XNULL,left,"name",xs_untypedAtomic,tc->trigger_title,
                        strlen(tc->trigger_title),NULL_XMLNS);
        node=insert_attribute(node,XNULL,XNULL,"object_type",xs_untypedAtomic,(tc->is_doc)?"document":"collection", 
                        (tc->is_doc)?8:10,NULL_XMLNS);
        node=insert_attribute(node,XNULL,XNULL,"object_name",xs_untypedAtomic,tc->doc_name,
            strlen(tc->doc_name),NULL_XMLNS);

        std::string trigger_event;
        (tc->trigger_event == TRIGGER_INSERT_EVENT) ? trigger_event="INSERT" : ((tc->trigger_event == TRIGGER_DELETE_EVENT) ? trigger_event="DELETE" : trigger_event="REPLACE");
        node=insert_attribute(node,XNULL,XNULL,"event", xs_untypedAtomic, trigger_event.c_str(), trigger_event.length(), NULL_XMLNS);

        std::string trigger_time;
        (tc->trigger_time == TRIGGER_BEFORE) ? trigger_time="BEFORE" : trigger_time="AFTER";
        node=insert_attribute(node,XNULL,XNULL,"time", xs_untypedAtomic, trigger_time.c_str(), trigger_time.length(), NULL_XMLNS);

        std::string trigger_granularity;
        (tc->trigger_granularity == TRIGGER_FOR_EACH_NODE) ? trigger_granularity="FOR_EACH_NODE" : trigger_granularity="FOR_EACH_STATEMENT";
        node=insert_attribute(node,XNULL,XNULL,"granularity", xs_untypedAtomic, trigger_granularity.c_str(), trigger_granularity.length(), NULL_XMLNS);

        std::ostringstream str1;
        tc->trigger_path->print(str1);
        node=insert_attribute(node,XNULL,XNULL,"on_path",xs_untypedAtomic,str1.str().c_str(),
                strlen(str1.str().c_str()),NULL_XMLNS);
    }
}
#endif

#ifdef SE_ENABLE_FTSEARCH
void print_ft_type_name(ft_index_type ftype, char* buf)
{   
    switch(ftype)
    {
        case ft_xml : strcpy(buf,"xml");
        break;case ft_xml_hl    : strcpy(buf,"ft_xml_hl");
        break;case ft_string_value  : strcpy(buf,"string-value");
        break;case ft_delimited_value   : strcpy(buf,"delimited-value");
        break;case ft_customized_value  : strcpy(buf,"customized-value");
        break;default           : strcpy(buf,"unknown");
    }   
}
void get_ftindexes (xptr node,const char* title)
{
    addTextValue(node,"$FTINDEXES.XML",14);
    xptr parent=insert_element(XNULL,XNULL,node,"ftindexes",xs_untyped,NULL_XMLNS);
    xptr left=XNULL;
    local_lock_mrg->put_lock_on_db();

    ft_index_cell_cptr ic = XNULL;
    catalog_iterator it(catobj_ft_indicies);

    char buf[64];
    while (it.next())
    {
        ic = it.get_object();

        if (!ic.found()) {
          // This is the case when the entity is deleted in this transaction, but is pointed to from the tree
          continue;
        } 

        if (left==XNULL)
        {
            left=insert_element(XNULL,XNULL,parent,"ftindex",xs_untyped,NULL_XMLNS);
        }
        else
            left=insert_element(left,XNULL,XNULL,"ftindex",xs_untyped,NULL_XMLNS);
        
        xptr node=insert_attribute(XNULL,XNULL,left,"name",xs_untypedAtomic,ic->index_title,
                        strlen(ic->index_title),NULL_XMLNS);
        node=insert_attribute(node,XNULL,XNULL,"object_type",xs_untypedAtomic,(ic->is_doc)?"document":"collection",
                        (ic->is_doc)?8:10,NULL_XMLNS);
        node=insert_attribute(node,XNULL,XNULL,"object_name",xs_untypedAtomic,ic->doc_name,
            strlen(ic->doc_name),NULL_XMLNS);
        
        print_ft_type_name(ic->ftype,buf);
        node=insert_attribute(node,XNULL,XNULL,"ft_type",xs_untypedAtomic,buf, strlen(buf),NULL_XMLNS);
        std::ostringstream str1;
        ic->object->print(str1);
        node=insert_attribute(node,XNULL,XNULL,"on_path",xs_untypedAtomic,str1.str().c_str(),
            strlen(str1.str().c_str()),NULL_XMLNS);
        if (ic->ftype==ft_customized_value && ic->custom_tree!=NULL)
        {
            CHECKP(left);
            xptr indir=((n_dsc*)XADDR(left))->indir;
            ft_custom_tree_t::sedna_rbtree_entry* cdc=ic->custom_tree->rb_minimum(ic->custom_tree->root);
            xptr cleft=XNULL;
            while (cdc!=NULL)
            {
                ft_custom_cell* cc=cdc->obj;
                if (cleft==XNULL)
                {
                    cleft=insert_element(XNULL,XNULL,left,"template",xs_untyped,NULL_XMLNS);
                }
                else
                    cleft=insert_element(cleft,XNULL,XNULL,"template",xs_untyped,NULL_XMLNS);
                xptr node=insert_attribute(XNULL,XNULL,cleft,"element_name",xs_untypedAtomic,cc->local,
                    strlen(cc->local),NULL_XMLNS);
                if (cc->get_xmlns()!=NULL_XMLNS)
                {
                    node=insert_attribute(node,XNULL,XNULL,"ns_prefix",xs_untypedAtomic,cc->get_xmlns()->prefix,strlen(cc->get_xmlns()->prefix),NULL_XMLNS);
                    node=insert_attribute(node,XNULL,XNULL,"ns_uri",xs_untypedAtomic,cc->get_xmlns()->uri,strlen(cc->get_xmlns()->uri),NULL_XMLNS);
                }
                print_ft_type_name(cc->cm,buf);
                insert_attribute(node,XNULL,XNULL,"ft_type",xs_untypedAtomic,buf, strlen(buf),NULL_XMLNS);
                cdc=ic->custom_tree->rb_successor(cdc);
                left=removeIndirection(indir);
            }
        }
    }
}
#endif

/*static inline xptr insert_generated_document(const char* name, xptr parent, xptr left)
{
    xptr temp = insert_element(left,XNULL,parent,"document",xs_untyped,XNULL);
    temp = insert_attribute(XNULL,XNULL,temp,"name",xs_untypedAtomic,name,strlen(name),XNULL);
    return removeIndirection(GETPARENTPOINTER(temp));
}
*/

void get_documents (xptr node,const char* title)
{
    addTextValue(node,"$DOCUMENTS.XML",12);
    xptr parent=insert_element(XNULL,XNULL,node,"documents",xs_untyped,NULL_XMLNS);
    xptr left=XNULL;
    local_lock_mrg->put_lock_on_db();

    metadata_cell_cptr mdc = XNULL;
    catalog_iterator it(catobj_metadata);
    while (it.next())
    {
        mdc = it.get_object();

        if (!mdc.found()) {
          // This is the case when the entity is deleted in this transaction, but is pointed to from the tree
          continue;
        } 

        if (left==XNULL)
        {
            left=insert_element(XNULL,XNULL,parent,(mdc->is_doc)?"document":"collection",xs_untyped,NULL_XMLNS);
        }
        else
            left=insert_element(left,XNULL,XNULL,(mdc->is_doc)?"document":"collection",xs_untyped,NULL_XMLNS);

        xptr temp = insert_attribute(XNULL,XNULL,left,"name",xs_untypedAtomic,mdc->name,
                        strlen(mdc->name),NULL_XMLNS);
        ////////////////////////////////////////////////////////////////////////////
        /// We must renew left pointer due to insert_attribute possibly has side effect - 
        /// it can move parent of the new attribute to another block (Ivan Shcheklein).
        left = removeIndirection(GETPARENTPOINTER(temp));
        ////////////////////////////////////////////////////////////////////////////
        if (!mdc->is_doc)
        {
            col_schema_node_cptr coll = mdc->snode;
            xptr d_left=XNULL;
            bt_key key;
            bt_cursor cursor=bt_lm(coll->metadata);
            
            if(!cursor.is_null())
            {
                do {
                    key=cursor.get_key();
                    d_left=insert_element(d_left,XNULL,left,"document",xs_untyped,NULL_XMLNS);
                    ////////////////////////////////////////////////////////////////////////////
                    /// We must renew left pointer due to insert_element can have side effect - 
                    /// it can move parent of the new element to another block (Ivan Shcheklein).
                    left = removeIndirection(GETPARENTPOINTER(d_left));
                    ////////////////////////////////////////////////////////////////////////////
                    insert_attribute(XNULL,XNULL,d_left,"name",xs_untypedAtomic,(char*)key.data(),
                        key.get_size(),NULL_XMLNS);
                } while(cursor.bt_next_key());
            }
        }
    }
}

void get_catalog(xptr node,const char* title)
{
    addTextValue(node,"$CATALOG.XML",12);
    xptr parent=insert_element(XNULL,XNULL,node,"catalog",xs_untyped,NULL_XMLNS);
    xptr left=XNULL;

    metadata_cell_cptr mdc = XNULL;
    catalog_iterator it(catobj_metadata);

    while (it.next())
    {
        mdc = it.get_object();

        if (!mdc.found()) {
          // This is the case when the entity is deleted in this transaction, but is pointed to from the tree
          continue;
        } 

        if (left==XNULL)
        {
            left=insert_element(XNULL,XNULL,parent,(mdc->is_doc)?"document":"collection",xs_untyped,NULL_XMLNS);
        }
        else
            left=insert_element(left,XNULL,XNULL,(mdc->is_doc)?"document":"collection",xs_untyped,NULL_XMLNS);

        insert_attribute(XNULL,XNULL,left,"name",xs_untypedAtomic,mdc->name,strlen(mdc->name),NULL_XMLNS);
    }
}

void get_collections(xptr node,const char* title)
{
    addTextValue(node,"$COLLECTIONS.XML",12);
    xptr parent=insert_element(XNULL,XNULL,node,"collections",xs_untyped,NULL_XMLNS);
    xptr left=XNULL;

    metadata_cell_cptr mdc = XNULL;
    catalog_iterator it(catobj_metadata);

    while (it.next())
    {
        mdc = it.get_object();

        if (!mdc.found()) {
          // This is the case when the entity is deleted in this transaction, but is pointed to from the tree
          continue;
        }

        if (!mdc->is_doc)
        {
            if (left==XNULL)
            {
                left=insert_element(XNULL,XNULL,parent,"collection",xs_untyped,NULL_XMLNS);
            }
            else
                left=insert_element(left,XNULL,XNULL,"collection",xs_untyped,NULL_XMLNS);
            insert_attribute(XNULL,XNULL,left,"name",xs_untypedAtomic,mdc->name,
                        strlen(mdc->name),NULL_XMLNS);
        }
    }
}

schema_node_xptr get_system_doc(document_type type, const char* title)
{
    system_fun func   = NULL;
    const char* param = NULL;
    
    switch(type)
    {
        case DT_DOCUMENTS     : func = get_documents; break;
        case DT_INDEXES       : func = get_indexes; break;
#ifdef SE_ENABLE_FTSEARCH
        case DT_FTINDEXES     : func = get_ftindexes; break;
#endif
#ifdef SE_ENABLE_TRIGGERS
        case DT_TRIGGERS      : func = get_triggers; break;
#endif
        case DT_SCHEMA        : func = get_schema; break;
        case DT_COLLECTIONS   : func = get_collections; break;
        case DT_ERRORS        : func = get_errors; break;
        case DT_VERSION       : func = get_version; break;
        case DT_DOCUMENT_     : func = get_document_full; param = title + 10; break;
        case DT_COLLECTION_   : func = get_collection_full; param = title + 12; break;
        case DT_SCHEMA_       : func = get_schema; param = title + 8; break;
        default               : throw USER_EXCEPTION2(SE1003, (std::string("Document '") + title + "' is not system.").c_str()); 
    }
    
    local_lock_mrg->lock(lm_s);
    doc_schema_node_cptr scm(doc_schema_node_object::create(false));
    xptr blk=createNewBlock(scm.ptr());
    node_blk_hdr* block_hdr=(node_blk_hdr*) XADDR(blk);
    n_dsc* node= GETPOINTERTODESC(block_hdr,block_hdr->free_first);
    block_hdr->free_first=*((shft*)node);
    block_hdr->desc_first=CALCSHIFT(node,block_hdr);
    block_hdr->desc_last=block_hdr->desc_first;
    block_hdr->count=1;
    block_hdr->snode->nodecnt++;
    d_dsc::init(node);
    xptr nodex=ADDR2XPTR(node);
    xptr tmp=add_record_to_indirection_table(nodex);
    CHECKP(nodex);
    VMM_SIGNAL_MODIFICATION(nodex);
    node->indir=tmp;
    nid_create_root(nodex,false);
    CHECKP(nodex);
    (*func)(nodex,param);
    if (sys_schema==NULL) sys_schema=se_new std::vector<schema_node_xptr>;
    sys_schema->push_back(scm.ptr());
    return scm.ptr();
}

/// We should clear dynamic memory which was allocated by temporary schema nodes
/// and check that TMPNIDBLCK is XNULL.
void system_tables_on_kernel_statement_end()
{
    if (sys_schema!=NULL)
    {
        std::vector<schema_node_xptr>::iterator it=sys_schema->begin();
        while (it!=sys_schema->end())
        {
            (*it)->drop();
            it++;
        }
		sys_schema->clear();
        TMPNIDBLK = XNULL;
    }
}

