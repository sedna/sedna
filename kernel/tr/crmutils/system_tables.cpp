/*
 * File:  system_tables.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include <sstream>

#include "tr/crmutils/crmutils.h"
#include "tr/structures/schema.h"
#include "tr/structures/metadata.h"
#include "tr/mo/micro.h"
#include "tr/locks/locks.h"
#include "tr/vmm/vmm.h"
#include "tr/idx/index_data.h"
#include "tr/structures/indirection.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/idx/btree/btstruct.h"
#include "tr/idx/btree/btree.h"
#include "tr/idx/index_data.h"
typedef void (*system_fun)(xptr root, const char* title);
static std::vector<schema_node*>* sys_schema=NULL;

inline void print_type_name(xmlscm_type keytype, char* buf)
{
    strcpy(buf, xmlscm_type2c_str(keytype));
}

xptr fill_schema(schema_node* scm, xptr& node,xptr& neighb)
{
	xptr parent=insert_element(neighb,XNULL,node,convert_type(scm->type),xs_untyped,NULL);
	xptr indir=((n_dsc*)XADDR(parent))->indir;
	xptr left =insert_attribute(XNULL,XNULL,parent,"name",xs_untypedAtomic,scm->name,(scm->name==NULL)?0:strlen(scm->name),NULL);
	if (scm->xmlns!=NULL)
	{
		left=insert_attribute(left,XNULL,XNULL,"prefix",xs_untypedAtomic,scm->xmlns->prefix,(scm->xmlns->prefix==NULL)?0:strlen(scm->xmlns->prefix),NULL);
		left=insert_attribute(left,XNULL,XNULL,"uri",xs_untypedAtomic,scm->xmlns->uri,(scm->xmlns->uri==NULL)?0:strlen(scm->xmlns->uri),NULL);
	}
	char buf[20];
	u_itoa(scm->nodecnt,buf,10);
	left=insert_attribute(left,XNULL,XNULL,"total_nodes",xs_untypedAtomic,buf,strlen(buf),NULL);
	u_itoa(scm->blockcnt,buf,10);
	left=insert_attribute(left,XNULL,XNULL,"total_blocks",xs_untypedAtomic,buf,strlen(buf),NULL);
	u_itoa(scm->extnids,buf,10);
	left=insert_attribute(left,XNULL,XNULL,"total_ext_nids",xs_untypedAtomic,buf,strlen(buf),NULL);
	u_itoa(scm->indir_blk_cnt,buf,10);
	left=insert_attribute(left,XNULL,XNULL,"total_indir_blocks",xs_untypedAtomic,buf,strlen(buf),NULL);
	#ifdef WIN32
	_i64toa(scm->textcnt,buf,10);
	#else
	sprintf(buf,"%lld",scm->textcnt);
	#endif
	left=insert_attribute(left,XNULL,XNULL,"total_text",xs_untypedAtomic,buf,strlen(buf),NULL);
	sc_ref *sc= scm->first_child;
	while (sc!=NULL)
	{
		left=fill_schema(sc->snode,XNULL,left);
		sc=sc->next;
	}
	return removeIndirection(indir);
}
void get_schema(xptr node,const char* title)
{
	addTextValue(node,"$SCHEMA.XML",12);
	xptr parent=insert_element(XNULL,XNULL,node,"schema",xs_untyped,NULL);
	xptr left=XNULL;
	metadata_sem_down();
	pers_sset<sn_metadata_cell,unsigned short>::pers_sset_entry* mdc=metadata->rb_minimum(metadata->root);
	while (mdc!=NULL)
	{
		if (title==NULL || my_strcmp(title,(mdc->obj->document_name==NULL)?mdc->obj->collection_name:mdc->obj->document_name)==0)
		{
			if (left==XNULL)
			{
				left=insert_element(XNULL,XNULL,parent,(mdc->obj->document_name==NULL)?"collection":"document",xs_untyped,NULL);
			}
			else
				left=insert_element(left,XNULL,XNULL,(mdc->obj->document_name==NULL)?"collection":"document",xs_untyped,NULL);
			xptr cd=insert_attribute(XNULL,XNULL,left,"name",xs_untypedAtomic,(mdc->obj->document_name==NULL)?mdc->obj->collection_name:mdc->obj->document_name,
				strlen((mdc->obj->document_name==NULL)?mdc->obj->collection_name:mdc->obj->document_name),NULL);
			fill_schema(mdc->obj->snode,left,cd);
		}
		mdc=metadata->rb_successor(mdc);
	}
	metadata_sem_up();

}
void get_document_full (xptr node,const char* title)
{
	char* docn=se_new char[11+strlen(title)];
	docn[0]='\0';
	strcat(docn,"$DOCUMENT_");
	strcat(docn,title);
	addTextValue(node,docn,strlen(docn));
	delete [] docn;
	xptr parent=insert_element(XNULL,XNULL,node,"document",xs_untyped,NULL,NULL);
	insert_attribute(XNULL,XNULL,parent,"name",xs_untypedAtomic,title,strlen(title),NULL);
	schema_node* scn=find_document(title);	
	if (scn!=NULL) getDebugInfo(scn, parent);

}
void get_collection_full (xptr node,const char* title)
{
	char* docn=se_new char[13+strlen(title)];
	docn[0]='\0';
	strcat(docn,"$COLLECTION_");
	strcat(docn,title);
	addTextValue(node,docn,strlen(docn));
	delete [] docn;
	xptr parent=insert_element(XNULL,XNULL,node,"collection",xs_untyped,NULL,NULL);
	insert_attribute(XNULL,XNULL,parent,"name",xs_untypedAtomic,title,strlen(title),NULL);
	schema_node* scn=find_collection(title);	
	if (scn!=NULL) getDebugInfo(scn, parent);

}

document_type get_document_type(counted_ptr<db_entity> db_ent)
{
    const char* title = db_ent->name;
    
    if(title == NULL || title[0] != '$') return DT_NON_SYSTEM;
    
    if(db_ent->type == dbe_document)
    {
        if(!my_strcmp(title, "$documents"))       return DT_DOCUMENTS;
        if(!my_strcmp(title, "$collections"))     return DT_COLLECTIONS;
        if(!my_strcmp(title, "$modules"))         return DT_MODULES;
        if(!my_strcmp(title, "$schema"))          return DT_SCHEMA;
        if(!my_strcmp(title, "$indexes"))         return DT_INDEXES;
        if(!my_strcmp(title, "$version"))         return DT_VERSION;
#ifdef SE_ENABLE_FTSEARCH
        if(!my_strcmp(title, "$ftindexes"))       return DT_FTINDEXES;
#endif
        if(!my_strcmp(title, "$errors"))          return DT_ERRORS;
        if(strstr(title, "$collection_")==title)  return DT_COLLECTION_;
        if(strstr(title, "$document_")==title)    return DT_DOCUMENT_;
        if(strstr(title, "$schema_")==title)      return DT_SCHEMA_;
    }

    return DT_NON_SYSTEM;
}

void get_version(xptr node,const char* title)
{
	addTextValue(node,"$VERSION.XML",12);
	xptr parent=insert_element(XNULL,XNULL,node,"sedna",xs_untyped,NULL);
    insert_attribute(XNULL,XNULL,parent,"version",xs_untypedAtomic,SEDNA_VERSION,
						strlen(SEDNA_VERSION),NULL);
	insert_attribute(XNULL,XNULL,parent,"build",xs_untypedAtomic,SEDNA_BUILD,
						strlen(SEDNA_BUILD),NULL);
	
	
}
void get_errors(xptr node,const char* title)
{
	addTextValue(node,"$ERRORS.XML",12);
	xptr parent=insert_element(XNULL,XNULL,node,"errors",xs_untyped,NULL);
	xptr left=XNULL;
	for (int i=0;i<=SE5100;i++)
	{
		if (left==XNULL)
		{
			left=insert_element(XNULL,XNULL,parent,"error",xs_untyped,NULL);
		}
		else
			left=insert_element(left,XNULL,XNULL,"error",xs_untyped,NULL,NULL);
		
		insert_attribute(XNULL,XNULL,left,"code",xs_untypedAtomic,user_error_code_entries[i].code,
						strlen(user_error_code_entries[i].code),NULL);
		insert_attribute(XNULL,XNULL,left,"roll_back",xs_untypedAtomic,(user_error_code_entries[i].act==ueca_ROLLBACK_TRN)?"y":"n",
						1,NULL);
		insert_text(XNULL,XNULL,left,user_error_code_entries[i].descr,
						strlen(user_error_code_entries[i].descr));

	}
}
void get_indexes (xptr node,const char* title)
{
	addTextValue(node,"$INDEXES.XML",12);
	xptr parent=insert_element(XNULL,XNULL,node,"indexes",xs_untyped,NULL,NULL);
	xptr left=XNULL;
	index_sem_down();
	local_lock_mrg->put_lock_on_db();
	pers_sset<index_cell,unsigned short>::pers_sset_entry* mdc=indexdata->rb_minimum(indexdata->root);
	char buf[200];
	while (mdc!=NULL)
	{
		if (left==XNULL)
		{
			left=insert_element(XNULL,XNULL,parent,"index",xs_untyped,NULL);
		}
		else
			left=insert_element(left,XNULL,XNULL,"index",xs_untyped,NULL);
		
		index_cell* ic=mdc->obj;
		xptr node=insert_attribute(XNULL,XNULL,left,"name",xs_untypedAtomic,ic->index_title,
						strlen(ic->index_title),NULL);
		node=insert_attribute(node,XNULL,XNULL,"object_type",xs_untypedAtomic,(ic->is_doc)?"document":"collection", 
						(ic->is_doc)?8:10,NULL);
		node=insert_attribute(node,XNULL,XNULL,"object_name",xs_untypedAtomic,ic->doc_name,
			strlen(ic->doc_name),NULL);
		print_type_name(ic->keytype,buf);
		node=insert_attribute(node,XNULL,XNULL,"as_type",xs_untypedAtomic,buf,
		strlen(buf),NULL);
		std::ostringstream str1, str2;
		ic->object->print(str1);
		node=insert_attribute(node,XNULL,XNULL,"on_path",xs_untypedAtomic,str1.str().c_str(),
		strlen(str1.str().c_str()),NULL);
		ic->key->print(str2);
		node=insert_attribute(node,XNULL,XNULL,"by_path",xs_untypedAtomic,str2.str().c_str(),
		strlen(str2.str().c_str()),NULL);
			
		mdc=indexdata->rb_successor(mdc);
	}
	index_sem_up();
}

#ifdef SE_ENABLE_FTSEARCH
void print_ft_type_name(ft_index_type ftype, char* buf)
{	
	switch(ftype)
	{
		case ft_xml	: strcpy(buf,"xml");
        break;case ft_xml_hl	: strcpy(buf,"ft_xml_hl");
        break;case ft_string_value	: strcpy(buf,"string-value");
        break;case ft_delimited_value	: strcpy(buf,"delimited-value");
		break;case ft_customized_value	: strcpy(buf,"customized-value");
		break;default			: strcpy(buf,"unknown");
	}	
}
void get_ftindexes (xptr node,const char* title)
{
	addTextValue(node,"$FTINDEXES.XML",14);
	xptr parent=insert_element(XNULL,XNULL,node,"ftindexes",xs_untyped,NULL,NULL);
	xptr left=XNULL;
	index_sem_down();
	local_lock_mrg->put_lock_on_db();
	pers_sset<ft_index_cell,unsigned short>::pers_sset_entry* mdc=ft_indexdata->rb_minimum(ft_indexdata->root);
	char buf[200];
	while (mdc!=NULL)
	{
		if (left==XNULL)
		{
			left=insert_element(XNULL,XNULL,parent,"ftindex",xs_untyped,NULL);
		}
		else
			left=insert_element(left,XNULL,XNULL,"ftindex",xs_untyped,NULL);
		
		ft_index_cell* ic=mdc->obj;
		xptr node=insert_attribute(XNULL,XNULL,left,"name",xs_untypedAtomic,ic->index_title,
						strlen(ic->index_title),NULL);
		node=insert_attribute(node,XNULL,XNULL,"object_type",xs_untypedAtomic,(ic->is_doc)?"document":"collection",
						(ic->is_doc)?8:10,NULL);
		node=insert_attribute(node,XNULL,XNULL,"object_name",xs_untypedAtomic,ic->doc_name,
			strlen(ic->doc_name),NULL);
		
		print_ft_type_name(ic->ftype,buf);
		node=insert_attribute(node,XNULL,XNULL,"ft_type",xs_untypedAtomic,buf,
		strlen(buf),NULL);
		std::ostringstream str1;
		ic->object->print(str1);
		node=insert_attribute(node,XNULL,XNULL,"on_path",xs_untypedAtomic,str1.str().c_str(),
		strlen(str1.str().c_str()),NULL);
		if (ic->ftype==ft_customized_value && ic->custom_tree!=NULL)
		{
			CHECKP(left);
			xptr indir=((n_dsc*)XADDR(left))->indir;
			pers_sset<ft_custom_cell,unsigned short>::pers_sset_entry* cdc=ic->custom_tree->rb_minimum(ic->custom_tree->root);
			xptr cleft=XNULL;
			while (cdc!=NULL)
			{
				ft_custom_cell* cc=cdc->obj;
				if (cleft==XNULL)
				{
					cleft=insert_element(XNULL,XNULL,left,"template",xs_untyped,NULL);
				}
				else
					cleft=insert_element(cleft,XNULL,XNULL,"template",xs_untyped,NULL);
				xptr node=insert_attribute(XNULL,XNULL,cleft,"element_name",xs_untypedAtomic,cc->local,
					strlen(cc->local),NULL);
				if (cc->ns!=NULL)
				{
					node=insert_attribute(node,XNULL,XNULL,"ns_prefix",xs_untypedAtomic,cc->ns->prefix,strlen(cc->ns->prefix),NULL);
					node=insert_attribute(node,XNULL,XNULL,"ns_uri",xs_untypedAtomic,cc->ns->uri,strlen(cc->ns->uri),NULL);
				}
				print_ft_type_name(cc->cm,buf);
				insert_attribute(node,XNULL,XNULL,"ft_type",xs_untypedAtomic,buf,
		strlen(buf),NULL);
				cdc=ic->custom_tree->rb_successor(cdc);
				left=removeIndirection(indir);
			}

		}
		mdc=ft_indexdata->rb_successor(mdc);
	}
	index_sem_up();
}
#endif

/*static inline xptr insert_generated_document(const char* name, xptr parent, xptr left)
{
    xptr temp = insert_element(left,XNULL,parent,"document",xs_untyped,NULL);
    temp = insert_attribute(XNULL,XNULL,temp,"name",xs_untypedAtomic,name,strlen(name),NULL);
    return removeIndirection(GETPARENTPOINTER(temp));
}
*/

void get_documents (xptr node,const char* title)
{
	addTextValue(node,"$DOCUMENTS.XML",12);
	xptr parent=insert_element(XNULL,XNULL,node,"documents",xs_untyped,NULL,NULL);
	xptr left=XNULL;
	local_lock_mrg->put_lock_on_db();
	metadata_sem_down();
/*	
	left = insert_generated_document("$documents", parent, left);
	left = insert_generated_document("$indexes", XNULL, left);
#ifdef SE_ENABLE_FTSEARCH
	left = insert_generated_document("$ftindexes", XNULL, left);
#endif
	left = insert_generated_document("$schema", XNULL, left);
    left = insert_generated_document("$collections", XNULL, left);
	left = insert_generated_document("$errors", XNULL, left);
	left = insert_generated_document("$version", XNULL, left);
	left = insert_generated_document("$modules", XNULL, left);
*/	
	pers_sset<sn_metadata_cell,unsigned short>::pers_sset_entry* mdc=metadata->rb_minimum(metadata->root);
	while (mdc!=NULL)
	{
		if (left==XNULL)
		{
			left=insert_element(XNULL,XNULL,parent,(mdc->obj->document_name==NULL)?"collection":"document",xs_untyped,NULL);
		}
		else
			left=insert_element(left,XNULL,XNULL,(mdc->obj->document_name==NULL)?"collection":"document",xs_untyped,NULL);

		xptr temp = insert_attribute(XNULL,XNULL,left,"name",xs_untypedAtomic,(mdc->obj->document_name==NULL)?mdc->obj->collection_name:mdc->obj->document_name,
						strlen((mdc->obj->document_name==NULL)?mdc->obj->collection_name:mdc->obj->document_name),NULL);
		////////////////////////////////////////////////////////////////////////////
		/// We must renew left pointer due to insert_attribute possibly has side effect - 
		/// it can move parent of the new attribute to another block (Ivan Shcheklein).
		left = removeIndirection(GETPARENTPOINTER(temp));
        ////////////////////////////////////////////////////////////////////////////
		if (mdc->obj->document_name==NULL)
		{
			col_schema_node* coll=(col_schema_node*)mdc->obj->snode;
			bt_key key;
			key.setnew("");
			xptr d_left=XNULL;
			bt_cursor cursor=bt_find_gt((coll->metadata)->btree_root, key);
			
			if(!cursor.is_null())
			{
				do {
					key=cursor.get_key();
					d_left=insert_element(d_left,XNULL,left,"document",xs_untyped,NULL,NULL);
					////////////////////////////////////////////////////////////////////////////
					/// We must renew left pointer due to insert_element can have side effect - 
					/// it can move parent of the new element to another block (Ivan Shcheklein).
					left = removeIndirection(GETPARENTPOINTER(d_left));
					////////////////////////////////////////////////////////////////////////////
					insert_attribute(XNULL,XNULL,d_left,"name",xs_untypedAtomic,(char*)key.data(),
						key.get_size(),NULL);
				} while(cursor.bt_next_key());
			}
			
		}
		mdc=metadata->rb_successor(mdc);
	}
	metadata_sem_up();
}

void get_catalog(xptr node,const char* title)
{
	addTextValue(node,"$CATALOG.XML",12);
	xptr parent=insert_element(XNULL,XNULL,node,"catalog",xs_untyped,NULL);
	xptr left=XNULL;
	metadata_sem_down();
	pers_sset<sn_metadata_cell,unsigned short>::pers_sset_entry* mdc=metadata->rb_minimum(metadata->root);
	while (mdc!=NULL)
	{
		if (left==XNULL)
		{
			left=insert_element(XNULL,XNULL,parent,(mdc->obj->document_name==NULL)?"collection":"document",xs_untyped,NULL);
		}
		else
			left=insert_element(left,XNULL,XNULL,(mdc->obj->document_name==NULL)?"collection":"document",xs_untyped,NULL);

		insert_attribute(XNULL,XNULL,left,"name",xs_untypedAtomic,(mdc->obj->document_name==NULL)?mdc->obj->collection_name:mdc->obj->document_name,
						strlen((mdc->obj->document_name==NULL)?mdc->obj->collection_name:mdc->obj->document_name),NULL);
		
		mdc=metadata->rb_successor(mdc);
	}
	metadata_sem_up();
}

void get_collections(xptr node,const char* title)
{
	addTextValue(node,"$COLLECTIONS.XML",12);
	xptr parent=insert_element(XNULL,XNULL,node,"collections",xs_untyped,NULL);
	xptr left=XNULL;
	metadata_sem_down();
	pers_sset<sn_metadata_cell,unsigned short>::pers_sset_entry* mdc=metadata->rb_minimum(metadata->root);
	while (mdc!=NULL)
	{
		if (mdc->obj->document_name==NULL)
		{
			if (left==XNULL)
			{
				left=insert_element(XNULL,XNULL,parent,"collection",xs_untyped,NULL);
			}
			else
				left=insert_element(left,XNULL,XNULL,"collection",xs_untyped,NULL);
			insert_attribute(XNULL,XNULL,left,"name",xs_untypedAtomic,mdc->obj->collection_name,
						strlen(mdc->obj->collection_name),NULL);
		}
		
		mdc=metadata->rb_successor(mdc);
	}
	metadata_sem_up();
	  
}
void get_modules(xptr node,const char* title)
{
	addTextValue(node,"$MODULES.XML",12);
	xptr parent=insert_element(XNULL,XNULL,node,"modules",xs_untyped,NULL);	
	//metadata_sem_down();
	col_schema_node* coll=(col_schema_node*)find_collection("$modules");
	//metadata_sem_up();
	bt_key key;
	key.setnew("");
	xptr d_left=XNULL;
	bt_cursor cursor=bt_find_gt((coll->metadata)->btree_root, key);
	if(!cursor.is_null())
	{
		do {
			key=cursor.get_key();
			d_left=insert_element(d_left,XNULL,parent,"module",xs_untyped,NULL,NULL);
			insert_attribute(XNULL,XNULL,d_left,"name",xs_untypedAtomic,(char*)key.data(),
			key.get_size(),NULL);
	    } while(cursor.bt_next_key());
	}
}

schema_node* get_system_doc(document_type type, const char* title)
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
	    case DT_SCHEMA        : func = get_schema; break;
	    case DT_COLLECTIONS   : func = get_collections; break;
	    case DT_ERRORS        : func = get_errors; break;
	    case DT_VERSION       : func = get_version; break;
	    case DT_MODULES       : func = get_modules; break;
	    case DT_DOCUMENT_     :	func = get_document_full; param = title + 10; break;
	    case DT_COLLECTION_   :	func = get_collection_full; param = title + 12; break;
	    case DT_SCHEMA_       : func = get_schema; param = title + 8; break;
	    default               : throw USER_EXCEPTION2(SE1003, (std::string("Document '") + title + "' is not system.").c_str()); 
	}
	
	local_lock_mrg->lock(lm_s);
	doc_schema_node* scm=	doc_schema_node::init(false);
	xptr blk=createNewBlock(scm,false);
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
	if (sys_schema==NULL) sys_schema=se_new std::vector<schema_node*>;
	sys_schema->push_back(scm);
	return scm;
}

void clear_temporary(void)
{
	if (sys_schema!=NULL)
	{
		std::vector<schema_node*>::iterator it=sys_schema->begin();
		while (it!=sys_schema->end())
		{
			(*it)->delete_scheme_node();
			it++;
		}
	}
}

