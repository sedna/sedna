#include "crmutils.h"
#include "schema.h"
#include "metadata.h"
#include "micro.h"
#include "locks.h"
#include "vmm.h"
#include "index_data.h"
#include "indirection.h"
#include "error_codes.h"
#include <sstream>

typedef void (*system_fun)(xptr root, const char* title);
static std::vector<schema_node*>* sys_schema=NULL;
void print_type_name(xmlscm_type keytype, char* buf)
{
	switch(keytype)
	{
		case xs_integer	: strcpy(buf,"integer");
        break;case xs_float	: strcpy(buf,"float");
        break;case xs_double	: strcpy(buf,"double");
        break;case xs_string	: strcpy(buf,"string");
		break;case xs_anyType	:			strcpy(buf,"anyType");
		break;case xs_anySimpleType	:	strcpy(buf,"anySimpleType");
		break;case xs_anyAtomicType:		strcpy(buf,"anyAtomicType");
		break;case xs_IDREFS	:			strcpy(buf,"IDREFS");
		break;case xs_NMTOKENS:				strcpy(buf,"NMTOKENS");
		break;case xs_ENTITIES:				strcpy(buf,"ENTITIES");
		break;case xdt_untyped:				strcpy(buf,"untyped");
		break;case xdt_untypedAtomic:		strcpy(buf,"untypedAtomic");
		break;case xs_dateTime	:			strcpy(buf,"dateTime");
		break;case xs_date		:			strcpy(buf,"date");
		break;case xs_time		:			strcpy(buf,"time");
		break;case xs_duration	:			strcpy(buf,"duration");
		break;case xdt_yearMonthDuration:	strcpy(buf,"yearMonthDuration");
		break;case xdt_dayTimeDuration:		strcpy(buf,"dayTimeDuration");
		break;case xs_normalizedString:		strcpy(buf,"normalizedString");
		break;case xs_token	:			strcpy(buf,"token");
		break;case xs_language:				strcpy(buf,"language");
		break;case xs_NMTOKEN	:			strcpy(buf,"NMTOKEN");
		break;case xs_Name	:				strcpy(buf,"Name");
		break;case xs_NCName	:			strcpy(buf,"NCName");
		break;case xs_ID		:			strcpy(buf,"ID");
		break;case xs_IDREF	:			strcpy(buf,"IDREF");
		break;case xs_ENTITY	:			strcpy(buf,"ENTITY");
		break;case xs_decimal	:			strcpy(buf,"decimal");
		break;case xs_gYearMonth	:		strcpy(buf,"gYearMonth");
		break;case xs_gYear	:			strcpy(buf,"gYear");
		break;case xs_gMonthDay:			strcpy(buf,"gMonthDay");
		break;case xs_gDay:					strcpy(buf,"gDay");
		break;case xs_gMonth:				strcpy(buf,"gMonth");
		break;case xs_boolean	:			strcpy(buf,"boolean");
		break;case xs_base64Binary:			strcpy(buf,"base64Binary");
		break;case xs_hexBinary:			strcpy(buf,"hexBinary");
		break;case xs_anyURI	:			strcpy(buf,"anyURI");
		break;case xs_QName	:			strcpy(buf,"QName");
		break;case xs_NOTATION:				strcpy(buf,"NOTATION");
		break;case se_separator:		    strcpy(buf,"separator");
        break;default			: strcpy(buf,"unknown");
	}	
}
xptr fill_schema(schema_node* scm, xptr& node,xptr& neighb)
{
	xptr parent=insert_element(neighb,XNULL,node,convert_type(scm->type),xdt_untyped,NULL);
	xptr indir=((n_dsc*)XADDR(parent))->indir;
	xptr left =insert_attribute(XNULL,XNULL,parent,"name",xdt_untypedAtomic,scm->name,(scm->name==NULL)?0:strlen(scm->name),NULL);
	if (scm->xmlns!=NULL)
	{
		left=insert_attribute(left,XNULL,XNULL,"prefix",xdt_untypedAtomic,scm->xmlns->prefix,(scm->xmlns->prefix==NULL)?0:strlen(scm->xmlns->prefix),NULL);
		left=insert_attribute(left,XNULL,XNULL,"uri",xdt_untypedAtomic,scm->xmlns->uri,(scm->xmlns->uri==NULL)?0:strlen(scm->xmlns->uri),NULL);
	}
	char buf[20];
	u_itoa(scm->nodecnt,buf,10);
	left=insert_attribute(left,XNULL,XNULL,"total_nodes",xdt_untypedAtomic,buf,strlen(buf),NULL);
	u_itoa(scm->blockcnt,buf,10);
	left=insert_attribute(left,XNULL,XNULL,"total_blocks",xdt_untypedAtomic,buf,strlen(buf),NULL);
	u_itoa(scm->extnids,buf,10);
	left=insert_attribute(left,XNULL,XNULL,"total_ext_nids",xdt_untypedAtomic,buf,strlen(buf),NULL);
	u_itoa(scm->indir_blk_cnt,buf,10);
	left=insert_attribute(left,XNULL,XNULL,"total_indir_blocks",xdt_untypedAtomic,buf,strlen(buf),NULL);
	#ifdef WIN32
	_i64toa(scm->textcnt,buf,10);
	#else
	sprintf(buf,"%lld",scm->textcnt);
	#endif
	left=insert_attribute(left,XNULL,XNULL,"total_text",xdt_untypedAtomic,buf,strlen(buf),NULL);
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
	xptr parent=insert_element(XNULL,XNULL,node,"SCHEMA",xdt_untyped,NULL);
	xptr left=XNULL;
	metadata_sem_down();
	pers_sset<sn_metadata_cell,unsigned short>::pers_sset_entry* mdc=metadata->rb_minimum(metadata->root);
	while (mdc!=NULL)
	{
		if (title==NULL || my_strcmp(title,(mdc->obj->document_name==NULL)?mdc->obj->collection_name:mdc->obj->document_name)==0)
		{
			if (left==XNULL)
			{
				left=insert_element(XNULL,XNULL,parent,(mdc->obj->document_name==NULL)?"COLLECTION":"DOCUMENT",xdt_untyped,NULL);
			}
			else
				left=insert_element(left,XNULL,XNULL,(mdc->obj->document_name==NULL)?"COLLECTION":"DOCUMENT",xdt_untyped,NULL);
			xptr cd=insert_attribute(XNULL,XNULL,left,"name",xdt_untypedAtomic,(mdc->obj->document_name==NULL)?mdc->obj->collection_name:mdc->obj->document_name,
				strlen((mdc->obj->document_name==NULL)?mdc->obj->collection_name:mdc->obj->document_name),NULL);
			fill_schema(mdc->obj->snode,left,cd);
		}
		mdc=metadata->rb_successor(mdc);
	}
	metadata_sem_up();

}
void get_document_full (xptr node,const char* title)
{
	char* docn=new char[11+strlen(title)];
	docn[0]='\0';
	strcat(docn,"$DOCUMENT_");
	strcat(docn,title);
	addTextValue(node,docn,strlen(docn));
	delete [] docn;
	xptr parent=insert_element(XNULL,XNULL,node,"DOCUMENT",xdt_untyped,NULL,NULL);
	insert_attribute(XNULL,XNULL,parent,"name",xdt_untypedAtomic,title,strlen(title),NULL);
	schema_node* scn=find_document(title);	
	getDebugInfo(scn, parent);

}
bool is_document_system(const char* title)
{
	return (title[0]=='$');
}
void get_version(xptr node,const char* title)
{
	addTextValue(node,"$VERSION.XML",12);
	xptr parent=insert_element(XNULL,XNULL,node,"SEDNA",xdt_untyped,NULL);
    insert_attribute(XNULL,XNULL,parent,"VERSION",xdt_untypedAtomic,SEDNA_VERSION,
						strlen(SEDNA_VERSION),NULL);
	insert_attribute(XNULL,XNULL,parent,"BUILD",xdt_untypedAtomic,SEDNA_BUILD,
						strlen(SEDNA_BUILD),NULL);
	
	
}
void get_errors(xptr node,const char* title)
{
	addTextValue(node,"$ERRORS.XML",12);
	xptr parent=insert_element(XNULL,XNULL,node,"ERRORS",xdt_untyped,NULL);
	xptr left=XNULL;
	for (int i=0;i<=SE5100;i++)
	{
		if (left==XNULL)
		{
			left=insert_element(XNULL,XNULL,parent,"ERROR",xdt_untyped,NULL);
		}
		else
			left=insert_element(left,XNULL,XNULL,"ERROR",xdt_untyped,NULL,NULL);
		
		insert_attribute(XNULL,XNULL,left,"code",xdt_untypedAtomic,user_error_code_entries[i].code,
						strlen(user_error_code_entries[i].code),NULL);
		insert_attribute(XNULL,XNULL,left,"roll_back",xdt_untypedAtomic,(user_error_code_entries[i].act==ueca_ROLLBACK_TRN)?"Y":"N",
						1,NULL);
		insert_text(XNULL,XNULL,left,user_error_code_entries[i].descr,
						strlen(user_error_code_entries[i].descr));

	}
}
void get_indexes (xptr node,const char* title)
{
	addTextValue(node,"$INDEXES.XML",12);
	xptr parent=insert_element(XNULL,XNULL,node,"INDEXES",xdt_untyped,NULL,NULL);
	xptr left=XNULL;
	index_sem_down();
	local_lock_mrg->put_lock_on_db();
	pers_sset<index_cell,unsigned short>::pers_sset_entry* mdc=indexdata->rb_minimum(indexdata->root);
	char buf[200];
	while (mdc!=NULL)
	{
		if (left==XNULL)
		{
			left=insert_element(XNULL,XNULL,parent,"INDEX",xdt_untyped,NULL);
		}
		else
			left=insert_element(left,XNULL,XNULL,"INDEX",xdt_untyped,NULL);
		
		index_cell* ic=mdc->obj;
		xptr node=insert_attribute(XNULL,XNULL,left,"title",xdt_untypedAtomic,ic->index_title,
						strlen(ic->index_title),NULL);
		node=insert_attribute(node,XNULL,XNULL,"indexed_object",xdt_untypedAtomic,(ic->is_doc)?"doc":"col",
						3,NULL);
		node=insert_attribute(node,XNULL,XNULL,"object_title",xdt_untypedAtomic,ic->doc_name,
			strlen(ic->doc_name),NULL);
		print_type_name(ic->keytype,buf);
		node=insert_attribute(node,XNULL,XNULL,"key_type",xdt_untypedAtomic,buf,
		strlen(buf),NULL);
		std::ostringstream str1, str2;
		ic->object->print(str1);
		node=insert_attribute(node,XNULL,XNULL,"value_path",xdt_untypedAtomic,str1.str().c_str(),
		strlen(str1.str().c_str()),NULL);
		ic->key->print(str2);
		node=insert_attribute(node,XNULL,XNULL,"key_path",xdt_untypedAtomic,str2.str().c_str(),
		strlen(str2.str().c_str()),NULL);


			
		mdc=indexdata->rb_successor(mdc);
	}
	index_sem_up();
}
void get_documents (xptr node,const char* title)
{
	addTextValue(node,"$DOCUMENTS.XML",12);
	xptr parent=insert_element(XNULL,XNULL,node,"DOCUMENTS",xdt_untyped,NULL,NULL);
	xptr left=XNULL;
	metadata_sem_down();
	local_lock_mrg->put_lock_on_db();
	pers_sset<sn_metadata_cell,unsigned short>::pers_sset_entry* mdc=metadata->rb_minimum(metadata->root);
	while (mdc!=NULL)
	{
		if (left==XNULL)
		{
			left=insert_element(XNULL,XNULL,parent,(mdc->obj->document_name==NULL)?"COLLECTION_DOCS":"SA_DOCUMENT",xdt_untyped,NULL);
		}
		else
			left=insert_element(left,XNULL,XNULL,(mdc->obj->document_name==NULL)?"COLLECTION_DOCS":"SA_DOCUMENT",xdt_untyped,NULL);
		if (mdc->obj->document_name==NULL)
		{
			col_schema_node* coll=(col_schema_node*)mdc->obj->snode;
			pers_sset<dn_metadata_cell,unsigned int>::pers_sset_entry* dc=coll->metadata->rb_minimum(coll->metadata->root);
			xptr d_left=XNULL;
			while (dc!=NULL)
			{
				d_left=insert_element(d_left,XNULL,left,"DOCUMENT",xdt_untyped,NULL,NULL);
				insert_attribute(XNULL,XNULL,d_left,"name",xdt_untypedAtomic,dc->obj->document_name,
						strlen(dc->obj->document_name),NULL);
				dc=coll->metadata->rb_successor(dc); 
			}
		}
		insert_attribute(XNULL,XNULL,left,"name",xdt_untypedAtomic,(mdc->obj->document_name==NULL)?mdc->obj->collection_name:mdc->obj->document_name,
						strlen((mdc->obj->document_name==NULL)?mdc->obj->collection_name:mdc->obj->document_name),NULL);
		
		mdc=metadata->rb_successor(mdc);
	}
	metadata_sem_up();
}
void get_catalog(xptr node,const char* title)
{
	addTextValue(node,"$CATALOG.XML",12);
	xptr parent=insert_element(XNULL,XNULL,node,"CATALOG",xdt_untyped,NULL);
	xptr left=XNULL;
	metadata_sem_down();
	pers_sset<sn_metadata_cell,unsigned short>::pers_sset_entry* mdc=metadata->rb_minimum(metadata->root);
	while (mdc!=NULL)
	{
		if (left==XNULL)
		{
			left=insert_element(XNULL,XNULL,parent,(mdc->obj->document_name==NULL)?"COLLECTION":"DOCUMENT",xdt_untyped,NULL);
		}
		else
			left=insert_element(left,XNULL,XNULL,(mdc->obj->document_name==NULL)?"COLLECTION":"DOCUMENT",xdt_untyped,NULL);

		insert_attribute(XNULL,XNULL,left,"name",xdt_untypedAtomic,(mdc->obj->document_name==NULL)?mdc->obj->collection_name:mdc->obj->document_name,
						strlen((mdc->obj->document_name==NULL)?mdc->obj->collection_name:mdc->obj->document_name),NULL);
		
		mdc=metadata->rb_successor(mdc);
	}
	metadata_sem_up();
	  
}
void get_collections(xptr node,const char* title)
{
	addTextValue(node,"$COLLECTIONS.XML",12);
	xptr parent=insert_element(XNULL,XNULL,node,"COLLECTION",xdt_untyped,NULL);
	xptr left=XNULL;
	metadata_sem_down();
	pers_sset<sn_metadata_cell,unsigned short>::pers_sset_entry* mdc=metadata->rb_minimum(metadata->root);
	while (mdc!=NULL)
	{
		if (mdc->obj->document_name==NULL)
		{
			if (left==XNULL)
			{
				left=insert_element(XNULL,XNULL,parent,"COLLECTION",xdt_untyped,NULL);
			}
			else
				left=insert_element(left,XNULL,XNULL,"COLLECTION",xdt_untyped,NULL);
			insert_attribute(XNULL,XNULL,left,"name",xdt_untypedAtomic,mdc->obj->collection_name,
						strlen(mdc->obj->collection_name),NULL);
		}
		
		mdc=metadata->rb_successor(mdc);
	}
	metadata_sem_up();
	  
}
schema_node* get_system_doc(const char* title)
{
	system_fun func=NULL;
	const char* param=NULL;
	if (!my_strcmp(title,"$catalog.xml"))
		func=get_catalog;
	else
	if (!my_strcmp(title,"$documents.xml"))
		func=get_documents;
	else
	if (!my_strcmp(title,"$indexes.xml"))
		func=get_indexes;
	else
	if (!my_strcmp(title,"$schema.xml"))
		func=get_schema;
	else
	if (!my_strcmp(title,"$collections.xml"))
		func=get_collections;
	else
	if (!my_strcmp(title,"$errors.xml"))
		func=get_errors;
	else
	if (!my_strcmp(title,"$version.xml"))
		func=get_version;
	if (strstr(title,"$document_")==title)
	{
		func=get_document_full;
		param=title+10;
	}
	if (strstr(title,"$schema_")==title)
	{
		func=get_schema;
		param=title+8;
	}
	
	if (func!=NULL)	
	{
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
		if (sys_schema==NULL) sys_schema=new std::vector<schema_node*>;
		sys_schema->push_back(scm);
		return scm;
	}
	else
	return NULL;
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