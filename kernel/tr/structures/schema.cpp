/*
 * File:  schema.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/structures/schema.h"
#include "tr/structures/nodes.h"
#include "tr/executor/base/XPathOnSchema.h"
#include "tr/idx/btree/btstruct.h"
#include "tr/idx/btree/btree.h"
#include "tr/idx/index_data.h"
#include "tr/vmm/vmm.h"
#ifdef SE_ENABLE_FTSEARCH
#include "tr/ft/ft_index_data.h"
#endif
#ifdef SE_ENABLE_TRIGGERS
#include "tr/triggers/triggers_data.h"
#endif


void sc_ref::init(void* p)
{
	sc_ref* scr=(sc_ref*)p;
	scr->name=NULL;
	scr->next=NULL;
	scr->prev=NULL;
	scr->snode=NULL;
	scr->type=element;
}

void sc_ref::destroy(sc_ref* scm,bool persistent)
{
	scm_free(scm->name,persistent);
	scm_free(scm,persistent);
}

void schema_node::init(void* p)
{
	schema_node* scr=(schema_node*)p;
	scr->bblk=XNULL;
	scr->first_child=NULL;
	scr->last_child=NULL;
	scr->name=NULL;
	scr->parent=NULL;
	scr->type=element;
	scr->index_object=NULL;
	#ifdef SE_ENABLE_FTSEARCH
	scr->ft_index_object=NULL;	
	#endif
	#ifdef SE_ENABLE_TRIGGERS
	scr->trigger_object=NULL;	
	#endif
	scr->xmlns=NULL;
	scr->nodecnt=0;
	scr->blockcnt=0;
	scr->extnids=0;
	scr->cl_hint=0;
	scr->indir_blk_cnt=0;
	scr->ind_entry=XNULL;
	scr->root=NULL;
	scr->textcnt=0;
	scr->lastnode_ind=XNULL;
}
void temp_schema_node::init(void* p)
{
	temp_schema_node* scr=(temp_schema_node*)p;
	scr->first_child=NULL;
	scr->next_sibl=NULL;
	scr->name=NULL;
	scr->parent=NULL;
	scr->type=element;
	scr->xmlns=NULL;
	scr->nodecnt=0;
	scr->nid_iteration=0;
	scr->nid_size=0;
	
	
}
void doc_schema_node::delete_index(index_cell* idx)
{
	
	schema_ind_cell* icell=this->sc_idx;
	while (icell!=NULL)
	{
		if (icell->index==idx) break;
		RECOVERY_CRASH;
		icell=icell->next;
	}
	if (icell->next!=NULL) icell->next->previous=icell->previous;
	if (icell->previous!=NULL) 
		icell->previous->next=icell->next;
	else
		this->sc_idx=icell->next;
	scm_free(icell,this->persistent);	
	this->remove_index(idx);	
}
void doc_schema_node::create_index(index_cell* idx)
{
	schema_ind_cell* sc=(schema_ind_cell*)scm_malloc(sizeof(schema_ind_cell),this->persistent);
	sc->index=idx;
	sc->previous=NULL;
	sc->next=this->sc_idx;
	if (this->sc_idx!=NULL)
		this->sc_idx->previous=sc;
	this->sc_idx=sc;
}

#ifdef SE_ENABLE_FTSEARCH
void doc_schema_node::delete_ft_index(ft_index_cell* idx)
{
	
	schema_ft_ind_cell* icell=this->sc_ft_idx;
	while (icell!=NULL)
	{
		if (icell->index==idx) break;
		RECOVERY_CRASH;
		icell=icell->next;
	}
	if (icell->next!=NULL) icell->next->previous=icell->previous;
	if (icell->previous!=NULL) 
		icell->previous->next=icell->next;
	else
		this->sc_ft_idx=icell->next;
	scm_free(icell,this->persistent);	
	this->remove_ft_index(idx);	
}
void doc_schema_node::create_ft_index(ft_index_cell* idx)
{
	schema_ft_ind_cell* sc=(schema_ft_ind_cell*)scm_malloc(sizeof(schema_ft_ind_cell),this->persistent);
	sc->index=idx;
	sc->previous=NULL;
	sc->next=this->sc_ft_idx;
	if (this->sc_ft_idx!=NULL)
		this->sc_ft_idx->previous=sc;
	this->sc_ft_idx=sc;
}
#endif
#ifdef SE_ENABLE_TRIGGERS
void doc_schema_node::delete_trigger(trigger_cell* trc)
{
	
	schema_trigger_cell* tcell=this->sc_triggers;
	while (tcell!=NULL)
	{
		if (tcell->trigger==trc) break;
		RECOVERY_CRASH;
		tcell=tcell->next;
	}
	if (tcell->next!=NULL) tcell->next->previous=tcell->previous;
	if (tcell->previous!=NULL) 
		tcell->previous->next=tcell->next;
	else
		this->sc_triggers=tcell->next;
	scm_free(tcell,this->persistent);	
	this->remove_trigger(trc);	
}
void doc_schema_node::create_trigger(trigger_cell* trc)
{
	schema_trigger_cell* sc=(schema_trigger_cell*)scm_malloc(sizeof(schema_trigger_cell),this->persistent);
	sc->trigger=trc;
	sc->previous=NULL;
	sc->next=this->sc_triggers;
	if (this->sc_triggers!=NULL)
		this->sc_triggers->previous=sc;
	this->sc_triggers=sc;
}
#endif
void doc_schema_node::init(void* p)
{
	schema_node::init(p);
	doc_schema_node* scr=(doc_schema_node*)p;
	scr->ind_free_space=XNULL;
	scr->first_ind_blk=XNULL;	
	scr->ext_nids_block=XNULL;
	scr->total_ext_nids=0;
	scr->root=scr;
	scr->sc_idx=NULL;
	#ifdef SE_ENABLE_FTSEARCH
	scr->sc_ft_idx=NULL;    
	#endif
	#ifdef SE_ENABLE_TRIGGERS
	scr->sc_triggers=NULL;    
	#endif
}
void col_schema_node::init(void* p)
{
	doc_schema_node::init(p);
	col_schema_node* scr=(col_schema_node*)p;
	scr->eblk=XNULL;
	scr->metadata=NULL;
}
/*dn_metadata_cell * col_schema_node::find_metadata_of_document_in_col(xptr node)
{
	pers_sset<dn_metadata_cell,unsigned int>::pers_sset_entry* mdc=this->metadata->rb_minimum(this->metadata->root);
	while (mdc!=NULL)
	{
		if (mdc->obj->root==node)	return mdc->obj;
		mdc=this->metadata->rb_successor(mdc);
	}
	return NULL;
}*/
xptr col_schema_node::search_metadata_cell(const char *document_name)
{
	bt_key key;
	key.setnew(document_name);
	return bt_find(metadata->btree_root, key).bt_next_obj();
}
void col_schema_node::delete_doc_from_coll(const char* doc_name)
{
	bt_key key;	
	key.setnew(doc_name);
	bt_delete(metadata->btree_root, key);
}
void col_schema_node::put_doc_in_coll(const char* doc_name, xptr node)
{
	bt_key key;
	key.setnew(doc_name);
	bt_insert(metadata->btree_root,key,node);
}
void col_schema_node::free_map()
{
	bt_drop(metadata->btree_root);
	scm_free(metadata,true);
}
void col_schema_node::replace_document_pointer(xptr old_xptr, xptr new_xptr)
{
	//1. find doc name
	CHECKP(old_xptr);
	xptr datap=((d_dsc*)XADDR(old_xptr))->data;
	int size=((d_dsc*)XADDR(old_xptr))->size;
	CHECKP(datap);
	shft shift= *((shft*)XADDR(datap));
	char* data=(char*)XADDR(BLOCKXPTR(datap))+shift;
	char *z=se_new char[size+1];
	memcpy(z,data,size);
	z[size]='\0';	
	//2. find doc and replace
	bt_key key;
	key.setnew(z);
	delete [] z;
	bt_find(metadata->btree_root, key).bt_set_next_obj(new_xptr);
	//3. clean
	
}
/*pers_sset<dn_metadata_cell,unsigned int>::pers_sset_entry* col_schema_node::search_metadata_cell(const char *document_name)
{
	return metadata->get(document_name,NULL);	
}
*/
/* Destructor frees memory occupied by name field */
void schema_node::destroy(schema_node* scm)
{
	if (scm->name) scm_free(scm->name,this->persistent);
	scm_free(scm,this->persistent);
}

void temp_schema_node::destroy(temp_schema_node* scm)
{
	if (scm->name) scm_free(scm->name,false);
	scm_free(scm,false);
}

void xml_ns::delete_namespace_node(xml_ns* ns )
{
	if (ns->uri) scm_free(ns->uri,ns->persistent);
	if (ns->prefix) scm_free(ns->prefix,ns->persistent);
	scm_free(ns,ns->persistent);
}
/* Find first child of given type of this node by name */
schema_node* schema_node::get_child(const xml_ns* _xmlns,const char* name, t_item type)	
{
	sc_ref* sc=this->first_child;
	while (sc!=NULL)
	{
		if (my_strcmp(name,sc->name)==0 && sc->type==type && sc->xmlns==_xmlns) return sc->snode;
		sc=sc->next;
	}
	return NULL;
}

temp_schema_node* temp_schema_node::get_child(const xml_ns* _xmlns,const char* name, t_item type)	
{
	temp_schema_node* sc=this->first_child;
	while (sc!=NULL)
	{
		if (my_strcmp(name,sc->name)==0 && sc->type==type && sc->xmlns==_xmlns) return sc;
		sc=sc->next_sibl;
	}
	return NULL;
}

/* Links child and parent nodes into tree structure according to
   the children and parent positions in the scheme.
   Check for name uniqueness is performed. 
   Class is responsible for release of node object
*/
void schema_node::add_child(schema_node* node)
{
	sc_ref* sc=(sc_ref*)scm_malloc(sizeof(sc_ref),this->persistent);
	sc_ref::init(sc);
	sc->snode=node;
	sc->xmlns=node->xmlns;
	if (node->name!=NULL)
	{
		char* z=(char*)scm_malloc(strlen(node->name)+1,this->persistent);
		strcpy(z,node->name);
		sc->name=z;
	}
	sc->type=node->type;
	sc->prev = this->last_child;
	if (this->last_child!=NULL)
	{
		this->last_child->next=sc;
	}
	else this->first_child=sc;
	this->last_child=sc;
	node->parent=this;
	//Index search
	if (node->persistent)
	{
		schema_ind_cell* ind=node->root->sc_idx;
		while (ind!=NULL)
		{
			t_scmnodes vec=
			ind->index->fits_to_index_as_key(node);
			t_scmnodes::iterator idn=vec.begin();
			while (idn!=vec.end())
			{
				node->add_index(ind->index,*idn);
				idn++;
				RECOVERY_CRASH;
			}
			ind=ind->next;
		}
		#ifdef SE_ENABLE_FTSEARCH
		schema_ft_ind_cell* ft_ind=node->root->sc_ft_idx;
		while (ft_ind!=NULL)
		{
			if (ft_ind->index->fits_to_index(node))
			{
				node->add_ft_index(ft_ind->index);
			}
			ft_ind=ft_ind->next;
		}
		#endif
		#ifdef SE_ENABLE_TRIGGERS
		schema_trigger_cell* sc_trigger=node->root->sc_triggers;
		while (sc_trigger!=NULL)
		{
			if (sc_trigger->trigger->fits_to_trigger(node))
			{
				node->add_trigger(sc_trigger->trigger);
			}
			RECOVERY_CRASH;
			sc_trigger=sc_trigger->next;
		}
		#endif

	}
}

void temp_schema_node::add_child(temp_schema_node* node)
{
	temp_schema_node* tmp=this->first_child;
	this->first_child=node;
	node->parent=this;
	node->next_sibl=tmp;
}

/*initialization of namespace info*/
xml_ns* xml_ns::init(const char* _uri, const char* _prefix,bool persistent)
{
	xml_ns* ns=(xml_ns*)scm_malloc(sizeof(xml_ns),persistent);
	ns->counter=0;
	ns->persistent=persistent;
	if (_uri!=NULL)
	{
		char* z=(char*)scm_malloc(strlen(_uri)+1,ns->persistent);
		strcpy(z,_uri);
		ns->uri=z;
	}
	else
		ns->uri=NULL;
	if (_prefix!=NULL)
	{
		char* z=(char*)scm_malloc(strlen(_prefix)+1,ns->persistent);
		strcpy(z,_prefix);
		ns->prefix=z;
	}
	else
		ns->prefix=NULL;
	return ns;
}
/*initialization of schema node*/
schema_node* schema_node::init( doc_schema_node* root,xml_ns* _xmlns,const char* name, t_item type,bool persistent)
{
	schema_node* sc=(schema_node*)scm_malloc(sizeof(schema_node),persistent);
	schema_node::init(sc);
	sc->persistent=persistent;
	if (name!=NULL)
	{
		char* z=(char*)scm_malloc(strlen(name)+1,sc->persistent);
		strcpy(z,name);
		sc->name=z;
	}
	sc->type=type;
	sc->xmlns=_xmlns;
	if(sc->xmlns&&sc->persistent)sc->xmlns->counter++;
	if (root!=NULL)
		sc->root=root;
	else
		sc->root=(doc_schema_node*)sc;
	return sc;
}
temp_schema_node* temp_schema_node::init( xml_ns* _xmlns,const char* name, t_item type)
{
	temp_schema_node* sc=(temp_schema_node*)scm_malloc(sizeof(temp_schema_node),false);
	temp_schema_node::init(sc);
	if (name!=NULL)
	{
		char* z=(char*)scm_malloc(strlen(name)+1,false);
		strcpy(z,name);
		sc->name=z;
	}
	sc->type=type;
	sc->xmlns=_xmlns;
	
	return sc;
}
doc_schema_node* doc_schema_node::init( bool persistent)
{
	doc_schema_node* sc=(doc_schema_node*)scm_malloc(sizeof(doc_schema_node),persistent);
	doc_schema_node::init(sc);
	sc->persistent=persistent;
	sc->type=document;
	return sc;
}

col_schema_node* col_schema_node::init( bool persistent)
{
	col_schema_node* sc=(col_schema_node*)scm_malloc(sizeof(col_schema_node),persistent);	
	col_schema_node::init(sc);
	sc->persistent=persistent;
	sc->type=document;
	if(sc->xmlns&&sc->persistent)sc->xmlns->counter++;
	return sc;
}

/* inserts new node to the schema as the child of the existing one*/
schema_node* schema_node::add_child( xml_ns* _xmlns,const char* name, t_item type)
{
	if (this->index_object!=NULL &&(type==element) &&  this->persistent)
	{
		throw USER_EXCEPTION(SE2032);
	}
	schema_node* sc=schema_node::init( this->root,_xmlns,name, type,this->persistent);
	this->add_child(sc);
	return sc;
}
temp_schema_node* temp_schema_node::add_child( xml_ns* _xmlns,const char* name, t_item type)
{
	temp_schema_node* sc=temp_schema_node::init( _xmlns,name, type);
	this->add_child(sc);
	return sc;
}


/* returns true if this contains the child and data;*/
int schema_node::is_node_in_scheme_and_in_data (const xml_ns* _xmlns,const  char* name, t_item type)
{
	schema_node* ch=this->get_child(_xmlns,name,type);
	if (ch!=NULL && ch->bblk!=XNULL) return true;
	return false;
}

/*returns the total number of children by scheme*/
shft schema_node::get_child_count()
{
	shft cnt=0;
	sc_ref* sc=this->first_child;
	while (sc!=NULL)
	{
		cnt++;
		sc=sc->next;
	}
	return cnt;
}

shft temp_schema_node::get_child_count()
{
	shft cnt=0;
	temp_schema_node* sc=this->first_child;
	while (sc!=NULL)
	{
		cnt++;
		sc=sc->next_sibl;
	}
	return cnt;
}

/*returns the name of the i'th child of the node*/
char* schema_node::get_name( int i)
{
	shft cnt=0;
	sc_ref* sc=this->first_child;
	while (sc!=NULL && cnt!=i)
	{
		cnt++;
		sc=sc->next;
	}
	if (sc!=NULL) return sc->snode->name;	
	throw SYSTEM_EXCEPTION("Wrong position number ");
}


/*returns the type of the i'th child of the node*/
t_item schema_node::get_type(int i)
{
	shft cnt=0;
	sc_ref* sc=this->first_child;
	while (sc!=NULL && cnt!=i)
	{
		cnt++;
		sc=sc->next;
	}
	if (sc!=NULL) return sc->snode->type;	
	throw SYSTEM_EXCEPTION("Wrong position number ");
}

/*returns position of the child with the given name and of the given descriptor type exist in schema as the
child of the node that corresponds to the current block header; -1 otherwise*/
int schema_node::has_child_by_schema(const xml_ns* _xmlns,const  char* name,t_item type)
{
	sc_ref* sc=this->first_child;
	int cnt=-1;
	while (sc!=NULL)
	{
		++cnt;
		if (sc->type==type && my_strcmp(name,sc->name)==0 &&  sc->xmlns==_xmlns) return cnt;
		sc=sc->next;
	}
	return -1;
}

int temp_schema_node::has_child_by_schema(const xml_ns* _xmlns,const  char* name,t_item type)
{
	temp_schema_node* sc=this->first_child;
	int cnt=-1;
	while (sc!=NULL)
	{
		++cnt;
		if (sc->type==type && my_strcmp(name,sc->name)==0 &&  sc->xmlns==_xmlns) return cnt;
		sc=sc->next_sibl;
	}
	return -1;
}

/*deletes schema node*/
void schema_node::delete_scheme_node()
{
	if(this->name!=NULL) scm_free(this->name,this->persistent);
	sc_ref* sc=this->first_child;
	sc_ref* sc1;
	while (sc!=NULL)
	{
		if(sc->name!=NULL) scm_free(sc->name,this->persistent);
		sc->snode->delete_scheme_node();
		sc1=sc->next;
		scm_free(sc,this->persistent);
		sc=sc1;
		RECOVERY_CRASH;
	}
	scm_free(this,this->persistent);
}

void temp_schema_node::delete_scheme_node()
{
	if(this->name!=NULL) scm_free(this->name,false);
	temp_schema_node* sc=this->first_child;
	temp_schema_node* sc1;
	while (sc!=NULL)
	{
		sc1=sc->next_sibl;
		sc->delete_scheme_node();
		sc=sc1;
		RECOVERY_CRASH;
	}
	scm_free(this,false);
}

schema_index* schema_node::add_index(index_cell* index, struct schema_node* object)
{
	
	schema_index* sci=(schema_index*)scm_malloc(sizeof(schema_index),this->persistent);
	sci->object=object;
	sci->index=index;
	sci->previous =NULL;
	sci->next=this->index_object;
	if (this->index_object != NULL)
		this->index_object->previous=sci;
	this->index_object=sci;  
	return sci;
}

void schema_node::delete_index(schema_index* index)
{
	if (index->previous!=NULL) 
		index->previous->next=index->next;
	else
		this->index_object=index->next;
	if (index->next!=NULL)
		index->next->previous=index->previous;
	scm_free(index,this->persistent);
}


//returns true if there are some ondexes left in the sub tree after the deletion of the currwnt index
void schema_node::remove_index(index_cell* index)
{
	schema_index* sci=this->index_object;
	schema_index* sci1;
	while (sci!=NULL)
	{
		sci1=sci->next;
		if (sci->index==index) this->delete_index(sci);
		sci=sci1;
		RECOVERY_CRASH;
	}
	
	sc_ref* sc=this->first_child;
	while (sc!=NULL)
	{
		sc->snode->remove_index(index);
		sc=sc->next;
		RECOVERY_CRASH;
	}
}

#ifdef SE_ENABLE_FTSEARCH
schema_ft_ind_cell* schema_node::add_ft_index(ft_index_cell* index)
{
	
	schema_ft_ind_cell* sci=(schema_ft_ind_cell*)scm_malloc(sizeof(schema_ft_ind_cell),this->persistent);
	sci->index=index;
	sci->previous =NULL;
	sci->next=this->ft_index_object;
	if (this->ft_index_object != NULL)
		this->ft_index_object->previous=sci;
	this->ft_index_object=sci;  
	return sci;
}

void schema_node::delete_ft_index(schema_ft_ind_cell* index)
{
	if (index->previous!=NULL) 
		index->previous->next=index->next;
	else
		this->ft_index_object=index->next;
	if (index->next!=NULL)
		index->next->previous=index->previous;
	scm_free(index,this->persistent);
}
void schema_node::remove_ft_index(ft_index_cell* index)
{
	schema_ft_ind_cell* sci=this->ft_index_object;
	while (sci!=NULL)
	{
		schema_ft_ind_cell* sci1=sci->next;
		if (sci->index==index) 
		{
			this->delete_ft_index(sci);
			break;
		}
		sci = sci1;
		RECOVERY_CRASH;
	}	
	sc_ref* sc=this->first_child;
	while (sc!=NULL)
	{
		sc->snode->remove_ft_index(index);
		sc=sc->next;
		RECOVERY_CRASH;
	}
}

#endif
#ifdef SE_ENABLE_TRIGGERS
schema_trigger_cell* schema_node::add_trigger(trigger_cell* trigger)
{
	
	schema_trigger_cell* sct=(schema_trigger_cell*)scm_malloc(sizeof(schema_trigger_cell),this->persistent);
	sct->trigger=trigger;
	sct->previous =NULL;
	sct->next=this->trigger_object;
	if (this->trigger_object != NULL)
		this->trigger_object->previous=sct;
	this->trigger_object=sct;  
	return sct;
}

void schema_node::delete_trigger(schema_trigger_cell* trigger)
{
	if (trigger->previous!=NULL) 
		trigger->previous->next=trigger->next;
	else
		this->trigger_object=trigger->next;
	if (trigger->next!=NULL)
		trigger->next->previous=trigger->previous;
	scm_free(trigger,this->persistent);
}
void schema_node::remove_trigger(trigger_cell* trigger)
{
	schema_trigger_cell* sct=this->trigger_object;
	while (sct!=NULL)
	{
    	schema_trigger_cell* sct1=sct->next;
		if (sct->trigger==trigger) 
		{
			this->delete_trigger(sct);
			break;
		}
		sct = sct1;
		RECOVERY_CRASH;
	}	
	sc_ref* sc=this->first_child;
	while (sc!=NULL)
	{
		sc->snode->remove_trigger(trigger);
		sc=sc->next;
		RECOVERY_CRASH;
	}
}

#endif
void schema_node::get_ref_pos_to_child(const char* uri,const  char* name,t_item type,std::vector<int> sc_refs)
{
	sc_ref* sc=this->first_child;
	int pos=0;
	while (sc!=NULL)
	{
		if (my_strcmp(name,sc->name)==0 && sc->type==type)
		{
			if ((sc->xmlns==NULL && uri==NULL) || my_strcmp(uri,sc->xmlns->uri)==0 )
				sc_refs.push_back(pos);
		}
		sc=sc->next;
		pos++;
	}
}
bool schema_node::is_ancestor_or_self (schema_node* node)
{
	if (this==node) return true;
	sc_ref* sc=this->first_child;
	while (sc!=NULL)
	{
		if (sc->snode->is_ancestor_or_self(node)) return true;
		sc=sc->next;
		RECOVERY_CRASH;
	}
	return false;
}
//UNREALIZED!!!!! TEMPORARY 
/* returns the name of atomic type*/
char* convertTypeToName(xmlscm_type i)
{return "";}

