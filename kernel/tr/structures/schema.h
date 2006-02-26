/*
 * File:  schema.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _SCHEMA_H
#define _SCHEMA_H
#include "nodes.h"
#include "pers_map.h"

#define ISINDEXSUPPORTED(schema) schema->index_object!=NULL

struct dn_metadata_cell 
{
	xptr root;
	char* document_name;
	inline bool less( dn_metadata_cell *p1) 
	{
		return ( my_strcmp(this->document_name,p1->document_name)<0);
	}
	inline bool equals( dn_metadata_cell *p1) 
	{
		return my_strcmp(this->document_name,p1->document_name)==0;
	}
	inline bool less(const void* p1,const void* p2) 
	{
		return my_strcmp(this->document_name,(char*)p1)<0;
	}
	inline bool equals(const void* p1,const void* p2) 
	{
		return  my_strcmp(this->document_name,(char*)p1)==0;
	}
};
struct xml_ns 
{
	char* prefix;
	char* uri;
	int counter;
	bool persistent;
	/*initialization of namespace info*/
	static xml_ns* init(const char* _uri, const char* _prefix,bool persistent);
	static void delete_namespace_node(xml_ns* ns );
	static inline bool equals(const xml_ns *p1, const xml_ns *p2)
	{
		return (p1==p2)||(p1!=NULL && p2!=NULL && my_strcmp(p1->uri,p2->uri)==0 && my_strcmp(p1->prefix,p2->prefix)==0);
	}

	inline bool less( xml_ns *p1) 
	{
		return my_strcmp(this->uri,((xml_ns*)p1)->uri)<0 ||
			(my_strcmp(this->uri,((xml_ns*)p1)->uri)==0 && my_strcmp(this->prefix,((xml_ns*)p1)->prefix)<0);
	}
	inline bool equals( xml_ns *p1) 
	{
		return my_strcmp(this->uri,((xml_ns*)p1)->uri)==0 &&
			my_strcmp(this->prefix,((xml_ns*)p1)->prefix)==0;
	}
	inline bool less(const void* p1,const void* p2) 
	{
		return my_strcmp(this->uri,(char*)p1)<0 ||
			(my_strcmp(this->uri,(char*)p1)==0 && my_strcmp(this->prefix,(char*)p2)<0);
	}
	inline bool equals(const void* p1,const void* p2) 
	{
		return my_strcmp(this->uri,(char*)p1)==0 
			&& my_strcmp(this->prefix,(char*)p2)==0;
	}
};
struct index_cell;
#ifdef SE_ENABLE_FTSEARCH
struct ft_index_cell;
struct schema_ft_ind_cell
/* ft_index reference object*/
{
	ft_index_cell* index;
	schema_ft_ind_cell* next;
	schema_ft_ind_cell* previous;
		
};
#endif
struct schema_ind_cell
/* index reference object*/
{
	index_cell* index;
	schema_ind_cell* next;
	schema_ind_cell* previous;
		
};
struct schema_index
/* index reference object*/
{
	schema_node* object;
	index_cell* index;
	schema_index* next;
	schema_index* previous;
		
};
/* schema node referencing object*/
struct sc_ref
{ 
	char*			name;	/* item name	; NULL for txt */
	xml_ns* xmlns;
	schema_node*	snode;	/* scheme node */
	t_item			type;	/* item typification */
	sc_ref* 		next;
	sc_ref* 		prev;
	static void init(void* p);
	/* Destructor frees memory occupied by name field */
	static void destroy(sc_ref* scm,bool persistent);	
};

/* Scheme node */
struct doc_scheme_node;
struct schema_node
{
	t_item				type;		/* type of node: element/text/attribute/simple */
	char*				name;		/* for elements/attributes */
	xml_ns* xmlns;
	xptr				bblk;		/* pointer to the first block of block chain */
	struct schema_node*	parent;		/* pointer to the parent node */
    sc_ref*				first_child;/* refernce to the first child */
	sc_ref*				last_child;	/* refernce to the last child  */
	/*statistics*/
	int nodecnt;
	int blockcnt;
	int extnids;
	int indir_blk_cnt;
	__int64 textcnt;
	/*stat end*/
	struct doc_schema_node* root;
	
	int cl_hint;
	xptr ind_entry;//pointer to first free space in inderection entry
	bool persistent;
	//INDEX SUPPORT
		
	schema_index* index_object;
	schema_index* add_index(index_cell* index,struct schema_node* object);
		
	//INNER FUNCTIONS
	void remove_index(index_cell* index);
	void delete_index(schema_index* index);
#ifdef SE_ENABLE_FTSEARCH
	//FT_INDEX SUPPORT
		
	schema_ft_ind_cell* ft_index_object;
	schema_ft_ind_cell* add_ft_index(ft_index_cell* index);
		
	//INNER FUNCTIONS
	void remove_ft_index(ft_index_cell* index);
	void delete_ft_index(schema_ft_ind_cell* index);
#endif
	
	
	static void init(void* p);


	/*initialisation of schema node*/
	static schema_node* init( doc_schema_node* root,xml_ns* _xmlns,const char* name, t_item type,bool persistent);
	/* Destructor frees memory occupied by name field */
	void destroy(schema_node* scm);	

	/* Find first child of given type of this node by name */
	schema_node* get_child(const xml_ns* _xmlns,const char* name, t_item type);

	/* Links child and parent nodes into tree structure according to
       the children and parent positions in the scheme.
       Check for name uniqueness is performed. 
       Class is responsible for release of node object
	*/
	void add_child(schema_node* node);

	/* inserts new node to the schema as the child of the existing one*/
	schema_node* add_child( xml_ns* _xmlns,const char* name, t_item type);
	
	/* returns true if this contains the child and data;*/
	int is_node_in_scheme_and_in_data (const xml_ns* _xmlns,const  char* name, t_item type);
	
	/*returns the total number of children by scheme*/
	shft get_child_count();

	/*returns the name of the i'th child of the node*/
	char* get_name (int i);

	/*returns the type of the i'th child of the node*/
	t_item get_type(int i);

	/*returns position of the child with the given name and of the given descriptor type exist in schema as the
	child of the node that corresponds to the current block header; -1 otherwise*/
	int has_child_by_schema(const xml_ns* _xmlns,const  char* name, t_item type);
	
	/*fiils vector with the positions of the childs with the given name, uri and of the given descriptor type exist in schema as the
	child of the node that corresponds to the current block header*/
	void get_ref_pos_to_child(const char* uri,const  char* name,t_item type,std::vector<int> sc_refs);

	/*deletes schema node*/
	void delete_scheme_node();
	bool is_ancestor_or_self (schema_node* node);
};



struct doc_schema_node: public schema_node
{
	
	xptr				first_ind_blk;		/* pointer to the first block of indirection chain */
	xptr				ind_free_space;		/* pointer to the last free space in indirection table*/
	xptr	ext_nids_block;
	__int64 total_ext_nids;
	schema_ind_cell* sc_idx;
#ifdef SE_ENABLE_FTSEARCH
	schema_ft_ind_cell* sc_ft_idx;
#endif
	/*initialisation of schema node*/
	static void init(void* p);
	static doc_schema_node* init( bool persistent);
	void create_index(index_cell* idx);
	void delete_index(index_cell* idx);
	#ifdef SE_ENABLE_FTSEARCH
		void create_ft_index(ft_index_cell* idx);
		void delete_ft_index(ft_index_cell* idx);
	#endif
	
};
struct col_schema_node: public doc_schema_node
{
	
	xptr				eblk;		/* pointer to the last block of block chain */
	pers_sset<dn_metadata_cell, unsigned int> * metadata;
	/*initialisation of schema node*/
	static void init(void* p);
	static col_schema_node* init( bool persistent);
	dn_metadata_cell *find_metadata_of_document_in_col(xptr node);
	pers_sset<dn_metadata_cell,unsigned int>::pers_sset_entry* search_metadata_cell(const char *document_name);
	void inline free_metadata_cell(pers_sset<dn_metadata_cell,unsigned int>::pers_sset_entry* entry)
	{
		dn_metadata_cell* mdc=entry->obj;
		this->metadata->rb_delete(entry);
		scm_free(mdc->document_name,true);
		scm_free(mdc,true);
	}
	void inline free_metadata(pers_sset<dn_metadata_cell,unsigned int>::pers_sset_entry* entry)
	{
		dn_metadata_cell* mdc=entry->obj;
		scm_free(mdc->document_name,true);
		scm_free(mdc,true);
		entry->obj=NULL;
	}
	void inline free_map()
	{
		pers_sset<dn_metadata_cell,unsigned int>::free(this->metadata);
	}
};
struct temp_schema_node
{
	t_item				type;		/* type of node: element/text/attribute/simple */
	char*				name;		/* for elements/attributes */
	xml_ns* xmlns;
	struct temp_schema_node*	parent;		/* pointer to the parent node */
    temp_schema_node*				first_child;/* refernce to the first child */
	temp_schema_node*				next_sibl;	/* refernce to the last child  */
	int nodecnt;
	int nid_size;
	int nid_iteration;
	
	static void init(void* p);

	/*initialisation of schema node*/
	static temp_schema_node* init( xml_ns* _xmlns,const char* name, t_item type);
	/* Destructor frees memory occupied by name field */
	void destroy(temp_schema_node* scm);	

	/* Find first child of given type of this node by name */
	temp_schema_node* get_child(const xml_ns* _xmlns,const char* name, t_item type);

	/* Links child and parent nodes into tree structure according to
       the children and parent positions in the scheme.
       Check for name uniqueness is performed. 
       Class is responsible for release of node object
	*/
	void add_child(temp_schema_node* node);

	/* inserts new node to the schema as the child of the existing one*/
	temp_schema_node* add_child( xml_ns* _xmlns,const char* name, t_item type);
	
	/*returns the total number of children by scheme*/
	shft get_child_count();

	

	/*returns position of the child with the given name and of the given descriptor type exist in schema as the
	child of the node that corresponds to the current block header; -1 otherwise*/
	int has_child_by_schema(const xml_ns* _xmlns,const  char* name, t_item type);
	
	/*deletes schema node*/
	void delete_scheme_node();
};



/* returns the name of atomic type*/
char* convertTypeToName(xmlscm_type i);


/*returns the type of the scheme node*/
#define GETTYPE(scm_node) scm_node->type

/*returns the name of the scheme node*/
#define GETNAME( scm_node)scm_node->name

/*updates pointer to the first block with the descriptors that correspond to schema node*/
#define UPDATEFIRSTBLOCKPOINTER(scm_node, block) scm_node->bblk=block



/*returns pointer to the first block with the descriptors that correspond to schema node*/
#define GETFIRSTBLOCKPOINTER(scm_node) scm_node->bblk


#endif

