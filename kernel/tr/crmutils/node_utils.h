/*
 * File:  node_utils.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __NODE_UTILS_H
#define __NODE_UTILS_H
/*Search utils*/

#include <vector>
#include <set>

#include "common/sedna.h"

#include "common/xptr.h"
#include "tr/structures/nodes.h"
#include "tr/vmm/vmm.h"
#include "tr/structures/schema.h"

class dynamic_context;

typedef bool (*comp_schema)(const schema_node* scm,const char* uri,const char* name, t_item type);


xptr giveAnyChild(n_dsc* source,shft size);

/*returns any non-attribute child of the current node*/
xptr giveAnyNonAttributeChild(n_dsc* source,shft size);
/*returns any  child of the current node from the dm:children() sequence*/
xptr giveAnyDmChildrenChild(n_dsc* source,shft size);
/*returns any attribute child of the current node*/
xptr giveAnyAttributeChild(n_dsc* source,shft size);
/*returns any  child of the current node not from the dm:children() sequence*/
xptr giveAnyNonDmChildrenChild(n_dsc* source,shft size);
/*returns last  child of the current node not from the dm:children() sequence*/
xptr getLastNonDmChildrenChild(n_dsc* source,shft size);
/*returns last  namespace child*/
xptr getLastNamespaceChild(n_dsc* source,shft size);
/*returns the first child by document order*/
xptr giveFirstByOrderChild(xptr source,shft size);
xptr giveFirstByOrderChild_CP(xptr source,shft size);

/*returns the first element child by document order*/
xptr getFirstByOrderElementChild(xptr source);
/*returns the first element child by document order*/
xptr getFirstByOrderTextChild(xptr source);
/*returns the first  child node by document order*/
xptr  getFirstByOrderChildNode(xptr source);
/*returns the first  child  in dm:children accessor + attributes*/
xptr  getFirstByOrderNNSChildNode(xptr source);
/*returns the last  child node by document order*/
xptr  getLastByOrderChildNode(xptr source);
/*returns the next element sibling by document order*/
xptr getNextByOrderElement(xptr source);

/*returns the first attribute child by document order*/
xptr getFirstByOrderAttributeChild(xptr source);
/*returns the next attribute sibling by document order*/
xptr getNextByOrderAttribute(xptr source);

/*returns the first none attribute child by document order*/
inline xptr getFirstByOrderNoneAttributeChild(xptr source) { return getFirstByOrderChildNode(source); }
/*returns the next none attribute sibling by document order*/
inline xptr getNextByOrderNoneAttribute(xptr source) 
{ 
	CHECKP(source);
	return GETRIGHTPOINTER(source); 
}


xptr findMedianNodeDescriptor (node_blk_hdr * block);
int getNearestBorder(node_blk_hdr * block,n_dsc* node);
n_dsc* getPreviousDescriptorOfSameSort(n_dsc* node);

/* returns the next in document order descriptor corresponding to the same scheme node*/
n_dsc* getNextDescriptorOfSameSort(n_dsc* node);

/* returns the next in document order descriptor corresponding to the same scheme node in xptr*/
xptr getNextDescriptorOfSameSortXptr(xptr nodex);
xptr getPreviousDescriptorOfSameSortXptr(xptr nodex);
/* returns the next in document order sibling corresponding to the same scheme node in xptr*/
xptr getNextSiblingOfSameSortXptr(xptr nodex);
n_dsc* getNextSiblingOfSameSort(n_dsc* node);
/*returns the next non-descendant node in document*/
xptr getNextNDNode(xptr node);
/*returns the next node in document*/
xptr getNextDONode(xptr node);
/*returns the previous node in document*/
xptr getPreviousDONode(xptr node);

/*returns the next non-descendant node in document that fits input schema_node */
xptr getNextNDNode(xptr node,schema_node* scn);
/*returns the next sibling node in document that fits input schema_node */
xptr getNextSiblingNode(xptr node,schema_node* scn);
/*returns the previous sibling node in document that fits input schema_node */
xptr getPreviousSiblingNode(xptr node,schema_node* scn);
/*returns the previous node in document that fits input schema_node*/
//xptr getPreviousDONode(xptr node,schema_node* scn);
/*returns the previous non-ancestor node in document that fits input schema_node*/
xptr getPreviousNANode(xptr node,schema_node* scn);

/* returns the xptr to the nearest left neighboring descriptor of the element*/
xptr findNodeWithSameNameToInsertAfter(xptr left_sib, xptr right_sib, xptr parent, const char* name,t_item node_type,xml_ns* ns);
xptr findNodeWithSameNameToInsertAfter_CP(xptr left_sib, xptr right_sib, xptr parent, const char* name,t_item node_type,xml_ns* ns);

/* returns the xptr to the nearest left neighboring descriptor of the attribute*/
xptr findAttributeWithSameNameToInsertAfter(xptr parent,const char* name,xml_ns* ns);
xptr findAttributeWithSameNameToInsertAfter_CP(xptr parent,const char* name,xml_ns* ns);

/* returns the xptr to the nearest right neighboring descriptor of the element*/
xptr findNodeWithSameNameToInsertBefore_CP(xptr left_sib, xptr right_sib, xptr parent,const  char* name,t_item node_type,xml_ns* ns);
xptr findNodeWithSameNameToInsertBefore(xptr left_sib, xptr right_sib, xptr parent,const  char* name,t_item node_type,xml_ns* ns);

/* returns the xptr to the nearest right neighboring descriptor of the attribute*/
xptr findAttributeWithSameNameToInsertBefore_CP(xptr parent,const char* name,xml_ns* ns);
xptr findAttributeWithSameNameToInsertBefore(xptr parent,const char* name,xml_ns* ns);

/* returns shift position either element descriptor contains pointer to the child of that type 0 otherwise*/
xptr* elementContainsChild(n_dsc* parent,const char* name,t_item type,xml_ns* ns);
 
/* returns the xptr to the first child of the node identified by name and type*/
xptr getChildPointer(n_dsc* node,const char* name,t_item type,xml_ns* ns);

inline xptr getChildPointerXptr(xptr node,const char* name,t_item type,xml_ns* ns)
{
 CHECKP(node);
 return getChildPointer((n_dsc*)XADDR(node),name,type,ns);
}
/*returns true iff there is already attribute set with the same local name and uri*/
xptr isAttributePointerSet(n_dsc* node,const char* name,const char* uri);
void getChildPointerList(xptr node,const char* name,t_item type,char* uri,std::vector<xptr> &childs);

/* utils for persistent string library */
xptr	getLeftmostDescriptorWithPstrInThisBlock(xptr blk, xptr node);

xptr	getRightmostDescriptorWithPstrInThisBlock(xptr blk,xptr node);

/* return the vector of schema node descandants of the current schema node*/
void getSchemeDescendantsOrSelf(schema_node* scm,const char* uri,const char* name, t_item type,  comp_schema cfun,std::vector<schema_node*> &result);
void getSchemeDescendants(schema_node* scm,const char* uri,const char* name, t_item type, comp_schema cfun,std::vector<schema_node*> &result);

/* return base uri if exists*/
xptr getBaseUri(xptr node);
/* return root */
xptr getRoot(xptr node);

/* returns nearest unempty page from right */
xptr getUnemptyBlockFore(xptr p); 
/* returns nearest unempty page from left */
xptr getUnemptyBlockBack(xptr p); 


/* return the vector of schema node descandants of the current schema node*/

int getChildrenXptr(const xptr& parent,const char* uri,const char* name, t_item type, comp_schema cfun,xptr& first, xptr*& res);
void getSchemeChilds(schema_node* scm,const char* uri,const char* name, t_item type, comp_schema cfun,std::vector<schema_node*> &result);
inline void getSchemeChildsOrSelf(schema_node* scm,const char* uri,const char* name, t_item type,  comp_schema cfun,std::vector<schema_node*> &result)
{
	if (cfun(scm,uri,name,type))	
		result.push_back(scm);
	getSchemeChilds(scm,uri,name,type,cfun,result);
}

xptr getFirstAttributeDescendantAndFillPath(std::vector<xptr> &descstack);
/* returns the first descandant or self of the current node that corresponds to the stated schema_node*/
xptr getFirstDescandantByScheme(xptr ancestor,schema_node* scm);


xptr getNextDescandantofSameSort (xptr ancestor,xptr node);
/*comparison function for schema nodes*/
 bool comp_type(const schema_node* scm,const char* uri,const char* name, t_item type);
 bool comp_qname_type(const schema_node* scm,const char* uri,const char* name, t_item type);
 bool comp_local_type(const schema_node* scm,const char* uri,const char* name, t_item type);
 bool comp_uri_type(const schema_node* scm,const char* uri,const char* name, t_item type);
/* returns the inderection pointer to the ancestor of the descriptor that corresponds
to the selected ancestor by scheme */
xptr getAncestorIndirectionByScheme (n_dsc* node, const schema_node * scm_node, const schema_node* scm_anc);

inline xptr getNodeAncestorIndirectionByScheme (xptr node, schema_node* scm_anc)
{
	CHECKP(node);
	schema_node* sn=(GETBLOCKBYNODE(node))->snode;
	return getAncestorIndirectionByScheme((n_dsc*)XADDR(node),sn,scm_anc);
}

inline xptr getAncestorIndirectionByScheme_XPTR (xptr node, schema_node * scm_node, schema_node* scm_anc)
{
	CHECKP(node);
	return getAncestorIndirectionByScheme ((n_dsc*)XADDR(node), scm_node, scm_anc);

}
/* returns the size of the descriptor structure in the curent node*/
shft  size_of_node(node_blk_hdr* block);

inline xptr removeIndirection(xptr indir)
{
	if (indir!=XNULL)
	{
		CHECKP(indir);
		return *((xptr*)XADDR(indir));
	}
	return XNULL;
}
inline xptr getNodeAncestorByScheme (xptr node, schema_node* scm_anc)
{
	xptr tmp=getNodeAncestorIndirectionByScheme (node, scm_anc);
	if (tmp!=XNULL) tmp=removeIndirection(tmp);
	return tmp;
}

typedef bool (*node_type_restriction)(t_item);

bool is_text(t_item t);
inline bool is_comment(t_item t)
{
	return t==comment;
}
inline bool is_pi(t_item t)
{
	return t==pr_ins;
}
bool is_node(t_item t);
bool dm_children_accessor_filter(t_item t);
bool dm_attribute_accessor_filter(t_item t);
/*
bool is_pnk_element(t_item t);
bool is_pnk_attribute(t_item t);
*/
bool inline is_element (xptr node)
{
	CHECKP(node);
	return (GETSCHEMENODEX(node)->type==element);
}
bool inline is_node_attribute (xptr node)
{
	CHECKP(node);
 return (GETSCHEMENODEX(node)->type==attribute);
}

bool inline is_node_child (xptr node)
{
	CHECKP(node);
	return (GETSCHEMENODEX(node)->type!=attribute &&GETSCHEMENODEX(node)->type!=xml_namespace);
}

bool inline is_node_in_collection (xptr node)
{

	CHECKP(node);
	return ((schema_node*)(GETSCHEMENODEX(node))->root)->nodecnt > 1;
}
//checks whether the right sibling of the node is  attribute
bool inline is_next_node_attribute (xptr node)
{
	CHECKP(node);
	node=GETRIGHTPOINTER(node);
	if (node!=XNULL)
	{
		CHECKP(node);
		return is_node_attribute (node);
	}
	return false;
}

//checks if the node is the descendant of one of the nodes in the vector
bool is_scmnode_has_ancestor_or_self(schema_node * scm_node, std::set<schema_node*>* scm_nodes_set );

#define get_in_scope_namespaces(n, r, cxt)  get_in_scope_namespaces_local(n, r, cxt)
//Namespaces
void get_in_scope_namespaces_local(xptr node,std::vector<xml_ns*> &result,dynamic_context *cxt);
void get_in_scope_namespaces_broad(xptr node,std::vector<xml_ns*> &result,dynamic_context *cxt);
void get_namespaces_for_inherit(xptr node,std::vector<xml_ns*> &result);
xml_ns* generate_pref(int ctr,const char* uri,dynamic_context *cxt);
#endif

