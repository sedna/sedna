/*
 * File:  updates.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/updates/updates.h"
#include "tr/mo/micro.h"
#include "tr/crmutils/node_utils.h"
#include "tr/executor/base/xptr_sequence.h"
//#include "crmutils.h"
#include "tr/nid/numb_scheme.h"
#include "tr/structures/indirection.h"
#include "tr/executor/xqops/PPConstructors.h"
#ifdef SE_ENABLE_TRIGGERS
#include "tr/triggers/triggers.h"
#endif
#define IGNORE_UPDATE_ERRORS
#ifdef SE_ENABLE_FTSEARCH
std::map<ft_index_cell*,xptr_sequence*> updated_nodes;
std::map<ft_index_cell*,xptr_sequence*> inserted_nodes;
std::map<ft_index_cell*,xptr_sequence*> deleted_nodes;
#endif
static bool carrier=false;
#ifdef SE_ENABLE_FTSEARCH
void clear_ft_sequences()
{
	std::map<ft_index_cell*,xptr_sequence*>::iterator it=updated_nodes.begin();
	while (it!=updated_nodes.end())
	{
		delete it->second;
		it++;
	}
	updated_nodes.clear();
	it=inserted_nodes.begin();
	while (it!=inserted_nodes.end())
	{
		delete it->second;
		it++;
	}
	inserted_nodes.clear();
	it=deleted_nodes.begin();
	while (it!=deleted_nodes.end())
	{
		delete it->second;
		it++;
	}
	deleted_nodes.clear();
}
void execute_modifications()
{
	if (is_rolled_back()) return; 
	std::map<ft_index_cell*,xptr_sequence*>::iterator it=inserted_nodes.begin();
	while (it!=inserted_nodes.end())
	{
		it->first->insert_to_index(it->second);
		it++;
	}
	it=updated_nodes.begin();
	while (it!=updated_nodes.end())
	{
		it->first->update_index(it->second);
		it++;
	}
	it=deleted_nodes.begin();
	while (it!=deleted_nodes.end())
	{
		it->first->delete_from_index(it->second);
		it++;
	}		
}
void update_insert_sequence(xptr node,ft_index_cell* icell)
{
	std::map<ft_index_cell*,xptr_sequence*>::iterator it=inserted_nodes.find(icell);
	if (it==inserted_nodes.end())
	{
		xptr_sequence* seq=se_new xptr_sequence();
		seq->add(node);
		inserted_nodes[icell]=seq;
	}
	else
		it->second->add(node);
				
}
void update_update_sequence(xptr node,ft_index_cell* icell)
{
	std::map<ft_index_cell*,xptr_sequence*>::iterator it=updated_nodes.find(icell);
	if (it==updated_nodes.end())
	{
		xptr_sequence* seq=se_new xptr_sequence();
		seq->add(node);
		updated_nodes[icell]=seq;
	}
	else
		it->second->add(node);
				
}
void update_delete_sequence(xptr node,ft_index_cell* icell)
{
	std::map<ft_index_cell*,xptr_sequence*>::iterator it=deleted_nodes.find(icell);
	if (it==deleted_nodes.end())
	{
		xptr_sequence* seq=se_new xptr_sequence();
		seq->add(node);
		deleted_nodes[icell]=seq;
	}
	else
		it->second->add(node);
}
void update_insert_sequence(xptr node,schema_ft_ind_cell* icell)
{
	if (icell==NULL) return;
	schema_ft_ind_cell* obj=icell;
	CHECKP(node);
	xptr ind=((n_dsc*)XADDR(node))->indir;
	while (obj!=NULL)
	{
		update_insert_sequence(ind,obj->index);
		obj=obj->next;
	}
	CHECKP(node);
}
void update_update_sequence(xptr node,schema_ft_ind_cell* icell)
{
	if (icell==NULL) return;
	schema_ft_ind_cell* obj=icell;
	CHECKP(node);
	xptr ind=((n_dsc*)XADDR(node))->indir;
	while (obj!=NULL)
	{
		update_update_sequence(ind,obj->index);
		obj=obj->next;
	}
	CHECKP(node);
}
void update_delete_sequence(xptr node,schema_ft_ind_cell* icell)
{
	if (icell==NULL ) return;
	schema_ft_ind_cell* obj=icell;
	CHECKP(node);
	xptr ind=((n_dsc*)XADDR(node))->indir;
	while (obj!=NULL)
	{
		update_delete_sequence(ind,obj->index);
		obj=obj->next;
	}
	CHECKP(node);
}
void init_ft_sequences (xptr& left, xptr& right, xptr& parent)
{
	if (IS_TMP_BLOCK(left)||IS_TMP_BLOCK(right)||IS_TMP_BLOCK(parent)) return;
	xptr tmp;
	schema_node* scn;
	if (parent!=XNULL)
	{
		tmp=parent;
		CHECKP(parent);
		scn=(GETBLOCKBYNODE(parent))->snode;
	}
	else
	{
		tmp=(left==XNULL)?right:left;
		CHECKP(tmp);
		scn=(GETBLOCKBYNODE(tmp))->snode->parent;
	}
	if (scn->root==scn || scn->root->sc_ft_idx==NULL) return;
	while (scn!=NULL)
	{
		schema_ft_ind_cell* obj=scn->ft_index_object;
		if (obj!=NULL)
		{
			tmp=getNodeAncestorIndirectionByScheme(tmp,scn);
			while (obj!=NULL)
			{
				update_update_sequence(tmp,obj->index);
				obj=obj->next;
			}
			tmp=removeIndirection(tmp);
		}
		scn=scn->parent;
	}
}
#endif

xptr deep_pers_copy(xptr left, xptr right, xptr parent, xptr node,bool save_types, unsigned short depth)
{
#ifdef SE_ENABLE_FTSEARCH
	if (!depth) init_ft_sequences(left,right,parent);	
#endif
	xptr res;
#ifdef SE_ENABLE_TRIGGERS
	if (parent==XNULL)
	{
		if(left!=XNULL)
		{
			CHECKP(left);
			parent=removeIndirection(((n_dsc*)XADDR(left))->pdsc);
		}
		else
		{
			CHECKP(right);
			parent=removeIndirection(((n_dsc*)XADDR(right))->pdsc);
		}
	}
    node = apply_per_node_triggers(node, XNULL, parent, NULL, TRIGGER_BEFORE, TRIGGER_INSERT_EVENT);
    if (node == XNULL)
    {
        if(left==XNULL) return XNULL;
        CHECKP(left);
		return left;
    }
#endif
	CHECKP(node);
	switch(GETTYPE(GETSCHEMENODEX(node)))
	{
	case element:
		{
			xptr node_indir=((n_dsc*)XADDR(node))->indir;
			res =insert_element(left, right, parent,GETNAME(GETSCHEMENODEX(node)), (save_types)?((e_dsc*)XADDR(node))->type:xs_untyped,(GETSCHEMENODEX(node))->xmlns);
			xptr indir= ((n_dsc*)XADDR(res))->indir;
			node=removeIndirection(node_indir);
			CHECKP(node);
			xptr left_ngh=XNULL;
			xptr child= giveFirstByOrderChild(node,COUNTREFERENCES((GETBLOCKBYNODE(node)),size_of_node((GETBLOCKBYNODE(node)))));
			if (child!=XNULL)
		 {
			 CHECKP(child);
			 node_indir=((n_dsc*)XADDR(child))->indir;
			 left_ngh=deep_pers_copy(XNULL, XNULL, res,child,save_types,depth+1);
			 child=removeIndirection(node_indir);
			 CHECKP(child);
			 child=GETRIGHTPOINTER(child);
			 while (child!=XNULL)
			 {
				 CHECKP(child);
				 node_indir=((n_dsc*)XADDR(child))->indir;
                 //MG: deep_pers_copy can return XNULL if a trigger canceled the insertion 
				 //    and there were now any left sibling
			 	 if(left_ngh==XNULL)  
					left_ngh=deep_pers_copy(XNULL, XNULL, res,child,save_types,depth+1);
				 else
					left_ngh=deep_pers_copy(left_ngh, XNULL, XNULL,child,save_types,depth+1);
				 child=removeIndirection(node_indir);
				 CHECKP(child);
				 child=GETRIGHTPOINTER(child);
			 }
			 res=removeIndirection(indir);			 
			 CHECKP(res);
		 }
		}
	 break;
	case text:
	 {
		 int size=((t_dsc*)XADDR(node))->size;
		 if (size>0)
		 {
			xptr ind_ptr=((t_dsc*)XADDR(node))->data;
			 if (size <=PSTRMAXSIZE)
			 {
				//char *z=se_new char[size];
				CHECKP(ind_ptr);
				shft shift= *((shft*)XADDR(ind_ptr));
				char* data=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
				//memcpy(z,data,size);
				res =insert_text(left, right, parent,&ADDR2XPTR(data),size,text_doc);
				//delete [] z;
			 }
			 else
				res =insert_text(left, right, parent,&ind_ptr,size,text_doc);
		}
		else throw SYSTEM_EXCEPTION("BAD DATA!!!");
	 }
	 break;
	case comment:
		{
		 int size=((t_dsc*)XADDR(node))->size;
		 if (size>0)
		 {
			char *z=se_new char[size];
			xptr ind_ptr=((t_dsc*)XADDR(node))->data;
			CHECKP(ind_ptr);
			shft shift= *((shft*)XADDR(ind_ptr));
			char* data=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
			memcpy(z,data,size);
			res =insert_comment(left, right, parent,z,size);
			delete [] z;
		}
		else res =insert_comment(left, right, parent,NULL,0);
	 }
	 break;
 case cdata:
		{
		 int size=((t_dsc*)XADDR(node))->size;
		 if (size>0)
		 {
			char *z=se_new char[size];
			xptr ind_ptr=((t_dsc*)XADDR(node))->data;
			CHECKP(ind_ptr);
			shft shift= *((shft*)XADDR(ind_ptr));
			char* data=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
			memcpy(z,data,size);
			res =insert_cdata(left, right, parent,z,size);
			delete [] z;
		}
		else res =insert_cdata(left, right, parent,NULL,0);
	 }
	 break;
 case pr_ins:
		{
		 int size=((t_dsc*)XADDR(node))->size;
		 int tsize=((pi_dsc*)XADDR(node))->target;
		 if (size>0)
		 {
			char *z=se_new char[size];
			xptr ind_ptr=((t_dsc*)XADDR(node))->data;
			CHECKP(ind_ptr);
			shft shift= *((shft*)XADDR(ind_ptr));
			char* data=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
			memcpy(z,data,size);
			res =insert_pi(left, right, parent,z,tsize,z+tsize+1,size-tsize-1);
			delete [] z;
		}
		else res =insert_pi(left, right, parent,NULL,0,NULL,0);
	 }
	 break;
 case attribute:
	 {
		int size=((t_dsc*)XADDR(node))->size;
		if (size>0)
		{
			char *z=se_new char[size];
			xptr ind_ptr=((t_dsc*)XADDR(node))->data;
			xmlscm_type typ=((a_dsc*)XADDR(node))->type;
			char* nam= GETNAME(GETSCHEMENODEX(node));
			CHECKP(ind_ptr);
			shft shift= *((shft*)XADDR(ind_ptr));
			char* data=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
			memcpy(z,data,size);
			CHECKP(node);
			res =insert_attribute(left, right, parent,nam, (save_types)?typ:xs_untypedAtomic,z,size,(GETSCHEMENODEX(node))->xmlns);
			delete [] z;
		}
		else
			res =insert_attribute(left, right, parent,GETNAME(GETSCHEMENODEX(node)), (save_types)?((a_dsc*)XADDR(node))->type:xs_untypedAtomic,NULL,0,(GETSCHEMENODEX(node))->xmlns);

	 }
	 break;
 case xml_namespace:
	 {
		 res =insert_namespace(left, right, parent,((ns_dsc*)XADDR(node))->ns);
	 }
	 break;
 default:void clear_temp();
	 throw SYSTEM_EXCEPTION("Deep copy error: document node copied");
 }
	
 CHECKP(res);
#ifdef SE_ENABLE_FTSEARCH
 update_insert_sequence(res,(GETBLOCKBYNODE(res))->snode->ft_index_object); 
#endif
#ifdef SE_ENABLE_TRIGGERS
 if (parent==XNULL) parent=removeIndirection(((n_dsc*)XADDR(left))->pdsc);
 apply_per_node_triggers(res, XNULL, parent, NULL, TRIGGER_AFTER, TRIGGER_INSERT_EVENT);
#endif
 CHECKP(res); 
 return res;
}
xptr copy_content(xptr newnode,xptr node,xptr left,bool save_types)
{
	CHECKP(node);
	xptr left_ngh=XNULL;
	xptr child= giveFirstByOrderChild(node,COUNTREFERENCES((GETBLOCKBYNODE(node)),size_of_node((GETBLOCKBYNODE(node)))));
	if (child!=XNULL)
	{
		CHECKP(child);
		xptr node_indir=((n_dsc*)XADDR(child))->indir;
		left_ngh=deep_pers_copy(left, XNULL, newnode,child,save_types);
		child=removeIndirection(node_indir);
		CHECKP(child);
		child=GETRIGHTPOINTER(child);
		while (child!=XNULL)
		{
			CHECKP(child);
			node_indir=((n_dsc*)XADDR(child))->indir;
			left_ngh=deep_pers_copy(left_ngh, XNULL, XNULL,child,save_types);
			child=removeIndirection(node_indir);
			CHECKP(child);
			child=GETRIGHTPOINTER(child);
		}
	}
	return left_ngh;
}
void swizzleNamespace (xml_ns*& ns,upd_ns_map*& updmap)
{
	if (updmap==NULL)
		updmap= se_new upd_ns_map;
	upd_ns_map::const_iterator it=updmap->find(ns);
	if (it==updmap->end())
	{
		xml_ns* tmp=(xml_ns*)entry_point->nslist->find(ns->uri,ns->prefix);
		if (tmp==NULL) 
		{
			tmp=xml_ns::init(ns->uri,ns->prefix,true);
			entry_point->nslist->put(tmp);
		}
		(*updmap)[ns]=tmp;
		ns=tmp;
	}
	else
		ns=it->second;
}
/*void checkSwiizleTab(upd_ns_map*& updmap)
{
	upd_ns_map::const_iterator it=updmap->begin();
	while (it!=updmap->end())
	{
		xml_ns* tmp=it->second;
		if (!tmp->counter)
			xml_ns::delete_namespace_node(tmp);
	}
}*/
xptr deep_temp_copy(xptr left, xptr right, xptr parent, xptr node,upd_ns_map*& updmap, unsigned short depth)
{
#ifdef SE_ENABLE_FTSEARCH
	if (!depth) init_ft_sequences(left,right,parent);	
#endif
#ifdef SE_ENABLE_TRIGGERS
    if (parent==XNULL) 
	{
		if(left!=XNULL)
		{
			CHECKP(left);
			parent=removeIndirection(((n_dsc*)XADDR(left))->pdsc);
		}
		else
		{
			CHECKP(right);
			parent=removeIndirection(((n_dsc*)XADDR(right))->pdsc);
		}
	}
    node = apply_per_node_triggers(node, XNULL, parent, NULL, TRIGGER_BEFORE, TRIGGER_INSERT_EVENT);
	if (node == XNULL)
	{
		if(left==XNULL) return XNULL;
		CHECKP(left);
		return left;
	}
#endif
	CHECKP(node);
	xptr res;

	switch(GETTYPE(GETSCHEMENODEX(node)))
	{
	case element:
		{
			xml_ns* ns=(GETSCHEMENODEX(node))->xmlns;
			if (ns!=NULL)	swizzleNamespace(ns,updmap);
			res =insert_element(left, right, parent,GETNAME(GETSCHEMENODEX(node)), ((e_dsc*)XADDR(node))->type,ns);
			CHECKP(node);
			xptr left_ngh=XNULL;
			xptr child= giveFirstByOrderChild(node,COUNTREFERENCES((GETBLOCKBYNODE(node)),size_of_node((GETBLOCKBYNODE(node)))));
			if (child!=XNULL)
		    {
				left_ngh=deep_temp_copy(XNULL, XNULL, res,child,updmap,depth+1);
				CHECKP(child);
				child=GETRIGHTPOINTER(child);
				while (child!=XNULL)
				{
					CHECKP(child);
					//MG: deep_temp_copy can return XNULL if a trigger canceled the insertion 
					//    and there were now any left sibling
					if(left_ngh==XNULL)  
						left_ngh=deep_temp_copy(XNULL, XNULL, res,child,updmap,depth+1);
					else
						left_ngh=deep_temp_copy(left_ngh, XNULL, XNULL,child,updmap,depth+1);
					CHECKP(child);
					child=GETRIGHTPOINTER(child);
				}
				if(left_ngh!=XNULL)
				{
					CHECKP(left_ngh);
					xptr indir= ((n_dsc*)XADDR(left_ngh))->pdsc;
					res=removeIndirection(indir);			 
				}
			}
		}
	 break;
 case text:
	 {
		 int size=((t_dsc*)XADDR(node))->size;
		 if (size>0)
		 {
			//char *z=se_new char[size];
			xptr ind_ptr=((t_dsc*)XADDR(node))->data;
			CHECKP(ind_ptr);
			shft shift= *((shft*)XADDR(ind_ptr));
			char* data=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
			//memcpy(z,data,size);
			res =insert_text(left, right, parent,&ADDR2XPTR(data),size,text_doc);
			//res =insert_text(left, right, parent,z,size);
			//delete [] z;
		}
		else throw SYSTEM_EXCEPTION("BAD DATA!!!");
	 }
	 break;
 case comment:
		{
		 int size=((t_dsc*)XADDR(node))->size;
		 if (size>0)
		 {
			char *z=se_new char[size];
			xptr ind_ptr=((t_dsc*)XADDR(node))->data;
			CHECKP(ind_ptr);
			shft shift= *((shft*)XADDR(ind_ptr));
			char* data=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
			memcpy(z,data,size);
			res =insert_comment(left, right, parent,z,size);
			delete [] z;
		}
		else res =insert_comment(left, right, parent,NULL,0);
	 }
	 break;
 case cdata:
		{
		 int size=((t_dsc*)XADDR(node))->size;
		 if (size>0)
		 {
			char *z=se_new char[size];
			xptr ind_ptr=((t_dsc*)XADDR(node))->data;
			CHECKP(ind_ptr);
			shft shift= *((shft*)XADDR(ind_ptr));
			char* data=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
			memcpy(z,data,size);
			res =insert_cdata(left, right, parent,z,size);
			delete [] z;
		}
		else res =insert_cdata(left, right, parent,NULL,0);
	 }
	 break;
 case pr_ins:
		{
		 int size=((t_dsc*)XADDR(node))->size;
		 int tsize=((pi_dsc*)XADDR(node))->target;
		 if (size>0)
		 {
			char *z=se_new char[size];
			xptr ind_ptr=((t_dsc*)XADDR(node))->data;
			CHECKP(ind_ptr);
			shft shift= *((shft*)XADDR(ind_ptr));
			char* data=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
			memcpy(z,data,size);
			res =insert_pi(left, right, parent,z,tsize,z+tsize+1,size-tsize-1);
			delete [] z;
		}
		else res =insert_pi(left, right, parent,NULL,0,NULL,0);
	 }
		break;
 case attribute:
	 {
		xml_ns* ns=(GETSCHEMENODEX(node))->xmlns;
		if (ns!=NULL)swizzleNamespace(ns,updmap);
		int size=((t_dsc*)XADDR(node))->size;
		if (size>0)
		{
			char *z=se_new char[size];
			xptr ind_ptr=((t_dsc*)XADDR(node))->data;
			xmlscm_type typ=((a_dsc*)XADDR(node))->type;
			char* nam= GETNAME(GETSCHEMENODEX(node));
			CHECKP(ind_ptr);
			shft shift= *((shft*)XADDR(ind_ptr));
			char* data=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
			memcpy(z,data,size);
			res =insert_attribute(left, right, parent,nam, typ,z,size,ns);
			delete [] z;
		}
		else
			res =insert_attribute(left, right, parent,GETNAME(GETSCHEMENODEX(node)), ((a_dsc*)XADDR(node))->type,NULL,0,ns);
	 }
	 break;
case xml_namespace:
	 {
		 xml_ns* ns=((ns_dsc*)XADDR(node))->ns;
		 if (ns!=NULL)swizzleNamespace(ns,updmap);
		 res =insert_namespace(left, right, parent,ns);
	 }
	 break;
 default:
	 throw SYSTEM_EXCEPTION("Update error");
 }
	CHECKP(res);
#ifdef SE_ENABLE_FTSEARCH
 update_insert_sequence(res,(GETBLOCKBYNODE(res))->snode->ft_index_object); 
#endif
#ifdef SE_ENABLE_TRIGGERS
    if (parent==XNULL) parent=removeIndirection(((n_dsc*)XADDR(left))->pdsc);
    apply_per_node_triggers(res, XNULL, parent, NULL, TRIGGER_AFTER, TRIGGER_INSERT_EVENT);
#endif
 CHECKP(res);
	return res;
}
xptr deep_node_copy(xptr left_ind, xptr right_ind, xptr parent_ind, xptr node)
{
	// removing indirection from params
	xptr left=removeIndirection(left_ind);
	xptr right=removeIndirection(right_ind);
	xptr parent=removeIndirection(parent_ind);
	upd_ns_map* ins_swiz=NULL;
	xptr res= deep_temp_copy(left, right, parent, node,ins_swiz);
	if (ins_swiz!=NULL) 
	{
//		checkSwiizleTab(ins_swiz);	
		delete ins_swiz;
	}
	return res;
}
xptr copy_to_temp(xptr node)
{
	if(PPConstructor::checkInitial()) carrier=true;
	return deep_pers_copy(XNULL,XNULL,PPConstructor::virt_root,node,true);
}

// copies a node without children to temp  (without using deep_pers_copy)  
/*xptr copy_node_to_temp(xptr node)
{
	if(PPConstructor::checkInitial()) carrier=true;
    CHECKP(node);
	xptr res;
    
}*/
void clear_temp()
{
	if (carrier)
	{
		nid_delete(PPConstructor::virt_root);

		PPConstructor::root_schema->delete_scheme_node();

		PPConstructor::firstCons=true;
        
        carrier = false;

	}
}


