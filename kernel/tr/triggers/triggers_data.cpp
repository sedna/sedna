/*
 * File:  trigger_data.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string.h>
#include "tr/triggers/triggers_data.h"
#include "tr/triggers/triggers_utils.h"
#include "common/errdbg/exceptions.h"
#include "tr/crmutils/node_utils.h"
#include "tr/vmm/vmm.h"
#include "tr/executor/base/tuple.h"
#include "tr/log/log.h"
#include "tr/structures/schema.h"
#include "tr/executor/por2qep/por2qep.h"
#include "tr/executor/base/dm_accessors.h"


//using namespace std;

//Global
qep_parameters_vec* qep_parameters = NULL;

pers_sset<trigger_cell,unsigned short> *triggerdata;
USemaphore trigger_sem;//SEMAPHORE!!!

static bool triggers_initialized = false;

//inits metadata library
void triggers_on_session_begin(pers_sset<trigger_cell,unsigned short> * _triggerdata_)
{
    triggerdata = _triggerdata_;
	//SEMAPHORE INIT SECTION
	if (USemaphoreOpen(&trigger_sem, TRIGGER_SEMAPHORE_STR, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4012, "TRIGGER_SEMAPHORE_STR");

    triggers_initialized = true;
}

void triggers_on_session_end()
{
    if (!triggers_initialized) return;
	//SEMAPHORE RELEASE SECTION
    if (USemaphoreClose(trigger_sem, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4013, "TRIGGER_SEMAPHORE_STR");
    triggers_initialized = false;
}

void triggers_on_statement_begin()
{
    current_nesting_level = 0;
}

void triggers_on_statement_end()
{
    clear_nested_updates_track_map();
    clear_built_trigger_actions_map();
    current_nesting_level = 0;
}

pers_sset<trigger_cell,unsigned short>::pers_sset_entry* search_triggerdata_cell(const char *trigger_title)
{
	return triggerdata->get(trigger_title,NULL);	
}

bool trigger_cell::fits_to_trigger(schema_node* snode)
{
	t_scmnodes objs=execute_abs_path_expr(snode->root,trigger_path);
	t_scmnodes::iterator it=objs.begin();
	while (it!=objs.end())
	{
		if (*it==snode)		
			return true;		
		it++;
	}
	return false;
}
bool trigger_cell::fits_to_trigger_path_to_parent(schema_node* parent)
{
	t_scmnodes objs=execute_abs_path_expr(parent->root,path_to_parent);
	t_scmnodes::iterator it=objs.begin();
	while (it!=objs.end())
	{
		if (*it==parent)		
			return true;		
		it++;
	}
	return false;
}
void inline free_triggerdata_cell(pers_sset<trigger_cell,unsigned short>::pers_sset_entry* entry)
{
	trigger_cell* trc=entry->obj;
	triggerdata->rb_delete(entry);
	if (trc->trigger_title!=NULL)
		scm_free(trc->trigger_title,true);
    if (trc->trigger_path!=NULL)
        delete_PathExpr(trc->trigger_path);
    if (trc->doc_name!=NULL)
        scm_free(trc->doc_name,true);
    trigger_action_cell *trac=trc->trigger_action;
    trigger_action_cell *trac2=NULL;
    while(trac!=NULL)
    {
        scm_free(trac->statement,true);
        trac2=trac->next;
        scm_free(trac, true);
        trac=trac2;
    }
	if (trc->path_to_parent!=NULL)
    {
		delete_PathExpr(trc->path_to_parent);
        scm_free(trc->innode.name,true);
    }
    scm_free(trc,true);
}
trigger_cell* trigger_cell::create_trigger (enum trigger_time time, enum trigger_event event, PathExpr *trigger_path,  enum trigger_granularity gran, scheme_list* action, inserting_node innode, PathExpr *path_to_parent, doc_schema_node* schemaroot,const char * trigger_title, const char* doc_name,bool is_doc)
{
	// I. Create and fill new trigger cell
	trigger_sem_down();
	if (search_triggerdata_cell(trigger_title)!=NULL)
	{
		trigger_sem_up();	
		throw USER_EXCEPTION(SE3200);
	}
	down_concurrent_micro_ops_number();
	trigger_cell* trc=(trigger_cell*)scm_malloc(sizeof(trigger_cell),true);
	trc->schemaroot = schemaroot;
	schemaroot->create_trigger(trc);
	trc->trigger_path = trigger_path;
	trc->trigger_event = event;
    trc->trigger_time = time;
    trc->trigger_granularity = gran;
    trc->trigger_action = (trigger_action_cell*)scm_malloc(sizeof(trigger_action_cell),true);
    trigger_action_cell* trac = trc->trigger_action;
    for(int i = 0; i < action->size(); i++)
    {
		if(strstr(action->at(i).internal.str, "PPQueryRoot") != NULL) // this is a query
        {
            trac->statement = (char*)scm_malloc(strlen(action->at(i).internal.str)+1,true);
            strncpy(trac->statement,action->at(i).internal.str+37, strlen(action->at(i).internal.str)-2);
            trac->cxt_size = atoi(action->at(i).internal.str+35);
// FIXME cxt_size for trigger statements must be extracted in scheme part
//            trac->cxt_size = atoi(action->at(i+1).internal.num); 
        }
        else  //this is update
        {
            trac->statement = (char*)scm_malloc(strlen(action->at(i).internal.str)+1,true);
            strcpy(trac->statement,action->at(i).internal.str);
        }
        if(i==action->size()-1)
            trac->next = NULL;
        else
            trac->next = (trigger_action_cell*)scm_malloc(sizeof(trigger_action_cell),true);
        trac = trac->next;
    }
    // if the trigger is on before insert and statement level
    if((trc->trigger_event == TRIGGER_INSERT_EVENT) &&
       (trc->trigger_time == TRIGGER_BEFORE) &&
       (trc->trigger_granularity == TRIGGER_FOR_EACH_NODE)&&
       (path_to_parent))
    {
        trc->path_to_parent = path_to_parent;
        trc->innode.name = (char*)scm_malloc(strlen(innode.name)+1,true);
        strcpy(trc->innode.name,innode.name);
        trc->innode.type = innode.type;
    }
	else
	{
		trc->path_to_parent = NULL;
		trc->innode.name = NULL;
	}
	trc->trigger_title=(char*)scm_malloc(strlen(trigger_title)+1,true);
	strcpy(trc->trigger_title,trigger_title);
	trc->doc_name=(char*)scm_malloc(strlen(doc_name)+1,true);
	strcpy(trc->doc_name,doc_name);
	trc->is_doc=is_doc;

	triggerdata->put(trc);
	trigger_sem_up();
//    hl_logical_log_ft_index(object_path, it,(char *) index_title, doc_name,is_doc,idc->custom_tree,true);

	// ALGORITHM: setting up trigger over discriptive scheme
	//II. Execute abs path (object_path) on the desriptive schema
	t_scmnodes sobj = execute_abs_path_expr(schemaroot, trigger_path);
	//III. For each schema node found (sn_obj)
	std::vector<xptr> start_nodes;
	for (int i = 0; i < sobj.size(); i++)
	{	
		sobj[i]->add_trigger(trc);
	}
		
   	up_concurrent_micro_ops_number();

	return trc;
}
void trigger_cell::delete_trigger (const char *trigger_title)
{
	trigger_sem_down();
	pers_sset<trigger_cell,unsigned short>::pers_sset_entry* trc=search_triggerdata_cell(trigger_title);
	if (trc!=NULL)
	{
		down_concurrent_micro_ops_number();
//		hl_logical_log_ft_index((idc->obj)->object,(idc->obj)->ftype,(idc->obj)->index_title,(idc->obj)->doc_name,(idc->obj)->is_doc,(idc->obj)->custom_tree,false);
		trigger_cell* tc=trc->obj;
		doc_schema_node* sm=(trc->obj)->schemaroot;
		free_triggerdata_cell(trc);
   		sm->delete_trigger(tc);
		trigger_sem_up();
		up_concurrent_micro_ops_number();
	}
	else
		trigger_sem_up();
}
trigger_cell* trigger_cell::find_trigger(const char* title)
{
    trigger_sem_down();
	pers_sset<trigger_cell,unsigned short>::pers_sset_entry* trc=search_triggerdata_cell(title);
	if (trc==NULL)
	{
		trigger_sem_up();	
		return NULL;
	}
	else
	{
		trigger_sem_up();	
		return trc->obj;
	}
}
xptr trigger_cell::execute_trigger_action(xptr parameter_new, xptr parameter_old, xptr parameter_where)
{
   xptr res_xptr;
   se_nullostream nulls;
   PPQueryEssence* qep_tree = NULL;
   qep_subtree* qep_subtree = NULL;
   bool is_qep_opened = false, is_subqep_opened = false, is_qep_built = false, is_subqep_built = false;
   std::vector<built_trigger_action> built_trigger_actions_vec;
   typedef std::pair< std::string, std::vector<built_trigger_action> > trigger_actions_pair;
   
   current_nesting_level++;
   if(current_nesting_level > TRIGGER_MAX_CASCADING_LEVEL) throw USER_EXCEPTION2(SE3206,trigger_title);
   
    built_trigger_actions_map::iterator mapIter;
   	mapIter = built_trigger_actions.find(std::string(trigger_title));
	if ( mapIter == built_trigger_actions.end() )            // trigger action has not been built yet -> build it and store into the map
	{
        std::pair<built_trigger_actions_map::iterator, bool> mapPair;
        trigger_action_cell* trac = trigger_action;
        try
        {
			built_trigger_action bta;
            is_qep_built = is_subqep_built = false;
	        qep_parameters = &(bta.parameters);
            while(trac!=NULL)
        	{
				if(strstr(trac->statement, "query") != NULL)
				{
					bta.action_qep_subtree = NULL;
					qep_tree = bta.action_qep_tree = build_qep(trac->statement, nulls, xml);
					is_qep_built = true;
					qep_tree->open();
					is_qep_opened = true;
					built_trigger_actions_vec.push_back(bta);
				}
				else
				{
       				bta.action_qep_tree = NULL;
           			qep_subtree = bta.action_qep_subtree = build_qep(trac->statement, trac->cxt_size);
					is_subqep_built = true;
					qep_subtree->tree.op->open();
					is_subqep_opened = true;
					built_trigger_actions_vec.push_back(bta);
				}
				trac = trac->next;
	        }
        }
        catch(SednaUserException &e) {
        if (is_qep_built)
	        delete_qep(qep_tree);
        if (is_subqep_built)
            delete_qep(qep_subtree);
        throw e;}
        
        mapPair = built_trigger_actions.insert(trigger_actions_pair(std::string(trigger_title), built_trigger_actions_vec));
        mapIter = mapPair.first;
    }

     //executing built actions
     try
     {
         int i = 0;
         int action_returns_value = ((trigger_time == TRIGGER_BEFORE)&&(trigger_granularity == TRIGGER_FOR_EACH_NODE)) ? 1 : 0;
         for(i = 0; i < mapIter->second.size()-action_returns_value; i++)
         {
             if(mapIter->second.at(i).action_qep_tree)
             {
                 qep_tree = mapIter->second.at(i).action_qep_tree;
                 qep_parameters = &(mapIter->second.at(i).parameters);
	 			 set_action_parameters(parameter_new, parameter_old, parameter_where, trigger_granularity, std::string(trigger_title));
           	     if(qep_tree->is_update())
   	             qep_tree->execute();
             }
             /*else
             {
                 qep_subtree = mapIter->second.at(i).action_qep_subtree;
                 qep_parameters = &(mapIter->second.at(i).parameters);
                 set_action_parameters(parameter_new, parameter_old, parameter_where, trigger_granularity, std::string(trigger_title));
				 tuple t = tuple(1);
		   		 qep_subtree->tree.op->next(t);
             }*/
		}
		if (action_returns_value)
        {
            qep_subtree = mapIter->second.at(i).action_qep_subtree;
            qep_parameters = &(mapIter->second.at(i).parameters);
            set_action_parameters(parameter_new, parameter_old, parameter_where, trigger_granularity, std::string(trigger_title));
			tuple t = tuple(1);
		   	qep_subtree->tree.op->next(t);
			if (!t.cells[0].is_node())
				res_xptr = XNULL;
		   	else
			{
				res_xptr = t.cells[0].get_node();
				CHECKP(res_xptr);
			}
       		// retrieve all items to make the qep_subtree usable next time
			while(!t.is_eos()) 
				qep_subtree->tree.op->next(t);
        }
		else res_xptr = XNULL;

	}
    catch (SednaUserException &e) {
        throw e;
    }

   current_nesting_level--;
   return res_xptr;
}

