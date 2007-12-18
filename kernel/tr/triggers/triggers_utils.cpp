/*
 * File:  triggers_utils.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/triggers/triggers_utils.h"
#include "tr/updates/updates.h"

static update_root_map update_roots;
int current_nesting_level;
built_trigger_actions_map built_trigger_actions;


void nested_updates_tracking(lock_mode mode, schema_node* root, const char* doc_name)
{
    if (mode != lm_s)
    {
        update_root_map::iterator mapIter;
		typedef std::pair <schema_node*, int> mapPair;
        mapIter = update_roots.find(root);
        
        if (mapIter == update_roots.end())
            update_roots.insert( mapPair(root, current_nesting_level) );
        else
		{
            schema_node* sc = mapIter->first;
			int cnl = mapIter->second;
            if(mapIter->second < current_nesting_level) throw USER_EXCEPTION2(SE3205,doc_name);
		}
    }
}

void set_action_parameters(xptr parameter_new, xptr parameter_old, xptr parameter_where, trigger_granularity gran, std::string trigger_title)
{
    qep_parameters_vec::iterator qepParIter;
    std::string error_detail;
    for(qepParIter = qep_parameters->begin(); qepParIter != qep_parameters->end(); qepParIter++)
    {
        switch ((*qepParIter)->get_type())
        {
            case TRIGGER_PARAMETER_NEW:   
                if(gran==TRIGGER_FOR_EACH_STATEMENT)
                {
                    error_detail = "Trigger variable $NEW in trigger " + std::string(trigger_title);
                    throw USER_EXCEPTION2(SE3209, error_detail.c_str());
                }
                if(parameter_new==XNULL)
                {
                    error_detail = "Trigger variable $NEW in trigger " + std::string(trigger_title);
                    throw USER_EXCEPTION2(SE3208, error_detail.c_str());
                }
                CHECKP(parameter_new);
                (*qepParIter)->set_xptr(parameter_new);
                break;
            case TRIGGER_PARAMETER_OLD:
                if(gran==TRIGGER_FOR_EACH_STATEMENT)
                {
                    error_detail = "Trigger variable $OLD in trigger " + std::string(trigger_title);
                    throw USER_EXCEPTION2(SE3209, error_detail.c_str());
                }
                if(parameter_old==XNULL)
                {
                    error_detail = "Trigger variable $OLD  in trigger " + std::string(trigger_title);
                    throw USER_EXCEPTION2(SE3208, error_detail.c_str());
                }
                CHECKP(parameter_old);
                (*qepParIter)->set_xptr(parameter_old);
                break;
            case TRIGGER_PARAMETER_WHERE: 
               if(gran==TRIGGER_FOR_EACH_STATEMENT)
               {
                   error_detail = "Trigger variable $WHERE in trigger " + std::string(trigger_title);
                   throw USER_EXCEPTION2(SE3209, error_detail.c_str());
               }
               if(parameter_where==XNULL)
               {
                   error_detail = "Trigger variable $WHERE in trigger " + std::string(trigger_title);
                   throw USER_EXCEPTION2(SE3208, error_detail.c_str());
               }
               CHECKP(parameter_where); 
               (*qepParIter)->set_xptr(parameter_where);
               break;
            default: throw USER_EXCEPTION(SE3202);
                     break;
        }
    }
}

void clear_built_trigger_actions_map()
{
    built_trigger_actions_map::iterator mapIter;
    std::vector<built_trigger_action>::iterator vecIter;
    
    if(built_trigger_actions.empty()) return;
    
    for(mapIter = built_trigger_actions.begin( ); mapIter != built_trigger_actions.end( ); mapIter++)
    {
        for(vecIter = mapIter->second.begin(); vecIter != mapIter->second.end(); vecIter++)
        {
            if(vecIter->action_qep_tree!=NULL)
            {
                vecIter->action_qep_tree->close();
                delete_qep(vecIter->action_qep_tree);
            }
            else
            {
                vecIter->action_qep_subtree->tree.op->close();
                delete_qep(vecIter->action_qep_subtree);
            }
        }
    }
    built_trigger_actions.clear();
}

void clear_nested_updates_track_map()
{
    update_roots.clear();
}

bool has_statement_triggers(trigger_event event, trigger_time time)
{
    update_root_map::iterator upd_root_iter=update_roots.begin();
    while(upd_root_iter!=update_roots.end())
    {
		if(upd_root_iter->second==current_nesting_level)
            if(find_trigger_for_docnode(upd_root_iter->first->root, 
                                        event, 
                                        time, 
                                        TRIGGER_FOR_EACH_STATEMENT, 
                                        NULL) != NULL)
            return true;
        upd_root_iter++;
    }
    return false;
}

schema_nodes_triggers_map* get_statement_triggers(schema_nodes_triggers_map* docs_triggers, trigger_event event, trigger_time time)
{
    update_root_map::iterator upd_root_iter=update_roots.begin();
    t_triggers_set triggers;
    while(upd_root_iter!=update_roots.end())
    {
		if(upd_root_iter->second==current_nesting_level)
        {
            find_triggers_for_docnode(upd_root_iter->first->root, 
                                      event, 
                                      time, 
                                      TRIGGER_FOR_EACH_STATEMENT, 
                                      &triggers);
            docs_triggers->insert(std::pair <schema_node*, t_triggers_set> (upd_root_iter->first, triggers));
        }
        upd_root_iter++;
    }
    return docs_triggers;
}

/* provides map: schema_node -> its statement triggers.
* !!! may contain trigger dublicatates (the same trigger set for different nodes)
*/
schema_nodes_triggers_map* get_statement_triggers_on_subtree(schema_node* scm_node, trigger_event event, trigger_time time, schema_nodes_triggers_map* nodes_triggers)
{
	t_triggers_set statement_triggers;
    find_triggers_for_node(scm_node,
   	  					   event,
      					   time,
       					   TRIGGER_FOR_EACH_STATEMENT,
       					   &statement_triggers);
     if(statement_triggers.size()>0)
         nodes_triggers->insert(std::pair<schema_node*, t_triggers_set> (scm_node,statement_triggers));    

    sc_ref*	sr = scm_node->first_child;
    while(sr!=NULL)
    {
        get_statement_triggers_on_subtree(sr->snode, event, time, nodes_triggers);
        sr=sr->next;
    }
    
    return nodes_triggers;
}
trigger_cell* find_trigger_for_newly_inserted_node(schema_node* parent, const char* ins_node_name, t_item ins_node_type, t_triggers_set* treated_triggers)
{
    schema_trigger_cell* sc_trigger;
	// check if insert into a document (not into constructor)
	if(parent->root!=NULL)
        sc_trigger=parent->root->sc_triggers;
    else return NULL;
    
	while (sc_trigger!=NULL)
	{
		if((sc_trigger->trigger->trigger_event == TRIGGER_INSERT_EVENT)&&
           (sc_trigger->trigger->trigger_granularity == TRIGGER_FOR_EACH_NODE)&&
		   (sc_trigger->trigger->trigger_time == TRIGGER_BEFORE))
            if( ((strcmp(sc_trigger->trigger->innode.name,"*") == 0)||(strcmp(sc_trigger->trigger->innode.name,ins_node_name) == 0)) && 
                (sc_trigger->trigger->innode.type == ins_node_type) &&
                (sc_trigger->trigger->fits_to_trigger_path_to_parent(parent)) &&
                (treated_triggers->find(sc_trigger->trigger) == treated_triggers->end()))
				    return sc_trigger->trigger;
    	sc_trigger=sc_trigger->next;
	}
    return NULL;
}

trigger_cell* find_trigger_for_node(schema_node* node, trigger_event event, trigger_time time, trigger_granularity granularity, t_triggers_set* treated_triggers)
{
    schema_trigger_cell* sc_trigger=node->trigger_object;
	while (sc_trigger!=NULL)
	{
        if((sc_trigger->trigger->trigger_event == event) &&
           (sc_trigger->trigger->trigger_time == time) &&
           (sc_trigger->trigger->trigger_granularity == granularity) &&
           ((treated_triggers) ? (treated_triggers->find(sc_trigger->trigger) == treated_triggers->end()) : true))
            return sc_trigger->trigger;
	   	else sc_trigger=sc_trigger->next;
    }
    return NULL;
}

t_triggers_set* find_triggers_for_node(schema_node* node, trigger_event event, trigger_time time, trigger_granularity granularity, t_triggers_set* triggers)
{
    schema_trigger_cell* sc_trigger=node->trigger_object;
	while (sc_trigger!=NULL)
	{
        if((sc_trigger->trigger->trigger_event == event) &&
           (sc_trigger->trigger->trigger_time == time) &&
           (sc_trigger->trigger->trigger_granularity == granularity))
            triggers->insert(sc_trigger->trigger);
	   	
		sc_trigger=sc_trigger->next;
    }
    return triggers;
}

trigger_cell* find_trigger_for_docnode(doc_schema_node* doc_node, trigger_event event, trigger_time time, trigger_granularity granularity, t_triggers_set* treated_triggers)
{
    schema_trigger_cell* sc_trigger=doc_node->sc_triggers;
	while (sc_trigger!=NULL)
	{
        if((sc_trigger->trigger->trigger_event == event) &&
           (sc_trigger->trigger->trigger_time == time) &&
           (sc_trigger->trigger->trigger_granularity == granularity) &&
           ((treated_triggers) ? (treated_triggers->find(sc_trigger->trigger) == treated_triggers->end()) : true))
            return sc_trigger->trigger;
	   	else sc_trigger=sc_trigger->next;
    }
    return NULL;
}
t_triggers_set* find_triggers_for_docnode(doc_schema_node* doc_node, trigger_event event, trigger_time time, trigger_granularity granularity, t_triggers_set* triggers)
{
    schema_trigger_cell* sc_trigger=doc_node->sc_triggers;
	while (sc_trigger!=NULL)
	{
        if((sc_trigger->trigger->trigger_event == event) &&
           (sc_trigger->trigger->trigger_time == time) &&
           (sc_trigger->trigger->trigger_granularity == granularity))
            triggers->insert(sc_trigger->trigger);
	   	
		sc_trigger=sc_trigger->next;
    }
    return triggers;
}

xptr prepare_old_node(xptr node, schema_node* scm_node, trigger_event event)
{
   	t_triggers_set treated_triggers;
    if (!isTriggersOn) return XNULL;
    
    CHECKP(node);
    
    if(find_trigger_for_node(scm_node, event, TRIGGER_AFTER, TRIGGER_FOR_EACH_NODE, &treated_triggers))
    {
        return copy_to_temp(node);
    }
    else
        return XNULL;
}

