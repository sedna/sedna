/*
 * File:  triggers_utils.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/triggers/triggers_utils.h"
#include "tr/updates/updates.h"

static update_root_map update_roots;
int current_nesting_level;
built_trigger_actions_map built_trigger_actions;


void nested_updates_tracking(lock_mode mode, schema_node_xptr root, const char* doc_name)
{
    if (mode != lm_s)
    {
        update_root_map::iterator mapIter;
		typedef std::pair <schema_node_xptr, int> mapPair;
        mapIter = update_roots.find(root);

        if (mapIter == update_roots.end())
            update_roots.insert( mapPair(root, current_nesting_level) );
        else
		{
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
            delete vecIter->action_qep_tree;
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
                                        NULL) != XNULL)
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
            docs_triggers->insert(std::pair <schema_node_xptr, t_triggers_set> (upd_root_iter->first, triggers));
        }
        upd_root_iter++;
    }
    return docs_triggers;
}

/* provides map: schema_node -> its statement triggers.
* !!! may contain trigger dublicatates (the same trigger set for different nodes)
*/
schema_nodes_triggers_map* get_statement_triggers_on_subtree(schema_node_cptr scm_node, trigger_event event, trigger_time time, schema_nodes_triggers_map* nodes_triggers)
{
	t_triggers_set statement_triggers;
    find_triggers_for_node(scm_node,
   	  					   event,
      					   time,
       					   TRIGGER_FOR_EACH_STATEMENT,
       					   &statement_triggers);
     if(statement_triggers.size()>0)
         nodes_triggers->insert(std::pair<schema_node_xptr, t_triggers_set> (scm_node.ptr(), statement_triggers));

    sc_ref_item*	sr = scm_node->children->first;
    while(sr!=NULL)
    {
        get_statement_triggers_on_subtree(sr->object.snode, event, time, nodes_triggers);
        sr=sr->next;
    }

    return nodes_triggers;
}

xptr find_trigger_for_newly_inserted_node(schema_node_cptr parent, const char* ins_node_name, t_item ins_node_type, t_triggers_set* treated_triggers)
{
    cat_list<trigger_cell_xptr>::item* sc_trigger;
    // check if insert into a document (not into constructor)
    if(parent->root.found())
        sc_trigger=parent->root->full_trigger_list->first;
    else return XNULL;

	while (sc_trigger!=NULL)
	{
		if((sc_trigger->object->trigger_event == TRIGGER_INSERT_EVENT)&&
           (sc_trigger->object->trigger_granularity == TRIGGER_FOR_EACH_NODE)&&
		   (sc_trigger->object->trigger_time == TRIGGER_BEFORE))
            if( ((strcmp(sc_trigger->object->innode.name,"*") == 0)||(strcmp(sc_trigger->object->innode.name,ins_node_name) == 0)) &&
                (sc_trigger->object->innode.type == ins_node_type) &&
                (sc_trigger->object->fits_to_trigger_path_to_parent(parent)) &&
                (treated_triggers->find(sc_trigger->object) == treated_triggers->end()))
				    return sc_trigger->object;
    	sc_trigger=sc_trigger->next;
	}
    return XNULL;
}

xptr find_trigger_for_node(schema_node_cptr node, trigger_event event, trigger_time time, trigger_granularity granularity, t_triggers_set* treated_triggers)
{
    cat_list<trigger_cell_xptr>::item* sc_trigger=node->trigger_list->first;
	while (sc_trigger!=NULL)
	{
        if((sc_trigger->object->trigger_event == event) &&
           (sc_trigger->object->trigger_time == time) &&
           (sc_trigger->object->trigger_granularity == granularity) &&
           ((treated_triggers) ? (treated_triggers->find(sc_trigger->object) == treated_triggers->end()) : true))
            return sc_trigger->object;
	   	else sc_trigger=sc_trigger->next;
    }
    return XNULL;
}

t_triggers_set* find_triggers_for_node(schema_node_cptr node, trigger_event event, trigger_time time, trigger_granularity granularity, t_triggers_set* triggers)
{
    cat_list<trigger_cell_xptr>::item* sc_trigger = node->trigger_list->first;
	while (sc_trigger!=NULL)
	{
        if((sc_trigger->object->trigger_event == event) &&
           (sc_trigger->object->trigger_time == time) &&
           (sc_trigger->object->trigger_granularity == granularity))
            triggers->insert(sc_trigger->object);

		sc_trigger=sc_trigger->next;
    }
    return triggers;
}

xptr find_trigger_for_docnode(doc_schema_node_cptr doc_node, trigger_event event, trigger_time time, trigger_granularity granularity, t_triggers_set* treated_triggers)
{
    cat_list<trigger_cell_xptr>::item* sc_trigger=doc_node->full_trigger_list->first;
    while (sc_trigger!=NULL)
    {
        if((sc_trigger->object->trigger_event == event) &&
           (sc_trigger->object->trigger_time == time) &&
           (sc_trigger->object->trigger_granularity == granularity) &&
           ((treated_triggers) ? (treated_triggers->find(sc_trigger->object) == treated_triggers->end()) : true))
            return sc_trigger->object;
        else sc_trigger=sc_trigger->next;
    }
    return XNULL;
}
t_triggers_set* find_triggers_for_docnode(doc_schema_node_cptr doc_node, trigger_event event, trigger_time time, trigger_granularity granularity, t_triggers_set* triggers)
{
    cat_list<trigger_cell_xptr>::item* sc_trigger=doc_node->full_trigger_list->first;
    while (sc_trigger!=NULL)
    {
        if((sc_trigger->object->trigger_event == event) &&
           (sc_trigger->object->trigger_time == time) &&
           (sc_trigger->object->trigger_granularity == granularity))
            triggers->insert(sc_trigger->object);

        sc_trigger=sc_trigger->next;
    }
    return triggers;
}

xptr prepare_old_node(xptr node, schema_node_cptr scm_node, trigger_event event)
{
   	t_triggers_set treated_triggers;
    if (!isTriggersOn) return XNULL;

    CHECKP(node);

    if (find_trigger_for_node(scm_node, event, TRIGGER_AFTER, TRIGGER_FOR_EACH_NODE, &treated_triggers) != XNULL) {
        return copy_to_temp(node);
    } else
        return XNULL;
}

