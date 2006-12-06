/*
 * File:  triggers.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/triggers/triggers.h"
#include "tr/crmutils/node_utils.h"
#include "tr/tr_globals.h"
#include "tr/auth/auc.h"

static t_triggers_set after_statement_triggers;

typedef std::map< schema_node*, std::vector<trigger_cell*> > node_triggers_map;

xptr apply_before_insert_triggers(xptr new_var, xptr where_var, const char* name, t_item node_type)
{
   	if (auth == BLOCK_AUTH_CHECK) return new_var;
    
    if (((new_var==XNULL)&&(name==NULL))||(where_var==XNULL)) throw SYSTEM_EXCEPTION("Bad parameters");

	//if insert while constructor
	if (IS_TMP_BLOCK(where_var)) return new_var;
    
    if(name==NULL)
    {
        name=GETNAME(GETSCHEMENODEX(new_var));
		node_type = GETTYPE(GETSCHEMENODEX(new_var));
    }

    //if the node is not element or attribute - return
   	if((node_type!=element)&&(node_type!=attribute))
       	return new_var;        

	t_triggers_set treated_triggers;
    schema_node* scm_parent_node;

    CHECKP(where_var);
    scm_parent_node=GETSCHEMENODEX(where_var);

    trigger_cell* trc;
    while(true)
	{
		if(scm_parent_node->has_child_by_schema(NULL, name, node_type)<0)
            trc = find_trigger_for_newly_inserted_node(scm_parent_node, name, node_type, &treated_triggers);
        else
            trc = find_trigger_for_node(scm_parent_node->get_child(NULL,name, node_type), TRIGGER_INSERT_EVENT, TRIGGER_BEFORE, TRIGGER_FOR_EACH_NODE, &treated_triggers);
        if(trc == NULL)
           return new_var;
        new_var=trc->execute_trigger_action(new_var, XNULL, where_var);
		name=GETNAME(GETSCHEMENODEX(new_var));
		node_type = GETTYPE(GETSCHEMENODEX(new_var));
        treated_triggers.insert(trc);
    }
}

void apply_after_insert_triggers(xptr new_var, xptr where_var)
{
   	if (auth == BLOCK_AUTH_CHECK) return;

    if ((new_var==XNULL)||(where_var==XNULL)) throw SYSTEM_EXCEPTION("Bad parameters");
	
	//if insert while constructor
	if (IS_TMP_BLOCK(where_var)) return;
    
    CHECKP(new_var);
    CHECKP(where_var);

	schema_node* scm_node = GETSCHEMENODEX(new_var);
    //if the node is not element or attribute - return
    t_item node_type = GETTYPE(scm_node);
    if((node_type!=element)&&(node_type!=attribute))
        return;

	t_triggers_set treated_triggers;
    trigger_cell* trc;
    while(true)
    {
        trc = find_trigger_for_node(scm_node, TRIGGER_INSERT_EVENT, TRIGGER_AFTER, TRIGGER_FOR_EACH_NODE, &treated_triggers);
        find_triggers_for_node(scm_node, TRIGGER_INSERT_EVENT, TRIGGER_AFTER, TRIGGER_FOR_EACH_STATEMENT, &after_statement_triggers);
        if(trc == NULL)
            return;
        trc->execute_trigger_action(new_var, XNULL, where_var);
        treated_triggers.insert(trc);
    }
}

xptr apply_before_delete_triggers_on_subtree(xptr node, node_triggers_map *fired_triggers)
{
   	if (auth == BLOCK_AUTH_CHECK) return node;

    schema_node* scm_node = GETSCHEMENODEX(node);
    node_triggers_map attribute_fired_triggers;
    node_triggers_map element_fired_triggers;
    typedef std::pair< schema_node*, std::vector<trigger_cell*> > mapPair;
    std::pair< node_triggers_map::iterator, bool > mapRes;
    
    /*1. Evalute triggers for this node if there are some in fired_triggers map*/
    node_triggers_map::iterator mapIter;
    trigger_cell* trc;
    mapIter = fired_triggers->find(scm_node);
    xptr parent=removeIndirection(((n_dsc*)XADDR(node))->pdsc);
    if( mapIter != fired_triggers->end())
        for(int i=0; i< mapIter->second.size(); i++)
        {
            trc = mapIter->second.at(i);
            if(trc->execute_trigger_action(XNULL, node, parent) == XNULL) return XNULL;
        }
       
    // if the node is attribute - it has no children to process
    if (GETTYPE(scm_node) == attribute) return node;
    
    /*2. Find all fired triggers for all the children of the node (attribute_fired_triggers and element_fired_triggers)*/
    sc_ref* scm_child = scm_node->first_child;
    while(scm_child !=NULL)
    {
        schema_trigger_cell* scm_trc = scm_child->snode->trigger_object;
        if(scm_trc!=NULL) 
        {
            std::vector<trigger_cell*> triggers_vec;
            if(scm_child->snode->type == attribute)
                mapRes = attribute_fired_triggers.insert( mapPair (scm_child->snode, triggers_vec) );
            else
                mapRes = element_fired_triggers.insert( mapPair (scm_child->snode, triggers_vec) );
            while(scm_trc!=NULL)
            {
                if((scm_trc->trigger->trigger_event == TRIGGER_DELETE_EVENT) &&
                   (scm_trc->trigger->trigger_granularity == TRIGGER_FOR_EACH_STATEMENT) &&
                   (scm_trc->trigger->trigger_time == TRIGGER_BEFORE))
                      mapRes.first->second.push_back(scm_trc->trigger);
                scm_trc=scm_trc->next;
            }
        }
        scm_child=scm_child->next;
    }
    /*Call this function on all children recursively*/
    xptr attr_child = getFirstByOrderAttributeChild(node);
    while(attr_child!=XNULL)
    {
        if(apply_before_delete_triggers_on_subtree(attr_child, &attribute_fired_triggers) ==XNULL)
            return XNULL;
        attr_child = getNextByOrderAttribute(attr_child);
    }
    xptr elem_child = getFirstByOrderElementChild(node);
    while(elem_child!=XNULL)
    {
        if(apply_before_delete_triggers_on_subtree(elem_child, &element_fired_triggers) == XNULL)
            return XNULL;
        elem_child = getNextByOrderElement(elem_child);
    }
    return node;
}

xptr apply_before_delete_triggers(xptr old_var)
{
   	if (auth == BLOCK_AUTH_CHECK) return old_var;
    
    node_triggers_map fired_triggers_for_this_node;
    schema_node* scm_node = GETSCHEMENODEX(old_var);
    schema_trigger_cell* scm_trc = scm_node->trigger_object;
    std::vector<trigger_cell*> triggers_vec;
    std::pair< node_triggers_map::iterator, bool > mapRes;
    typedef std::pair< schema_node*, std::vector<trigger_cell*> > mapPair;
    
    if(scm_trc!=NULL)
        mapRes = fired_triggers_for_this_node.insert( mapPair (GETSCHEMENODEX(old_var), triggers_vec) );
    while(scm_trc!=NULL)
    {
		if( (scm_trc->trigger->trigger_event == TRIGGER_DELETE_EVENT) &&
			(scm_trc->trigger->trigger_time == TRIGGER_BEFORE) &&
			(scm_trc->trigger->trigger_granularity == TRIGGER_FOR_EACH_NODE))
            mapRes.first->second.push_back( scm_trc->trigger );
        scm_trc=scm_trc->next;
    }
    return apply_before_delete_triggers_on_subtree(old_var, &fired_triggers_for_this_node);
}
void apply_after_delete_triggers(xptr old_var, xptr where_var)
{
   	if (auth == BLOCK_AUTH_CHECK) return;
    
    if (old_var==XNULL) throw SYSTEM_EXCEPTION("Bad parameters");
    CHECKP(old_var);

    //if the node is not element or attribute - return
    t_item node_type = GETTYPE(GETSCHEMENODEX(old_var));
    if((node_type!=element)&&(node_type!=attribute))
        return;

    schema_node* scm_node = GETSCHEMENODEX(old_var);
	t_triggers_set treated_triggers;
    trigger_cell* trc;
    while(true)
    {
        trc = find_trigger_for_node(scm_node, TRIGGER_DELETE_EVENT, TRIGGER_AFTER, TRIGGER_FOR_EACH_NODE, &treated_triggers);
        find_triggers_for_node(scm_node, TRIGGER_DELETE_EVENT, TRIGGER_AFTER, TRIGGER_FOR_EACH_STATEMENT, &after_statement_triggers);
        if(trc == NULL)
            return;
        trc->execute_trigger_action(XNULL, old_var, where_var);
        treated_triggers.insert(trc);
    }
}

xptr apply_before_replace_triggers(xptr new_node, xptr old_node)
{
   	if (auth == BLOCK_AUTH_CHECK) return old_node;
    
    schema_node* scm_node = GETSCHEMENODEX(old_node);
    schema_trigger_cell* scm_trc = scm_node->trigger_object;
    xptr parent=removeIndirection(((n_dsc*)XADDR(old_node))->pdsc);
    
   	t_triggers_set treated_triggers;
    trigger_cell* trc;
    while(true)
    {
        trc = find_trigger_for_node(scm_node, TRIGGER_REPLACE_EVENT, TRIGGER_BEFORE, TRIGGER_FOR_EACH_NODE, &treated_triggers);
        find_triggers_for_node(scm_node, TRIGGER_REPLACE_EVENT, TRIGGER_AFTER, TRIGGER_FOR_EACH_STATEMENT, &after_statement_triggers);
        if(trc == NULL)
            return old_node;
        if(trc->execute_trigger_action(new_node, old_node, parent) == XNULL) return XNULL;
        treated_triggers.insert(trc);
    }

	return old_node;
}

void apply_after_replace_triggers(xptr new_node, xptr old_node)
{
   	if (auth == BLOCK_AUTH_CHECK) return;

    if ((old_node==XNULL)||(new_node==XNULL)) throw SYSTEM_EXCEPTION("Bad parameters");
    CHECKP(old_node);

    //if the node is not element or attribute - return
    t_item node_type = GETTYPE(GETSCHEMENODEX(old_node));
    if((node_type!=element)&&(node_type!=attribute))
        return;

    schema_node* scm_node = GETSCHEMENODEX(old_node);
    schema_trigger_cell* scm_trc = scm_node->trigger_object;
    xptr parent=removeIndirection(((n_dsc*)XADDR(old_node))->pdsc);
    
   	t_triggers_set treated_triggers;
    trigger_cell* trc;
    while(true)
    {
        trc = find_trigger_for_node(scm_node, TRIGGER_REPLACE_EVENT, TRIGGER_AFTER, TRIGGER_FOR_EACH_NODE, &treated_triggers);
        if(trc == NULL)
            return;
        trc->execute_trigger_action(new_node, old_node, parent);
        treated_triggers.insert(trc);
    }
}

void apply_before_insert_for_each_statement_triggers(xptr_sequence* target_seq, xptr_sequence* upd_seq)
{
    t_scmnodes_set extended_nodes, extender_nodes;
    t_scmnodes_const matched_nodes;
    schema_nodes_triggers_map statement_triggers;
    schema_nodes_triggers_map::iterator statement_triggers_iter;
	t_triggers_set::iterator trigers_iter;
    xptr_sequence::iterator it1, it2;

   	if (auth == BLOCK_AUTH_CHECK) return;

    //if there are no statement level triggers at all - return
    if(!has_statement_triggers(TRIGGER_INSERT_EVENT, TRIGGER_BEFORE)) return;
    
    it1=target_seq->begin();
    it2=upd_seq->begin();

    //1. creating extended_nodes set
    while(it1!=target_seq->end())
    {
        extended_nodes.insert(GETSCHEMENODEX(removeIndirection(*it1)));
        it1++;
    }
    
    //2. creating extender_nodes vector
    while(it2!=upd_seq->end())
    {
		schema_node* scn = GETSCHEMENODEX(removeIndirection(*it2));
        extender_nodes.insert(GETSCHEMENODEX(removeIndirection(*it2)));
        it2++;
    }

    //3. finding out triggers and execute their action
    get_statement_triggers(&statement_triggers, TRIGGER_INSERT_EVENT, TRIGGER_BEFORE);
    for(statement_triggers_iter=statement_triggers.begin();statement_triggers_iter!=statement_triggers.end(); statement_triggers_iter++)
    {
        for(trigers_iter=statement_triggers_iter->second.begin();trigers_iter!=statement_triggers_iter->second.end();trigers_iter++)
        {
            trigger_cell* trc = *trigers_iter;

	   	    matched_nodes = execute_abs_path_expr((const schema_node*)(statement_triggers_iter->first), trc->trigger_path, &extended_nodes, &extender_nodes);
    	   	for(int i=0;i<matched_nodes.size(); i++)
        	    if(is_scmnode_has_ancestor_or_self((schema_node*)(matched_nodes.at(i)),&extender_nodes))
            	    //4. check if extender nodes has data of the type schema_node
                    for(it2=upd_seq->begin(); it2!=upd_seq->end(); it2++)
                       	if(getFirstDescandantByScheme(removeIndirection(*it2), (schema_node*)(matched_nodes.at(i)))!=XNULL)
                        {
                            trc->execute_trigger_action(XNULL, XNULL, XNULL);
                            break;
                        }
        }
    }
}

void apply_before_delete_for_each_statement_triggers(xptr_sequence* target_seq)
{
    std::map <schema_node*, std::vector<xptr> > scm_nodes_map;
    std::pair <std::map <schema_node*, std::vector<xptr> >::iterator, bool> scm_nodes_map_pair;
    std::map <schema_node*, std::vector<xptr> >::iterator scm_nodes_iter;
    xptr_sequence::iterator it1;
    std::set<trigger_cell*>::iterator set_triggers_iter;
    schema_nodes_triggers_map docs_statement_triggers;
    schema_nodes_triggers_map::iterator statement_triggers_iter;
  	if (auth == BLOCK_AUTH_CHECK) return;
    
    //if there are no statement level triggers at all - return
    if(!has_statement_triggers(TRIGGER_DELETE_EVENT, TRIGGER_BEFORE)) return;

	//1.create deleting sequence
    get_statement_triggers(&docs_statement_triggers, TRIGGER_DELETE_EVENT, TRIGGER_BEFORE);
    for(it1=target_seq->begin();it1!=target_seq->end(); it1++)
    {
        schema_node* scn=GETSCHEMENODEX(*it1);
        if(docs_statement_triggers.find(scn->root)!=docs_statement_triggers.end())
        {
            scm_nodes_iter=scm_nodes_map.find(scn);
	        if(scm_nodes_iter==scm_nodes_map.end())
    	    {
        	    std::vector<xptr> xptr_vec;
            	scm_nodes_map_pair=scm_nodes_map.insert(std::pair<schema_node*, std::vector<xptr> > (scn, xptr_vec));
	            scm_nodes_map_pair.first->second.push_back(*it1);
    	    }
        	else
            	scm_nodes_iter->second.push_back(*it1);
        }
    }
    //2.descriptive schema tree traversal finding out triggers STATEMENT DELETE AFTER
    scm_nodes_iter=scm_nodes_map.begin();
    for(scm_nodes_iter=scm_nodes_map.begin();scm_nodes_iter!=scm_nodes_map.end(); scm_nodes_iter++)
    {
        schema_nodes_triggers_map statement_triggers;
        //3.get all statement triggers ont the subtree
        get_statement_triggers_on_subtree(scm_nodes_iter->first, TRIGGER_DELETE_EVENT, TRIGGER_BEFORE, &statement_triggers);
        statement_triggers_iter=statement_triggers.begin();
        //iterate over the nodes with statement triggers
        for(statement_triggers_iter=statement_triggers.begin();statement_triggers_iter!=statement_triggers.end(); statement_triggers_iter++)
        {
            std::vector<xptr>::iterator xptr_iter;
            for(xptr_iter=scm_nodes_iter->second.begin();xptr_iter!=scm_nodes_iter->second.end();xptr_iter++)
            {
                //check if there is data corresponding to the schema node - if there is execute triggers
                if(getFirstDescandantByScheme(*xptr_iter, statement_triggers_iter->first)!=XNULL)
                {
                    for(set_triggers_iter=statement_triggers_iter->second.begin();set_triggers_iter!=statement_triggers_iter->second.end();set_triggers_iter++)
                        (*set_triggers_iter)->execute_trigger_action(XNULL, XNULL, XNULL);

                    break;
                }
            }
        }
    }
    
    return;
}
void apply_before_replace_for_each_statement_triggers(xptr_sequence* target_seq)
{
    t_scmnodes_set scmnodes;
    t_scmnodes_set::iterator scmnodes_iter;
    t_triggers_set triggers;
    t_triggers_set::iterator triggers_iter;
    xptr_sequence::iterator it1;
    std::set<trigger_cell*>::iterator set_triggers_iter;

   	if (auth == BLOCK_AUTH_CHECK) return;
    
    //if there are no statement level triggers at all - return
    if(!has_statement_triggers(TRIGGER_REPLACE_EVENT, TRIGGER_BEFORE)) return;
	
	//1. create a set of schema nodes
   	it1=target_seq->begin();
    while(it1!=target_seq->end())
    {
        schema_node* scn=GETSCHEMENODEX(removeIndirection(*it1));
        scmnodes.insert(scn);
        it1++;
    }
    //2. find statement triggers on the nodes from the node set
    for(scmnodes_iter=scmnodes.begin(); scmnodes_iter!=scmnodes.end(); scmnodes_iter++)
    {
        find_triggers_for_node(*scmnodes_iter, TRIGGER_REPLACE_EVENT, TRIGGER_BEFORE, TRIGGER_FOR_EACH_STATEMENT, &triggers);
        for(triggers_iter=triggers.begin(); triggers_iter!=triggers.end(); triggers_iter++)
            (*triggers_iter)->execute_trigger_action(XNULL, XNULL, XNULL);
    }
}

void apply_after_statement_triggers()
{
   	if (auth == BLOCK_AUTH_CHECK) return;
    
    if(after_statement_triggers.size()==0) return;
    
    // clear global triggers set to avoid triggers evaluation inside this evaluation
    t_triggers_set local_after_triggers;
    for(t_triggers_set::iterator i=after_statement_triggers.begin(); i!=after_statement_triggers.end(); i++)
        local_after_triggers.insert(*i);
        
    after_statement_triggers.clear();
    
    // clear updates trackings - after statement triggers are evaluated out of the update operation!
    clear_nested_updates_track_map();
    
    for(i=local_after_triggers.begin(); i!=local_after_triggers.end(); i++)
    {
        ((trigger_cell*)(*i))->execute_trigger_action(XNULL, XNULL, XNULL);
    }
}

xptr apply_per_node_triggers(xptr new_var, xptr old_var, xptr where_var, trigger_time time, trigger_event event, const char* new_name, t_item new_type)
{
    switch (time){
        case TRIGGER_BEFORE:
         switch (event){
             case TRIGGER_INSERT_EVENT:
                  return apply_before_insert_triggers(new_var, where_var, new_name, new_type);
                  
             case TRIGGER_DELETE_EVENT:
                  return apply_before_delete_triggers(old_var);
                  
             case TRIGGER_REPLACE_EVENT:
                  return apply_before_replace_triggers(new_var, old_var);
             
             default: 
                  throw SYSTEM_EXCEPTION("Bad trigger event");
        }
    
        case TRIGGER_AFTER:
          switch (event){
             case TRIGGER_INSERT_EVENT:
                  apply_after_insert_triggers(new_var, where_var);
                  return XNULL;
                  
             case TRIGGER_DELETE_EVENT:
                  apply_after_delete_triggers(old_var, where_var);
                  return XNULL;
                  
             case TRIGGER_REPLACE_EVENT:
                  apply_after_replace_triggers(new_var, old_var);
                  return XNULL;
                  
             default: 
                  throw SYSTEM_EXCEPTION("Bad trigger event");
        }
        default:
            throw SYSTEM_EXCEPTION("Bad trigger time");
        }
}

void apply_per_statement_triggers(xptr_sequence* target_seq, xptr_sequence* upd_seq, trigger_time time, trigger_event event)
{
    switch (time){
        case TRIGGER_BEFORE:
         switch (event){
             case TRIGGER_INSERT_EVENT:
                 apply_before_insert_for_each_statement_triggers(target_seq, upd_seq);
                 return;
             case TRIGGER_DELETE_EVENT:
                 apply_before_delete_for_each_statement_triggers(target_seq);
                 return;
             case TRIGGER_REPLACE_EVENT:
                 apply_before_replace_for_each_statement_triggers(target_seq);
				 return;
             default: 
                throw SYSTEM_EXCEPTION("Bad trigger event");
        }

        case TRIGGER_AFTER:
         switch (event){
             case TRIGGER_INSERT_EVENT:
                 apply_after_statement_triggers();
                 return;
             case TRIGGER_DELETE_EVENT:
                 apply_after_statement_triggers();
                 return;
             case TRIGGER_REPLACE_EVENT:
                 apply_after_statement_triggers();
                 return;
             default: 
                  throw SYSTEM_EXCEPTION("Bad trigger event");
        }
        default:
            throw SYSTEM_EXCEPTION("Bad trigger time");
        
    }
}

