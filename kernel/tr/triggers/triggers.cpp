/*
 * File:  triggers.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/xqp/XQuerytoLR.h"
#include "tr/triggers/triggers.h"
#include "tr/tr_globals.h"
#include "tr/auth/auc.h"
#include "tr/updates/updates.h"
#include "tr/cat/catptr.h"
#include "tr/log/log.h"
#include "tr/locks/locks.h"
#include "tr/crmutils/exec_output.h"
#include "tr/structures/nodeutils.h"

static t_triggers_set after_statement_triggers;

typedef std::map<schema_node_xptr, std::vector<trigger_cell_xptr> > node_triggers_map;

xptr triggers_test(xptr new_var, xptr where_var, const char* name, t_item node_type)
{
    if(name==NULL)
    {
        name=getSchemaNode(new_var)->get_name();
		CHECKP(where_var);
        name=getSchemaNode(new_var)->get_name();
		node_type = getNodeType(new_var);
    }
	return new_var;
}

void run_triggers(bool isOn)
{
    isTriggersOn = isOn;
}

xptr apply_before_insert_triggers(xptr new_var, xptr where_var)
{
   	if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return new_var;

	//if insert while constructor
	if (IS_TMP_BLOCK(where_var)) return new_var;

    if ((new_var==XNULL)||(where_var==XNULL)) throw SYSTEM_EXCEPTION("Bad parameters");

    CHECKP(new_var);
    const char* name=getSchemaNode(new_var)->get_name();
    t_item node_type = getNodeType(new_var);

    //if the node is not element or attribute - return
   	if((node_type!=element)&&(node_type!=attribute))
       	return new_var;

	t_triggers_set treated_triggers;
    schema_node_cptr scm_parent_node = XNULL;

    CHECKP(where_var);
    scm_parent_node=getSchemaNode(where_var);

    trigger_cell_cptr trc = XNULL;
    while(true)
	{
		if(scm_parent_node->find_first_child(NULL_XMLNS, name, node_type)<0)
            trc = find_trigger_for_newly_inserted_node(scm_parent_node, name, node_type, &treated_triggers);
        else
            trc = find_trigger_for_node(scm_parent_node->get_first_child(NULL_XMLNS, name, node_type), TRIGGER_INSERT_EVENT, TRIGGER_BEFORE, TRIGGER_FOR_EACH_NODE, &treated_triggers);

		if(!trc.found())
			return new_var;
        new_var=trc->execute_trigger_action(new_var, XNULL, where_var);
		if(new_var == XNULL)
			return new_var;
		node_type = getNodeType(new_var);
		if((node_type!=element)&&(node_type!=attribute))
			return new_var;
        name=getSchemaNode(new_var)->get_name();
        treated_triggers.insert(trc.ptr());
    }
}

void apply_after_insert_triggers(xptr new_var, xptr where_var, schema_node_cptr scm_node)
{
   	if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return;

	//if insert while constructor
	if (IS_TMP_BLOCK(where_var)) return;

    if ((new_var==XNULL)||(where_var==XNULL)) throw SYSTEM_EXCEPTION("Bad parameters");

    CHECKP(new_var);
	scm_node = getSchemaNode(new_var);
    //if the node is not element or attribute - return
    t_item node_type = getNodeType(new_var);
    if((node_type!=element)&&(node_type!=attribute))
        return;

	// care about after-statement triggers
    find_triggers_for_node(scm_node, TRIGGER_INSERT_EVENT, TRIGGER_AFTER, TRIGGER_FOR_EACH_STATEMENT, &after_statement_triggers);

	t_triggers_set treated_triggers;
    trigger_cell_cptr trc = XNULL;
    while(true)
    {
        trc = find_trigger_for_node(scm_node, TRIGGER_INSERT_EVENT, TRIGGER_AFTER, TRIGGER_FOR_EACH_NODE, &treated_triggers);
        if(!trc.found())
            return;
        trc->execute_trigger_action(new_var, XNULL, where_var);
        treated_triggers.insert(trc.ptr());
    }
}

xptr apply_before_delete_triggers_on_subtree(xptr node, node_triggers_map *fired_triggers)
{
   	if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return node;

    schema_node_cptr scm_node = getSchemaNode(node);
    node_triggers_map attribute_fired_triggers;
    node_triggers_map element_fired_triggers;
    typedef std::pair< schema_node_xptr, std::vector<trigger_cell_xptr> > mapPair;
    std::pair< node_triggers_map::iterator, bool > mapRes;

    /*1. Evalute triggers for this node if there are some in fired_triggers map*/
    node_triggers_map::iterator mapIter;
    trigger_cell_cptr trc = XNULL;
    mapIter = fired_triggers->find(scm_node.ptr());
    xptr parent=nodeGetParent(node);
    if( mapIter != fired_triggers->end())
        for(std::vector<trigger_cell_xptr>::size_type i=0; i< mapIter->second.size(); i++)
        {
            trc = mapIter->second.at(i);
            if(trc->execute_trigger_action(XNULL, node, parent) == XNULL) return XNULL;
        }

    // if the node is attribute - it has no children to process
    if (scm_node->type == attribute) return node;

    /*2. Find all fired triggers for all the children of the node (attribute_fired_triggers and element_fired_triggers)*/
    sc_ref_item* scm_child = scm_node->children->first;
    while(scm_child !=NULL)
    {
        cat_list<trigger_cell_xptr>::item* scm_trc = scm_child->object.snode->trigger_list->first;
        if(scm_trc!=NULL)
        {
            std::vector<trigger_cell_xptr> triggers_vec;
            if(scm_child->object.snode->type == attribute)
                mapRes = attribute_fired_triggers.insert( mapPair (scm_child->object.snode, triggers_vec) );
            else
                mapRes = element_fired_triggers.insert( mapPair (scm_child->object.snode, triggers_vec) );
            while(scm_trc!=NULL)
            {
                if((scm_trc->object->trigger_event == TRIGGER_DELETE_EVENT) &&
                   (scm_trc->object->trigger_granularity == TRIGGER_FOR_EACH_STATEMENT) &&
                   (scm_trc->object->trigger_time == TRIGGER_BEFORE))
                      mapRes.first->second.push_back(scm_trc->object);
                scm_trc=scm_trc->next;
            }
        }
        scm_child=scm_child->next;
    }
    /*Call this function on all children recursively*/
    xptr attr_child = getFirstAttributeChild(node);
    while(attr_child!=XNULL)
    {
        if(apply_before_delete_triggers_on_subtree(attr_child, &attribute_fired_triggers) ==XNULL)
            return XNULL;
        attr_child = getNextAttribute(attr_child);
    }
    xptr elem_child = getFirstElementChild(node);
    while(elem_child!=XNULL)
    {
        if(apply_before_delete_triggers_on_subtree(elem_child, &element_fired_triggers) == XNULL)
            return XNULL;
        elem_child = getNextElement(elem_child);
    }
    return node;
}

xptr apply_before_delete_triggers(xptr old_var, xptr where_var, schema_node_cptr scm_node)
{
   	if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return old_var;

   	if (IS_TMP_BLOCK(old_var)) return old_var;

	if ((scm_node->type != element) && (scm_node->type != attribute)) return old_var;

   	t_triggers_set treated_triggers;
    trigger_cell_cptr trc = XNULL;
    while(true)
	{
        trc = find_trigger_for_node(scm_node, TRIGGER_DELETE_EVENT, TRIGGER_BEFORE, TRIGGER_FOR_EACH_NODE, &treated_triggers);
		if(!trc.found())
		{
			CHECKP(old_var);
			return old_var;
		}
        if(trc->execute_trigger_action(XNULL, old_var, where_var) == XNULL) return XNULL;
        treated_triggers.insert(trc.ptr());
    }
    return old_var;
}


void apply_after_delete_triggers(xptr old_var, xptr where_var, schema_node_cptr scm_node)
{
   	if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return;

	// care about after-statement triggers
    find_triggers_for_node(scm_node, TRIGGER_DELETE_EVENT, TRIGGER_AFTER, TRIGGER_FOR_EACH_STATEMENT, &after_statement_triggers);

    if (old_var==XNULL) return; //old_var==XNULL if there are no for-each-node-after-triggers
    CHECKP(old_var);

    //if the node is not element or attribute - return
    t_item node_type = getNodeType(old_var);
    if((node_type!=element)&&(node_type!=attribute))
        return;

	t_triggers_set treated_triggers;
    trigger_cell_cptr trc = XNULL;
    while(true)
    {
        trc = find_trigger_for_node(scm_node, TRIGGER_DELETE_EVENT, TRIGGER_AFTER, TRIGGER_FOR_EACH_NODE, &treated_triggers);
        if(!trc.found())
        {
            return;
        }
        trc->execute_trigger_action(XNULL, old_var, where_var);
        treated_triggers.insert(trc.ptr());
    }
}

xptr apply_before_replace_triggers(xptr new_node, xptr old_node, schema_node_cptr scm_node)
{
   	if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return old_node;

	CHECKP(old_node);
    xptr parent= nodeGetParent(old_node);

   	t_triggers_set treated_triggers;
    trigger_cell_cptr trc = XNULL;
    while(true)
    {
        trc = find_trigger_for_node(scm_node, TRIGGER_REPLACE_EVENT, TRIGGER_BEFORE, TRIGGER_FOR_EACH_NODE, &treated_triggers);
        if(!trc.found())
            return new_node;
        new_node = trc->execute_trigger_action(new_node, old_node, parent);
		if(new_node==XNULL) return XNULL;
        treated_triggers.insert(trc.ptr());
    }

	return new_node;
}

void apply_after_replace_triggers(xptr new_node, xptr old_node, xptr where_var, schema_node_cptr scm_node)
{
   	if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return;

    // care about after-statement triggers
    find_triggers_for_node(scm_node, TRIGGER_REPLACE_EVENT, TRIGGER_AFTER, TRIGGER_FOR_EACH_STATEMENT, &after_statement_triggers);

    if (old_node==XNULL) return; //old_var==XNULL if there are no for-each-node-after-triggers
    CHECKP(old_node);

    //if the node is not element or attribute - return
    t_item node_type = scm_node->type;
    if((node_type!=element)&&(node_type!=attribute))
        return;

   	t_triggers_set treated_triggers;
    trigger_cell_cptr trc = XNULL;
    while(true)
    {
        trc = find_trigger_for_node(scm_node, TRIGGER_REPLACE_EVENT, TRIGGER_AFTER, TRIGGER_FOR_EACH_NODE, &treated_triggers);
        if(!trc.found())
            return;
        trc->execute_trigger_action(new_node, old_node, where_var);
        treated_triggers.insert(trc.ptr());
    }
}

void apply_before_insert_for_each_statement_triggers(xptr_sequence* target_seq, bool target_seq_direct, xptr_sequence* upd_seq, bool upd_seq_direct)
{
    t_scmnodes_set extended_nodes, extender_nodes;
    t_scmnodes matched_nodes;
    schema_nodes_triggers_map statement_triggers;
    schema_nodes_triggers_map::iterator statement_triggers_iter;
	t_triggers_set::iterator trigers_iter;
    xptr_sequence::iterator it1, it2;
	schema_node_cptr scn = XNULL;
    xptr node;

   	if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return;

    //if there are no statement level triggers at all - return
    if(!has_statement_triggers(TRIGGER_INSERT_EVENT, TRIGGER_BEFORE)) return;

    it1=target_seq->begin();
    it2=upd_seq->begin();

    //1. creating extended_nodes set
    while(it1!=target_seq->end())
    {
        if(target_seq_direct)
            node = *it1;
        else
            node = indirectionDereferenceCP(*it1);
        CHECKP(node);
        extended_nodes.insert(getSchemaPointer(node));
        it1++;
    }

    //2. creating extender_nodes vector
    while(it2!=upd_seq->end())
    {
        if(upd_seq_direct)
            node = *it2;
        else
            node = indirectionDereferenceCP(*it2);
        CHECKP(node);
        extender_nodes.insert(getSchemaPointer(node));
        it2++;
    }

    //3. finding out triggers and execute their action
    get_statement_triggers(&statement_triggers, TRIGGER_INSERT_EVENT, TRIGGER_BEFORE);
    for (statement_triggers_iter=statement_triggers.begin();statement_triggers_iter!=statement_triggers.end(); statement_triggers_iter++)
    {
        for (trigers_iter=statement_triggers_iter->second.begin();trigers_iter!=statement_triggers_iter->second.end();trigers_iter++)
        {
            trigger_cell_cptr trc = *trigers_iter;

            executeAbsPathExpression((const schema_node_xptr)(statement_triggers_iter->first), *trc->trigger_path, &matched_nodes, &extended_nodes, &extender_nodes);
            for (std::vector<schema_node_xptr>::size_type i=0; i < matched_nodes.size(); i++)
            {
                if (hasAncestorInSet((schema_node_xptr)(matched_nodes.at(i)),&extender_nodes))
                {
                    //4. check if extender nodes has data of the type schema_node
                    for (it2=upd_seq->begin(); it2!=upd_seq->end(); it2++)
                    {
                        if(getFirstDescandantBySchema(indirectionDereferenceCP(*it2), (schema_node_xptr)(matched_nodes.at(i)))!=XNULL)
                        {
                            trc->execute_trigger_action(XNULL, XNULL, XNULL);
                            break;
                        }
                    }
                }
            }
        }
    }
}

void apply_before_delete_for_each_statement_triggers(xptr_sequence* target_seq, bool target_seq_direct)
{
    std::map <schema_node_xptr, std::vector<xptr> > scm_nodes_map;
    std::pair <std::map <schema_node_xptr, std::vector<xptr> >::iterator, bool> scm_nodes_map_pair;
    std::map <schema_node_xptr, std::vector<xptr> >::iterator scm_nodes_iter;
    xptr_sequence::iterator it1;
	xptr node;
	schema_node_cptr scn = XNULL;
	t_triggers_set treated_triggers;
    std::set<trigger_cell_xptr>::iterator set_triggers_iter;
    schema_nodes_triggers_map docs_statement_triggers;
    schema_nodes_triggers_map::iterator statement_triggers_iter;
  	if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return;

    //if there are no statement level triggers at all - return
    if(!has_statement_triggers(TRIGGER_DELETE_EVENT, TRIGGER_BEFORE)) return;

	//1.create deleting sequence
    get_statement_triggers(&docs_statement_triggers, TRIGGER_DELETE_EVENT, TRIGGER_BEFORE);
    for(it1=target_seq->begin();it1!=target_seq->end(); it1++)
    {
        if(target_seq_direct)
            node = *it1;
        else
            node = indirectionDereferenceCP(*it1);
		CHECKP(node);
        scn = getSchemaNode(*it1);
        if(docs_statement_triggers.find(scn->root)!=docs_statement_triggers.end())
        {
            scm_nodes_iter=scm_nodes_map.find(scn.ptr());
	        if(scm_nodes_iter==scm_nodes_map.end())
    	    {
        	    std::vector<xptr> xptr_vec;
            	scm_nodes_map_pair=scm_nodes_map.insert(std::pair<schema_node_xptr, std::vector<xptr> > (scn.ptr(), xptr_vec));
	            scm_nodes_map_pair.first->second.push_back(*it1);
    	    }
        	else
            	scm_nodes_iter->second.push_back(*it1);
        }
    }
    //2.descriptive schema tree traversal finding out triggers STATEMENT DELETE BEFORE
    scm_nodes_iter=scm_nodes_map.begin();
    for(scm_nodes_iter=scm_nodes_map.begin();scm_nodes_iter!=scm_nodes_map.end(); scm_nodes_iter++)
    {
        schema_nodes_triggers_map statement_triggers;
        //3.get all statement triggers ont the subtree (may contain trigger dublicates)
        get_statement_triggers_on_subtree(scm_nodes_iter->first, TRIGGER_DELETE_EVENT, TRIGGER_BEFORE, &statement_triggers);
        statement_triggers_iter=statement_triggers.begin();
        //iterate over the nodes with statement triggers
        for(statement_triggers_iter=statement_triggers.begin();statement_triggers_iter!=statement_triggers.end(); statement_triggers_iter++)
        {
			// descr schema might be "wider" then real data: we need to check if there are data nodes
            std::vector<xptr>::iterator xptr_iter;
            for(xptr_iter=scm_nodes_iter->second.begin();xptr_iter!=scm_nodes_iter->second.end();xptr_iter++)
            {
                //check if there is data corresponding to the schema node - if there is execute triggers
                if(getFirstDescandantBySchema(*xptr_iter, statement_triggers_iter->first)!=XNULL)
                {
                    for(set_triggers_iter=statement_triggers_iter->second.begin();set_triggers_iter!=statement_triggers_iter->second.end();set_triggers_iter++)
					{
						if(treated_triggers.find(*set_triggers_iter) == treated_triggers.end())
						{
							(*set_triggers_iter)->execute_trigger_action(XNULL, XNULL, XNULL);
							treated_triggers.insert(*set_triggers_iter);
						}
					}
                    break;
                }
            }
        }
    }

    return;
}
void apply_before_replace_for_each_statement_triggers(xptr_sequence* target_seq, bool target_seq_direct)
{
    t_scmnodes_set scmnodes;
    t_scmnodes_set::iterator scmnodes_iter;
    t_triggers_set triggers;
    t_triggers_set::iterator triggers_iter;
    xptr_sequence::iterator it1;
    std::set<trigger_cell_xptr>::iterator set_triggers_iter;

   	if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return;

    //if there are no statement level triggers at all - return
    if(!has_statement_triggers(TRIGGER_REPLACE_EVENT, TRIGGER_BEFORE)) return;

	//1. create a set of schema nodes
   	it1=target_seq->begin();
    while(it1!=target_seq->end())
    {
		xptr p=indirectionDereferenceCP(*it1);
		CHECKP(p);
        schema_node_cptr scn=getSchemaNode(p);
        scmnodes.insert(scn.ptr());
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
   	if (tr_globals::internal_auth_switch == BLOCK_AUTH_CHECK) return;

    if(after_statement_triggers.size()==0) return;

    // clear global triggers set to avoid triggers evaluation inside this evaluation
    t_triggers_set local_after_triggers;
    for(t_triggers_set::iterator i=after_statement_triggers.begin(); i!=after_statement_triggers.end(); i++)
        local_after_triggers.insert(*i);

    after_statement_triggers.clear();

    // clear updates trackings - after statement triggers are evaluated out of the update operation!
    clear_nested_updates_track_map();

    for(t_triggers_set::iterator i=local_after_triggers.begin(); i!=local_after_triggers.end(); i++)
    {
        ((trigger_cell_xptr)(*i))->execute_trigger_action(XNULL, XNULL, XNULL);
    }
}

xptr apply_per_node_triggers(xptr new_var, xptr old_var, xptr where_var, schema_node_cptr scm_node, trigger_time time, trigger_event event)
{
    switch (time){
        case TRIGGER_BEFORE:
         switch (event){
             case TRIGGER_INSERT_EVENT:
                 if (!isTriggersOn) return new_var;
                 return apply_before_insert_triggers(new_var, where_var);
//                  return triggers_test(new_var, where_var, new_name, new_type);

             case TRIGGER_DELETE_EVENT:
                 if (!isTriggersOn) return old_var;
                  return apply_before_delete_triggers(old_var, where_var, scm_node);

             case TRIGGER_REPLACE_EVENT:
                 if (!isTriggersOn) return new_var;
                 return apply_before_replace_triggers(new_var, old_var, scm_node);

             default:
                  throw SYSTEM_EXCEPTION("Bad trigger event");
        }

        case TRIGGER_AFTER:
          switch (event){
             case TRIGGER_INSERT_EVENT:
                 if (!isTriggersOn) return XNULL;
                 apply_after_insert_triggers(new_var, where_var, scm_node);
                 return XNULL;

             case TRIGGER_DELETE_EVENT:
                 if (!isTriggersOn) return XNULL;
                 apply_after_delete_triggers(old_var, where_var, scm_node);
                 return XNULL;

             case TRIGGER_REPLACE_EVENT:
                 if (!isTriggersOn) return XNULL;
                 apply_after_replace_triggers(new_var, old_var, where_var, scm_node);
                 return XNULL;

             default:
                  throw SYSTEM_EXCEPTION("Bad trigger event");
        }
        default:
            throw SYSTEM_EXCEPTION("Bad trigger time");
        }
}

void apply_per_statement_triggers(xptr_sequence* target_seq, bool target_seq_direct, xptr_sequence* upd_seq, bool upd_seq_direct, trigger_time time, trigger_event event)
{
    if (!isTriggersOn) return;

    switch (time){
        case TRIGGER_BEFORE:
         switch (event){
             case TRIGGER_INSERT_EVENT:
                 apply_before_insert_for_each_statement_triggers(target_seq, target_seq_direct, upd_seq, upd_seq_direct);
                 return;
             case TRIGGER_DELETE_EVENT:
                 apply_before_delete_for_each_statement_triggers(target_seq, target_seq_direct);
                 return;
             case TRIGGER_REPLACE_EVENT:
                 apply_before_replace_for_each_statement_triggers(target_seq, target_seq_direct);
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


trigger_cell_xptr create_trigger (enum trigger_time tr_time,
                                  enum trigger_event tr_event,
                                  xpath::PathExpression *trigger_path,
                                  enum trigger_granularity tr_gran,
                                  scheme_list* action,
                                  inserting_node innode,
                                  xpath::PathExpression *path_to_parent,
                                  doc_schema_node_xptr schemaroot,
                                  const char * trigger_title,
                                  const char* doc_name,
                                  bool is_doc)
{
    // I. Create and fill new trigger cell
    if (find_trigger(trigger_title) != XNULL)
    {
        throw USER_EXCEPTION(SE3200);
    }
    down_concurrent_micro_ops_number();

    trigger_cell_cptr trc(trigger_cell_object::create(trigger_title, schemaroot), true);

    schemaroot.modify()->full_trigger_list->add(trc.ptr());
    trc->trigger_path        = trigger_path;
    trc->trigger_event       = tr_event;
    trc->trigger_time        = tr_time;
    trc->trigger_granularity = tr_gran;

    if (rcv_tac != NULL) // recovery mode
    {
        trc->trigger_action = rcv_tac; // trigger_action_cell sequence has already been recovered from logical log
        rcv_tac = NULL;
    }
    else
    {
        trc->trigger_action = (trigger_action_cell*)malloc(sizeof(trigger_action_cell));
        trigger_action_cell* trac = trc->trigger_action;
        for (std::vector<scm_elem>::size_type i = 0; i < action->size(); i++)
        {
            trac->statement = (char*)malloc(strlen(action->at(i).internal.str)+1);
            strcpy(trac->statement,action->at(i).internal.str);

            if (i == action->size() - 1)
                trac->next = NULL;
            else
                trac->next = (trigger_action_cell*)malloc(sizeof(trigger_action_cell));

            trac = trac->next;
            RECOVERY_CRASH;
        }
    }

    // if the trigger is on before insert and statement level
    if((trc->trigger_event == TRIGGER_INSERT_EVENT) &&
       (trc->trigger_time == TRIGGER_BEFORE) &&
       (trc->trigger_granularity == TRIGGER_FOR_EACH_NODE)&&
       (path_to_parent))
    {
        trc->path_to_parent = path_to_parent;
        trc->innode = inserting_node(innode.name, innode.type);
    }
    else
    {
        trc->path_to_parent = NULL;
    }
    trc->doc_name = (char*)malloc(strlen(doc_name)+1);
    strcpy(trc->doc_name,doc_name);
    trc->is_doc=is_doc;

    hl_logical_log_trigger(tr_time, tr_event, trigger_path, tr_gran, trc->trigger_action, trc->innode, path_to_parent, trigger_title, doc_name, is_doc, true);

    //II. Execute abs path (object_path) on the desriptive schema
    t_scmnodes sobj;
    executeAbsPathExpression(schemaroot, *trigger_path, &sobj, NULL, NULL);

    //III. For each schema node found (sn_obj)
    std::vector<xptr> start_nodes;
    for (size_t i = 0; i < sobj.size(); i++)
    {
        sobj[i].modify()->trigger_list->add(trc.ptr());
        RECOVERY_CRASH;
    }

    up_concurrent_micro_ops_number();

    return trc.ptr();
}

void delete_trigger (const char *trigger_title)
{
    trigger_cell_cptr trc = find_trigger(trigger_title);
    if (trc.found())
    {
        down_concurrent_micro_ops_number();
        hl_logical_log_trigger(
            trc->trigger_time,
            trc->trigger_event,
            trc->trigger_path,
            trc->trigger_granularity,
            trc->trigger_action,
            trc->innode,
            trc->path_to_parent,
            trc->trigger_title,
            trc->doc_name,
            trc->is_doc,
            false);

        trc->drop();

        up_concurrent_micro_ops_number();
    }
}

trigger_cell_xptr find_trigger(const char* title)
{
    trigger_cell_cptr trc(title);

    if (!trc.found())
    {
        return XNULL;
    }
    else
    {
        return trc.ptr();
    }
}

xptr trigger_cell_object::execute_trigger_action(xptr parameter_new, xptr parameter_old, xptr parameter_where)
{
    xptr res_xptr = XNULL;
    PPQueryEssence* qep_tree = NULL;

    bool is_qep_built     = false;
    bool output_enabled   = tr_globals::client->disable_output();

    std::vector<built_trigger_action> built_trigger_actions_vec;
    typedef std::pair< std::string, std::vector<built_trigger_action> > trigger_actions_pair;
    lock_mode cur_lock = local_lock_mrg->get_cur_lock_mode(); // push current lock level to restore it after updates/query execution

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
            is_qep_built = false;
            qep_parameters = &(bta.parameters);
            while (trac!=NULL)
            {
                qep_tree = bta.action_qep_tree = build_subquery_qep(trac->statement, TL_ASTQEPReady);
                is_qep_built = true;

                built_trigger_actions_vec.push_back(bta);
                trac = trac->next;
            }
        }
        catch(SednaUserException &e)
        {
            if (is_qep_built)
                delete qep_tree;
            if(output_enabled)
                tr_globals::client->enable_output();
            throw e;
        }

        mapPair = built_trigger_actions.insert(trigger_actions_pair(std::string(trigger_title), built_trigger_actions_vec));
        mapIter = mapPair.first;
    }

    //executing built actions
    std::vector<built_trigger_action>::size_type i;
    unsigned action_returns_value = ((trigger_time == TRIGGER_BEFORE) && (trigger_granularity == TRIGGER_FOR_EACH_NODE)) ? 1 : 0;

    for (i = 0; i < mapIter->second.size() - action_returns_value; i++)
    {
        qep_tree = mapIter->second.at(i).action_qep_tree;
        qep_parameters = &(mapIter->second.at(i).parameters);
        set_action_parameters(parameter_new, parameter_old, parameter_where, trigger_granularity, std::string(trigger_title));

        qep_tree->open();

        if (qep_tree->is_update())
            qep_tree->execute();

        qep_tree->close();
    }

    if (action_returns_value)
    {
        qep_tree = mapIter->second.at(i).action_qep_tree;
        qep_parameters = &(mapIter->second.at(i).parameters);
        set_action_parameters(parameter_new, parameter_old, parameter_where, trigger_granularity, std::string(trigger_title));
        tuple t = tuple(1);

        // We should only get a subquery here; actually it's checked on sema
        U_ASSERT(dynamic_cast<PPSubQuery *>(qep_tree) != NULL);

        qep_tree->open();

        dynamic_cast<PPSubQuery *>(qep_tree)->next(t);
        if (!t.cells[0].is_node())
            res_xptr = XNULL;
        else
            res_xptr = t.cells[0].get_node();

        qep_tree->close();
    }
    else
    {
        res_xptr = XNULL;
    }

    if (output_enabled)
        tr_globals::client->enable_output();

    current_nesting_level--;
    local_lock_mrg->lock(cur_lock);

    if (res_xptr!=XNULL)
        CHECKP(res_xptr);

    return res_xptr;
}
