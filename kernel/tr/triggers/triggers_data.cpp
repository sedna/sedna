/*
 * File:  trigger_data.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string.h>
#include <sstream>

#include "common/sedna.h"

#include "tr/triggers/triggers_data.h"
#include "tr/triggers/triggers_utils.h"
#include "common/errdbg/exceptions.h"
#include "tr/vmm/vmm.h"
#include "tr/executor/base/tuple.h"
#include "tr/log/log.h"
#include "tr/structures/schema.h"
#include "tr/xqp/XQuerytoLR.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/cat/catstore.h"

#include "tr/structures/nodeutils.h"

//using namespace std;

//Global
qep_parameters_vec* qep_parameters = NULL;

trigger_action_cell *rcv_tac = NULL; // for recovery purposes
bool isTriggersOn = true;

static bool triggers_initialized = false;

//inits metadata library
void triggers_on_session_begin()
{
    triggers_initialized = true;
}

void triggers_on_session_end()
{
    if (!triggers_initialized) return;
    triggers_initialized = false;
}

void triggers_on_transaction_begin(bool isRecovery)
{
    isTriggersOn = !isRecovery;
}

void triggers_on_transaction_end(bool is_commit)
{
    isTriggersOn = is_commit;
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

bool trigger_cell_object::fits_to_trigger(schema_node_cptr snode)
{
    t_scmnodes objs;
    executePathExpression(snode->root, *trigger_path, &objs, NULL, NULL);
    t_scmnodes::iterator it=objs.begin();
    while (it!=objs.end())
    {
        if (*it==snode.ptr())
            return true;
        it++;
    }
    return false;
}

bool trigger_cell_object::fits_to_trigger_path_to_parent(schema_node_cptr parent)
{
    t_scmnodes objs;
    executePathExpression(parent->root, *path_to_parent, &objs, NULL, NULL);
    t_scmnodes::iterator it=objs.begin();
    while (it!=objs.end())
    {
        if (*it==parent.ptr())
            return true;
        it++;
    }
    return false;
}


void trigger_cell_object::serialize_data(se_simplestream &stream)
{
    U_ASSERT(schemaroot != XNULL);
    cs_set_hint(schemaroot);

    stream.write_string(trigger_title);
    stream.write(&schemaroot, sizeof(doc_schema_node_xptr));
    stream.write_string(doc_name);
    stream.write(&is_doc, sizeof(bool));
    stream.write(&trigger_event, sizeof(enum trigger_event));
    stream.write(&trigger_time, sizeof(enum trigger_time));
    stream.write(&trigger_granularity, sizeof(enum trigger_granularity));

    for (trigger_action_cell *i = trigger_action; i != NULL; i = i->next)
    {
        stream.write_string(i->statement);
    }

    void * n = NULL;
    stream.write(&n, sizeof(void *));

    if (trigger_path) {
        stream.write_string(trigger_path->toLRString().c_str());
    } else {
        stream.write_string(NULL);
    }

    if (path_to_parent) {
        stream.write_string(path_to_parent->toLRString().c_str());
    } else {
        stream.write_string(NULL);
    }

    stream.write_string(innode.name);
    stream.write(&innode.type, sizeof(t_item));

//    stream.write(&err_cntr, sizeof(int));
}

void trigger_cell_object::deserialize_data(se_simplestream &stream)
{
    setDefaultSpace(catalog_space_base);

    trigger_title = (char *) cat_malloc(this, stream.read_string_len());
    stream.read_string(SSTREAM_SAVED_LENGTH, trigger_title);
    stream.read(&schemaroot, sizeof(doc_schema_node_xptr));
    doc_name = (char *) cat_malloc(this, stream.read_string_len());
    stream.read_string(SSTREAM_SAVED_LENGTH, doc_name);
    stream.read(&is_doc, sizeof(bool));
    stream.read(&trigger_event, sizeof(enum trigger_event));
    stream.read(&trigger_time, sizeof(enum trigger_time));
    stream.read(&trigger_granularity, sizeof(enum trigger_granularity));

    trigger_action_cell *i = NULL, *j;
    trigger_action = NULL;

    while (* (void **) stream.get_content() != NULL) {
        j = i;
        i = (trigger_action_cell *) cat_malloc(this, sizeof(trigger_action_cell));
        i->next = NULL;
        i->statement = (char *) cat_malloc(this, stream.read_string_len());
        stream.read_string(SSTREAM_SAVED_LENGTH, i->statement);
        if (trigger_action == NULL) { trigger_action = i; }
        if (j != NULL) { j->next = i; }
    }

    stream.read(&i, sizeof(void *));

    char* trigger_path_str = NULL;
    char* path_to_parent_str = NULL;
    se_size_t len;

    if ((len = stream.read_string_len()) != 0)
        trigger_path_str = (char *)malloc(len);
    stream.read_string(SSTREAM_SAVED_LENGTH, trigger_path_str);

    if (trigger_path_str != NULL) {
        trigger_path = new xpath::PathExpression(trigger_path_str, NULL);
        free(trigger_path_str);
    } else {
        trigger_path = NULL;
    }

    if ((len = stream.read_string_len()) != 0)
        path_to_parent_str = (char *)malloc(len);
    stream.read_string(SSTREAM_SAVED_LENGTH, path_to_parent_str);

    if (path_to_parent_str != NULL) {
        path_to_parent = new xpath::PathExpression(path_to_parent_str, NULL);
        free(path_to_parent_str);
    } else {
        path_to_parent = NULL;
    }

    innode.name = (char *) cat_malloc(this, stream.read_string_len());
    stream.read_string(SSTREAM_SAVED_LENGTH, innode.name);
    stream.read(&innode.type, sizeof(t_item));

    popDefaultSpace();
}

void trigger_cell_object::drop()
{
    schemaroot.modify()->delete_trigger(p_object);
    catalog_delete_name(catobj_triggers, this->trigger_title);
    cs_free(p_object);
    catalog_delete_object(this);
}

