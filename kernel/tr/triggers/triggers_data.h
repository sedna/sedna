/*
 * File:  triggers_data.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _TRIGGERS_DATA_H
#define _TRIGGERS_DATA_H

#include <vector>
#include <string>

#include "common/sedna.h"
#include "common/u/usem.h"
#include "common/xptr.h"

#include "tr/executor/base/XPathOnSchema.h"
#include "tr/executor/xqops/PPXptr.h"
#include "tr/cat/simplestream.h"

#include "tr/triggers/trigger_types.h"

extern bool isTriggersOn;

inline const char*
trigger_event2string(trigger_event te)
{
    switch(te)
    {
    case TRIGGER_INSERT_EVENT:  return "INSERT";
    case TRIGGER_DELETE_EVENT:  return "DELETE";
    case TRIGGER_REPLACE_EVENT: return "REPLACE";
    default: throw USER_EXCEPTION2(SE1003, "Imossible case in trigger event to string conversion");
    }
}
inline const char*
trigger_time2string(trigger_time te)
{
    switch(te)
    {
    case TRIGGER_BEFORE: return "BEFORE";
    case TRIGGER_AFTER:  return "AFTER";
    default: throw USER_EXCEPTION2(SE1003, "Imossible case in trigger time to string conversion");
    }
}
inline const char*
trigger_granularity2string(trigger_granularity te)
{
    switch(te)
    {
    case TRIGGER_FOR_EACH_NODE:      return "FOR EACH NODE";
    case TRIGGER_FOR_EACH_STATEMENT: return "FOR EACH STATEMENT";
    default: throw USER_EXCEPTION2(SE1003, "Imossible case in trigger granularity to string conversion");
    }
}

extern trigger_action_cell *rcv_tac; // for recovery purposes
typedef std::vector<PPXptr*> qep_parameters_vec;
struct trigger_cell_object : public catalog_object {

public:

    /* Common catalog object interface */

    static const int magic = 0x037;
    int get_magic() { return magic; };
    void serialize_data(se_simplestream &stream);
    void deserialize_data(se_simplestream &stream);
    void drop();

    /* Fields */
    char * trigger_title; /* persistent string */

    /*
     * schemaroot - pointer to a document, which trigger is created for.
     * doc_name, is_doc - the name of the document (or collection), which the index was created for.
     * Latters are calculateble, so I don't think we should store it.
     */
    doc_schema_node_xptr schemaroot; /* persistent */
    char * doc_name;                 /* persistent string */
    bool is_doc;                     /* persistent */

    /* trigger type */
    enum trigger_event trigger_event; /* persistent */
    enum trigger_time trigger_time; /* persistent */
    enum trigger_granularity trigger_granularity; /* persistent */

    /* trigger action */
    trigger_action_cell* trigger_action; /* persistent special */

    xpath::PathExpression *trigger_path;   /* persistent special */
    xpath::PathExpression *path_to_parent; /* persistent special */
    inserting_node innode;    /* persistent special */

    bool fits_to_trigger(schema_node_cptr snode);
    bool fits_to_trigger_path_to_parent(schema_node_cptr parent);

    xptr execute_trigger_action(xptr parameter_new, xptr parameter_old, xptr parameter_where);

    inline trigger_cell_object() {};

    inline trigger_cell_object(const char * _title,
                               const doc_schema_node_xptr _schemaroot) : schemaroot(_schemaroot),
                                                                         doc_name(NULL),
                                                                         is_doc(false),
                                                                         trigger_event(TRIGGER_INSERT_EVENT),
                                                                         trigger_time(TRIGGER_BEFORE),
                                                                         trigger_granularity(TRIGGER_FOR_EACH_NODE),
                                                                         trigger_action(NULL),
                                                                         trigger_path(NULL),
                                                                         path_to_parent(NULL)
    {
        innode.name = NULL;
        this->trigger_title = cat_strcpy(this, _title);
    }

    static catalog_object_header * create(const char * _title,
                                          const doc_schema_node_xptr _schemaroot)
    {
        trigger_cell_object * obj =
          new (cat_malloc_context(CATALOG_PERSISTENT_CONTEXT, sizeof(trigger_cell_object)))
          trigger_cell_object(_title, _schemaroot);

        catalog_object_header * header = catalog_create_object(obj);

        catalog_set_name(catobj_triggers, _title, header);
        return header;
    }
};

extern qep_parameters_vec* qep_parameters;

/* Inits metadata library */
void triggers_on_session_begin();
void triggers_on_session_end();

void triggers_on_transaction_begin(bool isRecovery);
void triggers_on_transaction_end(bool is_commit);

void triggers_on_statement_begin();
void triggers_on_statement_end();


/*
 * Counted pointer for index cell.
 */

struct trigger_cell_cptr : public catalog_cptr_template<trigger_cell_object> {
    explicit inline trigger_cell_cptr (catalog_object_header * aobj, bool writable = false) :
        catalog_cptr_template<trigger_cell_object>(aobj, writable) {} ;
    explicit inline trigger_cell_cptr (const char * title, bool write_mode = false) :
        catalog_cptr_template<trigger_cell_object>(catalog_find_name(catobj_triggers, title), write_mode) {};
    inline trigger_cell_cptr (const xptr p, bool writable = false) :
        catalog_cptr_template<trigger_cell_object>(p, writable) {};
};

#endif /* _TRIGGERS_DATA_H */
