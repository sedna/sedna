/*
 * File:  dm_accessors.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _DM_ACCESSORS_H
#define _DM_ACCESSORS_H

#include "common/sedna.h"

#include "tr/structures/nodeinterface.h"
#include "tr/executor/base/tuple.h"
#include "tr/executor/base/dynamic_context.h"


/// internal function for converting xmlscm_type to C string
const char* xmlscm_type2c_str(xmlscm_type type);

/**
 * Data Model Accessors according to XQuery Data Model
 */
tuple_cell dm_base_uri    (Node node, dynamic_context *cxt);
tuple_cell dm_document_uri(Node node);
tuple_cell dm_nilled      (Node node);
tuple_cell dm_node_name   (Node node);
tuple_cell dm_parent      (Node node);
tuple_cell dm_string_value(Node node);
tuple_cell dm_typed_value (Node node);

//void se_get_in_scope_namespaces(Node node, std::vector<xmlns_ptr> &result, dynamic_context *cxt);


/*
 *  Unimplemented accessors
 * These accessors are not used in xquery language, nor seem to ever be used.
 *
 * AF comment: The following functions seem to be useless because they are implemented
 * as parts of path operators of physical plans. But I'm still going to think about
 * their implementation...
 */

/*
dm_node_kind_type dm_node_kind   (Node node);
??? dm_attributes(xptr node);
??? dm_children(xptr node);
??? dm_is_id(xptr node);
??? dm_is_idrefs(xptr node);
??? dm_namespace_bindings(xptr node);
??? dm_namespace_nodes(xptr node);
??? dm_type_name(xptr node);
??? dm_unparsed_entity_public_id(xptr node, entityname)
??? dm_unparsed_entity_system_id(xptr node, entityname)
*/

tuple_cell se_node_local_name   (xptr node);
tuple_cell se_node_namespace_uri(xptr node);

#endif

