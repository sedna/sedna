/*
 * File:  dm_accessors.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _DM_ACCESSORS_H
#define _DM_ACCESSORS_H

#include "sedna.h"
#include "tuple.h"


/// internal function for getting parent of a node
xptr get_parent_node(xptr node);

/// internal function for converting xmlscm_type to C string
const char* xmlscm_type2c_str(xmlscm_type type);

enum dm_node_kind_type {
                         nk_document,				// "document"
                         nk_element,				// "element"
                         nk_attribute,				// "attribute"
                         nk_text, 					// "text"
                         nk_namespace,				// "namespace"
                         nk_processing_instruction,	// "processing-instruction"
                         nk_comment
                       };


/**
 * Data Model Accessors according to XQuery Data Model
 */
tuple_cell        dm_base_uri    (xptr node);
tuple_cell        dm_document_uri(xptr node);
tuple_cell        dm_nilled      (xptr node);
dm_node_kind_type dm_node_kind   (xptr node);
tuple_cell        dm_node_name   (xptr node);
tuple_cell        dm_parent      (xptr node);
tuple_cell        dm_string_value(xptr node);
tuple_cell        dm_typed_value (xptr node);



/*******************************************************************************
The following functions seem to be useles because they are implemented as 
parts of path operators of physical plans. But I'm still going to think about 
their implementation... (AF)

??? dm_attributes(xptr node);
??? dm_children(xptr node);
??? dm_is_id(xptr node);
??? dm_is_idrefs(xptr node);
??? dm_namespace_bindings(xptr node);
??? dm_namespace_nodes(xptr node);
??? dm_namespaces(xptr node);
??? dm_type_name(xptr node);
??? dm_unparsed_entity_public_id(xptr node, entityname)
??? dm_unparsed_entity_system_id(xptr node, entityname)
*******************************************************************************/

tuple_cell se_node_local_name   (xptr node);
tuple_cell se_node_namespace_uri(xptr node);


#endif

