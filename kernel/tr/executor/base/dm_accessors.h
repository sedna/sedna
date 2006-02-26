/*
 * File:  dm_accessors.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _DM_ACCESSORS_H
#define _DM_ACCESSORS_H

#include "PPBase.h"


/// internal function for getting parent of a node
xptr get_parent_node(xptr node);

/**
 * Data Model Accessors according to XQuery Data Model
 */

tuple_cell dm_base_uri		(xptr node);
tuple_cell dm_node_name		(xptr node);
tuple_cell dm_parent		(xptr node);
tuple_cell dm_string_value	(xptr node);
tuple_cell dm_typed_value	(xptr node);
tuple_cell dm_type_name		(xptr node);
tuple_cell dm_nilled		(xptr node);
tuple_cell dm_document_uri	(xptr node);

/*******************************************************************************
The following functions seem to be useles because they are implemented as 
parts of path operators of physical plans. But I'm still going to think about 
their implementation...

??? dm_children(xptr node);
??? dm_attributes(xptr node);
??? dm_namespaces(xptr node);
*******************************************************************************/


/// deprecated accessor (was in previous versions of Data Model)
enum dm_node_kind_type {
                         nk_document,				// "document"
                         nk_element,				// "element"
                         nk_attribute,				// "attribute"
                         nk_text, 					// "text"
                         nk_namespace,				// "namespace"
                         nk_processing_instruction,	// "processing-instruction"
                         nk_comment
                       };

dm_node_kind_type dm_node_kind(xptr node);






#endif

