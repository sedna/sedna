/*
 * File:  XPathOnSchema.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _XPATHONSCHEMA_H
#define _XPATHONSCHEMA_H

#include <vector>
#include <set>

#include "common/sedna.h"

#include "tr/structures/schema.h"
#include "tr/executor/base/XPath.h"


typedef std::vector<schema_node_xptr>  t_scmnodes_const;
typedef std::vector<schema_node_xptr>  t_scmnodes;
typedef std::set<schema_node_xptr>     t_scmnodes_set;

t_scmnodes_const execute_abs_path_expr(schema_node_cptr root,
                                       const xpath::PathExpression * path_expr,
                                       t_scmnodes_set* extended_nodes = NULL,
                                       t_scmnodes_set* extender_nodes = NULL);

t_scmnodes_const execute_node_test(schema_node_cptr node,
                                   const xpath::NodeTest & nt,
                                   t_scmnodes_set* extended_nodes = NULL,
                                   t_scmnodes_set* extender_nodes = NULL);



#endif /* _XPATHONSCHEMA_H */

