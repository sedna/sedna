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


typedef std::vector<const schema_node*> t_scmnodes_const;
typedef std::vector<schema_node*> t_scmnodes;
typedef std::set<schema_node*> t_scmnodes_set;


t_scmnodes_const descendant_or_self_nodes(schema_node *node);
t_scmnodes_const descendant_nodes        (schema_node *node);

t_scmnodes_const execute_node_test_axis_child             (schema_node *node, const NodeTest &nt);
t_scmnodes_const execute_node_test_axis_descendant        (schema_node *node, const NodeTest &nt);
t_scmnodes_const execute_node_test_axis_attribute         (schema_node *node, const NodeTest &nt);
t_scmnodes_const execute_node_test_axis_self              (schema_node *node, const NodeTest &nt);
t_scmnodes_const execute_node_test_axis_descendant_or_self(schema_node *node, const NodeTest &nt);
t_scmnodes_const execute_node_test_axis_descendant_attr   (schema_node *node, const NodeTest &nt);
t_scmnodes_const execute_node_test                        (schema_node *node, const NodeTest &nt);

t_scmnodes_const execute_abs_path_expr_rec(const t_scmnodes_const &nodes, const PathExpr &pe);

t_scmnodes_const execute_abs_path_expr(const schema_node *root, const PathExpr *path_expr);
t_scmnodes       execute_abs_path_expr(schema_node *root, const PathExpr *path_expr);

#ifdef SE_ENABLE_TRIGGERS

t_scmnodes_const descendant_or_self_nodes(schema_node *node, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes);
t_scmnodes_const descendant_nodes        (schema_node *node, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes);

t_scmnodes_const execute_node_test_axis_child             (schema_node *node, const NodeTest &nt, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes);
t_scmnodes_const execute_node_test_axis_descendant        (schema_node *node, const NodeTest &nt, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes);
t_scmnodes_const execute_node_test_axis_attribute         (schema_node *node, const NodeTest &nt, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes);
t_scmnodes_const execute_node_test_axis_self              (schema_node *node, const NodeTest &nt, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes);
t_scmnodes_const execute_node_test_axis_descendant_or_self(schema_node *node, const NodeTest &nt, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes);
t_scmnodes_const execute_node_test_axis_descendant_attr   (schema_node *node, const NodeTest &nt, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes);
t_scmnodes_const execute_node_test                        (schema_node *node, const NodeTest &nt, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes);

t_scmnodes_const execute_abs_path_expr_rec(const t_scmnodes_const &nodes, const PathExpr &pe, t_scmnodes_set* extended_nodes, t_scmnodes* extender_nodes);

t_scmnodes_const execute_abs_path_expr(const schema_node *root, const PathExpr *path_expr, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes);
t_scmnodes       execute_abs_path_expr(schema_node *root, const PathExpr *path_expr, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes);

#endif

#endif

