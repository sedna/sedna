/*
 * File:  scheme_utils.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/crmutils/scheme_utils.h"
#include "tr/vmm/vmm.h"
#include "tr/crmutils/node_utils.h"

xptr sch_right_sibling(xptr& node)
{
	CHECKP(node);
	return ((n_dsc*)XADDR(node))->rdsc;
 
}
xptr sch_left_sibling(xptr& node)
{
	CHECKP(node);
	return ((n_dsc*)XADDR(node))->ldsc;
}
xptr sch_child_dm(xptr& node)
{
	return getFirstByOrderChildNode(node);
}
t_item sch_kind(xptr& node)
{
	CHECKP(node);
	return (GETBLOCKBYNODE(node))->snode->type;
}
char* sch_name(xptr& node)
{
	CHECKP(node);
	return (GETBLOCKBYNODE(node))->snode->name;
}

