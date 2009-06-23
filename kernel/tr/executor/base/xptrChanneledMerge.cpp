/*
 * File:  xptrChanneledMerge.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/base/xptrChanneledMerge.h"

node_cell* node_cell::init(xptr node)
{
    node_cell* nc=(node_cell*)malloc(sizeof(node_cell));
    nc->node=node;
    return nc;
}
xptr xptrChanneledMerge::getNextNode()
{
    if (!top)
    {
        if (forward)
            top=merge_tree->rb_minimum(merge_tree->root);
        else
            top=merge_tree->rb_maximum(merge_tree->root);
        if(!top) return XNULL;
    }
    xptr tmp=(nodeFN)(top->obj->node);
    xptr res=top->obj->node;
    if (tmp!=XNULL)
    {
        sedna_rbtree<node_cell>::sedna_rbtree_entry* nxt=(forward)?merge_tree->rb_successor(top):merge_tree->rb_predecessor(top);
        if (nxt)
        {
            tmp_cell->node=tmp;
            if ((forward && tmp_cell->less(nxt->obj))||(!forward&&nxt->obj->less(tmp_cell)))
            {
                top->obj->node=tmp; 
            }
            else
            {
                node_cell* nc=top->obj;
                nc->node=tmp;
                merge_tree->rb_delete(top);
                merge_tree->put(nc);
                top=nxt;//(following)?merge_tree->rb_minimum(merge_tree->root):merge_tree->rb_maximum(merge_tree->root);                
            }
        }
        else
        {           
            top->obj->node=tmp;     
        }           
    }
    else
    {           
        sedna_rbtree<node_cell>::sedna_rbtree_entry* nxt=(forward)?merge_tree->rb_successor(top):merge_tree->rb_predecessor(top);
        node_cell * tmp_obj=top->obj;
        merge_tree->rb_delete(top);
        top=nxt;
        free(tmp_obj);
        //top=(following)?merge_tree->rb_minimum(merge_tree->root):merge_tree->rb_maximum(merge_tree->root);
    }   
    return res;
}
xptrChanneledMerge::xptrChanneledMerge(next_node_fn _nodeFN_, bool _forward_):nodeFN(_nodeFN_), forward(_forward_)
{
    merge_tree=sedna_rbtree<node_cell>::init();
    tmp_cell=node_cell::init(XNULL);
    top=NULL;
}
xptrChanneledMerge::~xptrChanneledMerge()
{
    clear_merge ();
    sedna_rbtree<node_cell>::sset_free(merge_tree);
    free(tmp_cell);
}
void xptrChanneledMerge::clear_merge  ()
{
    sedna_rbtree<node_cell>::sedna_rbtree_entry* tmp=merge_tree->rb_minimum(merge_tree->root);
    while (tmp!=NULL)
    {       
        free(tmp->obj);
        tmp->obj=NULL;
        tmp=merge_tree->rb_successor(tmp);
    }   
    top=NULL;
    merge_tree->clear();
}
void xptrChanneledMerge::addChannel(xptr node)
{
    if (node!=XNULL) merge_tree->put(node_cell::init(node));
}
