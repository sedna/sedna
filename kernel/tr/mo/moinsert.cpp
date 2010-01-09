/*
 * File:  modelete.h
 *
 * Contains front-end (high-level) insert operations for Sedna internal data representation
 *
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/mo/mo.h"

#include "tr/mo/microoperations.h"
#include "tr/mo/microsurgery.h"
#include "tr/mo/indirection.h"
#include "tr/mo/indexupdate.h"
#include "tr/mo/blocks.h"
#include "tr/mo/modebug.h"

#include "tr/log/log.h"
#include "tr/crmutils/node_utils.h"

#include "common/errdbg/event_log.h"

#define CHECK_CONSTRAINT(c) U_ASSERT(c)
//#define CHECK_CONSTRAINT(c) if (!(c)) { throw SYSTEM_EXCEPTION("Bad parameters"); }

xptr last_inserted_node_indirection = XNULL;

inline void check_common_constraints(const xptr &left_sib, const xptr &right_sib, const xptr &parent)
{
    CHECK_CONSTRAINT((right_sib != XNULL) || (left_sib != XNULL) || (parent != XNULL));
    CHECK_CONSTRAINT((right_sib == XNULL) || (left_sib == XNULL) || (getRightSiblingCP(left_sib) == right_sib));
    CHECK_CONSTRAINT((parent == XNULL) || (left_sib == XNULL) || (getParentCP(left_sib) == parent));
    CHECK_CONSTRAINT((parent == XNULL) || (right_sib == XNULL) || (getParentCP(right_sib) == parent));
}

void find_relatives(node_info_t &node_info)
{
    if (node_info.left_sibling != XNULL) {
        CHECKP(node_info.left_sibling);

        if (node_info.right_sibling == XNULL) {
            node_info.right_sibling = getRightSibling(node_info.left_sibling);
        }

        if (node_info.parent == XNULL) {
            node_info.parent_indir = getParentIndirection(node_info.left_sibling);
            node_info.parent = indirectionDereferenceCP(node_info.parent_indir);
        }
    } else if (node_info.right_sibling != XNULL) {
        CHECKP(node_info.right_sibling);

        if (node_info.left_sibling == XNULL) {
            node_info.left_sibling = getLeftSibling(node_info.right_sibling);
        }

        if (node_info.parent == XNULL) {
            node_info.parent_indir = getParentIndirection(node_info.right_sibling);
            node_info.parent = getParentCP(node_info.right_sibling);
        }
    } else if (node_info.parent != XNULL) {
        CHECKP(node_info.parent);
        int child_count = getChildCountSP(node_info.parent);

        if (node_info.node_type == attribute) {
            n_dsc * parent = (n_dsc *) XADDR(node_info.parent);
            if (giveAnyChild(parent, child_count) != XNULL) {
                node_info.left_sibling = giveAnyAttributeChild(parent, child_count);

                if (node_info.left_sibling == XNULL) {
                    node_info.left_sibling = getLastNamespaceChild(parent, child_count);
                }

                if (node_info.left_sibling == XNULL) {
                    node_info.right_sibling = giveFirstByOrderChild(node_info.parent, child_count);
                } else {
                    node_info.right_sibling = getRightSiblingCP(node_info.left_sibling);
                }
            }
        } else if (node_info.node_type == xml_namespace) {
            n_dsc * parent = (n_dsc *) XADDR(node_info.parent);
            if (giveAnyChild(parent, child_count) != XNULL) {
                node_info.left_sibling = getLastNamespaceChild(parent, child_count);
                if (node_info.left_sibling == XNULL) {
                    node_info.right_sibling = giveFirstByOrderChild(node_info.parent, child_count);
                } else {
                    node_info.right_sibling = getRightSiblingCP(node_info.left_sibling);
                }
            }
        } else {
            node_info.right_sibling = giveAnyDmChildrenChild((n_dsc*)XADDR(node_info.parent), child_count);
            if (node_info.right_sibling == XNULL) {
                node_info.left_sibling = getLastNonDmChildrenChild((n_dsc*)XADDR(node_info.parent), child_count);
            } else {
                node_info.left_sibling = getLeftSiblingCP(node_info.right_sibling);
            }
        }
    }

    node_info.left_sibling_indir = getIndirectionSafeCP(node_info.left_sibling);
    node_info.right_sibling_indir = getIndirectionSafeCP(node_info.right_sibling);

    if (node_info.parent_indir == XNULL) {
        node_info.parent_indir = getIndirectionSafeCP(node_info.parent);
    }
}

xptr insert_element(xptr left_sib, xptr right_sib, xptr parent, const char* name, xmlscm_type type, xmlns_ptr ns)
{
    node_info_t node_info = {left_sib, right_sib, parent, element, type, ns, const_cast<char *>(name)};

    check_common_constraints(left_sib, right_sib, parent);
    find_relatives(node_info);
    microoperation_begin(node_info.parent);

/* Insert node */
    if (node_info.left_sibling != XNULL && getBlockHeaderCP(node_info.left_sibling)->snode->same_node(ns, name, element)) {
        insertNodeWithLeftBrother(node_info.left_sibling, &node_info);
    } else if (node_info.right_sibling != XNULL && getBlockHeaderCP(node_info.right_sibling)->snode->same_node(ns, name, element)) {
        insertNodeWithRightBrother(node_info.right_sibling, &node_info);
    } else {
        insertNodeGeneral(&node_info);
    }

    U_ASSERT(node_info.node_xptr != XNULL);

/* Update node count in schema */
    node_info.snode.modify()->nodecnt++;

    if (IS_DATA_BLOCK(node_info.node_xptr))
    {
/* Update logical log */
        hl_logical_log_element(node_info.indirection, node_info.left_sibling_indir, node_info.right_sibling_indir, node_info.parent_indir,
                               node_info.name, node_info.scm_type, (ns != NULL)?ns->uri:NULL, (ns != NULL)?ns->prefix:NULL, true);
    } else {
/* Update temporary pointer */
//        node_info.snode->lastnode_ind = node_info.indirection;
    }

    indexAddNode(node_info.snode, node_info.node_xptr);

    microoperation_end(node_info.parent);

    molog(("MOLOG Insert element node: L:0x%llx R:0x%llx P:0x%llx -> 0x%llx",
                  node_info.left_sibling.to_logical_int(),
                  node_info.right_sibling.to_logical_int(),
                  node_info.parent.to_logical_int(),
                  node_info.node_xptr.to_logical_int()));

    MOCHECK(checkBlock(block_xptr(node_info.node_xptr)));

    last_inserted_node_indirection = node_info.indirection;
    return node_info.node_xptr;
}

xptr insert_text(xptr left_sib, xptr right_sib, xptr parent, const void* value, strsize_t size, text_type ttype)
{
    node_info_t node_info = {left_sib, right_sib, parent, text, 0, NULL_XMLNS};
    schema_node_cptr parent_snode;
    enum text_insert_t { ti_new_node, ti_addtext_after, ti_addtext_before } insert_type = ti_new_node;

    if (size < 1 && IS_DATA_BLOCK(parent)) throw USER_EXCEPTION(SE2009);
    if (size > STRMAXSIZE) throw USER_EXCEPTION(SE2037);

    check_common_constraints(left_sib, right_sib, parent);
    find_relatives(node_info);
    parent_snode = getBlockHeaderCP(node_info.parent)->snode;

    microoperation_begin(node_info.parent);

    indexDeleteNode(parent_snode, node_info.parent);

    if (node_info.left_sibling != XNULL && getBlockHeaderCP(node_info.left_sibling)->snode->type == text) {
        if (parent_snode->type == virtual_root) {
            insertNodeWithLeftBrother(node_info.left_sibling, &node_info);
        } else {
            node_info.node_xptr = node_info.left_sibling;
            if (size > 0) { insertTextValue(ip_tail, node_info.node_xptr, value, size, ttype); }
            insert_type = ti_addtext_after;
        }
    } else if (node_info.right_sibling != XNULL && getBlockHeaderCP(node_info.right_sibling)->snode->type == text) {
        if (parent_snode->type == virtual_root) {
            insertNodeWithRightBrother(node_info.right_sibling, &node_info);
        } else {
            node_info.node_xptr = node_info.right_sibling;
            if (size > 0) { insertTextValue(ip_head, node_info.node_xptr, value, size, ttype); }
            insert_type = ti_addtext_before;
        }
    } else {
        insertNodeGeneral(&node_info);
    }

    if (insert_type == ti_new_node) {
        insertTextValue(node_info.node_xptr, value, size, ttype);
    } else {
        node_info.indirection = getIndirectionSafeCP(node_info.node_xptr);
    }

/* Update node count in schema */
    node_info.snode = getBlockHeaderCP(node_info.node_xptr)->snode;
    node_info.snode.modify();
    if (insert_type == ti_new_node) { node_info.snode->nodecnt++; }
    node_info.snode->textcnt += size;

/* Update logical log */
    if (IS_DATA_BLOCK(parent))
    {
        if (insert_type == ti_new_node) {
            CHECKP(node_info.node_xptr);
            t_dsc* desc = (t_dsc*) XADDR(node_info.node_xptr);
            size_t size = (size_t) getTextSize(desc);
            xptr data = getTextPtr(desc);

            if (isPstrLong(desc)) {
                hl_logical_log_text(node_info.indirection, node_info.left_sibling_indir, node_info.right_sibling_indir,
                                    node_info.parent_indir, data, true);
            } else {
                CHECKP(data);
                hl_logical_log_text(node_info.indirection, node_info.left_sibling_indir, node_info.right_sibling_indir,
                                    node_info.parent_indir, (char *) XADDR(data), size, true);
            }
        } else {
            if (ttype==text_mem)
                hl_logical_log_text_edit(node_info.indirection, (char *)(value), size, (insert_type == ti_addtext_before), true);
            else
            {
                hl_logical_log_text_edit(node_info.indirection, size, (insert_type == ti_addtext_before), true);
            }
        }
    } else {
/* Update temporary pointer */
//        node_info.snode->lastnode_ind = node_info.indirection;
    }

    indexAddNode(parent_snode, node_info.parent);
    microoperation_end(node_info.parent);
    last_inserted_node_indirection = node_info.indirection;
    return node_info.node_xptr;
}


xptr insert_attribute(xptr left_sib, xptr right_sib, xptr parent, const char* name, xmlscm_type type, const char* value, int data_size, xmlns_ptr ns)
{
    node_info_t node_info = {left_sib, right_sib, parent, attribute, type, ns, const_cast<char *>(name)};
    schema_node_cptr parent_snode;

    check_common_constraints(left_sib, right_sib, parent);
    find_relatives(node_info);

/* Special attribute user-error-level constraints */
    parent_snode = getBlockHeaderCP(node_info.parent)->snode;
    if (parent_snode->type == document) { throw XQUERY_EXCEPTION(XPTY0004); }
    if (parent_snode->type != virtual_root && isAttributePointerSet((n_dsc *) XADDR(node_info.parent), name, (ns==NULL) ? NULL : ns->uri) != XNULL) {
        throw USER_EXCEPTION(XQDY0025);
    }

    microoperation_begin(node_info.parent);

    insertNodeGeneral(&node_info);
    U_ASSERT(node_info.node_xptr != XNULL);
    insertTextValue(node_info.node_xptr, value, data_size, text_mem);

/* Update node count in schema */
    node_info.snode.modify()->nodecnt++;
    node_info.snode->textcnt += (size_t) data_size;

    if (IS_DATA_BLOCK(node_info.node_xptr))
    {
/* Update logical log */
        hl_logical_log_attribute(node_info.indirection, node_info.left_sibling_indir, node_info.right_sibling_indir, node_info.parent_indir,
                                  node_info.name, node_info.scm_type, value, data_size,
                                 (ns != NULL) ? ns->uri : NULL, (ns != NULL) ? ns->prefix : NULL, true);
    } else {
/* Update temporary pointer */
//        node_info.snode->lastnode_ind = node_info.indirection;
    }

    indexAddNode(node_info.snode, node_info.node_xptr);
    microoperation_end(node_info.parent);
    last_inserted_node_indirection = node_info.indirection;
    return node_info.node_xptr;
}

xptr insert_namespace(xptr left_sib, xptr right_sib, xptr parent, xmlns_ptr ns)
{
    node_info_t node_info = {left_sib, right_sib, parent, xml_namespace, 0};

    check_common_constraints(left_sib, right_sib, parent);
    find_relatives(node_info);
    microoperation_begin(node_info.parent);

/* Insert node */
    if (node_info.left_sibling != XNULL && getBlockHeaderCP(node_info.left_sibling)->snode->type == xml_namespace) {
        insertNodeWithLeftBrother(node_info.left_sibling, &node_info);
    } else if (node_info.right_sibling != XNULL && getBlockHeaderCP(node_info.right_sibling)->snode->type == xml_namespace) {
        insertNodeWithRightBrother(node_info.right_sibling, &node_info);
    } else {
        insertNodeGeneral(&node_info);
    }

    U_ASSERT(node_info.node_xptr != XNULL);

    CHECKP(node_info.node_xptr);
    VMM_SIGNAL_MODIFICATION(node_info.node_xptr);
    ((ns_dsc *) XADDR(node_info.node_xptr))->ns = node_info.snode->root->xmlns_register(ns);
    ns = xmlns_touch(((ns_dsc *) XADDR(node_info.node_xptr))->ns);

/* Update node count in schema */
    node_info.snode.modify()->nodecnt++;

    if (IS_DATA_BLOCK(node_info.node_xptr))
    {
/* Update logical log */
        hl_logical_log_namespace(node_info.indirection, node_info.left_sibling_indir, node_info.right_sibling_indir, node_info.parent_indir,
                                 (ns != NULL) ? ns->uri : NULL, (ns != NULL) ? ns->prefix : NULL, true);
    } else {
/* Update temporary pointer */
//        node_info.snode->lastnode_ind = node_info.indirection;
    }

    microoperation_end(node_info.parent);
    last_inserted_node_indirection = node_info.indirection;
    return node_info.node_xptr;
}

xptr __insert_common_text_node(node_info_t &node_info, const char* value, strsize_t size)
{
    check_common_constraints(node_info.left_sibling, node_info.right_sibling, node_info.parent);
    find_relatives(node_info);
    microoperation_begin(node_info.parent);

/* Insert node */
    if (node_info.left_sibling != XNULL && getBlockHeaderCP(node_info.left_sibling)->snode->type == node_info.node_type) {
        insertNodeWithLeftBrother(node_info.left_sibling, &node_info);
    } else if (node_info.right_sibling != XNULL && getBlockHeaderCP(node_info.right_sibling)->snode->type == node_info.node_type) {
        insertNodeWithRightBrother(node_info.right_sibling, &node_info);
    } else {
        insertNodeGeneral(&node_info);
    }

    U_ASSERT(node_info.node_xptr != XNULL);

    insertTextValue(node_info.node_xptr, value, size, text_mem);

/* Update node count in schema */
    node_info.snode.modify()->nodecnt++;
    node_info.snode->textcnt += size;

    if (!IS_DATA_BLOCK(node_info.node_xptr)) {
/* Update temporary pointer */
//        node_info.snode->lastnode_ind = node_info.indirection;
    }

    indexAddNode(node_info.snode, node_info.node_xptr);
    last_inserted_node_indirection = node_info.indirection;
    return node_info.node_xptr;
}


xptr insert_comment(xptr left_sib, xptr right_sib, xptr parent, const char* value, strsize_t size) {
    node_info_t node_info = {left_sib, right_sib, parent, comment};
    __insert_common_text_node(node_info, value, size);
    if (IS_DATA_BLOCK(node_info.node_xptr)) {
/* Update logical log */
        hl_logical_log_comment(node_info.indirection, node_info.left_sibling_indir, node_info.right_sibling_indir, node_info.parent_indir, value, (size_t) size, true);
    }
    microoperation_end(node_info.parent);
    return node_info.node_xptr;
}

xptr insert_cdata(xptr left_sib, xptr right_sib, xptr parent, const char* value, strsize_t size) {
    U_ASSERT(false);
    node_info_t node_info = {left_sib, right_sib, parent, cdata};
    __insert_common_text_node(node_info, value, size);
    if (IS_DATA_BLOCK(node_info.node_xptr)) {
/* Update logical log */
        hl_logical_log_comment(node_info.indirection, node_info.left_sibling_indir, node_info.right_sibling_indir, node_info.parent_indir, value, (size_t) size, true);
    }
    microoperation_end(node_info.parent);
    return node_info.node_xptr;
}

xptr insert_pi(xptr left_sib, xptr right_sib, xptr parent, const char* target, int tsize, const char* data, strsize_t dsize) {
    strsize_t full_size = tsize + 1 + dsize;
    char * value = (char *) malloc(full_size);
    node_info_t node_info = {left_sib, right_sib, parent, pr_ins};

    memcpy(value, target, tsize);
    value[tsize] = ' ';
    if (dsize > 0) { memcpy(value + tsize + 1, data, dsize); }

    __insert_common_text_node(node_info, value, full_size);

    ((pi_dsc*)XADDR(node_info.node_xptr))->target = tsize;

    if (IS_DATA_BLOCK(node_info.node_xptr)) {
/* Update logical log */
        hl_logical_log_pi(node_info.indirection, node_info.left_sibling_indir, node_info.right_sibling_indir, node_info.parent_indir, value, (size_t) full_size, tsize, true);
    }
    microoperation_end(node_info.parent);
    free(value);
    return node_info.node_xptr;
}

xptr insert_doc_node(doc_schema_node_cptr doc_snode, const char* doc_name, const char* collection_name)
{
    xptr block = XNULL;
    col_schema_node_cptr col_snode = XNULL;
    n_dsc* node = NULL;
    xptr nodex;
    xptr result = XNULL;

    if (doc_snode->get_magic() == col_schema_node_object::magic) {
        col_snode = doc_snode.ptr();
    }

    if (col_snode.found() && (col_snode->eblk != XNULL)) {
        block = col_snode->eblk;
        /* Update the last block pointer. */
        CHECKP(block);
        while ((GETBLOCKBYNODE(block))->nblk != XNULL) {
            block = (GETBLOCKBYNODE(block))->nblk;
            col_snode->eblk = block;
            CHECKP(block);
        }
    }

    if (doc_snode->persistent) {
        down_concurrent_micro_ops_number();
    }

    /* If block doesn't exists, create it */
    if (block == XNULL) {
        block = createBlock(doc_snode.ptr());
        if (col_snode.found()) { col_snode->eblk = block; }
    }

    /* If block is full create one after it. BTW, this can only happen to collection blocks */
    if (getBlockHeaderCP(block)->free_first == 0) {
        U_ASSERT(col_snode.found());
        block = createBlock(XNULL, block);
        col_snode->eblk = block;
    }

    /* Insert node */
    node = nodeListInsertCP(block, LAST_NODE);
    nodex = ADDR2XPTR(node);
    result = createIndirectionForNewNodeCP(nodex);

    updateBlockChains(result, nodex, up_insert);

    U_ASSERT(getBlockHeader(block)->snode == doc_snode.ptr());
    doc_snode.modify()->nodecnt++;
    doc_snode->textcnt += strlen(doc_name);
    /* Initialize node NID */
    nid_create_root(nodex, doc_snode->persistent);
    /* Initialize document name, wich is actually node text */
    insertTextValue(nodex, doc_name, strlen(doc_name), text_mem);

    if (doc_snode->persistent) {
        hl_logical_log_document(result, doc_name, collection_name, true);
        up_concurrent_micro_ops_number();
    }

    return result;
}


