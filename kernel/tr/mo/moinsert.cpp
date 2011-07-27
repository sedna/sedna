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
#include "tr/mo/nodemoutils.h"

#include "tr/log/log.h"

#include "common/errdbg/event_log.h"

using namespace internal;

#define CHECK_CONSTRAINT(c) if (!(c)) { throw SYSTEM_EXCEPTION("Bad parameters in microoperation"); }

xptr last_inserted_node_indirection = XNULL;

inline
void check_ns_constraints_on_insert(xmlns_ptr ns)
{
    U_ASSERT(ns == NULL_XMLNS || (ns->prefix != NULL && ns->uri != NULL && strlen(ns->uri) != 0));
}

inline
void check_common_constraints(const xptr &left_sib, const xptr &right_sib, const xptr &parent)
{
    CHECK_CONSTRAINT((right_sib != XNULL) || (left_sib != XNULL) || (parent != XNULL));
    CHECK_CONSTRAINT((right_sib == XNULL) || (left_sib == XNULL) || (nodeGetRightSibling(checkp(left_sib)) == right_sib));
    CHECK_CONSTRAINT((parent == XNULL) || (left_sib == XNULL) || (nodeGetParent(checkp(left_sib)) == parent));
    CHECK_CONSTRAINT((parent == XNULL) || (right_sib == XNULL) || (nodeGetParent(checkp(right_sib)) == parent));
}

void find_relatives(node_info_t &node_info)
{
    if (node_info.left_sibling != XNULL) {
        CHECKP(node_info.left_sibling);

        if (node_info.right_sibling == XNULL) {
            node_info.right_sibling = nodeGetRightSibling(node_info.left_sibling);
        }

        if (node_info.parent == XNULL) {
            node_info.parent_indir = nodeGetParentIndirection(node_info.left_sibling);
            node_info.parent = indirectionDereferenceCP(node_info.parent_indir);
        }
    } else if (node_info.right_sibling != XNULL) {
        CHECKP(node_info.right_sibling);

        if (node_info.left_sibling == XNULL) {
            node_info.left_sibling = nodeGetLeftSibling(node_info.right_sibling);
        }

        if (node_info.parent == XNULL) {
            node_info.parent_indir = nodeGetParentIndirection(node_info.right_sibling);
            node_info.parent = nodeGetParent(node_info.right_sibling);
        }
    } else if (node_info.parent != XNULL) {
        CHECKP(node_info.parent);

        if (node_info.node_type == attribute) {
            if (getAnyChild(node_info.parent) != XNULL) {
                node_info.left_sibling = getAnyAttributeChild(node_info.parent);

                if (node_info.left_sibling == XNULL) {
                    node_info.left_sibling = getLastNamespaceChild(node_info.parent);
                }

                if (node_info.left_sibling == XNULL) {
                    node_info.right_sibling = getFirstChild(node_info.parent);
                } else {
                    node_info.right_sibling = nodeGetRightSibling(checkp(node_info.left_sibling));
                }
            }
        } else if (node_info.node_type == xml_namespace) {
            if (getAnyChild(node_info.parent) != XNULL) {
                node_info.left_sibling = getLastNamespaceChild(node_info.parent);
                if (node_info.left_sibling == XNULL) {
                    node_info.right_sibling = getFirstChild(node_info.parent);
                } else {
                    node_info.right_sibling = nodeGetRightSibling(checkp(node_info.left_sibling));
                }
            }
        } else {
            node_info.right_sibling = getAnyDmChildrenChild(node_info.parent);
            if (node_info.right_sibling == XNULL) {
                node_info.left_sibling = getLastNonDmChildrenChild(node_info.parent);
            } else {
                node_info.left_sibling = nodeGetLeftSibling(checkp(node_info.right_sibling));
            }
        }
    }

    node_info.left_sibling_indir = getIndirectionSafeCP(node_info.left_sibling);
    node_info.right_sibling_indir = getIndirectionSafeCP(node_info.right_sibling);

    if (node_info.parent_indir == XNULL) {
        node_info.parent_indir = getIndirectionSafeCP(node_info.parent);
    }
}

inline
bool sameNodeType(xptr node, t_item type, const char* name, xmlns_ptr ns) {
    if (node == XNULL) { return false; }
    if (type == element) {
        return getSchemaNode(node)->same_node(ns, name, type);
    } else {
        return getNodeType(checkp(node)) == type;
    }
}

void insert_node_base(node_info_t &node_info, t_item type, const char* name, xmlns_ptr ns) {
    if (sameNodeType(node_info.left_sibling, type, name, ns)) {
        insertNodeWithLeftBrother(node_info.left_sibling, &node_info);
    } else if (sameNodeType(node_info.right_sibling, type, name, ns)) {
        insertNodeWithRightBrother(node_info.right_sibling, &node_info);
    } else {
        insertNodeGeneral(&node_info);
    }
}

xptr insert_element(xptr left_sib, xptr right_sib, xptr parent, const char* name, xmlscm_type type, xmlns_ptr ns)
{
    node_info_t node_info = {left_sib, right_sib, parent, element, type, ns, const_cast<char *>(name)};

    check_common_constraints(left_sib, right_sib, parent);
    check_ns_constraints_on_insert(ns);
    find_relatives(node_info);
    microoperation_begin(node_info.parent);

    insert_node_base(node_info, element, name, ns);

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

enum text_insert_t { ti_new_node, ti_addtext_after, ti_addtext_before };

xptr insert_text(xptr left_sib, xptr right_sib, xptr parent, const text_source_t source, int cdataflag)
{
    node_info_t node_info = {left_sib, right_sib, parent, text, 0, NULL_XMLNS};
    schema_node_cptr parent_snode;
    enum text_insert_t insert_type = ti_new_node;

    if (source.size > STRMAXSIZE) throw USER_EXCEPTION(SE2037);

    check_common_constraints(left_sib, right_sib, parent);
    find_relatives(node_info);
    parent_snode = getSchemaNode(node_info.parent);

    if (source.size < 1 && (IS_DATA_BLOCK(node_info.parent))) throw USER_EXCEPTION(SE2009);

    node_info.cdataflag = cdataflag;

//    if (source.size < 1 && parent != XNULL && parent_snode->type != virtual_root) throw USER_EXCEPTION(SE2009);

    microoperation_begin(node_info.parent);

    indexDeleteNode(parent_snode, node_info.parent);

    if (node_info.left_sibling != XNULL && getNodeType(checkp(node_info.left_sibling)) == text) {
        if (parent_snode->type == virtual_root) {
            insertNodeWithLeftBrother(node_info.left_sibling, &node_info);
        } else {
            node_info.node_xptr = node_info.left_sibling;
            if (source.size > 0) {
                insertTextValue(ip_tail, node_info.node_xptr, source);
            }
            insert_type = ti_addtext_after;
        }
    } else if (node_info.right_sibling != XNULL && getNodeType(checkp(node_info.right_sibling)) == text) {
        if (parent_snode->type == virtual_root) {
            insertNodeWithRightBrother(node_info.right_sibling, &node_info);
        } else {
            node_info.node_xptr = node_info.right_sibling;
            if (source.size > 0) {
                insertTextValue(ip_head, node_info.node_xptr, source);
            }
            insert_type = ti_addtext_before;
        }
    } else {
        insertNodeGeneral(&node_info);
    }

    if (insert_type == ti_new_node) {
        insertTextValue(node_info.node_xptr, source);

        if (cdataflag & cdata_section > 0) {
            ((internal::text_node *) xaddr(checkp(node_info.node_xptr)))->flags |= cdata_section;
        }
    } else {
        node_info.indirection = getIndirectionSafeCP(node_info.node_xptr);
    }

/* Update node count in schema */
    node_info.snode = getSchemaNode(node_info.node_xptr);
    node_info.snode.modify();
    if (insert_type == ti_new_node) { node_info.snode->nodecnt++; }
    node_info.snode->textcnt += source.size;

/* Update logical log */

    // TODO: CDATA flag is not stored in logical log
    if (IS_DATA_BLOCK(parent))
    {
        if (insert_type == ti_new_node) {
            CHECKP(node_info.node_xptr);
            node_text_t * desc = getTextFromAnyNode(node_info.node_xptr);
            size_t size = (size_t) nodeGetTextSize(desc);
            CHECKP(node_info.node_xptr);
            bool isPstrLongS = isPstrLong(desc);
            xptr data = nodeGetTextPointer(desc);

            if (isPstrLongS) {
                hl_logical_log_text(node_info.indirection, node_info.left_sibling_indir, node_info.right_sibling_indir,
                                    node_info.parent_indir, data, true);
            } else {
                hl_logical_log_text(node_info.indirection, node_info.left_sibling_indir, node_info.right_sibling_indir,
                                    node_info.parent_indir, (char *) XADDR(data), size, true);
            }
        } else {
            if (source.type == text_source_t::text_mem) {
                hl_logical_log_text_edit(node_info.indirection, source.u.cstr, (size_t) source.size, (insert_type == ti_addtext_before), true);
            } else {
                hl_logical_log_text_edit(node_info.indirection, tsGetActualSize(source), (insert_type == ti_addtext_before), true);
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

xptr insert_attribute(xptr left_sib, xptr right_sib, xptr parent, const char* name, xmlscm_type type, const char* value, strsize_t data_size, xmlns_ptr ns)
{
    node_info_t node_info = {left_sib, right_sib, parent, attribute, type, ns, const_cast<char *>(name)};
    schema_node_cptr parent_snode;

    check_common_constraints(left_sib, right_sib, parent);
    check_ns_constraints_on_insert(ns);
    find_relatives(node_info);

/* Special attribute user-error-level constraints */
    parent_snode = getBlockHeader(checkp(node_info.parent))->snode;
    if (data_size > PSTRMAXSIZE) { throw USER_EXCEPTION(SE2037); }
    if (parent_snode->type == document) { throw XQUERY_EXCEPTION(XPTY0004); }
    if (parent_snode->type != virtual_root && findAttribute(node_info.parent, name, ns) != XNULL) {
        throw USER_EXCEPTION(XQDY0025);
    }

    microoperation_begin(node_info.parent);

    insertNodeGeneral(&node_info);
    U_ASSERT(node_info.node_xptr != XNULL);
    insertTextValue(node_info.node_xptr, text_source_mem(value, data_size));

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

    insert_node_base(node_info, xml_namespace, NULL, NULL);

    U_ASSERT(node_info.node_xptr != XNULL);

    WRITEP(node_info.node_xptr);
    getNamespaceNode(node_info.node_xptr)->ns = node_info.snode->root->xmlns_register(ns);
    ns = xmlns_touch(getNamespaceNode(node_info.node_xptr)->ns);

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

    insert_node_base(node_info, node_info.node_type, NULL, NULL);

    U_ASSERT(node_info.node_xptr != XNULL);

    insertTextValue(node_info.node_xptr, text_source_mem(value, size));

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

xptr insert_pi(xptr left_sib, xptr right_sib, xptr parent, const char* target, size_t tsize, const char* data, size_t dsize) {
    strsize_t full_size = tsize + 1 + dsize;
    char * value = (char *) malloc(full_size);
    node_info_t node_info = {left_sib, right_sib, parent, pr_ins};

    memcpy(value, target, tsize);
    value[tsize] = ' ';
    if (dsize > 0) { memcpy(value + tsize + 1, data, dsize); }

    __insert_common_text_node(node_info, value, full_size);

    getPINode(node_info.node_xptr)->separator = tsize;

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
    node_base_t* node = NULL;
    xptr nodex = XNULL;
    xptr result = XNULL;

    if (doc_snode->get_magic() == col_schema_node_object::magic) {
        col_snode = doc_snode.ptr();
    }

    if (col_snode.found() && (col_snode->eblk != XNULL)) {
        block = col_snode->eblk;
        /* Update the last block pointer. */
        CHECKP(block);
        while (getBlockHeader(block)->nblk != XNULL) {
            block = getBlockHeader(block)->nblk;
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
    if (getBlockHeader(checkp(block))->free_first == 0) {
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
    insertTextValue(nodex, text_source_cstr(doc_name));

    if (doc_snode->persistent) {
        hl_logical_log_document(result, doc_name, collection_name, true);
        up_concurrent_micro_ops_number();
    }

    return result;
}


