/*
 * File:  modelete.h
 *
 * Contains front-end (high-level) delete operations for Sedna internal data representation
 *
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/mo/mo.h"

#include "tr/mo/microoperations.h"
#include "tr/mo/indirection.h"
#include "tr/crmutils/node_utils.h"
#include "tr/mo/microsurgery.h"

#include "tr/log/log.h"
#include "tr/mo/indexupdate.h"
#include "tr/triggers/triggers.h"


void readNodeInfo(xptr node_xptr, node_info_t * node_info)
{
    n_dsc n_node;

    CHECKP(node_xptr);
    memcpy(&n_node, XADDR(node_xptr), sizeof(n_dsc));

    node_info->node_xptr = node_xptr;
    node_info->snode = getBlockHeader(node_xptr)->snode;
    node_info->node_type = node_info->snode->type;

    if (node_info->node_type == element) {
        node_info->scm_type = ((e_dsc *) XADDR(node_xptr))->type;
    } else if (node_info->node_type == attribute) {
        node_info->scm_type = ((a_dsc *) XADDR(node_xptr))->type;
    }

    if (node_info->node_type == element || node_info->node_type == attribute) {
        node_info->ns = node_info->snode->get_xmlns();
    } else if (node_info->node_type == xml_namespace) {
        node_info->ns = xmlns_touch(((ns_dsc*) XADDR(node_xptr))->ns);
    } else if (node_info->node_type == pr_ins) {
        node_info->pi_target_size = ((pi_dsc*) XADDR(node_xptr))->target;
    }

    if (node_info->snode->has_text()) {
        t_dsc t_node;

        memcpy(&t_node, XADDR(node_xptr), sizeof(t_dsc));
        node_info->text_size = t_node.size;

        if (node_info->text_size == 0) {
            node_info->text_data = XNULL;
        } else if (node_info->text_size <= PSTRMAXSIZE) {
            node_info->text_data = textDereferenceCP(t_node.data);
        } else {
            node_info->text_data = t_node.data;
        }
    } else {
        node_info->text_size = 0;
    }

    node_info->indirection = n_node.indir;
    node_info->left_sibling = n_node.ldsc;
    node_info->right_sibling = n_node.rdsc;
    node_info->left_sibling_indir = getIndirectionSafeCP(node_info->left_sibling);
    node_info->right_sibling_indir = getIndirectionSafeCP(node_info->right_sibling);
    node_info->parent_indir = n_node.pdsc;
}

void logicalLogDelete(node_info_t * node_info)
{
    char * text_buf = NULL;

    if (node_info->text_size <= PSTRMAXSIZE && node_info->text_size > 0) {
        text_buf = (char *) malloc(node_info->text_size);
        CHECKP(node_info->text_data);
        memcpy(text_buf, XADDR(node_info->text_data), (size_t) node_info->text_size);
    }

    switch (node_info->node_type) {
      case element :
        hl_logical_log_element(
              node_info->indirection, node_info->left_sibling_indir, node_info->right_sibling_indir, node_info->parent_indir,
              node_info->snode->name, node_info->scm_type,
              (node_info->ns != NULL_XMLNS) ? node_info->ns->uri : NULL, (node_info->ns != NULL_XMLNS) ? node_info->ns->prefix : NULL, false
            );
        break;
      case attribute :
        hl_logical_log_attribute(
              node_info->indirection, node_info->left_sibling_indir, node_info->right_sibling_indir, node_info->parent_indir,
              node_info->snode->name, node_info->scm_type, text_buf, node_info->text_size,
              (node_info->ns != NULL_XMLNS) ? node_info->ns->uri : NULL, (node_info->ns != NULL_XMLNS) ? node_info->ns->prefix : NULL, false
            );
        break;
      case comment :
        hl_logical_log_comment(
              node_info->indirection, node_info->left_sibling_indir, node_info->right_sibling_indir, node_info->parent_indir,
              text_buf, node_info->text_size, false
            );
        break;
      case text :
        if (text_buf == NULL && node_info->text_size > 0) {
            hl_logical_log_text(
                  node_info->indirection, node_info->left_sibling_indir, node_info->right_sibling_indir, node_info->parent_indir,
                  node_info->text_data, false
                );
        } else {
            hl_logical_log_text(
                  node_info->indirection, node_info->left_sibling_indir, node_info->right_sibling_indir, node_info->parent_indir,
                  text_buf, node_info->text_size, false
                );
        }
        break;
      case xml_namespace :
        hl_logical_log_namespace(
              node_info->indirection, node_info->left_sibling_indir, node_info->right_sibling_indir, node_info->parent_indir,
              (node_info->ns != NULL_XMLNS) ? node_info->ns->uri : NULL, (node_info->ns != NULL_XMLNS) ? node_info->ns->prefix : NULL, false
            );
        break;
      case pr_ins :
        hl_logical_log_pi(
              node_info->indirection, node_info->left_sibling_indir, node_info->right_sibling_indir, node_info->parent_indir,
              text_buf, node_info->text_size, node_info->pi_target_size, false
            );
        break;
      case document :
        break;
      default :
        throw SYSTEM_EXCEPTION("Unexpected node deletion: consistency failure.");
    }

    if (text_buf != NULL) {
        free(text_buf);
    }
}


void mergeSiblingTextNodes(xptr left, xptr right)
{
    if (left == XNULL || right == XNULL) {
        return ;
    }

    left = indirectionDereferenceCP(left);
    right = indirectionDereferenceCP(right);

    if (getBlockHeaderCP(left)->snode->type == text && getBlockHeaderCP(right)->snode->type == text) {
        t_dsc left_dsc, right_dsc;
        CHECKP(left);
        memcpy(&left_dsc, XADDR(left), sizeof(t_dsc));
        CHECKP(right);
        memcpy(&right_dsc, XADDR(right), sizeof(t_dsc));

        if (right_dsc.size <= PSTRMAXSIZE) {
            microoperation_begin(left);
            insertTextValue(ip_tail, left, &right_dsc.data, right_dsc.size, text_doc);
            hl_logical_log_text_edit(left, right_dsc.size, false, true);
            microoperation_end(left);

            delete_node(right);
        } else if (left_dsc.size <= PSTRMAXSIZE) {
            microoperation_begin(right);
            insertTextValue(ip_head, right, &left_dsc.data, left_dsc.size, text_doc);
            hl_logical_log_text_edit(right, left_dsc.size, true, true);
            microoperation_end(right);

            delete_node(left);
        } else {
            microoperation_begin(left);
            update_idx_delete_text(left);
            pstr_long_append_tail(left, right);
            hl_logical_log_text_edit(left, right_dsc.size, false, true);
            update_idx_add_text(left);
            microoperation_end(left);

            delete_node(right);
        }
    }
}


bool delete_node(xptr node_xptr, bool is_drop_document)
{
    node_info_t node_info = {node_xptr};
    node_info.snode = getBlockHeaderCP(node_xptr)->snode;
    bool children_not_deleted = false;
    xptr node_tmp;

    if (!is_drop_document && node_info.snode->type == document) {
        throw USER_EXCEPTION(SE2036); // Document node deletion is prohibited by this function
    }

    readNodeInfo(node_xptr, &node_info);

#ifdef SE_ENABLE_TRIGGERS
    if(apply_per_node_triggers(XNULL, node_xptr, indirectionDereferenceCP(node_info.parent_indir), node_info.snode.ptr(), TRIGGER_BEFORE, TRIGGER_DELETE_EVENT) == XNULL)
        return false;
    node_tmp = prepare_old_node(node_xptr, node_info.snode.ptr(), TRIGGER_DELETE_EVENT);
    CHECKP(node_xptr);
#endif


    /* Delete node children */

    if (node_info.snode->has_children()) {
        xptr l, n = getLastByOrderChildNode(node_xptr);
        while (n != XNULL) {
            l = getLeftSiblingCP(n);
            if (!delete_node(n)) { children_not_deleted = true; };
            n = l;
        }
    }

    if (children_not_deleted) { return false; }

    readNodeInfo(node_xptr, &node_info);

    molog(("MOLOG Delete node: 0x%llx - L:0x%llx R:0x%llx ",
                  node_info.node_xptr.to_logical_int(),
                  node_info.left_sibling.to_logical_int(),
                  node_info.right_sibling.to_logical_int()));

    microoperation_begin(node_xptr);

    logicalLogDelete(&node_info);

    node_info.snode.modify();

    if (node_info.text_size != 0) {
        deleteTextValue(node_info.node_xptr, true);
        node_info.snode->textcnt -= node_info.text_size;
    }

    nid_delete(node_info.node_xptr);
    nodeDeleteCP(node_info.node_xptr);
    node_info.snode->nodecnt--;

    updateBlockChains(node_info.node_xptr, node_info.indirection, up_delete);

    microoperation_end(node_xptr);

    mergeSiblingTextNodes(node_info.left_sibling_indir, node_info.right_sibling_indir);

#ifdef SE_ENABLE_TRIGGERS
    if (IS_DATA_BLOCK(node_xptr) && node_info.node_type != document) {
        apply_per_node_triggers(XNULL, node_tmp, indirectionDereferenceCP(node_info.parent_indir), node_info.snode.ptr(), TRIGGER_AFTER, TRIGGER_DELETE_EVENT);
    }
#endif

    MOCHECK(checkBlock(block_xptr(node_xptr)));

    return true;
}


void delete_doc_node(xptr node)
{
    delete_node(node, true);
}


