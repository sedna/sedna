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
#include "tr/mo/microsurgery.h"
#include "tr/mo/indexupdate.h"

#include "tr/log/log.h"
#include "tr/strings/strings.h"

#ifdef SE_ENABLE_TRIGGERS
#include "tr/triggers/triggers.h"
#endif /* SE_ENABLE_TRIGGERS */

#include "tr/updates/updates.h"

using namespace internal;

void readNodeInfo(xptr node_xptr, node_info_t * node_info)
{
    node_base_t n_node;

    CHECKP(node_xptr);
    memcpy(&n_node, XADDR(node_xptr), sizeof(node_base_t));

    node_info->node_xptr = node_xptr;
    node_info->snode = getBlockHeader(node_xptr)->snode;
    node_info->node_type = node_info->snode->type;
    node_info->scm_type = getScmType(node_xptr);

    if (node_info->node_type == element || node_info->node_type == attribute) {
        node_info->ns = node_info->snode->get_xmlns();
    } else if (node_info->node_type == xml_namespace) {
        node_info->ns = NSNode(node_xptr).getNamespaceLocal();
    } else if (node_info->node_type == pr_ins) {
        node_info->pi_target_size = PINode(node_xptr).getPITargetSize();
    }

    if (node_info->snode->has_text()) {
        node_info->text_size = CommonTextNode(node_xptr).getTextSize();
        node_info->text_data = CommonTextNode(node_xptr).getTextPointer();
        CHECKP(node_xptr);
    } else {
        node_info->text_size = 0;
    }

    node_info->parent_indir = n_node.pdsc;
    node_info->indirection = n_node.indir;
    node_info->left_sibling = n_node.ldsc;
    node_info->right_sibling = n_node.rdsc;
    node_info->left_sibling_indir = getIndirectionSafeCP(node_info->left_sibling);
    node_info->right_sibling_indir = getIndirectionSafeCP(node_info->right_sibling);
}

void logicalLogDelete(node_info_t * node_info, const delete_context_t * doc_info)
{
    char * text_buf = NULL;

    if (node_info->text_size <= PSTRMAXSIZE && node_info->text_size > 0) {
        text_buf = (char *) malloc((size_t) node_info->text_size);
        CHECKP(node_info->text_data);
        memcpy(text_buf, XADDR(node_info->text_data), (size_t) node_info->text_size);
    }

    /* Latter casting to size_t is done due to the fact, that all hl_ functions work with short ( < PSTRMAXSIZE ) strings
     * The only case, when the string is larger is processed with special log function. */

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
              node_info->snode->name, node_info->scm_type, text_buf, (size_t) node_info->text_size,
              (node_info->ns != NULL_XMLNS) ? node_info->ns->uri : NULL, (node_info->ns != NULL_XMLNS) ? node_info->ns->prefix : NULL, false
            );
        break;
      case comment :
        hl_logical_log_comment(
              node_info->indirection, node_info->left_sibling_indir, node_info->right_sibling_indir, node_info->parent_indir,
              text_buf, (size_t) node_info->text_size, false
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
                  text_buf, (size_t) node_info->text_size, false
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
              text_buf, (size_t) node_info->text_size, node_info->pi_target_size, false
            );
        break;
      case document :
        U_ASSERT(doc_info != NULL);
        hl_logical_log_document(node_info->indirection, doc_info->doc_name, doc_info->collection_name, false);
        break;
      default :
        throw SYSTEM_EXCEPTION("Unexpected node deletion: consistency failure.");
    }

    if (text_buf != NULL) {
        free(text_buf);
    }
}


static const delete_context_t delete_on_text_merge = {NULL, NULL, false, false, true};

void mergeSiblingTextNodes(xptr left, xptr right)
{
    if (left == XNULL || right == XNULL) {
        return ;
    }

    left = indirectionDereferenceCP(left);
    right = indirectionDereferenceCP(right);
    schema_node_cptr snode = getBlockHeaderCP(left)->snode;

    if (getNodeType(checkp(left)) == text && getNodeType(checkp(right)) == text) {
        text_node left_dsc, right_dsc;
        CHECKP(left);
        memcpy(&left_dsc, XADDR(left), sizeof(text_node));
        CHECKP(right);
        memcpy(&right_dsc, XADDR(right), sizeof(text_node));

        if (right_dsc.text.size <= maxDescriptorTextSize) {
            U_ASSERT(right_dsc.text.size > 0);
            microoperation_begin(left);

            indexDeleteNode(snode, left);
            insertTextValue(ip_tail, left, text_source_mem(right_dsc.text.data, right_dsc.text.size));
            hl_logical_log_text_edit(left_dsc.base.indir, right_dsc.text.size, false, true);
            indexAddNode(snode, left);

            microoperation_end(left);

            delete_node(right, &delete_on_text_merge);
        } else if (right_dsc.text.size <= PSTRMAXSIZE) {
            microoperation_begin(left);

            indexDeleteNode(snode, left);
            insertTextValue(ip_tail, left, text_source_node(right));
            hl_logical_log_text_edit(left_dsc.base.indir, right_dsc.text.size, false, true);
            indexAddNode(snode, left);

            microoperation_end(left);

            delete_node(right, &delete_on_text_merge);
        } else if (left_dsc.text.size <= maxDescriptorTextSize) {
            U_ASSERT(left_dsc.text.size > 0);
            microoperation_begin(right);

            indexDeleteNode(snode, right);
            insertTextValue(ip_head, right, text_source_mem(left_dsc.text.data, left_dsc.text.size));
            hl_logical_log_text_edit(right_dsc.base.indir, left_dsc.text.size, true, true);
            indexAddNode(snode, right);

            microoperation_end(right);

            delete_node(left, &delete_on_text_merge);
        } else if (left_dsc.text.size <= PSTRMAXSIZE) {
            microoperation_begin(right);

            indexDeleteNode(snode, right);
            insertTextValue(ip_head, right, text_source_node(left));
            hl_logical_log_text_edit(right_dsc.base.indir, left_dsc.text.size, true, true);
            indexAddNode(snode, right);

            microoperation_end(right);

            delete_node(left, &delete_on_text_merge);
        } else {
            microoperation_begin(left);

            indexDeleteNode(snode, left);
            pstr_long_append_tail(left, right);
            hl_logical_log_text_edit(left_dsc.base.indir, CommonTextNode(right).getTextSize(), false, true);
            indexAddNode(snode, left);

            microoperation_end(left);

            delete_node(right, &delete_on_text_merge);
        }
    }
}

bool delete_node(xptr node_xptr, const delete_context_t * context)
{
    node_info_t node_info = {node_xptr};
    node_info.snode = getBlockHeaderCP(node_xptr)->snode;
    bool children_not_deleted = false;
    xptr node_tmp = XNULL;

    if (!context->document_delete && node_info.snode->type == document) {
        throw USER_EXCEPTION(SE2036); // Document node deletion is prohibited by this function
    }

    CHECKP(node_xptr);
    node_info.parent_indir = nodeGetParentIndirection(node_xptr);

#ifdef SE_ENABLE_TRIGGERS
    if(apply_per_node_triggers(XNULL, node_xptr, indirectionDereferenceCP(node_info.parent_indir), node_info.snode.ptr(), TRIGGER_BEFORE, TRIGGER_DELETE_EVENT) == XNULL)
        return false;
    node_tmp = prepare_old_node(node_xptr, node_info.snode.ptr(), TRIGGER_DELETE_EVENT);
    CHECKP(node_xptr);
#endif /* SE_ENABLE_TRIGGERS */

#ifdef SE_ENABLE_FTSEARCH
	//TODO: full-text indexes updates need to be rewritten
	xptr xnull = XNULL;
	const xptr parent = nodeGetParent(node_xptr);
	if (parent != XNULL)
		init_ft_sequences(xnull, xnull, parent);
	CHECKP(node_xptr);
#endif /* SE_ENABLE_FTSEARCH */

    /* Delete node children */

    if (node_info.snode->has_children()) {
        xptr l, n = getLastChild(node_xptr);
        while (n != XNULL) {
            l = nodeGetLeftSibling(checkp(n));
            if (!delete_node(n, context)) { children_not_deleted = true; };
            n = l;
        }
    }

    if (children_not_deleted) { return false; }

    readNodeInfo(node_xptr, &node_info);

#ifdef SE_ENABLE_FTSEARCH
    update_delete_sequence(node_xptr, node_info.snode);
#endif /* SE_ENABLE_FTSEARCH */

    molog(("MOLOG Delete node: 0x%llx - L:0x%llx R:0x%llx ",
                  node_info.node_xptr.to_logical_int(),
                  node_info.left_sibling.to_logical_int(),
                  node_info.right_sibling.to_logical_int()));

    microoperation_begin(node_xptr);

    if (!context->no_index_update) {
        indexDeleteNode(node_info.snode, node_info.node_xptr);
    }

    logicalLogDelete(&node_info, context);

    node_info.snode.modify();

    if (node_info.text_size != 0) {
        deleteTextValue(node_info.node_xptr);
        node_info.snode->textcnt -= node_info.text_size;
    }

    nid_delete(node_info.node_xptr);
    nodeDeleteCP(node_info.node_xptr);
    node_info.snode->nodecnt--;

    updateBlockChains(node_info.node_xptr, node_info.indirection, up_delete);

    if (!context->no_index_update && node_info.node_type == text) {
        node_info.parent = indirectionDereferenceCP(node_info.parent_indir);
        indexAddNode(node_info.snode, node_info.parent);
    }

    microoperation_end(node_xptr);

    if (!context->no_merge_text_nodes) { mergeSiblingTextNodes(node_info.left_sibling_indir, node_info.right_sibling_indir); }

#ifdef SE_ENABLE_TRIGGERS
    if (IS_DATA_BLOCK(node_xptr) && node_info.node_type != document) {
        apply_per_node_triggers(XNULL, node_tmp, indirectionDereferenceCP(node_info.parent_indir), node_info.snode.ptr(), TRIGGER_AFTER, TRIGGER_DELETE_EVENT);
    }
#endif

    MOCHECK(checkBlock(block_xptr(node_xptr)));

    return true;
}


void delete_doc_node(xptr node, const char* doc_name, const char* collection_name)
{
    delete_context_t doc_info = {doc_name, collection_name, true, false, true};
    delete_node(node, &doc_info);
}


